/*
 * Copyright 2024 Kioshi Morosin <knm@hex.lc>
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, you can obtain one at https://mozilla.org/MPL/2.0/.
 */

#ifndef LA_H
#define LA_H

#include <concepts>
#include <array>
#include <tuple>
#include <span>
#include <algorithm>
#include <numeric>
#include <functional>
#include <ostream>

namespace la {
    using dim_t = std::size_t;

    /**
     * Represents a numeric type that can be used as the underlying scalar type for a matrix.
     * This is NOT the same as `std::is_scalar`, which includes pointer types, nor is it the
     * same as `std::is_arithmetic`, which may not include user-defined types that support
     * the necessary operations otherwise.
     */
    template <class S> concept scalar_type = requires(S s1, S s2) {
        { s1 + s2 } ->  std::convertible_to<S>;
        { -s1 } -> std::convertible_to<S>;
        { s1 * s2 } -> std::convertible_to<S>;
        { s1 / s2 } -> std::convertible_to<S>;

        // zero-initialization is performed both when S is default-constructed and when 0 is
        // converted to S, so this should indicate that S is zero-initializable.
        S{ } == S{ 0 };
    };

    namespace detail {
        template <class T, dim_t> using inner_storage_t = T;

        template <class T, dim_t N> constexpr auto make_n_tuple_of() {
            return [] <dim_t ...I>(std::integer_sequence<dim_t, I...>) {
                return std::tuple<inner_storage_t<T, I>...>{ };
            }(std::make_integer_sequence<dim_t, N>{ });
        }

        template <class T, dim_t N> using storage_1d = std::invoke_result_t<decltype(&make_n_tuple_of<T, N>)>;

        template <dim_t Rows, dim_t Cols, scalar_type S> struct matrix_storage {
            storage_1d<storage_1d<S, Cols>, Rows> _v;

            template <dim_t R, dim_t C> constexpr auto& at() {
                return std::get<C>(std::get<R>(_v));
            }

            template <dim_t R, dim_t C> constexpr const auto& at() const {
                return std::get<C>(std::get<R>(_v));
            }
        };
    }

    // forward definition so we can have a forward definition for operator<<
    template <dim_t Rows, dim_t Cols, scalar_type S> class matrix;
}

template <la::dim_t Rows, la::dim_t Cols, la::scalar_type S> std::ostream& operator<<(std::ostream& os, const la::matrix<Rows, Cols, S>& m);

namespace la {
    /**
     * A matrix with fixed dimensions. The underlying data structure is undefined, but
     * should in most cases be a simple Rows*Cols array of S.
     * @tparam Rows number of rows
     * @tparam Cols number of columns
     * @tparam S type of scalars
     */
    template <dim_t Rows, dim_t Cols, scalar_type S> class matrix {
        using row_t = matrix<1, Cols, S>;
        using col_t = matrix<Rows, 1, S>;

        detail::matrix_storage<Rows, Cols, S> _v;
    public:
        static constexpr dim_t rows = Rows;
        static constexpr dim_t cols = Cols;
        using scalar_t = S;

        constexpr matrix() = default;

        /**
         * Construct a 1*Cols matrix given a list of scalars.
         */
        template <class ...E> constexpr explicit matrix(E&& ...e) {
            [&]<dim_t ...I>(std::integer_sequence<dim_t, I...>){
                ((_v.template at<0, I>() = e), ...);
            }(std::make_integer_sequence<dim_t, Cols>{ });
        }

        /**
         * Construct a Rows*Cols matrix given a list of rows.
         * @tparam C number of columns
         */
        template <dim_t C, class ...E> constexpr explicit matrix(const E(&...e)[C]) {
            auto r = std::make_tuple(e...);
            [&]<dim_t ...J>(std::integer_sequence<dim_t, J...>){
                ((_v.template at<J / C, J % C>() = std::get<J / C>(r)[J % C]), ...);
            }(std::make_integer_sequence<dim_t, sizeof...(E) * C>{ });
        }

        /**
         * Access a cell of the matrix
         * @tparam R row index
         * @tparam C column index
         * @return a reference to the cell
         */
        template <dim_t R, dim_t C> constexpr auto& at() {
            return _v.template at<R, C>();
        }

        /**
         * Access a cell of the matrix
         * @tparam R row index
         * @tparam C column index
         * @return a const reference to the cell
         */
        template <dim_t R, dim_t C> constexpr const auto& at() const {
            return _v.template at<R, C>();
        }

        /**
         * Extract a row of the matrix
         * @tparam R row index
         * @return the row as a 1*Cols matrix
         */
        template <dim_t R> constexpr matrix<1, Cols, S> row() const {
            return [&]<dim_t ...I>(std::integer_sequence<dim_t, I...>){
                return matrix<1, Cols, S>{ at<R, I>()... };
            }(std::make_integer_sequence<dim_t, Cols>{ });
        }

        /**
         * Extract a column of the matrix
         * @tparam C column index
         * @return the column as a Rows*1 matrix
         */
        template <dim_t C> constexpr matrix<Rows, 1, S> col() const {
            return [&]<dim_t ...I>(std::integer_sequence<dim_t, I...>) {
                return matrix<Rows, 1, S>{ { at<I, C>() }... };
            }(std::make_integer_sequence<dim_t, Rows>{ });
        }

        /**
         * Compute the transpose of the matrix
         * @return a Cols*Rows matrix
         */
        constexpr matrix<Cols, Rows, S> T() const {
            matrix<Cols, Rows, S> r;
            [&]<dim_t ...I>(std::integer_sequence<dim_t, I...>){
                ((r.template at<I / Cols, I % Cols>() = this->template at<I % Cols, I / Cols>()), ...);
            }(std::make_integer_sequence<dim_t, Rows * Cols>{ });
            return r;
        }

        friend std::ostream& ::operator<<(std::ostream& os, const la::matrix<Rows, Cols, S>& m);
    };

    // deduction guides for the constructors (they're confusing)
    template <class ...E> matrix(E&&... e) -> matrix<1, sizeof...(E), std::common_type_t<E...>>;
    template <dim_t Cols, class ...E> matrix(const E(&...e)[Cols]) -> matrix<sizeof...(E), Cols, std::common_type_t<E...>>;

    /**
     * Compare if two matrices are cell-wise equal using the == operator on each cell. Matrices with
     * different dimensions are by definition not equal.
     * @return whether the matrices are equal
     */
    template <dim_t LRows, dim_t LCols, scalar_type LS, dim_t RRows, dim_t RCols, scalar_type RS> constexpr bool operator==(const matrix<LRows, LCols, LS>& lhs, const matrix<RRows, RCols, RS>& rhs) {
        if constexpr (LRows == RRows && LCols == RCols && std::is_convertible_v<LS, RS>) {
            return [&]<dim_t ...J>(std::integer_sequence<dim_t, J...>){
                return ((lhs.template at<J / LCols, J % LCols>() == rhs.template at<J / LCols, J % LCols>()) && ...);
            }(std::make_integer_sequence<dim_t, LRows * LCols>{ });
        }
        return false;
    }

    namespace detail {
        template <dim_t Rows, dim_t Cols, scalar_type S, class UnaryOp> requires std::is_convertible_v<std::invoke_result_t<UnaryOp, S>, S>
        constexpr auto unary_apply(UnaryOp&& op, const matrix<Rows, Cols, S>& m) {
            matrix<Rows, Cols, S> result;
            [&]<dim_t ...J>(std::integer_sequence<dim_t, J...>){
                ((result.template at<J / Cols, J % Cols>() = std::invoke(op, m.template at<J / Cols, J % Cols>())), ...);
            }(std::make_integer_sequence<dim_t, Rows * Cols>{ });
            return result;
        }

        template <dim_t Rows, dim_t Cols, scalar_type S, class BinaryOp> requires std::is_convertible_v<std::invoke_result_t<BinaryOp, S, S>, S>
        constexpr auto binary_apply(BinaryOp&& op, const matrix<Rows, Cols, S>& lhs, const matrix<Rows, Cols, S>& rhs) {
            matrix<Rows, Cols, S> result;
            [&]<dim_t ...J>(std::integer_sequence<dim_t, J...>){
                ((result.template at<J / Cols, J % Cols>() = std::invoke(op, lhs.template at<J / Cols, J % Cols>(), rhs.template at<J / Cols, J % Cols>())), ...);
            }(std::make_integer_sequence<dim_t, Rows * Cols>{ });
            return result;
        }
    }

    /**
     * Add two matrices cell-wise.
     * @return A Rows*Cols matrix containing the sum of each element of each matrix
     */
    template <dim_t Rows, dim_t Cols, scalar_type S> constexpr matrix<Rows, Cols, S> operator+(const matrix<Rows, Cols, S>& lhs, const matrix<Rows, Cols, S>& rhs) {
        return detail::binary_apply(std::plus<S>{ }, lhs, rhs);
    }

    /**
     * Negate a matrix cell-wise.
     * @return A Rows*Cols matrix containing the negation of each element.
     */
    template <dim_t Rows, dim_t Cols, scalar_type S> constexpr matrix<Rows, Cols, S> operator-(const matrix<Rows, Cols, S>& m) {
        return detail::unary_apply(std::negate<S>{ }, m);
    }

    /**
     * Multiply a row vector on the left with a column vector on the right.
     * @return the inner product of vectors
     */
    template <dim_t N, scalar_type S> constexpr S operator*(const matrix<1, N, S>& lhs, const matrix<N, 1, S>& rhs) {
        return [&]<dim_t ...J>(std::integer_sequence<dim_t, J...>){
            return ((lhs.template at<0, J>() * rhs.template at<J, 0>()) + ...);
        }(std::make_integer_sequence<dim_t, N>{ });
    }

    /**
     * Multiply two matrices of compatible dimension
     * @return a matrix with the same number of rows as the left matrix and the number of columns as the right matrix.
     */
    template <dim_t LRows, dim_t LColsRRows, scalar_type LS, dim_t RCols, scalar_type RS> constexpr auto operator*(const matrix<LRows, LColsRRows, LS>& lhs, const matrix<LColsRRows, RCols, RS>& rhs) {
        matrix<LRows, RCols, decltype(LS{} * RS{})> result;
        [&]<dim_t ...J>(std::integer_sequence<dim_t, J...>){
            ((result.template at<J / RCols, J % RCols>() = lhs.template row<J / RCols>() * rhs.template col<J % RCols>()), ...);
        }(std::make_integer_sequence<dim_t, LRows * RCols>{ });
        return result;
    }
}

/**
 * Print a matrix to an output stream.
 * @return the same output stream
 */
template <la::dim_t Rows, la::dim_t Cols, la::scalar_type S> std::ostream& operator<<(std::ostream& os, const la::matrix<Rows, Cols, S>& m) {
    if constexpr (Rows == 1) {
        os << "[";
        [&]<la::dim_t ...J>(std::integer_sequence<la::dim_t, J...>){
            ((os << m.template at<0, J>() << ", "), ...);
        }(std::make_integer_sequence<la::dim_t, Cols - 1>{ });
        return os << m.template at<0, Cols - 1>() << "]";
    }
    os << "[";
    [&]<la::dim_t ...J>(std::integer_sequence<la::dim_t, J...>){
        ((os << m.template row<J>() << ", "), ...);
    }(std::make_integer_sequence<la::dim_t, Rows - 1>{ });
    return os << m.template row<Rows - 1>() << "]";
}

#endif //LA_H
