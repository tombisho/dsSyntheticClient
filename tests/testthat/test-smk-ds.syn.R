#-------------------------------------------------------------------------------
# Copyright (c) 2019-2021 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

context("ds.syn::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG", "LAB_GLUC_ADJUSTED"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.syn::smk")
test_that("simple synthetic test", {
    res <- ds.syn2(data="D", method = "cart", m = 1, seed = 123)
    expect_equal(object = length(res$sim1$syn$LAB_TSC), expected = 2163)
    browser()
    bonj = datashield.errors()
    print(bonj)
})

#
# Done
#

context("ds.syn::smk::shutdown")

# test_that("shutdown", {
#     ds_expect_variables(c("D"))
# })


disconnect.studies.dataset.cnsim()

context("ds.syn::smk::done")
