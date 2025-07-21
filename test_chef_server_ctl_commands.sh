#!/bin/bash

# Comprehensive test script for chef-server-ctl knife-oriented commands
# Tests all commands and their variations based on wrap-knife.rb implementation

set +e  # Don't exit on errors - continue testing

# Test base names following the pattern requested
BASE_USER="userabc123"
BASE_ORG="orgabc123"
BASE_EMAIL="${BASE_USER}@example.com"
BASE_PASSWORD="${BASE_USER}"
BASE_FIRST="${BASE_USER}"
BASE_LAST="${BASE_USER}"
BASE_MIDDLE="${BASE_USER}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to run a command and show results
run_test() {
    local test_name="$1"
    shift
    local cmd="$@"
    
    echo -e "${GREEN}Running test: $test_name${NC}"
    echo "Command: $cmd"
    
    # Execute the command with proper evaluation
    if sudo $cmd; then
        echo -e "${GREEN}✓ Test passed: $test_name${NC}"
    else
        echo -e "${RED}✗ Test failed: $test_name${NC}"
        FAILED_TESTS+=("$test_name")
    fi
    echo "----------------------------------------"
}

# Initial cleanup - remove any leftover users/orgs/files from previous runs
echo -e "${BLUE}=== Initial Cleanup ===${NC}"
echo "Cleaning up any leftover users, organizations, and files from previous test runs..."

# NOTE: Commented out extensive cleanup since we now clean up after each test
# Delete test users (ignore errors if they don't exist)
#for i in "" 2 3 4 5 6 7 8 9 10; do
#    if [ -n "$i" ] || [ "$i" = "" ]; then
#        user_name="${BASE_USER}${i}"
#        echo "Attempting to delete user: $user_name"
#        sudo bash -c "yes | chef-server-ctl user-delete '$user_name'" 2>/dev/null || true
#    fi
#done

# Delete additional test users that might exist
#for user in "baduser" "baduser2" "nonexistentuser"; do
#    echo "Attempting to delete user: $user"
#    sudo bash -c "yes | chef-server-ctl user-delete '$user'" 2>/dev/null || true
#done

# Delete test organizations (ignore errors if they don't exist)
#for i in "" 2 3 4 5; do
#    if [ -n "$i" ] || [ "$i" = "" ]; then
#        org_name="${BASE_ORG}${i}"
#        echo "Attempting to delete organization: $org_name"
#        sudo bash -c "yes | chef-server-ctl org-delete '$org_name'" 2>/dev/null || true
#    fi
#done

# Delete additional test orgs that might exist
#for org in "nonexistentorg"; do
#    echo "Attempting to delete organization: $org"
#    sudo bash -c "yes | chef-server-ctl org-delete '$org'" 2>/dev/null || true
#done

# Remove any PEM files created during previous test runs
echo "Removing any leftover PEM files..."
for i in 2 3 4 5 6 7 8 9 10; do
    pem_file="${BASE_USER}${i}.pem"
    if [ -f "$pem_file" ]; then
        echo "Removing: $pem_file"
        rm -f "$pem_file"
    fi
done

for i in 2 3 4 5; do
    validator_file="${BASE_ORG}${i}-validator.pem"
    if [ -f "$validator_file" ]; then
        echo "Removing: $validator_file"
        rm -f "$validator_file"
    fi
done

# Remove any other potential PEM files
for file in *.pem; do
    if [ -f "$file" ]; then
        echo "Removing additional PEM file: $file"
        rm -f "$file"
    fi
done

echo -e "${GREEN}Initial cleanup complete${NC}"
echo "=================================================="

echo -e "${BLUE}Starting comprehensive chef-server-ctl command tests${NC}"
echo "Base names: user=$BASE_USER, org=$BASE_ORG, email=$BASE_EMAIL"
echo "=================================================="

# Test 1: user-create - Format 1 (5 args) without file output
run_test "user-create (5 args, no file)" \
    chef-server-ctl user-create "$BASE_USER" "$BASE_FIRST" "$BASE_LAST" "$BASE_EMAIL" "$BASE_PASSWORD"

# Clean up after test 1
echo "Cleaning up user: $BASE_USER"
sudo bash -c "yes | chef-server-ctl user-delete '$BASE_USER'" 2>/dev/null || true

# Test 2: user-create - Format 1 (5 args) with --filename
run_test "user-create (5 args, with --filename)" \
    chef-server-ctl user-create "${BASE_USER}2" "$BASE_FIRST" "$BASE_LAST" "${BASE_USER}2@example.com" "$BASE_PASSWORD" --filename "${BASE_USER}2.pem"

# Test 3: user-create - Format 1 (5 args) with -f
run_test "user-create (5 args, with -f)" \
    chef-server-ctl user-create "${BASE_USER}3" "$BASE_FIRST" "$BASE_LAST" "${BASE_USER}3@example.com" "$BASE_PASSWORD" -f "${BASE_USER}3.pem"

# Test 4: user-create - Format 2 (6 args with middle name) without file output
run_test "user-create (6 args with middle name, no file)" \
    chef-server-ctl user-create "${BASE_USER}4" "$BASE_FIRST" "$BASE_MIDDLE" "$BASE_LAST" "${BASE_USER}4@example.com" "$BASE_PASSWORD"

# Clean up after test 4
echo "Cleaning up user: ${BASE_USER}4"
sudo bash -c "yes | chef-server-ctl user-delete '${BASE_USER}4'" 2>/dev/null || true

# Test 5: user-create - Format 2 (6 args with middle name) with --filename
run_test "user-create (6 args with middle name, with --filename)" \
    chef-server-ctl user-create "${BASE_USER}5" "$BASE_FIRST" "$BASE_MIDDLE" "$BASE_LAST" "${BASE_USER}5@example.com" "$BASE_PASSWORD" --filename "${BASE_USER}5.pem"

# Test 6: user-create - Format 2 (6 args with middle name) with -f
run_test "user-create (6 args with middle name, with -f)" \
    chef-server-ctl user-create "${BASE_USER}6" "$BASE_FIRST" "$BASE_MIDDLE" "$BASE_LAST" "${BASE_USER}6@example.com" "$BASE_PASSWORD" -f "${BASE_USER}6.pem"

# Test 6a: user-create - with --orgname option (from knife-opc)
run_test "user-create (with --orgname option)" \
    chef-server-ctl user-create "${BASE_USER}7" "$BASE_FIRST" "$BASE_LAST" "${BASE_USER}7@example.com" "$BASE_PASSWORD" --orgname "$BASE_ORG" -f "${BASE_USER}7.pem"

# Test 6b: user-create - with -o option (short form of --orgname)
run_test "user-create (with -o option)" \
    chef-server-ctl user-create "${BASE_USER}8" "$BASE_FIRST" "$BASE_LAST" "${BASE_USER}8@example.com" "$BASE_PASSWORD" -o "$BASE_ORG" -f "${BASE_USER}8.pem"

# Test 6c: user-create - with --prompt-for-password (may hang waiting for input)
run_test "user-create (with --prompt-for-password - may hang)" \
    chef-server-ctl user-create "${BASE_USER}9" "$BASE_FIRST" "$BASE_LAST" "${BASE_USER}9@example.com" --prompt-for-password -f "${BASE_USER}9.pem"

# Test 6d: user-create - with -p option (short form of --prompt-for-password)
run_test "user-create (with -p option - may hang)" \
    chef-server-ctl user-create "${BASE_USER}10" "$BASE_FIRST" "$BASE_LAST" "${BASE_USER}10@example.com" -p -f "${BASE_USER}10.pem"

# Test 7: user-list - basic
run_test "user-list (basic)" \
    chef-server-ctl user-list

# Test 8: user-list - with --all-info (should be filtered out)
run_test "user-list (with --all-info - should be filtered)" \
    chef-server-ctl user-list --all-info

# Test 9: user-list - with -a (should be filtered out)
run_test "user-list (with -a - should be filtered)" \
    chef-server-ctl user-list -a

# Test 9a: user-list - with --with-uri option (from knife-opc)
run_test "user-list (with --with-uri)" \
    chef-server-ctl user-list --with-uri

# Test 9b: user-list - with -w option (short form of --with-uri)
run_test "user-list (with -w)" \
    chef-server-ctl user-list -w

# Test 10: user-show - basic
run_test "user-show (basic)" \
    chef-server-ctl user-show "$BASE_USER"

# Test 11: user-delete - basic
run_test "user-delete (basic)" \
    bash -c "yes | chef-server-ctl user-delete '$BASE_USER'"

# Test 12: user-edit - basic (will likely open editor or fail)
run_test "user-edit (basic - may open editor)" \
    EDITOR=vim chef-server-ctl user-edit "${BASE_USER}2"

# Test 13: org-create - basic (ORG_SHORT_NAME ORG_FULL_NAME format)
run_test "org-create (basic)" \
    chef-server-ctl org-create "$BASE_ORG" "Test Organization ABC123"

# Test 13a: org-create - with --filename option
run_test "org-create (with --filename)" \
    chef-server-ctl org-create "${BASE_ORG}2" "Test Organization ABC123 2" --filename "${BASE_ORG}2-validator.pem"

# Clean up after test 13a
echo "Cleaning up org: ${BASE_ORG}2"
sudo bash -c "yes | chef-server-ctl org-delete '${BASE_ORG}2'" 2>/dev/null || true

# Test 13b: org-create - with -f option
run_test "org-create (with -f)" \
    chef-server-ctl org-create "${BASE_ORG}3" "Test Organization ABC123 3" -f "${BASE_ORG}3-validator.pem"

# Clean up after test 13b
echo "Cleaning up org: ${BASE_ORG}3"
sudo bash -c "yes | chef-server-ctl org-delete '${BASE_ORG}3'" 2>/dev/null || true

# Test 13c: org-create - with --association_user option
run_test "org-create (with --association_user)" \
    chef-server-ctl org-create "${BASE_ORG}4" "Test Organization ABC123 4" --association_user "${BASE_USER}2"

# Clean up after test 13c
echo "Cleaning up org: ${BASE_ORG}4"
sudo bash -c "yes | chef-server-ctl org-delete '${BASE_ORG}4'" 2>/dev/null || true

# Test 13d: org-create - with -a option (short form of --association_user)
run_test "org-create (with -a option)" \
    chef-server-ctl org-create "${BASE_ORG}5" "Test Organization ABC123 5" -a "${BASE_USER}2"

# Clean up after test 13d
echo "Cleaning up org: ${BASE_ORG}5"
sudo bash -c "yes | chef-server-ctl org-delete '${BASE_ORG}5'" 2>/dev/null || true

# Test 14: org-list - basic
run_test "org-list (basic)" \
    chef-server-ctl org-list

# Test 15: org-show - basic
run_test "org-show (basic)" \
    chef-server-ctl org-show "$BASE_ORG"

# Test 16: org-user-add - basic
run_test "org-user-add (basic)" \
    chef-server-ctl org-user-add "$BASE_ORG" "${BASE_USER}2"

# Test 16a: org-user-add - with --admin option
run_test "org-user-add (with --admin)" \
    chef-server-ctl org-user-add "${BASE_ORG}2" "${BASE_USER}3" --admin

# Test 16b: org-user-add - with -a option (admin flag, not association_user)
run_test "org-user-add (with -a admin flag)" \
    chef-server-ctl org-user-add "${BASE_ORG}3" "${BASE_USER}4" -a

# Test 17: org-user-remove - basic
run_test "org-user-remove (basic)" \
    chef-server-ctl org-user-remove "$BASE_ORG" "${BASE_USER}2"

# Test 18: org-delete - basic
run_test "org-delete (basic)" \
    bash -c "yes | chef-server-ctl org-delete '$BASE_ORG'"

# Error condition tests - these should fail gracefully

echo -e "\n${BLUE}=== Testing Error Conditions ===${NC}"

# Test 19: user-create with insufficient arguments
run_test "user-create (insufficient args - should fail)" \
    chef-server-ctl user-create "baduser"

# Clean up after test 19 (in case it partially succeeded)
echo "Cleaning up potential user: baduser"
sudo bash -c "yes | chef-server-ctl user-delete 'baduser'" 2>/dev/null || true

# Test 20: user-create with too many arguments
run_test "user-create (too many args - should fail)" \
    chef-server-ctl user-create "baduser2" "first" "middle" "last" "email@test.com" "password123" "extra" "args"

# Clean up after test 20 (in case it partially succeeded)
echo "Cleaning up potential user: baduser2"
sudo bash -c "yes | chef-server-ctl user-delete 'baduser2'" 2>/dev/null || true

# Test 21: user-show non-existent user
run_test "user-show (non-existent user)" \
    chef-server-ctl user-show "nonexistentuser"

# Test 22: org-show non-existent org
run_test "org-show (non-existent org)" \
    chef-server-ctl org-show "nonexistentorg"

# Test 23: user-delete non-existent user
run_test "user-delete (non-existent user)" \
    bash -c "yes | chef-server-ctl user-delete 'nonexistentuser'"

# Test 24: org-delete non-existent org
run_test "org-delete (non-existent org)" \
    bash -c "yes | chef-server-ctl org-delete 'nonexistentorg'"

# Cleanup - remove any files created during testing
echo -e "\n${BLUE}=== Cleanup ===${NC}"
for i in 2 3 5 6 7 8 9 10; do
    if [ -f "${BASE_USER}${i}.pem" ]; then
        echo "Removing ${BASE_USER}${i}.pem"
        rm -f "${BASE_USER}${i}.pem"
    fi
done

for i in 2 3; do
    if [ -f "${BASE_ORG}${i}-validator.pem" ]; then
        echo "Removing ${BASE_ORG}${i}-validator.pem"
        rm -f "${BASE_ORG}${i}-validator.pem"
    fi
done

echo -e "\n${GREEN}=== Test Script Complete ===${NC}"
echo "Review the output above for any failures or unexpected behavior."
echo "Note: Some commands may fail due to server state, existing users/orgs, or permissions."
echo "This is expected and helps identify the actual behavior of each command."
