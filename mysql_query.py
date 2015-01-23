#-------------------------------------------------------------------------------
# Name:        GetCustomers
# Purpose:
#
# Author:      romicar
#
# Created:     22.01.2015
# Copyright:   (c) romicar 2015
# Licence:     <your licence>
#-------------------------------------------------------------------------------

#!/usr/bin/python

import MySQLdb

# Open database connection
db = MySQLdb.connect("localhost","test","test","test" )

# prepare a cursor object using cursor() method
cursor = db.cursor()

# Prepare SQL query to INSERT a record into the database.
sql = "SELECT * FROM customer"
try:
   # Execute the SQL command
   cursor.execute(sql)
   # Fetch all the rows in a list of lists.
   results = cursor.fetchall()
   print "All the customers from the DB:\n"
   for row in results:
      customer_id = row[0]
      customer_name = row[1]
      # Now print fetched result
      print "customer_id=%s,customer_name=%s\n" % \
             (customer_id, customer_name )
except:
   print "Error: unable to fecth data"

# disconnect from server
db.close()

