package net.fortywo.test.jdbctest;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.sql.SQLException;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Jan 14, 2009
 * Time: 11:35:22 AM
 * To change this template use File | Settings | File Templates.
 */
public class SimpleJDBCTest {
    private static final String
            SQL_PROXY_DRIVER_CLASS = "net.opense.sqlgateway.client.SQLProxyDriver",
            GATEWAY_PROXY_URL = "http://localhost:8080/usqlg",
            NATIVE_DRIVER_CLASS = "com.mysql.jdbc.Driver",
            NATIVE_DB_URL = "jdbc:mysql://localhost:3306/information_schema",
//            NATIVE_DB_URL = "jdbc:mysql://localhost:3306/test",
            JDBC_URL = "jdbc:sqlgateway:@" + GATEWAY_PROXY_URL
                    + "?" + NATIVE_DRIVER_CLASS
                    + "?" + NATIVE_DB_URL,
            USERNAME = "root",
            PASSWORD = "";

    public static void main(final String args[]) throws Exception {
//        simpleTest(NATIVE_DRIVER_CLASS, NATIVE_DB_URL);
        simpleTest(SQL_PROXY_DRIVER_CLASS, JDBC_URL);
    }

    private static void simpleTest(final String driverClassName,
                                   final String dbURL) throws ClassNotFoundException, SQLException {
        System.out.println("testing with " + driverClassName + ", " + dbURL);

        Class.forName(driverClassName);
        Connection con = DriverManager.getConnection
                (dbURL, USERNAME, PASSWORD);
        
        Statement stmt = con.createStatement();
        ResultSet rs = stmt.executeQuery("SELECT TABLE_SCHEMA, TABLE_NAME FROM TABLES");
        while (rs.next()) {
            String a = rs.getString("TABLE_SCHEMA");
            String b = rs.getString("TABLE_NAME");
            System.out.println("    a = " + a + ", b = " + b);
        }
        rs.close();
        stmt.close();
    }
}
