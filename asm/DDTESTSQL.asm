*          DATA SET DDTESTSQL  AT LEVEL 002 AS OF 05/01/02                      
*PHASE TESTSQL                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
         TITLE 'TEST SQL IN A SIMPLE ASSEMBLER PROGRAM'                         
TESTSQL  CSECT                                                                  
                                                                                
***********************************************************************         
*THIS MACRO ALLOWS EXEC SQL STATEMENTS TO BE BYPASSED BY THE ASSEMBLER*         
*THE DB2 PRE-COMPLILER WILL REPLACE EXEC SQL STATEMENTS               *         
***********************************************************************         
                                                                                
*        MACRO                                                                  
*LABEL   EXEC                                                                   
*LABEL   DS    0H                                                               
*        MEND                                                                   
                                                                                
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE 0,*TESTSQL,REGSAVE                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVI   COLSMAX,132                                                      
         MVC   TITLE(20),=CL20'TEST SQL REQUESTS'                               
*                                                                               
         LA    R2,CLIDATA          R2=A(MEDIA CLIENT EXTRACT RECORD)            
         USING MXCLID,R2                                                        
         MVI   MXCLID,C' '                                                      
         MVC   MXCLID+1(MXCLIDX-MXCLID),MXCLID                                  
*                                                                               
LOOP     GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BE    EXIT                                                             
         MVC   P(20),C                                                          
         CLC   C(5),=C'CODE='                                                   
         BE    SINGLE                                                           
         CLC   C(5),=C'NAME='                                                   
         BE    MULTI                                                            
         B     LOOP      '                                                      
         EJECT                                                                  
***********************************************************************         
*CLIENT TABLE DEFINITION                                              *         
*IF PLACED AT THE END OF THE PROGRAM WE GET WARNINGS ABOUT TABLE      *         
*PREVIOUSLY DECLARED OR REFERENCED.                                   *         
*IF OMMITTED WE GET MESSAGE "WARNINGS HAVE BEEN SUPRESSED DUE TO LACK *         
*OF TABLE DEFINITIONS".                                               *         
***********************************************************************         
                                                                                
         EXEC  SQL DECLARE CLIENT TABLE                 (              X        
               ActionLast         CHAR(1)       NOT NULL,              X        
               ActionDate         DATE          NOT NULL,              X        
               ActionTime         TIME          NOT NULL,              X        
               AgencyCode         CHAR(2)       NOT NULL,              X        
               MediaCode          SMALLINT      NOT NULL,              X        
               ClientCode         CHAR(5)       NOT NULL,              X        
               ShortName          CHAR(8)       NOT NULL,              X        
               Filter1            CHAR(1)       NOT NULL,              X        
               Filter2            CHAR(1)       NOT NULL,              X        
               Filter3            CHAR(1)       NOT NULL,              X        
               CreativeAgencyCode SMALLINT              ,              X        
               BuyingAgencyCode   SMALLINT              ,              X        
               BillingGroup       CHAR(2)               ,              X        
               SchemeCode         CHAR(4)               ,              X        
               Name               CHAR(30)      NOT NULL,              X        
               Address1           CHAR(30)              ,              X        
               Address2           CHAR(30)              ,              X        
               Address3           CHAR(30)              ,              X        
               Address4           CHAR(30)              ,              X        
               InvoicableCommRate CHAR(20)              ,              X        
               VATType            CHAR(8)               ,              X        
               FormulaSchemeCode  CHAR(10)              )                       
         EJECT                                                                  
***********************************************************************         
*SQL DESCRIPTOR AREAS - MUST APPEAR BEFORE THEY ARE REFERENCED        *         
***********************************************************************         
                                                                                
SQDDA1   EXEC  SQL INCLUDE SQLDA                                                
         EJECT                                                                  
***********************************************************************         
*SQL SINGLETON - GETS COLS FROM ONE ROW INTO LOCAL STORAGE            *         
***********************************************************************         
                                                                                
SINGLE   MVC   MXCLICDE,C+5        SINGLE CLIENT SELECT VIA CLI CODE            
         MVC   MXCLISHR,SPACES                                                  
         MVC   MXCLINAM,SPACES                                                  
         LA    R3,WSSQL            NEED W/S TO BUILD SQL PARAM LIST             
         USING SQLDSECT,R3                                                      
*                                                                               
         EXEC  SQL                                                     X        
               SELECT Shortname,Name                                   X        
               INTO   :MXCLISHR,:MXCLINAM                              X        
               FROM   CLIENT                                           X        
               WHERE  Mediacode=0                                      X        
               AND    ClientCode=:MXCLICDE                                      
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
         MVC   P+30(4),FULL                                                     
         MVC   P+40(5),MXCLICDE                                                 
         MVC   P+50(8),MXCLISHR                                                 
         MVC   P+60(30),MXCLINAM                                                
         GOTO1 =V(PRINTER)                                                      
         B     LOOP                                                             
         EJECT                                                                  
***********************************************************************         
*SQL BROWSE - REQUIRES A CURSOR TO GET MULTIPLE ROWS                  *         
*NOTE THAT :WORK IN THE LIKE CLAUSE GIVES AN ERROR UNLESS IT IS       *         
*DEFINED BEFORE ITS USE IN THE DECLARE STATEMENT                      *         
*NOTE ALSO THAT THE DECLARE CURSOR STATEMENT CAN BE PLACED AT THE END *         
*OF THE PROGRAM (AFTER THE VARIABLES IT USES)                         *         
***********************************************************************         
                                                                                
WORK     DS    CL8                 USED BY LIKE CLAUSE IN DECLARE               
*                                                                               
         EXEC  SQL    DECLARE CLICUR CURSOR FOR                        X        
               SELECT ClientCode,ShortName,Name                        X        
               FROM   CLIENT                                           X        
               WHERE  MediaCode=1                                      X        
               AND    ShortName LIKE :WORK                                      
*                                                                               
MULTI    MVC   MXCLISHR,C+5        MULTI CLIENT SELECT VIA CLI NAME             
         MVC   MXCLICDE,SPACES                                                  
         MVC   MXCLINAM,SPACES                                                  
         MVC   WORK,MXCLISHR                                                    
         LA    RE,WORK                                                          
         LA    R0,L'MXCLISHR                                                    
MULTI1   CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R0,MULTI1                                                        
         MVI   0(RE),C'%'          SET SQL LIKE CHR AT END OF STRING            
*                                                                               
         EXEC  SQL OPEN CLICUR                                                  
*                                                                               
MULTI2   EXEC  SQL    FETCH CLICUR                                     X        
               INTO   :MXCLICDE,:MXCLISHR,:MXCLINAM                             
*                                                                               
         BAS   RE,OUTCODE          GET SQL RETURN CODE                          
         MVC   P+30(4),FULL                                                     
         CLC   SQLCODE,=H'100'     TEST NO MORE DATA                            
         BE    MULTIX                                                           
         MVC   P+40(5),MXCLICDE                                                 
         MVC   P+50(8),MXCLISHR                                                 
         MVC   P+60(30),MXCLINAM                                                
         GOTO1 =V(PRINTER)                                                      
         MVC   SQLCODE,=H'100'     *TEMP* SIMULATE SQL EOF                      
         B     MULTI2                                                           
*                                                                               
MULTIX   EXEC  SQL CLOSE CLICUR                                                 
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         B     LOOP                                                             
         EJECT                                                                  
OUTCODE  MVI   FULL,C' '                                                        
         LH    RF,SQLCODE                                                       
         CVD   RF,DUB                                                           
         UNPK  FULL+1(3),DUB                                                    
         OI    FULL+3,C'0'                                                      
         LTR   RF,RF                                                            
         BNMR  RE                                                               
         MVI   FULL,C'-'                                                        
         BR    RE                                                               
*                                                                               
EXIT     XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
*SQL COMMUNICATIONS AREA - MUST BE IN A SEPARATE AREA AND COVERED BY  *         
*A DSECT FOR A RE-ENTRANT PROGRAM                                     *         
***********************************************************************         
                                                                                
         EXEC  SQL INCLUDE SQLCA                                                
*                                                                               
SSB      DC    F'0',F'0'                                                        
UTL      DC    F'0',AL1(04),XL3'00'                                             
*                                                                               
         LTORG                                                                  
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
BYTE1    DS    C                                                                
DMCB     DS    6F                                                               
C        DC    CL80' '                                                          
*                                                                               
WSSQL    DS    CL80                                                             
*                                                                               
CLIDATA  DS    CL250                                                            
*                                                                               
REGSAVE  DS    1000D                                                            
         EJECT                                                                  
       ++INCLUDE MXCLID                                                         
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINTL                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DDTESTSQL 05/01/02'                                      
         END                                                                    
