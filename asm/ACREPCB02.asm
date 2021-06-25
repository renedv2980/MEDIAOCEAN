*          DATA SET ACREPCB02  AT LEVEL 058 AS OF 08/16/00                      
*PHASE ACCB02A,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'SCAN FOR LOW JOB BALANCES'                                      
         SPACE 1                                                                
ACCB02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACCB**,R8,R9,RR=RE                                           
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=GENERAL W/S                               
         LA    RC,SPACEND                                                       
         USING WORKD,RC            RC=LOCAL W/S                                 
         L     RF,=V(HELLO)                                                     
         AR    RF,RE                                                            
         ST    RF,VHELLO                                                        
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    EXIT                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
*                                                                               
         CLI   MODE,PROCLEVC                                                    
         BE    JOB                                                              
         CLI   MODE,REQLAST                                                     
         BE    EXIT                                                             
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC=HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC=LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC=EQUAL                                 
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* FIRST FOR REQUEST                                                   *         
***********************************************************************         
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVC   P(25),=CL25'HI THERE'                                            
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS JOB RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
JOB      DS    0H                                                               
         L     R2,ADACC                                                         
         USING ACTRECD,R2                                                       
         LA    R3,ACTRECD+ACCORFST                                              
         USING ABLELD,R3                                                        
         XR    RF,RF                                                            
JOB02    CLI   ABLEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   ABLEL,ABLELQ                                                     
         BE    *+12                                                             
         IC    RF,ABLLN                                                         
         BXH   R3,RF,JOB02                                                      
*                                                                               
         ZAP   BALANCE,ABLDR                                                    
         SP    BALANCE,ABLCR                                                    
*                                                                               
         CP    BALANCE,=P'0'                                                    
         BE    EXIT                                                             
         CP    BALANCE,=P'10'                                                   
         BH    EXIT                                                             
         CP    BALANCE,=P'-10'                                                  
         BL    EXIT                                                             
*                                                                               
         MVC   P(L'ACTKACT),ACTKACT                                             
         CURED (P8,BALANCE),(16,P+20),2,COMMAS=YES,MINUS=YES                    
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
BCSPACES DC    CL256' '                                                         
ACCMST   DC    C'ACCMST '                                                       
ACCDIR   DC    C'ACCDIR '                                                       
ACCFIL   DC    C'ACCOUNT'                                                       
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* LOCAL W/S                                                           *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
VHELLO   DS    A                                                                
BCPARM   DS    6A                                                               
TIMEKEY  DS    XL64                                                             
IOKEY    DS    XL64                                                             
IOKEYSAV DS    XL64                                                             
BALANCE  DS    D                                                                
*                                                                               
         EJECT                                                                  
*        ACREPWORKD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*        ACGENFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*        DDREPXTRAD                                                             
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
*        DDMASTD                                                                
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*        DDEBLOCK                                                               
         PRINT OFF                                                              
       ++INCLUDE DDEBLOCK                                                       
         PRINT ON                                                               
*        ACLANGEQU                                                              
         PRINT OFF                                                              
       ++INCLUDE DDLANGEQUS                                                     
         PRINT ON                                                               
*        ACDDEQUS                                                               
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
*        ACGOBLOCK                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
*        ACMASTD                                                                
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*        ACPRORATAD                                                             
         PRINT OFF                                                              
PRORATAD DSECT                                                                  
       ++INCLUDE ACPRORATAD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058ACREPCB02 08/16/00'                                      
         END                                                                    
