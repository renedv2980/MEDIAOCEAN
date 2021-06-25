*          DATA SET REREP7502X AT LEVEL 040 AS OF 07/23/96                      
*PHASE RE7502A,+0                                                               
***********************************************************************         
* HISTORY:                                                                      
*                                                                               
* 10JUL96 (SPE) ORIGINATION                                                     
*                                                                     *         
* FIRST STAB AT BEING AN ASS CODER                                              
*                          ** END TOMBSTONE **                        *         
***********************************************************************         
         TITLE 'STANDARD COMMENT LISTING PROGRAM'                               
RE7502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE7502,RR=R5                                                 
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
*                                                                               
*        CHECK AND PROCESS MODE SETTINGS                                        
*                                                                               
         LA    R1,MODETAB          POINT TO MODE/PROC TABLE                     
         ZIC   R2,0(R1)            GET NUMBER OF ENTRIES                        
         ZIC   R3,1(R1)            GET LENGTH OF EACH ENTRY                     
         LA    R1,2(R1)            POINT TO 1ST ENTRY                           
         ZIC   R0,MODE             GET CURRENT MODE                             
MAIN10   EQU   *                                                                
         ZIC   RF,0(R1)            GET TABLE ENTRY MODE                         
         CR    R0,RF               GOT IT?                                      
         BNE   MAIN20              NO, CHECK NEXT                               
         ZICM  RF,1(R1),3          YES, GET THE ROUTINE ADDR                    
         GOTO1 (RF)                GO TO THE ROUTINE                            
         B     MAIN30              ZERO IS GOOD RETURN                          
MAIN20   EQU   *                                                                
         AR    R1,R3               POINT TO THE NEXT ENTRY                      
         BCT   R2,MAIN10           LOOP                                         
*                                                                               
MAIN30   EQU   *                                                                
*                                                                               
*        MAIN COMMON EXIT                                                       
*                                                                               
MAINGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     XIT                                                              
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        MODE/PROCESS ROUTINE TABLE                                             
*                                                                               
*                                                                               
*        CL1  -  NUMBER OF ENTRIES                                              
*        CL1  -  LENGTH OF ONE ENTRY                                            
*        CL4  -  ENTRY:                                                         
*                  CL1 - MODE                                                   
*                  CL3 - ROUTINE ADDRESS                                        
*                                                                               
*                                                                               
MODETAB  EQU   *                                                                
         DC    AL1(NUMMDTAB)       NUMBER OF TABLE ENTRIES                      
         DC    AL1(MDTABLEN)       LENGTH OF A TABLE ENTRY                      
*                                                                               
MDENTRY  EQU   *                                                                
         DC    AL1(REQFRST),AL3(COMMPRNT) COMMENT PRINT                         
*                                  REQUIRES NO OTHER TABLE ENTRIES              
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
*        DC    AL1(PROCCONT),AL3(POST)    WOTV/WOOD FIX                         
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
COMMPRNT NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   RCMTKTYP,X'2E'                                                   
         MVC   RCMTKREP,RCREPFL                                                 
         MVC   KEY,IOAREA                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEYSAVE,KEY,0                 
         B     COMM0040                                                         
COMM0020 EQU   *                                                                
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'REPDIR',KEYSAVE,KEY,0                 
COMM0040 EQU   *                                                                
         CLC   KEY(17),KEYSAVE                                                  
         BNE   XIT                                                              
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,IOAREA,DMWORK         
*                                                                               
*                                                                               
         LA    R6,RCMTREC          LOAD ADDRESS OF COMMENT REC                  
         MVI   ELCODE,2            RETRIEVE COMMENT LINE ELEMENT                
         ZAP   RCNTR,=P'00'        CLEAR RECORD COUNTER                         
         MVC   P,BLANKS            CLEAR PRINT                                  
         GOTO1 REPORT              PRINT A BLANK LINE BETWEEN RECS              
         MVC   P+4(8),RCMTKCDE     MOVE COMMENT CODE TO PRINT                   
*                                                                               
**********BELOW WAS USED, REMOVED AND RECODED                                   
*********AP    LCNTR,ONE           ADD 1 TO LCNTR                               
*********ZIC   R8,RCMT#LIN         GET # OF TEXT LINES INTO REG                 
*********CVD   R8,DUB              CONVERT TO DECIMAL FOR ADDING                
*********AP    LCNTR,DUB           ADD # LINES TO DETERM HEAD BRK               
*********CP    LCNTR,=P'52'        COMPARE WITH # LINES PRINTED                 
*                                                                               
         ZIC   R7,LINE             PUT LINE IN REG7                             
         ZIC   R8,RCMT#LIN         PUT #LINES INTO REG 8                        
         ZIC   R9,MAXLINES         PUT MAXLINES INTO REG9                       
         AR    R7,R8               ADD REG 8 TO REG 7                           
         CR    R7,R9               COMPARE REG 7 TO REG 9                       
         BL    NOPGBRK             IF R7 IS LOWER DO NOT FORCE HEAD             
*                                                                               
         MVI   FORCEHED,C'Y'       IF = OR HIGH FORCE HEADING                   
         SR    R7,R7                                                            
NOPGBRK  EQU   *                                                                
         BAS   RE,GETEL            GET ELELMENT                                 
         BZ    PROC00              IF ZERO BRANCH TO PROC00                     
         B     COMM0020            OTHERWISE GET THE NEXT RECORD                
GETNXT   EQU   *                                                                
         BAS   RE,NEXTEL           GET THE NEXT ELEMENT                         
         BNZ   COMM0020            IF NOT ZERO THEN END OF REC ELEMENTS         
PROC00   EQU   *                                                                
         USING RCMTELM2,R6         USE RCMTLEM2 IN REG 6                        
         ZIC   RE,RCMT2LEN         ZERO AND INSERT CHAR OF LEN INTO RE          
         SH    RE,=H'3'            SUBT ID,LEN, AND A BYTE FOR THE EX           
         EX    RE,MVCHAR           EXECUTE THE MOVE TO PRINT                    
         B     PRINTIT             BRANCH TO PRINT                              
MVCHAR   MVC   P+20(0),RCMT2TXT    MOVE COMMENT LINE TO PRINT                   
PRINTIT  EQU   *                                                                
         AP    RCNTR,=P'1'         ADD 1 TO REC COUNTER                         
         ZAP   PNO,RCNTR           MOVE RCNTR TO PNO FOR EDIT                   
         OI    PNO+1,X'0F'         STRIP OFF SIGN                               
         MVC   PATRN,MASK          MOVE MASK INTO PATRN                         
         ED    PATRN,PNO           EDIT PNO ONTO PATRN                          
         MVC   P+13(3),PATRN       MOVE TO PRINT                                
         CP    RCNTR,=P'1'         CHECK IF REC CNTR = 1                        
         BE    SHOWKEY             IF 1ST LINE BYPASS CLEAR                     
         MVC   P+4(8),BLANKS       CLEAR KEY IF NOT 1ST LINE                    
SHOWKEY  EQU   *                                                                
         DROP  R6                  DROP R6                                      
         GOTO1 REPORT              PRINT LINE                                   
         B     GETNXT              GO TO GET NXT ELEMENT                        
         GETEL R6,34,ELCODE                                                     
*                                                                               
XIT      XIT1                                                                   
END      XBASE                                                                  
         LTORG                                                                  
PMWORK   DS    CL20                                                             
RELO     DS    A                                                                
         DS    F                                                                
LIMIT    DC    CL1' '                                                           
ONE      DC    PL1'1'                                                           
BLANKS   DC    132CL1' '                                                        
LCNTR    DC    PL2'00'                                                          
RCNTR    DC    PL2'00'                                                          
PNO      DS    PL2                                                              
PATRN    DS    CL3                                                              
MASK     DC    X'202020'                                                        
ELCODE   DS    X                                                                
SAVEKEY  DS    CL32                                                             
IOAREA   DS    CL1000                                                           
         SPACE 3                                                                
         ORG   IOAREA                                                           
       ++INCLUDE REGENCMT                                                       
         ORG                                                                    
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040REREP7502X07/23/96'                                      
         END                                                                    
