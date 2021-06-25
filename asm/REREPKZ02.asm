*          DATA SET REREPKZ02  AT LEVEL 155 AS OF 05/01/02                      
*PHASE REKZ02A,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE PERVERT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
         TITLE 'REREPKZ02 - GENERAL CONTRACT FIXER   '                          
*********************************************************************           
*                                                                   *           
*        REREPKZ02 --- GENERAL CONTRACT FIXER                       *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* NOV30/95 (BU ) --- INITIAL ENTRY:                                 *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*                                                                   *           
*********************************************************************           
*                                                                               
REKZ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**REKZ02,R7,R8,R9,RR=RE                                        
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         STM   R2,RC,SAVEREGS      SAVE REGS 2 -> C                             
         EJECT                                                                  
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
         B     MODEEXIT                                                         
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     MODEEXIT                                                         
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
*        DC    AL1(REQFRST),AL3(INITIAL)  REQUEST A CONTRACT                    
         DC    AL1(REQFRST),AL3(CBPURGE)  PURGE CONS/BUYS FOR KATZ              
*                                                                               
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
         DC    AL1(PROCCONT),AL3(POST)    WOTV/WOOD FIX                         
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
*                                                                               
INITIAL  NTR1                                                                   
         XC    PROCCTR,PROCCTR     INITIALIZE CONTRACT COUNTER                  
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,ALTHDR           STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
         B     MODEEXIT                                                         
         EJECT                                                                  
*              SHOW CONTRACT DETAILS                                            
         EJECT                                                                  
*                                                                               
*   POST:  CHANGE CONTRACT TYPE FOR SYN ORDERS TO 'Y' WHERE NOT                 
*     ALREADY DONE.                                                             
*                                                                               
POST     NTR1                                                                   
*                                                                               
POST0010 EQU   *                                                                
         CLI   RCONTYPE,C'Y'       CONTRACT TYPE ALREADY 'Y'?                   
         BE    MODEEXIT            YES - SKIP IT                                
         MVC   P+1(19),=C'TYPE CHANGED TO Y: '                                  
         MVC   P+20(10),=C'CONTRACT= '                                          
         GOTO1 HEXOUT,DMCB,RCONKCON,P+33,4,=C'TOG'                              
         GOTO1 REPORT                                                           
         MVI   RCONTYPE,C'Y'       INSERT NEW CONTRACT TYPE                     
         CLI   QOPTION1,C'U'       UPDATE RECORD?                               
         BNE   MODEEXIT            NO                                           
         BAS   RE,PUTCON           REWRITE RECORD                               
MODEEXIT LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   FOR EACH ORDER IN TABLE, DELETE ORDER.  SET DELETE BITS, CLEAR              
*      THE X'03' ELEMENTS FROM THE 0C RECORD.                                   
*                                                                               
*                                                                               
CBPURGE  NTR1                                                                   
*                                                                               
         LA    R2,CONTABLE         SET A(CONTRACT TABLE)                        
         B     CPRG0020                                                         
***      EASTMAN    ****                                                        
CONTABLE DC    C'EA01019473'                                                    
CTABLEN  EQU   *-CONTABLE                                                       
         DC    C'EA01130233'                                                    
         DC    C'EA00996559'                                                    
         DC    C'EA01287381'                                                    
         DC    C'EA01294198'                                                    
         DC    C'EA01296661'                                                    
         DC    C'EA01296622'                                                    
         DC    C'EA01316855'                                                    
***      KRGS       ****                                                        
         DC    C'K?01316855'                                                    
***      KHM        ****                                                        
         DC    C'KF01137092'                                                    
         DC    C'KF01137108'                                                    
         DC    C'KF01137112'                                                    
         DC    C'KF01137114'                                                    
         DC    C'KF01137115'                                                    
         DC    C'KF01137117'                                                    
         DC    C'KF01316855'                                                    
         DC    C'KF01137122'                                                    
         DC    C'KF01139534'                                                    
***      BANNER     ****                                                        
         DC    C'BF01166243'                                                    
         DC    C'BF01166242'                                                    
         DC    C'BF00996559'                                                    
         DC    C'BF01071599'                                                    
         DC    C'BF01286237'                                                    
         DC    C'BF01286268'                                                    
         DC    C'BF01316855'                                                    
         DC    C'BF01439153'                                                    
         DC    C'BF01469689'                                                    
***      CHRISTAL   ****                                                        
         DC    C'CR01166243'                                                    
         DC    C'CR01166236'                                                    
         DC    C'CR00996559'                                                    
         DC    C'CR01280230'                                                    
         DC    C'CR01316560'                                                    
         DC    C'CR01316855'                                                    
         DC    C'CR01457066'                                                    
         DC    C'CR01460001'                                                    
         DC    C'CR01439238'                                                    
         DC    C'CR01471406'                                                    
         DC    C'CR01017181'                                                    
         DC    C'CR01017184'                                                    
         DC    C'CR00991154'                                                    
***      KATZ RADIO ****                                                        
         DC    C'KU01142559'                                                    
         DC    C'KU01143500'                                                    
         DC    C'KU01143448'                                                    
         DC    C'KU01150994'                                                    
         DC    C'KU01150995'                                                    
         DC    C'KU01151726'                                                    
         DC    C'KU01151751'                                                    
         DC    C'KU01151727'                                                    
         DC    C'KU01151768'                                                    
         DC    C'KU01150885'                                                    
         DC    C'KU01150890'                                                    
         DC    C'KU01152418'                                                    
         DC    C'KU01152425'                                                    
         DC    C'KU01152430'                                                    
         DC    C'KU01150997'                                                    
         DC    C'KU01150210'                                                    
         DC    C'KU01150204'                                                    
         DC    C'KU01156711'                                                    
         DC    C'KU01156764'                                                    
         DC    C'KU01156757'                                                    
         DC    C'KU01156776'                                                    
         DC    C'KU01136679'                                                    
         DC    C'KU01152880'                                                    
         DC    C'KU01155584'                                                    
         DC    C'KU01155588'                                                    
         DC    C'KU01155599'                                                    
         DC    C'KU01166273'                                                    
         DC    C'KU01166282'                                                    
         DC    C'KU00996559'                                                    
         DC    C'KU01284660'                                                    
         DC    C'KU01264988'                                                    
         DC    C'KU01262998'                                                    
         DC    C'KU01265001'                                                    
         DC    C'KU01265009'                                                    
         DC    C'KU01316855'                                                    
         DC    C'KU01330599'                                                    
         DC    C'KU01330634'                                                    
         DC    C'KU01264998'                                                    
         DC    C'KU01265001'                                                    
         DC    C'KU01309657'                                                    
         DC    C'KU01464816'                                                    
         DC    C'KU01454163'                                                    
         DC    C'KU01450963'                                                    
         DC    C'KU01450974'                                                    
         DC    C'KU01450997'                                                    
         DC    H'00'                                                            
CPRG0020 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE REACHED?                        
         BE    CPRG1000            YES - FINISHED                               
         XC    KEY,KEY             CLEAR KEY FOR BUYLINE READ                   
         MVI   KEY,X'8C'           INSERT RECORD TYPE                           
         MVC   KEY+21,0(R2)        INSERT REP CODE                              
         GOTO1 =V(HEXIN),DMCB,2(R2),KEY+23,8                                    
*                                  NINE'S COMP THE CON #                        
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),KEY+23(4)                                             
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   KEY+23(4),WORK+15   INSERT THE COMP'D KEY                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    CPRG0060            YES                                          
         MVC   P+1(11),=C'NOT FOUND: '                                          
         MVC   P+15(10),0(R2)                                                   
         GOTO1 REPORT                                                           
         LA    R2,CTABLEN(R2)      NO  - BUMP TO NEXT TABLE ENTRY               
         B     CPRG0020            GO BACK FOR NEXT                             
CPRG0060 EQU   *                                                                
         MVC   P+1(11),=C'    FOUND: '                                          
         MVC   P+15(27),KEY                                                     
         GOTO1 REPORT                                                           
         GOTO1 GETCON                                                           
         ZICM  RF,RCONLEN,2                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,RCONREC),RCONREC,C'DUMP',(RF),=C'2D'          
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'03',RCONREC),0,0            
         ZICM  RF,RCONLEN,2                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,RCONREC),RCONREC,C'DUMP',(RF),=C'2D'          
         MVC   P+1(07),=C'PREKEY:'                                              
         MVC   P+10(34),KEY                                                     
         GOTO1 REPORT                                                           
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         MVC   P+1(08),=C'POSTKEY:'                                             
         MVC   P+10(34),KEY                                                     
         GOTO1 REPORT                                                           
         CLI   QOPTION1,C'U'       UPDATE PASS?                                 
         BNE   CPRG0080            NO  - DON'T REWRITE                          
         GOTO1 PUTCON              YES - REWRITE CONTRACT                       
         GOTO1 WRITE               REWRITE KEY AS DELETED                       
CPRG0080 EQU   *                                                                
*                                                                               
*   THE BUYS ARE NOW DELETED                                                    
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'0B'           INSERT RECORD TYPE                           
         MVC   KEY+16(2),0(R2)     INSERT REP CODE                              
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         PACK  KEY+18(1),WORK+18(1)    REVERSE THE COMPLIMENT                   
         PACK  KEY+19(1),WORK+17(1)                                             
         PACK  KEY+20(1),WORK+16(1)                                             
         PACK  KEY+21(1),WORK+15(1)                                             
         MVC   P+1(10),=C'BUYLINE = '                                           
         GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
         GOTO1 HEXOUT,DMCB,KEY+18,P+25,4,=C'TOG'                                
         GOTO1 REPORT                                                           
         GOTO1 HIGH                                                             
         B     CPRG0120                                                         
CPRG0100 EQU   *                                                                
         GOTO1 SEQ                                                              
CPRG0120 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME KEY THROUGH CON #?                      
         BE    CPRG0140            YES                                          
         MVC   P+1(14),=C'BUY NOT FOUND:'                                       
         MVC   P+18(27),KEYSAVE                                                 
         GOTO1 REPORT                                                           
         B     CPRG0240                                                         
CPRG0140 EQU   *                                                                
         MVC   P+1(07),=C'PREKEY:'                                              
         MVC   P+10(34),KEY                                                     
         GOTO1 REPORT                                                           
         OI    KEY+27,X'80'        TURN ON DELETE BIT                           
         MVC   P+1(08),=C'POSTKEY:'                                             
         MVC   P+10(34),KEY                                                     
         GOTO1 REPORT                                                           
         CLI   QOPTION1,C'U'       UPDATE PASS?                                 
         BNE   CPRG0160            NO  - DON'T REWRITE                          
         GOTO1 WRITE               REWRITE KEY AS DELETED                       
CPRG0160 EQU   *                                                                
         B     CPRG0100            GO BACK FOR NEXT BUY                         
CPRG0240 EQU   *                                                                
         LA    R2,CTABLEN(R2)      NO  - BUMP TO NEXT TABLE ENTRY               
         B     CPRG0020            GO BACK FOR NEXT                             
CPRG1000 EQU   *                                                                
         XIT1                      FINISHED - EXIT                              
*                                                                               
*                                  COMPLEMENT/REVERSE CONTRACT NUMBER           
         ZAP   WORK+15(5),=P'99999999'                                          
         MVO   WORK+15(5),RCONKCON                                              
         ZAP   WORK+5(5),=P'99999999'                                           
         SP    WORK+5(5),WORK+15(5)                                             
         MVO   WORK+15(5),WORK+5(5)                                             
         MVC   KEY+18(4),WORK+15                                                
         PACK  KEY+18(1),WORK+18(1)    REVERSE THE COMPLIMENT                   
         PACK  KEY+19(1),WORK+17(1)                                             
         PACK  KEY+20(1),WORK+16(1)                                             
         PACK  KEY+21(1),WORK+15(1)                                             
*        MVC   P+1(10),=C'BUYLINE = '                                           
**       GOTO1 HEXOUT,DMCB,RCONKCON,P+15,4,=C'TOG'                              
***      GOTO1 HEXOUT,DMCB,KEY+18,P+25,4,=C'TOG'                                
**       GOTO1 REPORT                                                           
*                                                                               
         LA    R5,RBUYREC                                                       
         ST    R5,AIOAREA          SET A(IO AREA)                               
         GOTO1 HIGH                                                             
         B     BUYL0060                                                         
BUYL0040 EQU   *                                                                
         GOTO1 SEQ                                                              
BUYL0060 EQU   *                                                                
         CLC   KEY(22),KEYSAVE                                                  
         BNE   BUYL0700            FINISHED - RELOAD CONTRACT                   
         GOTO1 GREC                                                             
         TM    RBUYCOMB,X'80'      COMBO PLACE HOLDER?                          
         BO    BUYL0040            YES - SKIP THE RECORD                        
         LA    R6,RBUYELEM                                                      
BUYL0070 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    BUYL0040            YES - GO GET NEXT RECORD                     
         CLI   0(R6),8             SPOT INTERFACE ELEMENT?                      
         BE    BUYL0080            YES - CHECK DATE                             
         ZIC   RF,1(R6)                                                         
         AR    R6,RF               NO  - BUMP TO NEXT ELEMENT                   
         B     BUYL0070            GO BACK FOR NEXT                             
BUYL0080 EQU   *                                                                
         USING RBUYSPEL,R6                                                      
         OC    RBUYSPDT,RBUYSPDT   ANY DATE TRANSFERRED?                        
         BNZ   BUYL0040            YES - GO BACK FOR NEXT RECORD                
         DROP  R6                                                               
         PRINT GEN                                                              
         MVC   P+1(12),=C'NO XFER DATE'                                         
         GOTO1 HEXOUT,DMCB,RCONKCON,P+20,4,=C'TOG'                              
***>>>   MVC   P+30(40),0(R6)                                                   
         GOTO1 REPORT                                                           
         B     BUYL0750                                                         
BUYL0700 EQU   *                                                                
***      MVC   P+1(16),=C'CONTRACT CORRECT'                                     
***      GOTO1 HEXOUT,DMCB,RCONKCON,P+20,4,=C'TOG'                              
***      GOTO1 REPORT                                                           
         PRINT NOGEN                                                            
BUYL0750 EQU   *                                                                
         MVC   KEY,KEYSAV2         RESTORE CONTRACT KEY                         
         GOTO1 READ                READ THE CONTRACT RECORD                     
BUYL0800 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
ALTHDR   EQU   *                                                                
         NTR1                                                                   
         USING ALTHDR,RF           ESTABLISH ADDRESSABILITY                     
         LA    R1,SAVEREGS-ALTHDR                                               
         AR    R1,RF                                                            
         LM    R2,RC,0(R1)                                                      
         DROP  RF                                                               
*                                                                               
         B     MODEEXIT                                                         
         EJECT                                                                  
GETCON   LA    RF,RCONREC                                                       
         B     LINKFILE                                                         
         SPACE 2                                                                
PUTCON   LA    RF,RCONREC                                                       
         B     PUTFILE                                                          
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         ST    RF,AIOAREA                                                       
         GOTO1 GREC                                                             
         MVC   LASTIO,DMCB+12      SAVE THESE VALUES                            
         MVC   LASTDA,KEY+28                                                    
         MVC   LASTLEN,27(R2)                                                   
*                                                                               
*  DATA MANAGER INTERFACE (CHECK ERRORS)                                        
*                                                                               
DMCHECK1 TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BZ    DM010                                                            
         TM    29(R2),X'80'        IS RECORD MARKED FOR DELETION?               
         BZ    DM020                                                            
         LTR   RB,RB               YES - RETURN WITH CC NE 0                    
         B     MODEEXIT                                                         
*                                                                               
DM010    TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    MODEEXIT                                                         
*                                                                               
DM020    MVC   WORK(41),=C'**********DATA MANAGER ERROR**********'              
         GOTO1 LOGIO,WORK+48,1,(41,WORK)                                        
         MVC   WORK(41),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
*                                                                               
*   ROUTINE TO TRACE DATA MANAGER CALLS                                         
*                                                                               
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   MTRACDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
*                                                                               
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
*                                                                               
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,MTRACDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         B     MODEEXIT                                                         
*                                                                               
MTRACDM8 DS    C                                                                
         DS    0H                                                               
         SPACE 3                                                                
PUTFILE  NTR1                                                                   
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BE    MODEEXIT            YES - DON'T REWRITE IT                       
         ST    RF,AIOAREA                                                       
         GOTO1 PREC                                                             
         B     MODEEXIT                                                         
         SPACE 3                                                                
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
*              WORK SPACE ETC                                                   
         SPACE 1                                                                
*                                                                               
PROCCTR  DS    F                   CONTRACTS READ      CTR                      
CHGDCTR  DS    F                   CONTRACTS PROCESSED CTR                      
BIGRCTR  DS    F                   CONTRACTS MADE LARGER                        
SMLRCTR  DS    F                   CONTRACTS MADE SMALLER                       
SAMESIZE DS    F                   CONTRACTS SAME SIZE                          
FIRSTCON DS    CL2                                                              
KEYSAV2  DS    CL27                ALTERNATE KEY SAVE AREA                      
AIOAREA  DS    A                                                                
COMMAND  DS    CL8                                                              
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
NEW23ELT DS    CL10                                                             
MYP      DS    CL132                                                            
TOTDAYS  DS    F                                                                
CYCLEDAT DS    CL6                                                              
DAYTABLE DS    14F                                                              
RELO     DS    F                   RELOCATION ADDRESS                           
SAVEREGS DS    11F                                                              
FILLER   DS    6000C                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*        PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENALL1                                                      
*                                                                               
*   COMMISSION RECORD IS ORG'D TO THE BUY RECORD, WHICH ISN'T USED,             
*    RATHER THAN THE CONTRACT RECORD, WHICH IS                                  
*                                                                               
         ORG RBUYREC                                                            
       ++INCLUDE REGENCOM                                                       
         ORG                                                                    
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
BROADTBL DSECT                                                                  
BRDTABLE DS    0CL7                                                             
         ORG   BRDTABLE                                                         
BRDSTART DS    XL3                                                              
BRDEND   DS    XL3                                                              
BRDWEEKS DS    XL1                                                              
BRDLEN   EQU   *-BRDSTART                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'155REREPKZ02 05/01/02'                                      
         END                                                                    
