*          DATA SET REREPSDUB  AT LEVEL 137 AS OF 05/01/02                      
*PHASE REST02C,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE HEXIN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE SCANNER                                                                
         TITLE 'REREPSDUB - KATZ RADIO INVOICE FIX - DOUBLE TAKE'               
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPSDUB -- SCAN STATIONS FOR TAKEOVER ORDERS.           *            
*                     DETERMINE ORIGINAL REP/ORDER, AND RETRIEVE   *            
*                     INVOICE DATA FROM ORIGINAL ORDER.  PLUG IN   *            
*                     TO TAKEOVER ORDER. GO BACK TWO TAKEOVERS.    *            
*                                                                  *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* NOV18/00 (BU ) --- ORIGINAL ENTRY                                *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  LIST OF DISPLAYS (REQUESTOR VALUE TRIGGER = Y IN BYTE:)         *            
*     QUESTOR     Y = DISPLAY STATION TABLE                        *            
*                                                                  *            
*     QRECORD+36  =  'FROM' REP                                    *            
*     QRECORD+38  =  'TO  ' REP                                    *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REST02   CSECT                                                                  
         NMOD1 0,**ST02**,R8,R9,RR=R5                                           
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    SW10                                                             
         XIT1                                                                   
         EJECT                                                                  
SW10     DS    0H                                                               
         OPEN  (INTAPE1,(INPUT))   TAPE1:  STATION LIST                         
*                                                                               
         GOTO1 INITIAL,DMCB,(RC)                                                
*                                                                               
         GOTO1 LOADTABL,DMCB,(RC)                                               
*                                                                               
         CLOSE (INTAPE1,REWIND)                                                 
*                                                                               
         GOTO1 FILEPROC,DMCB,(RC)  PROCESS BULK OF FILE                         
*                                                                               
         GOTO1 DISPTOTS,DMCB,(RC)  DISPLAY TOTALS FOR RUN                       
*                                                                               
*                                                                               
*                                                                               
         GOTO1 REPORT                                                           
         MVC   P+1(31),=C'***MAIN PROCESSING COMPLETED***'                      
         GOTO1 REPORT                                                           
*                                                                               
         XIT1                      EXIT                                         
         EJECT                                                                  
******************************************************************              
*  FILEPROC:  PROCESS EACH STATION IN THE STATION LIST.          *              
*        RETRIEVE EACH CONTRACT.  MARK IT AND KEY FOR DELETION.  *              
*        ACCESS EACH CONTRACT'S BUYS AND COVERSHEET RECORDS, AND *              
*        MARK THEM AND KEYS FOR DELETION.  PROCESS ALL CONTRACTS *              
*        FOR ALL STATIONS IN LIST.                               *              
******************************************************************              
FILEPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    KEY,KEY                                                          
         MVI   KEY,1               RETRIEVE REP RECORD                          
         MVC   KEY+25(2),=C'K3'                                                 
         GOTO1 HIGHDIR                                                          
         GOTO1 GETRECRD                                                         
         LA    R5,REC                                                           
         USING RREPREC,R5                                                       
         MVC   P+1(07),=C'REP IS:'                                              
         MVC   P+10(33),RREPNAME                                                
         GOTO1 REPORT                                                           
*                                                                               
         L     R5,ASTAAREA         SET A(STATION LIST)                          
FPRO0020 EQU   *                                                                
         CLI   0(R5),X'FF'         END OF TABLE REACHED?                        
         BE    FPRO0900            YES - JOB FINISHED                           
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'0C'           SET CONTRACT KEY                             
         MVC   KEY+2(9),0(R5)      INSERT REP/GR-SGRP/STATION                   
         MVC   SAVEKEY(11),KEY     SAVE HI-ORDER KEY                            
         GOTO1 HIGHDIR                                                          
         B     FPRO0060                                                         
FPRO0040 EQU   *                                                                
         GOTO1 SEQDIR                                                           
FPRO0060 EQU   *                                                                
         CLC   KEY(11),SAVEKEY     SAME REP/GR-SGRP/STATION?                    
         BNE   FPRO0800            NO  - BUMP STATION TABLE                     
*                                                                               
         GOTO1 GETRECRD            YES - RETRIEVE CONTRACT RECORD               
         BAS   RE,CONTPROC         PROCESS CONTRACT RECORD                      
*                                                                               
         B     FPRO0040            GO BACK FOR NEXT RECORD                      
FPRO0800 EQU   *                                                                
         LA    R5,STATABLN(R5)     BUMP TO NEXT TABLE ENTRY                     
         B     FPRO0020                                                         
FPRO0900 EQU   *                                                                
         MVC   P+1(25),=C'BUCKET FIX PASS COMPLETED'                            
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
******************************************************************              
*  CONTPROC:  PROCESS EACH CONTRACT RECORD                       *              
*        FIND X'2A' ELEMENT:  DETERMINE IF IT'S A TAKEOVER       *              
*             DONE AFTER 11/10/00.  IF NOT, SKIP IT              *              
*        RETRIEVE ORIGINAL RECORD.                               *              
*        UNLOAD ALL THE X'04' ELEMENTS.                          *              
*        RE-RETRIEVE THE TARGET RECORD.                          *              
*        ADD THE X'04' ELEMENTS TO IT                            *              
*        REWRITE THE RECORD.                                     *              
*                                                                *              
*                                                                *              
******************************************************************              
CONTPROC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    R5,REC                                                           
         USING RCONREC,R5                                                       
*                                                                               
*   TEST CYCLE DISPLAY                                                          
         MVC   P+1(14),=C'CONTRACT READ:'                                       
         MVC   P+16(27),RCONKEY                                                 
         GOTO1 REPORT                                                           
*   END CYCLE DISPLAY                                                           
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'2A'        RETRIEVE TAKEOVER ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   CPRO0800            NOT TAKEOVER:  SKIP IT                       
*                                                                               
*   TEST CYCLE DISPLAY1                                                         
         MVC   P+1(14),=C'2A ELT FOUND :'                                       
         MVC   P+16(10),0(R6)                                                   
         GOTO1 REPORT                                                           
*   END CYCLE DISPLAY                                                           
*                                                                               
         USING RCONMMEL,R6                                                      
         CLC   RCONMMDT,=X'C96A'   TKO ON OR AFTER NOV10/00                     
         BL    CPRO0800            PRIOR:  SKIP THIS ONE                        
*                                                                               
*   TEST CYCLE DISPLAY3                                                         
         MVC   P+1(14),=C'DATE OKAY    :'                                       
         MVC   P+16(10),0(R6)                                                   
         GOTO1 REPORT                                                           
*   END CYCLE DISPLAY                                                           
*                                                                               
         MVC   SAVCON#,RCONMMOC    SAVE ORIGINAL CONTRACT NUMBER                
         MVC   SAVREPC,RCONMMOR    SAVE ORIGINAL REP CODE                       
*                                                                               
         DROP  R6                                                               
*                                                                               
         L     RF,CONCTR           COUNT TOTAL RECORDS SCANNED                  
         LA    RF,1(RF)                                                         
         ST    RF,CONCTR           RESTORE COUNTER                              
*                                                                               
*   TEST DISPLAY                                                                
         CLC   CONCTR,=F'50'                                                    
         BH    TSTC0010                                                         
         EDIT  CONCTR,(4,P+1)                                                   
         MVC   P+6(13),=C'CONTRACT PRE:'                                        
         MVC   P+22(34),KEY                                                     
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         LA    R4,REC              A(RECORD)                                    
         SR    RF,RF                                                            
         ZICM  RF,RCONLEN,2        GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
TSTC0010 EQU   *                                                                
*                                                                               
         MVC   REKEY,RCONKEY       SAVE KEY FOR RESTART                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'                                                        
         MVC   KEY+21(2),SAVREPC   INSERT ORIGINAL REP CODE                     
*                                  CONVERT CON# TO REVERSE                      
         GOTO1 HEXOUT,DMCB,SAVCON#,MYWORK,4,=C'TOG'                             
         PACK  DUB(8),MYWORK(8)                                                 
*                                                                               
         UNPK  MYWORK(8),DUB(8)                                                 
         OI    MYWORK+7,X'F0'      TURN ON ALL ZONE BITS                        
*                                                                               
         GOTO1 =V(HEXIN),DMCB,MYWORK,MYWORK+12,8,=C'TOG'                        
*                                                                               
         MVC   NEWCON,MYWORK+12                                                 
*                                                                               
*   CALCULATE CONTRACT NUMBER REVERSED                                          
*                                                                               
         ZAP   MYWORK+30(5),=P'0'                                               
         MVO   MYWORK+30(5),NEWCON                                              
         ZAP   MYWORK+25(5),=P'99999999'                                        
         SP    MYWORK+25(5),MYWORK+30(5) GET 9'S COMPLEMENT                     
         MVO   MYWORK+20(5),MYWORK+25(5) CHANGE TO PWOS                         
         MVC   KEY+23(4),MYWORK+20 INSERT ORIGINAL CON# REVERSED                
*                                                                               
         GOTO1 HIGHDIR                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    CPRO0020            KEY FOUND                                    
         MVC   P+1(14),=C'KEY NOT FOUND:'                                       
         MVC   P+20(27),KEY                                                     
         MVC   P+50(27),KEYSAVE                                                 
         GOTO1 REPORT                                                           
         MVC   KEY,REKEY           RESTART FROM KEY                             
         GOTO1 HIGHDIR                                                          
         CLC   KEY(27),KEYSAVE     CHECK RESTART OKAY                           
         BE    CPRO0800            KEY FOUND: EXIT THIS ROUTINE                 
         MVC   P+1(14),=C'RESTART FAILS:'                                       
         MVC   P+20(27),KEY                                                     
         MVC   P+50(27),KEYSAVE                                                 
         GOTO1 REPORT                                                           
         DC    H'0'                                                             
CPRO0020 EQU   *                                                                
         GOTO1 GETRECRD            RETRIEVE ORIGINAL CONTRACT RECORD            
*                                                                               
*   TRY TO BACK UP ONE MORE CONTRACT:  LOOK FOR 'ORIGINAL' ORDER'S              
*       2A ELEMENT, AND SEE IF IT MEETS THE REQUIREMENT.  IF SO,                
*       GET IT AND USE ITS INTERNAL BUCKETS.                                    
*                                                                               
*                                                                               
*   TEST CYCLE DISPLAY                                                          
         MVC   P+1(14),=C'NEXT 2A FIND :'                                       
         MVC   P+16(27),RCONKEY                                                 
         GOTO1 REPORT                                                           
*   END CYCLE DISPLAY                                                           
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'2A'        RETRIEVE TAKEOVER ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   CPRO0048            NOT 2ND GENERATION TAKEOVER:                 
*                                     USE THIS RECORD                           
*                                                                               
*   TEST CYCLE DISPLAY1                                                         
         MVC   P+1(14),=C'2A 2ND GEN   :'                                       
         MVC   P+16(10),0(R6)                                                   
         GOTO1 REPORT                                                           
*   END CYCLE DISPLAY                                                           
*                                                                               
         USING RCONMMEL,R6                                                      
         CLC   RCONMMDT,=X'C96A'   TKO ON OR AFTER NOV10/00                     
         BL    CPRO0048            PRIOR:  USE THIS RECORD                      
*                                                                               
*   TEST CYCLE DISPLAY3                                                         
         MVC   P+1(14),=C'DATE OKAY    :'                                       
         MVC   P+16(10),0(R6)                                                   
         GOTO1 REPORT                                                           
*   END CYCLE DISPLAY                                                           
*                                                                               
         MVC   SAVCON#,RCONMMOC    SAVE ORIGINAL CONTRACT NUMBER                
         MVC   SAVREPC,RCONMMOR    SAVE ORIGINAL REP CODE                       
*                                                                               
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'                                                        
         MVC   KEY+21(2),SAVREPC   INSERT ORIGINAL REP CODE                     
*                                  CONVERT CON# TO REVERSE                      
         GOTO1 HEXOUT,DMCB,SAVCON#,MYWORK,4,=C'TOG'                             
         PACK  DUB(8),MYWORK(8)                                                 
*                                                                               
         UNPK  MYWORK(8),DUB(8)                                                 
         OI    MYWORK+7,X'F0'      TURN ON ALL ZONE BITS                        
*                                                                               
         GOTO1 =V(HEXIN),DMCB,MYWORK,MYWORK+12,8,=C'TOG'                        
*                                                                               
         MVC   NEWCON,MYWORK+12                                                 
*                                                                               
*   CALCULATE CONTRACT NUMBER REVERSED                                          
*                                                                               
         ZAP   MYWORK+30(5),=P'0'                                               
         MVO   MYWORK+30(5),NEWCON                                              
         ZAP   MYWORK+25(5),=P'99999999'                                        
         SP    MYWORK+25(5),MYWORK+30(5) GET 9'S COMPLEMENT                     
         MVO   MYWORK+20(5),MYWORK+25(5) CHANGE TO PWOS                         
         MVC   KEY+23(4),MYWORK+20 INSERT ORIGINAL CON# REVERSED                
*                                                                               
         GOTO1 HIGHDIR                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    CPRO0040            KEY FOUND                                    
         MVC   P+1(14),=C'KEY NOT FOUND:'                                       
         MVC   P+20(27),KEY                                                     
         MVC   P+50(27),KEYSAVE                                                 
         GOTO1 REPORT                                                           
         MVC   KEY,REKEY           RESTART FROM KEY                             
         GOTO1 HIGHDIR                                                          
         CLC   KEY(27),KEYSAVE     CHECK RESTART OKAY                           
         BE    CPRO0800            KEY FOUND: EXIT THIS ROUTINE                 
         MVC   P+1(14),=C'RESTART FAILS:'                                       
         MVC   P+20(27),KEY                                                     
         MVC   P+50(27),KEYSAVE                                                 
         GOTO1 REPORT                                                           
         DC    H'0'                                                             
CPRO0040 EQU   *                                                                
         GOTO1 GETRECRD            RETRIEVE ORIGINAL CONTRACT RECORD            
         B     CPRO0050                                                         
CPRO0048 EQU   *                                                                
         MVC   P+1(14),=C'1ST GEN ORDER:'                                       
         GOTO1 REPORT                                                           
CPRO0050 EQU   *                                                                
*                                                                               
*   TEST DISPLAY                                                                
         CLC   CONCTR,=F'50'                                                    
         BH    TSTC0020                                                         
         EDIT  CONCTR,(4,P+1)                                                   
         MVC   P+6(14),=C'CONTRACT ORIG:'                                       
         MVC   P+22(34),KEY                                                     
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         LA    R4,REC              A(RECORD)                                    
         SR    RF,RF                                                            
         ZICM  RF,RCONLEN,2        GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
TSTC0020 EQU   *                                                                
*                                                                               
         MVC   SAVCREA,RCONCREA    SAVE ORIGINAL CREATE DATE                    
         MVC   SAVHDRD,RCONHDRD    SAVE ORIGINAL HEADER DATE                    
         L     R3,AX03AREA         SET A(EST BUCKET TABLE)                      
         XC    0(10,R3),0(R3)      CLEAR FIRST ENTRY                            
         L     R4,AX04AREA         SET A(INV BUCKET TABLE)                      
         XC    0(10,R4),0(R4)      CLEAR FIRST ENTRY                            
         LA    R2,RCONELEM         RETRIEVE INVOICE BUCKETS                     
CPRO0080 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    CPRO0160            YES - NOW PROCESS TKO RECORD                 
         CLI   0(R2),X'03'         ESTIMATE ELEMENT?                            
         BE    CPRO0100            YES -                                        
         CLI   0(R2),X'04'         INVOICE ELEMENT?                             
         BE    CPRO0120            YES -                                        
         B     CPRO0140            NO  - SKIP THIS ELEMENT                      
CPRO0100 EQU   *                                                                
         MVC   0(10,R3),0(R2)      INSERT EST ELT INTO TABLE                    
         XC    10(10,R3),10(R3)    CLEAR NEXT ENTRY                             
         LA    R3,10(R3)           BUMP TO NEXT SLOT                            
         B     CPRO0140            CYCLE CONTRACT RECORD                        
CPRO0120 EQU   *                                                                
         MVC   0(10,R4),0(R2)      INSERT INV ELT INTO TABLE                    
         XC    10(10,R4),10(R4)    CLEAR NEXT ENTRY                             
         LA    R4,10(R4)           BUMP TO NEXT SLOT                            
         B     CPRO0140            CYCLE CONTRACT RECORD                        
CPRO0140 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         B     CPRO0080            GO BACK FOR NEXT                             
CPRO0160 EQU   *                                                                
         MVC   KEY,REKEY           RETRIEVE TARGET CONTRACT                     
         GOTO1 HIGHDIR                                                          
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NO                                           
         GOTO1 GETRECRD            GET THE RECORD AGAIN                         
*                                                                               
*   TEST DISPLAY                                                                
         CLC   CONCTR,=F'50'                                                    
         BH    TSTC0030                                                         
         EDIT  CONCTR,(4,P+1)                                                   
         MVC   P+6(14),=C'RETRIEVED TKO:'                                       
         MVC   P+22(34),KEY                                                     
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         LA    R4,REC              A(RECORD)                                    
         SR    RF,RF                                                            
         ZICM  RF,RCONLEN,2        GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
TSTC0030 EQU   *                                                                
*                                  FIRST PROCESS THE X'03' ELTS                 
         MVC   RCONCREA,SAVCREA    RESET ORIGINAL CREATE DATE                   
         MVC   RCONHDRD,SAVHDRD    RESET ORIGINAL HEADER DATE                   
*                                                                               
         LA    R6,RCONREC          SET A(CONTRACT RECORD)                       
         MVI   ELCODE,X'03'        RETRIEVE X'03' ELEMENTS                      
         BAS   RE,GETEL                                                         
         B     CPRO0200                                                         
CPRO0180 EQU   *                                                                
         BAS   RE,NEXTEL                                                        
CPRO0200 EQU   *                                                                
         BNE   CPRO0300            FINISHED W/X'03'                             
         XC    DATE03,DATE03                                                    
         L     R3,AX03AREA         SCAN TABLED X'03'S                           
CPRO0220 EQU   *                                                                
         CLI   0(R3),0             END OF X'03' ELTS?                           
         BE    CPRO0280            YES - NO EXACT MATCH                         
         CLC   0(4,R6),0(R3)       CON BKT 1ST 4 BYTES VS                       
*                                     TABLE 1ST 4 BYTES                         
         BNE   CPRO0260            NOT SAME BUCKET                              
*                                                                               
*   SAME BUCKET, MONTH OF SERVICE                                               
*                                                                               
         CLC   6(4,R6),6(R3)       CON BKT LAST 4 BYTES VS                      
*                                     TABLE LAST 4 BYTES                        
         BNE   CPRO0240            NOT SAME BUCKET                              
*                                                                               
*   SAME BUCKET, MONTH OF SERVICE, SAME DOLLAR VALUE                            
*                                                                               
         MVC   4(2,R6),4(R3)       SAME BUCKET:  REINSERT ORIGINAL              
*                                     ACTIVITY DATE                             
         B     CPRO0180            GO BACK FOR NEXT CON X'03'                   
DATE03   DS    XL2                                                              
         DS    0H                                                               
CPRO0240 EQU   *                                                                
*                                                                               
*   SAME BUCKET, MONTH OF SERVICE:  DIFFERENT DOLLARS                           
*        SAVE EARLIEST ACTIVITY DATE                                            
*                                                                               
         OC    DATE03,DATE03       ACTIVITY DATE ALREADY SAVED?                 
         BNZ   CPRO0260            YES                                          
         MVC   DATE03,4(R3)        NO  - SAVE ACTIVITY DATE                     
CPRO0260 EQU   *                                                                
         LA    R3,10(R3)           BUMP TO NEXT X'03' IN TABLE                  
         B     CPRO0220                                                         
CPRO0280 EQU   *                                                                
         OC    DATE03,DATE03       ANY ACTIVITY DATE?                           
         BZ    CPRO0180            NO  - GO BACK FOR NEXT CON X'03'             
         CLC   4(2,R6),=C'C96A'    BUCKET AFTER NOV10/00                        
         BL    CPRO0180            NO  - GO BACK FOR NEXT CON X'03'             
         MVC   4(2,R6),DATE03      YES - INSERT EARLIEST FOUND                  
*                                     ACTIVITY DATE                             
         B     CPRO0180            GO BACK FOR NEXT CON X'03'                   
CPRO0300 EQU   *                                                                
         L     R4,AX04AREA         SET A(INV ELT TABLE AREA)                    
CPRO0320 EQU   *                                                                
         CLI   0(R4),0             END OF TABLE?                                
         BE    CPRO0340            YES - ALL ELTS REINSERTED                    
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RCONREC,0(R4),0                
*                                  INSERT NEW ELEMENT INTO RECORD               
         LA    R4,10(R4)           BUMP TO NEXT TABLE SLOT                      
         B     CPRO0320            GO BACK FOR NEXT                             
CPRO0340 EQU   *                                                                
*                                                                               
*   TEST DISPLAY                                                                
         CLC   CONCTR,=F'50'                                                    
         BH    TSTC0040                                                         
         EDIT  CONCTR,(4,P+1)                                                   
         MVC   P+6(14),=C'UPDATED   TKO:'                                       
         MVC   P+22(34),KEY                                                     
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         LA    R4,REC              A(RECORD)                                    
         SR    RF,RF                                                            
         ZICM  RF,RCONLEN,2        GET LENGTH OF ENTRY                          
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
TSTC0040 EQU   *                                                                
*                                  FIRST PROCESS THE X'03' ELTS                 
         L     RF,CUPCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CUPCTR                                                        
*                                                                               
         CLI   QOPTION2,C'U'       UPDATE FILE?                                 
         BNE   CPRO0360            NO                                           
         GOTO1 PREC                YES                                          
CPRO0360 EQU   *                                                                
*                                                                               
CPRO0800 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
SAVCON#  DS    CL4                                                              
SAVREPC  DS    CL2                                                              
SAVCREA  DS    XL3                                                              
SAVHDRD  DS    XL3                                                              
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
* DISPLAY TOTALS                                                                
******************************************************************              
DISPTOTS NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   P+1(30),=C'STATIONS      PROCESSED      :'                       
         EDIT  STACTR,(12,P+36),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'CONTRACTS     PROCESSED      :'                       
         EDIT  CONCTR,(12,P+36),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         MVC   P+1(30),=C'CONTRACTS     UPDATED        :'                       
         EDIT  CUPCTR,(12,P+36),COMMAS=YES                                      
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*   INITIALIZATIONS ....                                                        
*                                                                               
*   NEED TO LOAD UP A TABLE OF ALL EXISTING STATIONS/REPS ON FILE               
*        TO DETERMINE IF A STATION RECORD MUST BE GENERATED.                    
*                                                                               
*                                                                               
*                                                                               
******************************************************************              
INITIAL  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         MVC   P+1(23),=C'FILE WRITING IS ENABLED'                              
         CLI   QOPTION2,C'U'       UPDATE RUN?                                  
         BE    INIT0010                                                         
         MVC   P+1(24),=C'FILE WRITING IS DISABLED'                             
         MVC   PREC(2),=X'0000'    FORCE DUMP                                   
INIT0010 EQU   *                                                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',800000,800000                                   
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         MVC   P+1(30),=C'CANNOT GET SPACE: COVAIL ABORT'                       
         GOTO1 REPORT                                                           
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVI   RCSUBPRG,2          SET HEADING FLAG                             
         MVC   ABLDAREA,P2         ADDITIONAL WORKSPACE                         
         MVC   ASTAAREA,P2         A(STATION 1 TABLE)                           
         MVC   ANEXTSTA,P2         A(NEXT STATION SLOT)                         
         L     RF,ABLDAREA         ESTABLISH WORKSPACE SETUP                    
         A     RF,=F'14000'        14K FOR STATION 1 STORAGE                    
*                                    2,000 ENTRIES -                            
*                                  +0    2 BYTES OLD REP                        
*                                  +2    5 BYTES STATION CALL LTRS              
         ST    RF,AIO2             A(IOAREA #2)                                 
*                                                                               
         A     RF,=F'4000'         4K FOR ALTERNATE IO AREA                     
         ST    RF,ATAPEREC         A(TAPE RECORD READ AREA)                     
*                                                                               
         A     RF,=F'4000'         4K FOR TAPE READ AREA                        
         ST    RF,AWORKBLK         A(WORK BLOCK FOR SCANNER)                    
*                                                                               
         A     RF,=F'6000'         6K FOR WORK BLOCK AREA                       
         ST    RF,AX04AREA         A(INVOICE BUCKET TABLE)                      
         ST    RF,ANEXTX04         A(NEXT SLOT INV BUCKET)                      
*                                                                               
         A     RF,=F'6000'         6K FOR WORK BLOCK AREA                       
         ST    RF,AX03AREA         A(ESTIMATE BUCKET TABLE)                     
         ST    RF,ANEXTX03         A(NEXT SLOT EST BUCKET)                      
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE FIRST PAGE BREAK                       
                                                                                
         XC    CONCTR,CONCTR       CLEAR COUNTERS                               
         XC    CONCTR2,CONCTR2                                                  
         XC    CONCTR3,CONCTR3                                                  
         XC    CONCTR4,CONCTR4                                                  
         XC    BUYCTR,BUYCTR                                                    
         XC    STACTR,STACTR                                                    
         XC    SALCTR,SALCTR                                                    
         XC    CONBYTES,CONBYTES                                                
         XC    BUYBYTES,BUYBYTES                                                
         MVC   OLDREP,QRECORD+36   'FROM' REP                                   
         MVC   NEWREP,QRECORD+38   'TO' REP                                     
         MVC   REPREP,QRECORD+40   'REPLACEMENT' REP                            
INIT0120 EQU   *                                                                
         XIT1                                                                   
INIT0200 EQU   *                                                                
         DC    H'0'                NO REFERENCE FOR THIS REP                    
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*   LOAD TABLE VALUES                                                           
******************************************************************              
*                                                                               
*   SCANNER DISPLACEMENT EQUATES: TO BEGINNING OF SCANNER WORK FIELD            
*                                                                               
OLDREPSC EQU   0                                                                
STATION  EQU   52                                                               
OLDGROUP EQU   104                                                              
NEWGROUP EQU   156                                                              
NEWREPSC EQU   208                                                              
*                                                                               
*   STATION SLOT DISPLACEMENT EQUATES: SLOTS IN TABLE ASTAAREA                  
*                                                                               
SOLDREP  EQU   0                                                                
SOLDGRP  EQU   2                                                                
SSTATION EQU   4                                                                
*                                                                               
*                                                                               
LOADTABL NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         LA    RF,REC              SET A(RECORD)                                
         ST    RF,AREC                                                          
*                                                                               
         L     R5,ATAPEREC         SET A(TAPE INPUT AREA)                       
LDIR0020 EQU   *                                                                
         L     R7,ANEXTSTA         SET A(NEXT STATION SLOT)                     
         XC    0(250,R5),0(R5)     CLEAR INPUT AREA                             
         GET   INTAPE1,(R5)        READ AGY/ADV TAPE RECORD INTO RDA            
*                                                                               
         L     RF,STATOTAL                                                      
         LA    RF,1(RF)                                                         
         ST    RF,STATOTAL                                                      
*                                     TAPE WILL BE PARSED ON ',' SEPS           
**       GOTO1 REPORT                                                           
**       MVC   P+1(13),=C'INPUT RECORD:'                                        
**       MVC   P+20(80),0(R5)                                                   
**       GOTO1 REPORT                                                           
*                                                                               
         XC    MYWORK,MYWORK       CLEAR WORK SPACE                             
         ZICM  RF,0(R5),2          GET LENGTH OF INPUT RECORD                   
         BCTR  RF,0                SUBTRACT 1 FOR MOVE                          
         EX    RF,LDIRMOV1         MOVE BY LENGTH                               
         B     LDIR0040                                                         
LDIRMOV1 MVC   MYWORK+8(0),4(R5)                                                
LDIR0040 EQU   *                                                                
         ZICM  RF,0(R5),2          LOAD LENGTH OF RECORD                        
         SH    RF,=H'4'            SUBTRACT CONTROL LENGTH                      
         STC   RF,MYWORK+5                                                      
*        MVC   P+1(15),=C'MYWORK  RECORD:'                                      
*        MVC   P+20(80),MYWORK                                                  
*        GOTO1 REPORT                                                           
*                                                                               
LDIR0060 EQU   *                                                                
         L     RF,AWORKBLK         SET A(WORKSPACE)                             
         XC    0(256,RF),0(RF)     CLEAR WORK SPACE                             
         GOTO1 =V(SCANNER),DMCB,(30,MYWORK),AWORKBLK,C',=,='                    
                                                                                
         CLI   DMCB+4,0                                                         
         BNE   LDIR0080                                                         
         DC    H'0'                                                             
*                                                                               
LDIR0080 EQU   *                                                                
*                                                                               
*   TEST PRNTBL                                                                 
*        MVC   P+1(12),=C'SCANNER OKAY'                                         
*        GOTO1 REPORT                                                           
*        L     R4,AWORKBLK         A(SCANNER OUTPUT)                            
*        LA    RF,256                                                           
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*   TEST PRNTBL END                                                             
*                                                                               
         L     RF,AWORKBLK         SET A(WORKSPACE)                             
         LA    RF,256(RF)                                                       
         ST    RF,AWRKBLK2                                                      
         XC    0(256,RF),0(RF)     CLEAR WORK SPACE                             
         L     RE,AWORKBLK         SET A(WORKSPACE)                             
         XC    MYWORK2,MYWORK2                                                  
         MVC   MYWORK2+8(12),STATION+12(RE)   SET STATION                       
         MVC   MYWORK2+5(01),STATION(RE)      SET LENGTH                        
*                                  SPLIT STATION/MEDIA INPUT                    
         GOTO1 =V(SCANNER),DMCB,MYWORK2,AWRKBLK2,C',=-='                        
                                                                                
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AWORKBLK         SET A(WORKSPACE)                             
         CLC   =C'N/A',OLDREPSC+12(R2)                                          
*                                  NO-REP STATION?                              
         BNE   LDIR0109            NO                                           
**       MVC   P+1(24),=C'NON-REP STATION  FOUND: '                             
**       MVC   P+30(12),OLDREPSC+12(R2)                                         
**       MVC   P+45(16),0(R5)                                                   
**       GOTO1 REPORT                                                           
         B     LDIR0020            GO BACK FOR NEXT                             
LDIR0109 EQU   *                                                                
         LA    RF,COCONVRT         SET A(COMPANY NAME CONVERSION TABLE)         
LDIR0100 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    LDIR0110            YES - NOT FOUND: SHOULDN'T HAPPEN            
*                                     PUT OUT MESSAGE                           
         CLC   OLDREPSC+12(12,R2),0(RF)                                         
*                                  SCANNER NAME IN TABLE?                       
         BE    LDIR0120            YES                                          
         LA    RF,LCOCONV(RF)      NO  - BUMP TO NEXT SLOT                      
         B     LDIR0100            GO BACK FOR NEXT                             
LDIR0110 EQU   *                                                                
         MVC   P+1(24),=C'OLD COMPANY  NOT FOUND: '                             
         MVC   P+30(12),OLDREPSC+12(R2)                                         
         MVC   P+45(16),0(R5)                                                   
         GOTO1 REPORT                                                           
         B     LDIR0020            GO BACK FOR NEXT                             
LDIR0120 EQU   *                                                                
         MVC   SOLDREP(2,R7),12(RF)                                             
*                                  INSERT OLD REP INTO TABLE                    
*   STATION HAS BEEN SPLIT IN ALTERNATE WORK AREA                               
*                                                                               
*                                                                               
*   TEST PRNTBL                                                                 
*        MVC   P+1(13),=C'STATION SPLIT'                                        
*        GOTO1 REPORT                                                           
*        L     R4,AWRKBLK2         A(STATION SPLIT)                             
*        LA    RF,64                                                            
*        GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*   TEST PRNTBL END                                                             
*                                                                               
         L     R4,AWRKBLK2                                                      
         MVC   SSTATION(4,R7),12(R4)                                            
*                                  INSERT STATION INTO TABLE                    
         MVC   SSTATION+4(1,R7),44(R4)                                          
*                                  INSERT MEDIA INTO TABLE                      
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,2               ACCESS STATION RECORD                        
         MVC   KEY+20(2),SOLDREP(R7)                                            
         MVC   KEY+22(5),SSTATION(R7)                                           
         GOTO1 HIGHDIR                                                          
         CLC   KEY(27),KEYSAVE     KEY FOUND ON FILE?                           
         BE    LDIR0140            YES                                          
         MVC   P+1(16),=C'STATION MISSING:'                                     
         MVC   P+20(27),KEYSAVE                                                 
         GOTO1 REPORT                                                           
         XC    0(STATABLN,R7),0(R7) CLEAR ENTRY                                 
         MVC   0(2,R7),=X'FFFF'    SET END OF TABLE                             
         B     LDIR0020            GO BACK FOR NEXT ENTRY                       
LDIR0140 EQU   *                                                                
         GOTO1 GETRECRD            RETRIEVE RECORD                              
         LA    R6,REC                                                           
         USING RSTAREC,R6                                                       
         MVC   SOLDGRP(2,R7),RSTAGRUP                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    R7,STATABLN(R7)     BUMP TO NEXT AVAILABLE SLOT                  
         ST    R7,ANEXTSTA                                                      
         MVC   0(2,R7),=X'FFFF'    SET END OF TABLE                             
         L     R1,STACTR                                                        
         LA    R1,1(R1)            INCREMENT COUNTER                            
         ST    R1,STACTR           SAVE COUNT                                   
         B     LDIR0020            GO BACK FOR NEXT RECORD                      
***>>>                                                                          
*                .2.4.6.8.0.2                                                   
COCONVRT DC    C'CHRISTAL    ',C'CR'                                            
LCOCONV  EQU   *-COCONVRT                                                       
         DC    C'EASTMAN     ',C'EA'                                            
         DC    C'KATZ RADIO  ',C'KU'                                            
***>>>   DC    C'KATZ HISPANI',C'KF'  PER J.BREWER 10/24/00                     
         DC    C'SENTRY      ',C'S3'                                            
         DC    C'CLR CHNL A  ',C'NU'                                            
         DC    C'CLR CHNL B  ',C'NU'                                            
         DC    C'CLR CHNL C  ',C'NU'                                            
         DC    C'CLC A       ',C'NU'                                            
         DC    C'CLC B       ',C'NU'                                            
         DC    C'CLC C       ',C'NU'                                            
         DC    C'CHANNEL     ',C'NU'                                            
         DC    C'DIAMOND     ',C'NU'                                            
         DC    C'EMERALD     ',C'NU'                                            
         DC    C'SAPPHIRE    ',C'NU'                                            
         DC    X'0000'                                                          
         DC    H'0'                                                             
LDIR0400 EQU   *                                                                
*                                                                               
*   FORCE A TEST FILE                                                           
**       L     R2,ASTAAREA                                                      
**       MVC   0(09,R2),=C'CRRCWNNDF'                                           
**       LA    R2,STATABLN(R2)                                                  
**       MVC   0(4,R2),=X'FFFFFFFF'                                             
*   END OF FORCE:  THIS CODE MUST BE REMOVED WHEN FILE IS AVAILABLE             
*                                                                               
         CLI   QUESTOR,C'Y'        PRINT TABLES?                                
         BNE   LDIR0440                                                         
*                                                                               
         L     R2,ASTAAREA         SET A(STATION TABLE)                         
*                                                                               
         LA    R3,1                SET COUNTER                                  
LDIR0420 EQU   *                                                                
         CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    LDIR0440            YES                                          
*                                                                               
         EDIT  (R3),(5,P+1)                                                     
         LA    R3,1(R3)                                                         
         MVC   P+16(04),=C'STA:'                                                
         MVC   P+23(STATABLN),0(R2)                                             
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,STATABLN(R2)     BUMP TO NEXT SLOT                            
         B     LDIR0420            GO BACK FOR NEXT SLOT                        
LDIR0440 EQU   *                                                                
         GOTO1 REPORT                                                           
         MVC   P+1(07),=C'STACTR='                                              
         EDIT  STACTR,(6,P+10)                                                  
         GOTO1 REPORT                                                           
*                                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
HIGHDIR  NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   TRACEKEY,KEY                                                     
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 2                                                                
SEQDIR   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
         B     DMCHECK                                                          
         SPACE 3                                                                
GETREC#2 LA    R6,GETREC                                                        
         B     LINKFIL2                                                         
         SPACE 2                                                                
LINKFIL2 NTR1                                                                   
         MVC   DMCB+12(4),AIO2                                                  
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               ,(0,DMWORK)                                                      
         B     DMCHECK                                                          
GETRECRD LA    R6,GETREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
PREC     LA    R6,PUTREC                                                        
         B     LINKFILE                                                         
         SPACE 2                                                                
LINKFILE NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R6)),REPFILE,KEY+28,             X        
               REC,(0,DMWORK)                                                   
         B     DMCHECK                                                          
*                                                                               
*                                                                               
WRT      MVC   COMMAND(8),DMWRT                                                 
                                                                                
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  NTR1                                                                   
         IC    R4,DMINBTS                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPDIR',KEYSAVE,KEY               
         OC    DMCB+8(1),DMCB+8                                                 
         XIT1                                                                   
COMMAND  DS    CL8                                                              
         SPACE 3                                                                
*        DATA MANAGER INTERFACE (CHECK ERRORS)                                  
         SPACE 1                                                                
DMCHECK  EQU   *                                                                
*                                                                               
         TM    DMCB+8,X'10'        TEST FOR RECORD NOT FOUND                    
         BO    NEXIT                                                            
         TM    DMCB+8,X'02'        TEST FOR RECORD DELETED                      
         BO    EQXIT               DELETE SET - PROCESS                         
         TM    DMCB+8,X'EF'        TEST FOR OTHER ERRORS                        
         BZ    EQXIT                                                            
         SPACE 1                                                                
         MVC   WORK(25),=C'*** DATA MANAGER ERROR***'                           
         GOTO1 LOGIO,WORK+48,1,(25,WORK)                                        
         MVC   WORK(25),SPACES                                                  
         BASR  RE,RF                                                            
         BAS   RE,DMTRACE                                                       
         DC    H'0'                BLOW UP                                      
         SPACE 2                                                                
EQXIT    CR    RB,RB                                                            
         XIT1                                                                   
         SPACE 1                                                                
NEXIT    LTR   RB,RB                                                            
         XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO TRACE DATA MANAGER CALLS                              
         SPACE 1                                                                
DMTRACE  NTR1                                                                   
         MVC   P,SPACES                                                         
         LM    R2,R5,DMCB                                                       
         MVC   TRACEDM8,DMCB+8                                                  
         MVC   P(8),0(R2)          COMMAND                                      
         MVC   P+10(8),0(R3)       FILE                                         
         LA    R6,4                                                             
         CLC   3(3,R3),=C'DIR'                                                  
         BNE   DMTRACE2                                                         
         LA    R4,TRACEKEY                                                      
         GOTO1 HEXOUT,DMCB,(R4),P+20,32,=C'N'                                   
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,(R1),(R5)                                                 
         GOTO1 HEXOUT,DMCB,(R5),P+20,32,=C'N'                                   
         B     DMTRACE4                                                         
         SPACE 1                                                                
DMTRACE2 GOTO1 HEXOUT,DMCB,(R4),P+20,(R6),=C'N'                                 
         LA    R5,34(R5)                                                        
         GOTO1 HEXOUT,DMCB,(R5),P+75,25                                         
         SPACE 1                                                                
DMTRACE4 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,TRACEDM8,P+130,1                                     
         MVI   SKIPSPEC,C'Y'                                                    
         GOTO1 REPORT                                                           
         XIT1                                                                   
         SPACE 2                                                                
TRACEDM8 DS    C                                                                
TRACEKEY DS    CL32                                                             
         EJECT                                                                  
         DS    0H                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
AREC     DS    A                                                                
ASPOFFTB DS    A                                                                
ANEXTSPO DS    A                                                                
AMISSSTA DS    A                                                                
AMISSADV DS    A                                                                
SAVEAIO  DS    A                                                                
AIO2     DS    A                                                                
ABLDAREA DS    A                                                                
ASTAAREA DS    A                   STATION AREA                                 
ANEXTSTA DS    A                   NEXT STATION SLOT                            
AX03AREA DS    A                   EST ELT TABLE AREA                           
ANEXTX03 DS    A                   NEXT SLOT                                    
AX04AREA DS    A                   INV ELT TABLE AREA                           
ANEXTX04 DS    A                   NEXT SLOT                                    
JOLCTR   DS    F                                                                
ASALAREA DS    A                                                                
ANEXTSAL DS    A                                                                
SALCTR   DS    F                                                                
ATAPEREC DS    A                                                                
AWORKBLK DS    A                                                                
AWRKBLK2 DS    A                                                                
LBLDAREA DS    F                                                                
CONBYTES DS    F                                                                
BUYBYTES DS    F                                                                
CONCTR   DS    F                                                                
CONCTR2  DS    F                                                                
CONCTR3  DS    F                                                                
CONCTR4  DS    F                                                                
CUPCTR   DS    F                                                                
NOCONCOV DS    F                                                                
BUYCTR   DS    F                                                                
STACTR   DS    F                                                                
STAREAD  DS    F                                                                
STATOTAL DS    F                                                                
STASKIPD DS    F                                                                
STALVDAT DS    F                                                                
STACOPYD DS    F                                                                
STAOUTPT DS    F                                                                
STAOLDNW DS    F                                                                
STAONFIL DS    F                                                                
STADUPED DS    F                                                                
STAPASS1 DS    F                                                                
STAPASS2 DS    F                                                                
STAGHOST DS    F                                                                
MISMATCH DS    F                                                                
SORTCTR  DS    F                                                                
SORTTYP1 DS    F                                                                
SORTTYP2 DS    F                                                                
STYP1RED DS    F                                                                
STYP2RED DS    F                                                                
MATCHSRT DS    F                                                                
NOBUYCON DS    F                                                                
DROP02C  DS    F                                                                
DROP02SC DS    F                                                                
DROP07C  DS    F                                                                
DROP07SC DS    F                                                                
DROP11C  DS    F                                                                
DROP11SC DS    F                                                                
DROP82C  DS    F                                                                
DROP82SC DS    F                                                                
COMBOCTL DS    F                                                                
         XIT1    .1.2.3.4.5.6.7.8.9.0.1.2.3.4.5.6                               
FOXES    DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                              
DUMMYREP DS    CL14                DUMMY 'STATION TABLE' ENTRY                  
*                                     WHEN STATION NOT IN LIST                  
COMPARE  DS    CL6                 COMPARE AREA: SORT VS BUY                    
*                                                                               
MYWORK   DS    CL64                                                             
MYWORK2  DS    CL64                                                             
OLDCON   DS    CL4                 ORIGINAL CONTRACT NUMBER                     
NEWCON   DS    CL4                 NEW      CONTRACT NUMBER                     
NEWCONRV DS    CL4                 NEW      CONTRACT NUMBER (REV)               
OLDCONRV DS    CL4                 OLD      CONTRACT NUMBER (REV)               
OLDSTA   DS    CL5                                                              
OLDOFF   DS    CL2                                                              
OLDADV   DS    CL4                                                              
OLDAGY   DS    CL6                 ORIGINAL AGENCY/OFFICE                       
ELEM     DS    CL64                                                             
*                                                                               
LASTSTA  DS    CL5                                                              
OLDREP   DS    CL2                 OLD REP CODE                                 
NEWREP   DS    CL2                 NEW REP CODE                                 
REPREP   DS    CL2                 REP REP CODE (REPLACEMENT)                   
OLDCONV# DS    CL2                 OLD REPCODE                                  
HIGHCON  DS    XL4                                                              
LOWCON   DS    XL4                                                              
SAVEKEY  DS    CL(L'KEY)                                                        
REKEY    DS    CL(L'KEY)                                                        
COVERFLG DS    CL1                                                              
*   EQUATES                                                                     
SALTABLN EQU   13                                                               
DSALTSTN EQU   0                   DISPLACE TO STATION                          
DSALTOSP EQU   5                   DISPLACE TO OLD S/P                          
DSALTNSP EQU   8                   DISPLACE TO NEW S/P                          
DSALTTEM EQU   11                  DISPLACE TO TEAM                             
*                                                                               
STATABLN EQU   9                                                                
*                                                                               
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
         DS    0H                                                               
INTAPE1  DCB   DDNAME=INTAPE1,DSORG=PS,RECFM=VB,LRECL=255,             X        
               BLKSIZE=6233,MACRF=GM,EODAD=LDIR0400                             
         SPACE 3                                                                
         SPACE 3                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL4008              AREA FOR RECORD: 4K CONTRACT                 
         DS    0D                                                               
*  INCLUDE REGENREPA               REP RECORD                                   
*  INCLUDE REGENBUY                BUY RECORD                                   
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REGENMKG                MAKEGOOD RECORD                              
*  INCLUDE REGENTKO                TKO EQUIV RECORD                             
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
*                                                                               
NMODAREA CSECT                                                                  
RECD     DSECT                                                                  
RECORD   DS    CL4008                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENREPA         REP RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSAL          S/P RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENDAR          DARE RECORD                                  
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUY          BUY RECORD                                   
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCON          CONTRACT RECORD                              
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCOV          COVERSHEET RECORD                            
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCFC          CONFIRM COMMENT RECORD                       
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENMKG          M/G     RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENTKO          TKO EQUIV RECORD                             
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENPRD          PRODUCT RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAGY          AGENCY  RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAGY2         AGENCY2 RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENADV          ADVERT  RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENREG          REGION  RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENOFF          OFFICE  RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENTEM          TEAM    RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCTG          CATEGRY RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENBUD          BUDGET  RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENINV          INVENTORY RECORD                             
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENAVLN         AVAIL     RECORD                             
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENPRPN         PROPOSAL  RECORD                             
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENEOM          EOM       RECORD                             
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENCLS          CLASS   RECORD                               
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENGRP          GROUP   RECORD                               
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
       EJECT                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'137REREPSDUB 05/01/02'                                      
         END                                                                    
