*          DATA SET PPREPTU02  AT LEVEL 011 AS OF 08/13/96                      
*PHASE PPTU02A,+0,NOAUTO                                                        
*INCLUDE DATVAL                                                                 
*                                                                               
 TITLE 'PPTU02 - UPLOAD TEARSHEET AND TEARSHEET COMMENT ELEMS'                  
*                                                                               
*        QOPT1 N= TEST RUN - DON'T MARK FILE                                    
*        QOPT2 Y= DUMP RECORD                                                   
*                                                                               
PPTU02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPTU02,R2,RR=R9           NOTE R2 AS SECOND BASE               
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPTUWRKD,R8                                                      
*                                                                               
**                                                                              
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'                                                      
         ZAP   OUTCNT,=P'0'                                                     
         ZAP   ERRCNT,=P'0'                                                     
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         CLI   QOPT1,C'N'        MEANS DON'T MARK FILE                          
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         OPEN  (IN,(INPUT))                                                     
         XCEF  REC,400           CLEAR TEST DEF'S (NO-OP FOR TESTING)           
GET      DS    0H                                                               
         BAS   RE,TAPEGET          NO-OP FOR TESTING ************               
         CLI   EOFSW,C'Y'                                                       
         BE    EXIT                                                             
*****    MVI   EOFSW,C'Y'          TESTING *************                        
         MVI   PRTSW,C'N'                                                       
         AP    INCNT,=P'1'                                                      
******************************     PROCESS UPLOAD REC                           
         LA    R7,REC                                                           
         USING PTURECD,R7          TEARSHEET UPLOAD REC                         
         MVC   SVESTST(12),=C'990101991231'     "DUMMY" EST. PERIOD             
*                                                                               
         LA    R6,PTUEST           ESTIMATE NUMBER                              
         LA    R5,3                LUP COUNTER                                  
         SR    R3,R3               CHARACTER COUNTER                            
PESTLUP  CLI   0(R6),C' '                                                       
         BE    PESTLUPX            END OF DATA                                  
         LA    R3,1(R3)            BUMP UP CHARACTER COUNTER                    
         LA    R6,1(R6)            NEXT CHARACTER                               
         BCT   R5,PESTLUP                                                       
PESTLUPX LTR   R3,R3                                                            
         BZ    PESTNG              EST IS BLANK                                 
         MVC   WORK(3),=3X'F0'                                                  
         MVZ   WORK(3),PTUEST                                                   
         BCTR  R3,0                                                             
         EX    R3,PESTNUMT         NUM TEST                                     
         BNE   PESTNG              EST NOT NUMERIC                              
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,PTUEST(0)                                                    
         CVB   R5,DUB                                                           
         STH   R5,SVEST            2 POSN BIN ESTIMATE NUMBER                   
         B     PGETEST             GO TO READ ESTIMATE RECORD                   
*                                                                               
PESTNG   DS    0H                                                               
         MVC   P+95(34),=C'ESTIMATE NUMBER MISSING OR INVALID'                  
         BAS   RE,PRNTMSG          PRINT REC KEY WITH ABOVE MSG                 
         B     PPUBEDT             EDIT PUB - SKIP ESTIMATE READ                
*                                                                               
PESTNUMT CLC   WORK(0),=3X'F0'     EXECUTED                                     
*                                                                               
PGETEST  DS    0H                                                               
         LA    R4,KEY                                                           
         USING PESTREC,R4                                                       
         XC    KEY,KEY                                                          
         MVC   PESTKAGY,PTUAGY       AGENCY                                     
         MVC   PESTKMED,PTUMED       MEDIA                                      
         MVI   PESTKRCD,X'07'        REC CODE                                   
         MVC   PESTKCLT,PTUCLT       CLIENT                                     
         MVC   PESTKPRD,PTUPRD       PRODUCT                                    
         MVC   PESTKEST,SVEST        ESTIMATE FROM ABOVE                        
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    PGESTOK                                                          
         MVC   P+95(25),=C'ESTIMATE RECORD NOT FOUND'                           
         BAS   RE,PRNTMSG          PRINT REC KEY WITH ABOVE MSG                 
         B     PPUBEDT             EDIT PUB                                     
         DROP  R4                                                               
PGESTOK  LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETEST                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,PESTREC                                                       
         USING PESTREC,R4                                                       
         MVC   SVESTST(12),PESTST     STORE EST START AND END DATES             
         DROP  R4                                                               
*                                                                               
*************************  CONVERT PUBLICATION TO 6-BYTE KEY FORMAT             
*                                                                               
PPUBEDT  DS    0H                                                               
         LA    R6,PTUPUB           PUBLICATION                                  
         LA    R5,17               LUP COUNTER                                  
         SR    R3,R3               CHARACTER COUNTER                            
PPUBLUP  CLI   0(R6),C' '                                                       
         BE    PPUBLUPX            END OF DATA                                  
         LA    R3,1(R3)            BUMP UP CHARACTER COUNTER                    
         LA    R6,1(R6)            NEXT CHARACTER                               
         BCT   R5,PPUBLUP                                                       
PPUBLUPX LTR   R3,R3                                                            
         BNZ   PPUBVAL                                                          
         B     PPUBNG              PUBLICATION ERROR                            
*                                                                               
PPUBVAL  GOTO1 PUBVAL,DMCB,((R3),PTUPUB),(0,NEWNUM)                             
         CLI   0(R1),X'FF'         INVALID PUB ?                                
         BNE   PDATRTN             NO - GO MASSAGE INS. DATE                    
*                                                                               
PPUBNG   DS    0H                                                               
         MVC   P+95(30),=C'PUBLICATION MISSING OR INVALID'                      
         BAS   RE,PRNTMSG          PRINT REC KEY WITH ABOVE MSG                 
*                                                                               
*                                                                               
PDATRTN  DS    0H    EDIT AND MASSAGE PTUIDATE (INS. DATE + LINE (OPT.)         
         LA    R0,PTUTDATE                                                      
         ST    R0,ENDTST    TO TEST FOR END OF "FULL" 12-BYTE PTUIDATE          
         MVI   SVLINE,1            "DEFAULT" LINE NUMBER                        
         MVC   WORK(64),SPACES                                                  
         MVC   WORK+3(2),=C'01'    "DEFAULT" DAY                                
         MVI   WORK+2,C'/'                                                      
         MVI   WORK+5,C'/'                                                      
         LA    R6,PTUIDATE         INS. DATE "+"                                
         CLI   0(R6),C'B'                                                       
         BE    PDRADJ                                                           
         CLI   0(R6),C'W'                                                       
         BNE   PDRMNTH                                                          
PDRADJ   LA    R6,1(R6)            BUMP PAST 1ST CHAR IF "B" OR "W"             
PDRMNTH  LA    R5,MONTHS           TABLE (JAN-DEC)                              
         LA    R4,1                FOR MONTH NUMBER                             
         LA    R3,12               LUP CNTR                                     
*                                                                               
PDRLUP   CLC   0(3,R6),0(R5)                                                    
         BE    PDRLUPX             MONTH OK                                     
         LA    R4,1(R4)            BUMP UP MONTH NUMBER                         
         LA    R5,3(R5)            NEXT TABLE MONTH                             
         BCT   R3,PDRLUP                                                        
         B     PPDATNG             MONTH NO GOOD                                
PDRLUPX  DS    0H                  MONTH OK                                     
         CVD   R4,DUB                 CONVERT MONTH NUM                         
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(2),DUB+6(2)       TO EBCDIC IN WORK                         
***                             SET "DEFAULT YEAR" IN WORK                      
         MVC   WORK+6(2),SVESTST     START YR FROM ESTIMATE REC                 
         CLC   SVESTST(2),SVESTEND   START & END YEARS EQUAL ?                  
         BE    PDR05                 YES                                        
         CLC   WORK(2),SVESTST+2     NO - IF INSERT. MO. LT                     
         BNL   PDR05                      EST. START MO.  USE                   
         MVC   WORK+6(2),SVESTEND       END YR FROM ESTIMATE REC                
*                                                                               
PDR05    DS    0H                DEFAULT INS. DATE (MM/DD/YY) DONE              
         LA    R6,3(R6)            BUMP PAST ALPHA-MONTH IN INS. DATE           
*                                                                               
PDR10    DS    0H                TEST REST OF PTUIDATE                          
         C     R6,ENDTST           AT END OF PTUIDATE ?                         
         BNL   PDR50               YES - END OF EDIT                            
         CLI   0(R6),C' '                                                       
         BE    PDR50               NO MORE CHARACTERS TO TEST                   
         CLI   0(R6),C'/'                                                       
         BNE   PDR20                                                            
         LA    R6,1(R6)            BUMP PAST "/" TO EDIT YEAR                   
         MVC   WORK+30(3),=3X'F0'                                               
         MVZ   WORK+30(2),0(R6)                                                 
         CLI   1(R6),C' '                                                       
         BE    PDR10B              1-CHARACTER YEAR                             
         CLI   1(R6),C'-'                                                       
         BE    PDR10B              1-CHARACTER YEAR                             
         CLI   1(R6),C'/'                                                       
         BE    PPDATNG             TWO '/'S FOUND                               
         CLI   2(R6),C'/'                                                       
         BE    PPDATNG             TWO '/'S FOUND                               
         CLC   WORK+30(2),=3X'F0'  2-CHARACTER YEAR FOUND                       
         BNE   PPDATNG             YEAR NOT NUMERIC                             
         MVC   WORK+6(2),0(R6)     YEAR OK - REPLACE DEFAULT YEAR               
         LA    R6,2(R6)            BUMP PAST YEAR                               
         B     PDR10                                                            
PDR10B   CLI   WORK+30,X'F0'       1-CHARACTER YEAR FOUND                       
         BNE   PPDATNG             YEAR NOT NUMERIC                             
         MVC   WORK+7(1),0(R6)     YEAR OK - REPLACE DEFAULT YEAR               
         MVI   WORK+6,C'0'            ZERO FIRST POS'N OF YEAR                  
         LA    R6,1(R6)            BUMP PAST YEAR                               
         B     PDR10                                                            
*                                                                               
PDR20    DS    0H                TEST FOR LINE NUMBER                           
         CLI   0(R6),C'-'                                                       
         BNE   PDR30                                                            
         LA    R6,1(R6)            BUMP PAST "-" TO EDIT LINE NUM               
         MVC   WORK+30(3),=3X'F0'                                               
         MVZ   WORK+30(2),0(R6)                                                 
         CLI   1(R6),C' '                                                       
         BE    PDR20B              1-CHARACTER LINE NUM                         
         CLI   1(R6),C'/'                                                       
         BE    PDR20B              1-CHARACTER LINE NUM                         
         CLI   1(R6),C'-'                                                       
         BE    PPDATNG             2 '-'S FOUND                                 
         CLI   2(R6),C'-'                                                       
         BE    PPDATNG             2 '-'S FOUND                                 
         CLC   WORK+30(2),=3X'F0'  2-CHARACTER LINE NUM FOUND                   
         BNE   PPDATNG             LINE NUMBER NOT NUMERIC                      
         CLC   0(2,R6),=C'02'                                                   
         BL    PPDATNG             LINE NUMBER NOT GT 1                         
         PACK  DUB,0(2,R6)                                                      
         LA    R6,2(R6)            BUMP PAST LINE NUM                           
         B     PDR20E                                                           
PDR20B   CLI   WORK+30,X'F0'       1-CHARACTER LINE NUM FOUND                   
         BNE   PPDATNG             LINE NUMBER NOT NUMERIC                      
         CLI   0(R6),C'2'                                                       
         BL    PPDATNG             LINE NUMBER NOT GT 1                         
         PACK  DUB,0(1,R6)                                                      
         LA    R6,1(R6)            BUMP PAST LINE NUM                           
PDR20E   CVB   R5,DUB                                                           
         STC   R5,SVLINE        LINE OK - REPLACE DEFAULT (X'01') LINE          
         B     PDR10                                                            
*                                                                               
PDR30    DS    0H                EDIT DAY FOR NUMERIC                           
         MVC   WORK+30(3),=3X'F0'                                               
         MVZ   WORK+30(2),0(R6)                                                 
         CLI   1(R6),C' '                                                       
         BE    PDR30B              1-CHARACTER DAY                              
         CLI   1(R6),C'-'                                                       
         BE    PDR30B              1-CHARACTER DAY                              
         CLI   1(R6),C'/'                                                       
         BE    PDR30B              1-CHARACTER DAY                              
         CLC   WORK+30(2),=3X'F0'  2-CHARACTER DAY                              
         BNE   PPDATNG             DAY IS NOT NUMERIC                           
         MVC   WORK+3(2),0(R6)     DAY OK - REPLACE DEFAULT DAY                 
         LA    R6,2(R6)            BUMP PAST DAY                                
         B     PDR10                                                            
PDR30B   CLI   WORK+30,X'F0'       1-CHARACTER DAY                              
         BNE   PPDATNG             DAY IS NOT NUMERIC                           
         MVC   WORK+4(1),0(R6)     DAY OK - REPLACE DEFAULT DAY                 
         MVI   WORK+3,C'0'            ZERO 1ST POS'N OF DAY                     
         LA    R6,1(R6)            BUMP PAST DAY                                
         B     PDR10                                                            
*                                                                               
PDR50    GOTO1 =V(DATVAL),DMCB,(0,WORK),WORK+30,RR=RELO                         
         OC    DMCB(4),DMCB                                                     
         BZ    PPDATNG             DATE IS NOT VALID                            
         GOTO1 DATCON,DMCB,(0,WORK+30),(3,SVIDAT)                               
         B     PTDATED             CONTINUE EDITING                             
*                                                                               
PPDATNG  DS    0H                                                               
         MVC   P+95(34),=C'INSERTION DATE/LINE NUMBER INVALID'                  
         BAS   RE,PRNTMSG          PRINT REC KEY WITH ABOVE MSG                 
*                                                                               
PTDATED  DS    0H                  EDIT TAPE DATE                               
         GOTO1 =V(DATVAL),DMCB,(0,PTUTDATE),WORK+30,RR=RELO                     
         OC    DMCB(4),DMCB                                                     
         BZ    PTDATNG             DATE IS NOT VALID                            
         GOTO1 DATCON,DMCB,(0,WORK+30),(3,SVTDAT)                               
         B     PEDTREST            CONTINUE EDITING                             
*                                                                               
PTDATNG  DS    0H                                                               
         MVC   P+95(22),=C'DATE OF TAPE INVALID -'                              
         MVC   P+118(8),PTUTDATE                                                
         BAS   RE,PRNTMSG          PRINT REC KEY WITH ABOVE MSG                 
*                                                                               
PEDTREST DS    0H                  EDIT REMAINING FIELDS                        
*                                                                               
PED010   DS    0H                  EDIT STATUS                                  
         CLI   PTUSTAT,C' '                                                     
         BE    PED020                                                           
         CLI   PTUSTAT,C'A'                                                     
         BE    PED020                                                           
         CLI   PTUSTAT,C'N'                                                     
         BE    PED020                                                           
         MVC   P+103(08),=C'STATUS -'                                           
         MVC   P+112(1),PTUSTAT                                                 
         BAS   RE,PEDERROR            COMMON INVALID ERROR                      
*                                                                               
PED020   DS    0H            EDIT EVALUATION DATA FIELDS                        
         CLI   PTUSOK,C' '         SPACE                                        
         BE    PED022                                                           
         CLI   PTUSOK,C'N'                                                      
         BE    PED022                                                           
         CLI   PTUSOK,C'Y'                                                      
         BE    PED022                                                           
         MVC   P+103(07),=C'SPACE -'                                            
         MVC   P+111(1),PTUSOK                                                  
         BAS   RE,PEDERROR            COMMON INVALID ERROR                      
*                                                                               
PED022   CLI   PTUCOK,C' '         CAPTION                                      
         BE    PED024                                                           
         CLI   PTUCOK,C'N'                                                      
         BE    PED024                                                           
         CLI   PTUCOK,C'Y'                                                      
         BE    PED024                                                           
         MVC   P+103(09),=C'CAPTION -'                                          
         MVC   P+113(1),PTUCOK                                                  
         BAS   RE,PEDERROR            COMMON INVALID ERROR                      
*                                                                               
PED024   CLI   PTUPOK,C' '         POSITION                                     
         BE    PED026                                                           
         CLI   PTUPOK,C'N'                                                      
         BE    PED026                                                           
         CLI   PTUPOK,C'Y'                                                      
         BE    PED026                                                           
         MVC   P+103(10),=C'POSITION -'                                         
         MVC   P+114(1),PTUPOK                                                  
         BAS   RE,PEDERROR            COMMON INVALID ERROR                      
*                                                                               
PED026   CLI   PTUDOK,C' '         INS. DATE                                    
         BE    PED028                                                           
         CLI   PTUDOK,C'N'                                                      
         BE    PED028                                                           
         CLI   PTUDOK,C'Y'                                                      
         BE    PED028                                                           
         MVC   P+103(11),=C'INS. DATE -'                                        
         MVC   P+115(1),PTUDOK                                                  
         BAS   RE,PEDERROR            COMMON INVALID ERROR                      
*                                                                               
PED028   CLI   PTUZOK,C' '         ZONE                                         
         BE    PED030                                                           
         CLI   PTUZOK,C'N'                                                      
         BE    PED030                                                           
         CLI   PTUZOK,C'Y'                                                      
         BE    PED030                                                           
         MVC   P+103(06),=C'ZONE -'                                             
         MVC   P+110(1),PTUZOK                                                  
         BAS   RE,PEDERROR            COMMON INVALID ERROR                      
*                                                                               
PED030   DS    0H                  REPO QUALITY                                 
         MVI   SVREPO,0                                                         
         CLC   PTUREPO(2),SPACES                                                
         BE    PED040              DONE WITH REPO QUALITY                       
         MVC   WORK+30(3),=3X'F0'                                               
         MVZ   WORK+30(2),PTUREPO                                               
         CLI   PTUREPO+1,C' '                                                   
         BE    PED030B             1-CHARACTER REPO QUALITY                     
         CLC   WORK+30(2),=3X'F0'  2-CHARACTER REPO QUALITY                     
         BNE   PED030X             INVALID - NOT NUMERIC                        
         CLC   PTUREPO(2),=C'01'                                                
         BL    PED030X             INVALID - ZERO VALUE                         
         CLC   PTUREPO(2),=C'10'                                                
         BH    PED030X             INVALID - GT TEN                             
         PACK  DUB,PTUREPO(2)                                                   
         B     PED030E                                                          
PED030B  CLI   WORK+30,X'F0'       1-CHARACTER REPO QUALITY FOUND               
         BNE   PED030X             INVALID - NOT NUMERIC                        
         CLI   PTUREPO,C'0'                                                     
         BE    PED030X             INVALID - ZERO VALUE                         
         PACK  DUB,PTUREPO(1)                                                   
PED030E  CVB   R5,DUB                                                           
         STC   R5,SVREPO        REPO OK - REPLACE DEFAULT (X'00') VALUE         
         B     PED040                                                           
PED030X  DS    0H                                                               
         MVC   P+103(14),=C'REPO QUALITY -'                                     
         MVC   P+118(2),PTUREPO                                                 
         BAS   RE,PEDERROR            COMMON INVALID ERROR                      
*                                                                               
PED040   DS    0H                                                               
         CLI   PTUSTATI,C'Y'       UPDATE STAT ?                                
         BE    PPGETBUY            YES                                          
         CLI   PTUEVALI,C'Y'       UPDATE EVALUATION DATA ?                     
         BE    PPGETBUY            YES                                          
         CLI   PTUCOMI,C'Y'        UPDATE COMMENTS ?                            
         BE    PPGETBUY            YES - GO DO UPDATES                          
         MVC   P+103(22),=C'- NO UPDATES REQUESTED'                             
         MVC   P+56(1),PTUSTATI                                                 
         MVC   P+62(1),PTUEVALI                                                 
         MVC   P+69(1),PTUCOMI                                                  
         BAS   RE,PEDERROR         COMMON INVALID ERROR                         
         B     GET                 EDITS DONE - NEXT REC                        
*                                                                               
PEDERROR NTR1                      "COMMON" UPDATE FIELDS ERROR                 
         MVC   P+95(7),=C'INVALID'                                              
         BAS   RE,PRNTMSG          PRINT REC KEY WITH ABOVE MSG                 
         B     EXIT                                                             
*                                                                               
*                                                                               
PPGETBUY DS    0H                                                               
         CLI   PRTSW,C'Y'          ERRORS FOUND ?                               
         BE    GET                 YES - GET NEXT REC                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PBUYREC,R4                                                       
         MVC   PBUYKAGY,PTUAGY     AGENCY                                       
         MVC   PBUYKMED,PTUMED     MEDIA                                        
         MVI   PBUYKRCD,X'20'      RECORD CODE                                  
         MVC   PBUYKCLT,PTUCLT     CLIENT CODE                                  
         MVC   PBUYKPRD,PTUPRD     PRODUCT CODE                                 
         MVC   PBUYKPUB(6),NEWNUM      PUB CODE                                 
         MVC   PBUYKDAT,SVIDAT     INSERTION DATE - BIN YMD                     
         MVC   PBUYKEST,SVEST      ESTIMATE NUMBER                              
         MVC   PBUYKLIN,SVLINE     LINE NUMBER                                  
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    PGBUYOK                                                          
         MVC   P+95(20),=C'BUY RECORD NOT FOUND'                                
         BAS   RE,PRNTMSG          PRINT REC KEY WITH ABOVE MSG                 
         B     GET                 NEXT RECORD                                  
         DROP  R4                                                               
*                                                                               
PGBUYOK  LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETBUY                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*******               PROCESS TEARSHEET ELEM'S IN BUY                           
*                                                                               
PTS      DS    0H                                                               
         CLI   QOPT2,C'Y'          DUMP RECORD ?                                
         BNE   PTS10               NO                                           
         CP    OUTCNT,=P'25'       25 RECORDS DUMPED ?                          
         BH    PTS10               YES - NO MORE DUMPING                        
         MVC   P(14),=C'*** BEFORE ***'                                         
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
PTS10    DS    0H                                                               
         LA    R3,PBUYREC+33                                                    
         MVI   ELCODE,X'95'        TEARSHEET ELEMENT CODE                       
         BAS   RE,NEXTEL           LOOK FOR ELEMENT                             
         ST    R3,FULL             SAVE ADDRESS (OF END OF RECORD)              
         BE    PTSADD              NO T'SHT ELEM FOUND - GO ADD ONE             
*                                                                               
PTSCHG   DS    0H                  PROCESS CHANGE TO TEARSHEET ELEM             
         CLI   PTUSTATI,C'Y'       UPDATE STATUS ?                              
         BE    PTSC10              YES                                          
         CLI   PTUEVALI,C'Y'       UPDATE EVALUATION DATA ?                     
         BNE   PTCOM          NO CHG TO T'SHEET ELEM - GO TEST COMMENTS         
*                                                                               
PTSC10   DS    0H                                                               
         USING PTSHTELD,R3                                                      
         MVI   PTSHCIN1,0          ZERO CHANGE INDICATORS                       
         MVI   PTSHCIN2,0                                                       
         CLI   PTUSTATI,C'Y'       UPDATE STATUS ?                              
         BNE   PTSC20              NO                                           
         CLC   PTSHSTAT,PTUSTAT    FIELDS DIFFERENT ?                           
         BE    PTSC20              NO                                           
         MVC   PTSHSTAT,PTUSTAT                                                 
         OI    PTSHCIN1,X'01'      STATUS CHANGED INDICATOR                     
PTSC20   CLI   PTUEVALI,C'Y'       UPDATE EVALUATION DATA ?                     
         BNE   PTSCEND             NO - CHANGES DONE                            
         CLC   PTSHIND1,PTUSOK     FIELDS DIFFERENT ?                           
         BE    PTSC20A             NO                                           
         MVC   PTSHIND1,PTUSOK                                                  
         OI    PTSHCIN1,X'02'      SPACE CHANGED INDICATOR                      
PTSC20A  CLC   PTSHIND2,PTUCOK     FIELDS DIFFERENT ?                           
         BE    PTSC20B             NO                                           
         MVC   PTSHIND2,PTUCOK                                                  
         OI    PTSHCIN1,X'04'      CAPTION CHANGED INDICATOR                    
PTSC20B  CLC   PTSHIND3,PTUPOK     FIELDS DIFFERENT ?                           
         BE    PTSC20C             NO                                           
         MVC   PTSHIND3,PTUPOK                                                  
         OI    PTSHCIN1,X'08'      POSITION CHANGED INDICATOR                   
PTSC20C  CLC   PTSHIND4,PTUDOK     FIELDS DIFFERENT ?                           
         BE    PTSC20D             NO                                           
         MVC   PTSHIND4,PTUDOK                                                  
         OI    PTSHCIN1,X'10'      INS. DATE CHANGED INDICATOR                  
PTSC20D  CLC   PTSHIND5,PTUZOK     FIELDS DIFFERENT ?                           
         BE    PTSC20E             NO                                           
         MVC   PTSHIND5,PTUZOK                                                  
         OI    PTSHCIN1,X'20'      ZONES CHANGED INDICATOR                      
PTSC20E  CLC   PTSHREPO,SVREPO     FIELDS DIFFERENT ?                           
         BE    PTSC20F             NO                                           
         MVC   PTSHREPO,SVREPO                                                  
         OI    PTSHCIN2,X'01'      REPO QUALITY CHANGED INDICATOR               
PTSC20F  MVC   PTSHPAGE,PTUPAGE                                                 
*                                                                               
PTSCEND  DS    0H                                                               
         MVC   PTSHCDAT,SVTDAT     DATE OF UPLOAD TAPE TO CHANGE DATE           
         MVC   PTSHBID,=C'*TU'     CHANGED VIA UPLOAD TAPE                      
         LA    R4,PBUYREC+33                                                    
         USING PBDELEM,R4                                                       
         CLI   PTUSTATI,C'Y'       UPDATE STATUS ?                              
         BNE   PTSCEND2            NO                                           
         NI    PBDSTAT,X'EF'       SET OFF X'10' BIT                            
         CLI   PTSHSTAT,C'A'       SEE IF STATUS APPROVED ('A')                 
         BNE   *+8                                                              
         OI    PBDSTAT,X'10'       SET ON TEAR SHEET INDICATOR                  
         DROP  R4                                                               
PTSCEND2 CLI   PTUCOMI,C'Y'        UPDATE COMMENTS ?                            
         BNE   PBUYWRT             NO - UPDATE OVER - GO WRITE BUY              
         OI    PTSHCIN2,X'02'      COMMENTS CHANGED INDICATOR                   
         B     PTCOM               GO TEST COMMENTS                             
*                                                                               
*                                                                               
PTSADD   DS    0H                  ADD A TEARSHEET ELEM                         
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'9527'                                                 
*                                                                               
         LA    R3,ELEM             PTSHTELD USING STILL "ON"                    
         MVC   PTSHIDAT,SVTDAT     DATE OF UPLOAD TAPE TO ADD DATE              
         MVC   PTSHBID,=C'*TU'     ADDED VIA UPLOAD TAPE                        
*                                                                               
         MVC   PTSHSTAT(10),SPACES        INITIALIZE TO SPACES                  
         MVC   PTSHPAGE(10),SPACES                                              
*                                                                               
         MVC   PTSHSTAT,PTUSTAT                                                 
         MVC   PTSHIND1(5),PTUSOK      SPACE THRU ZONE INDICATORS               
         MVC   PTSHREPO,SVREPO                                                  
         MVC   PTSHPAGE,PTUPAGE                                                 
*                                                                               
         LA    R4,PBUYREC+33                                                    
         USING PBDELEM,R4                                                       
         CLI   PTSHSTAT,C'A'       SEE IF STATUS APPROVED ('A')                 
         BNE   *+8                                                              
         OI    PBDSTAT,X'10'       SET ON TEAR SHEET INDICATOR                  
         DROP  R4                                                               
*                                                                               
         L     R3,FULL             ADDRESS OF END OF RECORD                     
         GOTO1 RECUP,DMCB,(1,PBUYREC),ELEM,(R3)   ADD TEARSHEET ELEM            
*                                                                               
*******              TEST FOR COMMENTS                                          
*                                                                               
PTCOM    DS    0H                  TEARSHEET WORK DONE - TEST COMMENTS          
         CLI   PTUCOMI,C'Y'        COMMENTS FOLLOWING ?                         
         BNE   PBUYWRT             NO - GO FINISH THIS RECORD                   
         MVI   ELCODE,X'69'        TEARSHEET COMMENTS ELEM CODE                 
PTCDEL   LA    R3,PBUYREC+33       DELETE ALL "OLD" COMMENTS                    
         BAS   RE,NEXTEL                                                        
         BE    PTCXCEF             NO "MORE" COMMENT ELEMENTS                   
         GOTO1 RECUP,DMCB,(1,PBUYREC),(R3),0    ** DELETE ELEM **               
         B     PTCDEL                                                           
*                                                                               
PTCXCEF  XCEF  (R3),300        CLEAR END OF RECORD (R3 POINTS TO END)           
*                                                                               
         LA    R6,4                MAX. NO. OF COMMENTS                         
         LA    R4,PTUCOM1                                                       
PTCOM10  CLI   0(R4),C' '          1ST POS'N BLANK ?                            
         BE    PBUYWRT             YES - DONE - GO WRITE RECORD                 
         LA    R5,65(R4)           ADDRESS END OF COMMENT                       
PTCOM20  CLI   0(R5),C' '            POS'N BLANK ?                              
         BNE   PTCOM30             NO - GO ADD COMMENT                          
         BCTR  R5,0                MOVE 1 POS'N TO LEFT                         
         B     PTCOM20             TEST NEXT POS'N.                             
PTCOM30  DS    0H                                                               
         SR    R5,R4               R5 NOW = LENGTH OF COMMENT                   
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'69'          TEARSHET COMMENT CODE                        
*****    BCTR  R5,0    ** NO ADJUST - R5 ALREADY 1 LESS THAN DATA LNTH          
         EX    R5,PTCMOVE          EXECUTED MOVE TO ELEMENT DATA AREA           
         LA    R5,3(R5)            ACTUAL LENGTH OF ELEMENT                     
         STC   R5,ELEM+1           ELEMENT LENGTH                               
         GOTO1 RECUP,DMCB,(1,PBUYREC),ELEM,(R3)  ** ADD ELEM TO END **          
         AR    R3,R5               BUMP R3 TO NEW REC END                       
         LA    R4,66(R4)           NEXT COMMENT                                 
         BCT   R6,PTCOM10          TEST NEXT COMMENT                            
         B     PBUYWRT              DONE - GO WRITE RECORD                      
*                                                                               
PTCMOVE  MVC   ELEM+2(0),0(R4)     EXECUTED                                     
*                                                                               
*                                                                               
PBUYWRT  DS    0H                                                               
         AP    OUTCNT,=P'1'                                                     
         CLI   QOPT2,C'Y'          DUMP RECORD ?                                
         BNE   PBUYWRTX            NO                                           
         CP    OUTCNT,=P'25'       25 RECORDS DUMPED ?                          
         BH    PBUYWRTX            YES - NO MORE DUMPING                        
         MVC   P(14),=C'*** AFTER  ***'                                         
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PBUYWRTX DS    0H                                                               
         MVC   P+56(1),PTUSTATI                                                 
         MVC   P+62(1),PTUEVALI                                                 
         MVC   P+69(1),PTUCOMI                                                  
         SP    ERRCNT,=P'1'        NOT PRINTING AN ERROR                        
         CLI   RCWRITE,C'Y'        WRITE RECORD ?                               
         BNE   PBUYWRXX            NO                                           
         LA    RF,PBUYREC                                                       
         ST    RF,AREC                                                          
         GOTO1 PUTPRT                                                           
PBUYWRXX BAS   RE,PRNTMSG          PRINT REC KEY                                
         B     GET                 NEXT RECORD                                  
*                                                                               
*                                                                               
TAPEGET  NTR1                                                                   
         GET   IN,REC                                                           
         B     EXIT                                                             
*                                                                               
         SPACE 2                                                                
*                                                                               
NEXTEL   NTR1                                                                   
NEXTELC  CLI   0(R3),0                                                          
         BE    NEXTELX             END OF RECORD - LEAVE CC EQUAL               
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLC   ELCODE,0(R3)        ELEMENT FOUND ?                              
         BNE   NEXTELC             NO - LOOK AT "NEXT"                          
         LTR   R3,R3               YES - SET CC TO NOT EQUAL                    
NEXTELX  XIT1  REGS=(R3)                                                        
         EJECT                                                                  
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DC    PL8'0'                                                           
         DC    CL20'UPLOAD RECORDS READ'                                        
ERRCNT   DC    PL8'0'                                                           
         DC    CL20'INVALID UPLOAD RECS'                                        
OUTCNT   DC    PL8'0'                                                           
         DC    CL20'BUY RECORDS UPDATED'                                        
         DC    X'FF'                                                            
*                                                                               
EOFSW    DC    C'N'                                                             
*                                                                               
NEWNUM   DS    XL6                                                              
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTS                                                        
RUNL10   CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         MVC   P(20),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,28(R4)                                                        
         B     RUNL10                                                           
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
PRNTMSG  NTR1                     PRINT KEY FIELDS AND "MESSAGES"               
         CLI   PRTSW,C'Y'                                                       
         BE    PRNTOUT                                                          
         MVC   P(2),PTUAGY                                                      
         MVC   P+4(1),PTUMED                                                    
         MVC   P+6(3),PTUCLT                                                    
         MVC   P+10(3),PTUPRD                                                   
         MVC   P+14(3),PTUEST                                                   
         MVC   P+18(17),PTUPUB                                                  
         MVC   P+36(12),PTUIDATE                                                
         AP    ERRCNT,=P'1'                                                     
PRNTOUT  DS    0H                                                               
         BAS   RE,RPRT                                                          
         MVI   PRTSW,C'Y'                                                       
         B     EXIT                                                             
*                                                                               
EOF      CLOSE (IN,)                                                            
         MVI   EOFSW,C'Y'                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVC   HEAD5+55(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+61(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
DMPKEY   NTR1                                                                   
         SPACE 2                                                                
         LA    R5,KEY                                                           
         LA    R6,25                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R6),=C'N'                                 
*                                                                               
         MVC   WORK2(25),0(R5)                                                  
         TR    WORK2(25),TRTAB                                                  
         MVC   P+75(25),WORK2                                                   
         BAS   RE,RPRT                                                          
         B     EXIT                                                             
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         MVC   HALF,25(R5)                                                      
         LH    R6,HALF              USE RECORD LENGTH                           
         LA    R3,0(R5,R6)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,RPRT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         LTORG                                                                  
*                                                                               
IN       DCB   DDNAME=IN,                                              X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00350,                                            X        
               BLKSIZE=3500,                                           X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*                                                                               
         DS    F                                                                
REC      DS    4000C                                                            
         ORG   REC                                                              
SMYTEST  DS   0C                                                                
         DC   CL2'SJ'        AGENCY CODE                                        
         DC   CL1'N'         MEDIA CODE                                         
         DC   CL3'GP '       CLIENT CODE (LEFT ALIGNED)                         
         DC   CL3'GPA'       PRODUCT CODE (LEFT ALIGNED)                        
         DC   CL3'1  '       ESTIMATE NUMBER (3 DIGITS)                         
         DC   CL17'0001             '       PUBLICATION                         
         DC   CL12'WFEB01/91-02'     INSERTION DATE + LINE NUMBER               
         DC   CL8'MAY31/96'          DATE OF TAPE MMMDD/YY                      
         DC   CL2'  '        SPACES - SPARE FOR FUTURE USE                      
SMSTATI  DC   CL1'Y'         IF "Y" STATUS WILL BE UPDATED                      
*                            IF NOT "Y" STATUS WILL NOT BE CHANGED              
SMSTAT   DC   CL1'A'         STATUS - A= APPROVED                               
*                                     N= NOT APPROVED                           
*                                 SPACE= UNDETERMINED                           
SMEVALI  DC   CL1'Y'         EVALUATION INDICATOR                               
*                            IF "Y" EVALUATION DATA WILL BE UPDATED             
*                                                                               
*                            IF NOT "Y" FIELDS SHOULD BE SPACES                 
*                            EVALUATION DATA WILL NOT BE CHANGED                
*        EVALUATION DATA                                                        
*                                                                               
SMUSOK   DC   CL1'Y'         SPACE OK?        Y OR N OR SPACE                   
SMUCOK   DC   CL1'N'         CAPTION OK?      Y OR N OR SPACE                   
SMUPOK   DC   CL1' '         POSITION OK?     Y OR N OR SPACE                   
SMUDOK   DC   CL1'Y'         INS. DATE OK?    Y OR N OR SPACE                   
SMUZOK   DC   CL1'N'         ZONE OK?         Y OR N OR SPACE                   
         DC   CL4'    '      SPACES - SPARE FOR FUTURE USE                      
SMUREPO  DC   CL2'10'        REPO QUALITY  01-10 OR SPACES                      
SMUPAGE  DC   CL10'1234 TO 10'  PAGE NOTATION (LEFT ALIGN-SPACE PAD)            
         DC   CL10'          '      SPACES - SPARE FOR FUTURE USE               
SMUCOMI  DC   CL1'Y'         IF 'Y' COMMENTS FOLLOW,                            
*                            COMMENTS WILL BE UPDATED                           
*                                                                               
SMUCOM1  DC   CL66'TENTOTAL'        FIRST COMMENT  (SPACE FILLED)               
SMUCOM2  DC   CL66'SEVENTEEN TOTAL' SECOND COMMENT (SPACE FILLED)               
SMUCOM3  DC   C' '                                                              
SMYLNTH  EQU  *-SMYTEST                                                         
*        TOTAL RECORD LENGTH IS 350 BYTES (219 HERE)                            
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
PPTUWRKD DSECT                                                                  
WORK2    DS    CL64                                                             
ELCODE   DS    X                                                                
ELEM     DS    CL200                                                            
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ITOT     DS    F                                                                
SAVERE   DS    F                                                                
ENDTST   DS    F                                                                
SVEST    DS    H                   BINARY ESTIMATE NUM                          
SVESTST  DS    CL6                 ESTIMATE START DATE - YYMMDD                 
SVESTEND DS    CL6                 ESTIMATE END   DATE - YYMMDD                 
SVLINE   DS    X                   LINE NUMBER FOR BUY KEY (BIN)                
SVREPO   DS    X                   REPO QUALITY FOR TSHT ELEM (BIN)             
SVIDAT   DS    XL3                 BINARY (YMD) INSERTION DATE                  
SVTDAT   DS    XL3                 BINARY (YMD) DATE OF TAPE                    
PRTSW    DS    X                   "Y" = KEYS HAVE BEEN PRINTED                 
KEY2     DS    CL25                                                             
IO       DS    1000X                                                            
REPS     DS    CL4                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE PPTEARUP                                                       
         EJECT                                                                  
PTSHTELD DSECT                                                                  
       ++INCLUDE PTSHTEL                                                        
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
*                                                                               
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
         EJECT                                                                  
*                                                                               
NORECD   DSECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011PPREPTU02 08/13/96'                                      
         END                                                                    
