*          DATA SET SPREPXZ02  AT LEVEL 009 AS OF 05/08/07                      
*PHASE SPXZ02A                                                                  
*INCLUDE SORTER                                                                 
         SPACE 2                                                                
*===================================================================*           
* 08MAY07  SET LOWSTART TO 2004 AND SUBSEQUENT                      *           
*        --DON'T COPY CABLE BUYS (NO CBL STAS ON PC STA FILE)       *           
* 12APR99  CHANGED TO DO TWO ADDITIONAL CLIENTS FOR TLDA PCN/PGB    *           
*                                                                   *           
* 03MAY94 RADICALLY MODIFIED ON TO NO LONGER TRANSLATE              *           
* COMMERCIAL SEQUENCE NUMBERS. PEPSI SAYS THEY ARE NO LONGER GOING  *           
* TO RUN FILM ANALYSIS REPORTS                                      *           
*===================================================================*           
         SPACE 2                                                                
*********************************************************************           
* PROGRAM READS SPTFILE TAPE FOR AGENCY PC AND DOES THE FOLLOWING - *           
*    + DELETES ALL RECORDS FOR CLIENTS PCX, TR AND GA               *           
*    + COPIES CLT, PRD, EST AND CML RECS FROM PC TO PCX, TR, GA     *           
*    + COPIES PC GOALS AND BUYS TO PCX (FROM INPUT TAPE)            *           
*    + COPIES TR AND GA GOALS AND BUYS TO PCX (FROM DISK)           *           
*    AS OF 15OCT96 AGENCY GA NO LONGER EXISTS AND IS REMOVED        *           
*********************************************************************           
         SPACE 2                                                                
* 06NOV87  ALPHA AGY SHOULD BE PE NOT PC FOR DEMO LOOKUPS                       
* 12AUG88  GO BACKWARDS IN TIME TO INCLUDE 1986 DATA                            
* 06SEP88  GO BACKWARDS IN TIME TO INCLUDE 1984/1985 DATA                       
* 07FEB89  LIMIT DATA TO 1985 AND SUBS. (LOWSTART)                              
* 13JAN92  READ MGRP EQUIVALENCE RECORDS                                        
         TITLE 'SPXZ02 - PEPSI FILE DATA TRANSFER'                              
SPXZ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPXZ02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPXZ02,RB,RC                                                     
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    XZ10                                                             
         CLI   MODE,CLTFRST                                                     
         BE    XZ30                                                             
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* RUNFRST PROCESSING                                                            
         SPACE 1                                                                
XZ10     DS    0H                                                               
*                                                                               
         OPEN  (FILEIN,(INPUT))                                                 
         OPEN  (FILEOUT,(OUTPUT))                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,REPCARD                                 
         B     EXIT                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,13,A),FORMAT=BI,WORK=1'                      
REPCARD  DC    CL80'RECORD TYPE=V,LENGTH=4004'                                  
         SPACE 1                                                                
         EJECT                                                                  
* CLTFRST PROCESSSING                                                           
         SPACE 1                                                                
XZ30     DS    0H                                                               
         BAS   RE,BLDMGREQ         BUILD MKTGRP EQUIVALENCE TABLE               
         BAS   RE,BLDPCX           COPY PC RECORDS TO PCX                       
*                                                                               
         BAS   RE,XZAGYTOT         INITIALIZE COUNTER AND XFRAGY                
         MVI   TOTIND,C'-'         SET FLAG FOR DELETING RECORDS                
         MVC   SVAGYA,=C'PE '                                                   
*                                                                               
XZ34     DS    0H                                                               
         L     R0,ADBUY                                                         
         SH    R0,=H'4'                                                         
         GET   FILEIN,(R0)                                                      
         AP    INCNT,=P'1'                                                      
* SET 2X'00' AT EOR                                                             
         L     RE,ADBUY                                                         
         SH    RE,=H'4'            POINT TO RECLEN                              
         AH    RE,0(RE)            POINT TO END OF REC                          
         XC    0(2,RE),0(RE)       CLEAR ELCODE AND LEN                         
*                                                                               
         L     R2,ADBUY                                                         
*                                                                               
         LA    R6,0(R2)            POINT TO AGYMD                               
         CLI   0(R2),X'10'         TEST BUY                                     
         BH    XZTEST                                                           
         LA    R6,1(R2)                                                         
         CLI   0(R2),0             TEST HEADER                                  
         BE    XZTEST                                                           
         CLI   0(R2),2             TEST GOAL                                    
         BE    XZTEST                                                           
**NOP    LA    R6,2(R2)                                                         
**NOP    CLC   0(2,R2),=X'0A21'    TEST CML                                     
**NOP    BE    XZTEST                                                           
         B     XZKEEP                                                           
*                                                                               
XZTEST   CLC   0(1,R6),BAGYMD      TEST AGENCY PC                               
         BNE   XZKEEP                                                           
         CLC   1(2,R6),PCX         PURGE CLIENTS PCX/TR/GA                      
         BE    XZDEL                                                            
         CLC   1(2,R6),TR                                                       
         BE    XZDEL                                                            
         SPACE 1                                                                
* RECORD IS A REAL AGENCY PC RECORD *                                           
         SPACE 1                                                                
         CLI   0(R2),X'10'         TEST BUY                                     
         BH    XZBUY                                                            
         CLI   0(R2),2             TEST GOAL                                    
         BE    XZGOAL                                                           
         B     XZKEEP                                                           
         EJECT                                                                  
         USING BUYRECD,R2                                                       
XZKEEP   CLC   =X'FFFFFFFF',0(R2)  TEST FILE TRAILER REC                        
         BE    XZ34                YES - SKIP                                   
         L     R0,ADBUY                                                         
         SH    R0,=H'4'                                                         
         PUT   FILEOUT,(R0)        PUT ALL NON-PC RECS TO OUTPUT                
         AP    OUTCNT,=P'1'                                                     
         B     XZ34                AND CONTINUE                                 
         SPACE 1                                                                
XZDEL    DS    0H                                                               
         L     R2,ADBUY                                                         
         CLI   0(R2),X'10'         TEST BUY RECORD                              
         BL    *+8                                                              
         BAS   RE,XZTOT            EXTRACT DOLLAR VALUE                         
         AP    DELCNT,=P'1'        BUMP DELETE COUNTER                          
         B     XZ34                AND READ NEXT RECORD                         
         EJECT                                                                  
XZBUY    DS    0H                                                               
         L     R0,ADBUY                                                         
         SH    R0,=H'4'                                                         
         PUT   FILEOUT,(R0)        PUT ORIGINAL BUY TO OUTPUT                   
         AP    OUTCNT,=P'1'                                                     
*                                                                               
         CLC   1(2,R2),PC          TEST CLIENT = PC                             
         BNE   XZ34                NO - CONTINUE                                
*                                                                               
         TM    BUYREC+15,X'80'     TEST BUY DELETED                             
         BO    XZ34                                                             
*                                                                               
         ZIC   R0,BUYREC+9                                                      
         BAS   RE,XZEST                                                         
         BNE   XZ34                                                             
*                                                                               
         MVC   1(2,R2),PCX         SET CLIENT = PCX                             
         MVC   BDXFRAGY,XFRAGY     SET 'COPIED BUY' IND                         
         AP    AGYCNT,=P'1'        ADD TO COPIED COUNTER                        
*                                                                               
         L     R0,ADBUY                                                         
         SH    R0,=H'4'            POINT TO LEN                                 
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R0)                                     
         B     XZ34                                                             
*                                                                               
         USING GOALRECD,R2                                                      
XZGOAL   DS    0H                                                               
         L     R0,ADBUY                                                         
         SH    R0,=H'4'                                                         
         PUT   FILEOUT,(R0)        PUT ORIGINAL GOALREC TO OUTPUT               
         AP    OUTCNT,=P'1'                                                     
*                                                                               
         CLC   2(2,R2),PC          TEST CLIENT = PC                             
         BNE   XZ34                NO - CONTINUE                                
*                                                                               
         TM    15(R2),X'80'        TEST BUY DELETED                             
         BO    XZ34                                                             
*                                                                               
         ZIC   R0,GOALREC+7                                                     
         BAS   RE,XZEST                                                         
         BNE   XZ34                                                             
*                                                                               
         MVC   2(2,R2),PCX         SET CLIENT = PCX                             
         MVC   GDXFRAGY,XFRAGY     SET 'COPIED' IND                             
         AP    AGYCNT,=P'1'        ADD TO COPIED COUNTER                        
*                                                                               
         L     R0,ADBUY                                                         
         SH    R0,=H'4'            POINT TO LEN                                 
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R0)                                     
         B     XZ34                                                             
         EJECT                                                                  
*================================================================*              
* NOW PROCESS DATA FROM OTHER SPTFILES                           *              
* EACH BUY RECORD WILL GENERATE TWO RECORDS ON THE PC FILE       *              
* ONE UNDER CLIENT PCX AND ONE UNDER THE CLIENT=AGENCY           *              
*================================================================*              
         SPACE 1                                                                
XZ50     DS    0H                                                               
         BAS   RE,XZAGYTOT         PRINT AGENCY TOTALS                          
         CLOSE FILEIN              AND CLOSE INPUT TAPE                         
*                                                                               
         L     R2,ADBUY                                                         
         MVI   0(R2),X'FF'         SET EOF REC                                  
         BAS   RE,XZTOT            FORCE LAST - CLT TO BUFFER                   
         SPACE 1                                                                
* COMPLETE THE PCAGYTAB ENTRIES WITH PACKED CLIENT CODES *                      
         SPACE 1                                                                
XZ52     LA    R4,PCAGYTAB                                                      
         USING AGYTABD,R4                                                       
*                                                                               
XZ54     DS    0H                                                               
         SPACE 1                                                                
* PACK CODE USED BY AGENCY FOR CLIENT PC *                                      
         SPACE 1                                                                
         GOTO1 CLPACK,DMCB,AGYCLTG,AGYCLTGP                                     
         SPACE 1                                                                
* PACK CODE USED ON PC FILE FOR THIS AGENCY DATA *                              
         SPACE 1                                                                
         GOTO1 CLPACK,DMCB,AGYCLTA,AGYCLTAP                                     
         MVC   KEY+2(2),AGYCLTAP            AND MOVE TO KEY                     
         SPACE 1                                                                
         LA    R4,AGYTABL(R4)                                                   
         CLI   0(R4),X'FF'                                                      
         BNE   XZ54                                                             
*                                                                               
         LA    RE,PCAGYTAB         POINT TO FIRST TABLE ENTRY                   
         B     XZ78                                                             
         DROP  R4                                                               
         EJECT                                                                  
XZ60     DS    0H                  CLOSE PREVIOUS SPOT SYSTEM                   
         GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'SPOT'                                 
*                                                                               
         XC    CNDATA,CNDATA                                                    
         LA    R4,CNDATA                                                        
         USING CND,R4                                                           
         MVC   CNAGY(2),SVAGYA     MOVE ALPHA AGENCY CODE                       
*                                                                               
         GOTO1 CONFID,DMCB,(R4),(1,FULL)                                        
         OC    FULL,FULL           TEST ID FOUND                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,UTL                                                           
         MVC   4(1,RE),CNSSE       MOVE SPOT SYSTEM NUMBER                      
         MVC   SVAGYB,CNSCD        AND SAVE SPOT AGENCY NUMBER                  
         DROP  R4                                                               
*          DATA SET SPREPXK02  AT LEVEL 054 AS OF 03/02/94                      
*                                                                               
* NOW TEST TO OPEN NEW TRAFFIC FILES                                            
* IF THEY EXIST OPEN THEM. IF THEY DON'T, GO AWAY                               
*                                                                               
         MVI   FTRFDIR,C' '        DEFAULT TO OPEN NEW TRAFFIC                  
         MVI   FTRFFIL,C' '                                                     
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'SYSFLES',0                            
         ICM   RE,15,DMCB+12       A(FILE LIST FOR THIS SPOT SYSTEM)            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         ICM   R1,3,2(RE)          NUMBER OF FILES IN SYSTEM                    
         LA    RE,4(RE)            BUMP TO FIRST FILE IN LIST                   
*                                                                               
XZ60A    CLI   3(RE),X'33'         STRFDR?                                      
         BE    XZ60B               YES -- SEE IF IT'S NOP                       
         LA    RE,8(RE)            NO, TRY NEXT FILE                            
         BCT   R1,XZ60A                                                         
         B     XZ60C               NO TRAFFIC FILES -- REMOVE FROM LIST         
*                                                                               
XZ60B    TM    0(RE),X'80'         ARE TRAFFIC FILES NOP                        
         BZ    XZ60D               NO                                           
*                                                                               
XZ60C    MVI   FTRFDIR,C'Z'        YES -- TAKE THEM OUT OF THE LIST             
         MVI   FTRFFIL,C'Z'                                                     
*                                                                               
XZ60D    DS    0H                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',FLIST,ADBUY                     
*                                                                               
XZ61     XC    KEY,KEY                                                          
         IC    R0,SVAGYB                                                        
         STC   R0,KEY+1            SET AGENCY IN KEY (LEFT ALIGN)               
         OI    KEY+1,X'01'         SET MEDIA = SPOT TV                          
         MVC   KEY+2(2),SVPCCLT                                                 
         GOTO1 HIGH                READ CLTHDR ON AGY FILE                      
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R8,=A(AGCLTHDR)                                                  
         ST    R8,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         BAS   RE,BLDPRD           BUILD PRODUCT TRANSLATE TABLE                
*                                                                               
**NOP    XC    KEY,KEY                                                          
**NOP    MVC   KEY(2),=X'0A21'                                                  
**NOP    MVC   KEY+2(3),1(R8)      A-M/CLT                                      
**NOP    BAS   RE,BLDCML           BUILD COMMERCIAL TRANSLATE TABLE             
         EJECT                                                                  
*=========================================================*                     
* READ ALL GOAL RECORDS FOR THIS CLIENT                   *                     
*=========================================================*                     
         SPACE 1                                                                
         MVI   DMINBTS,X'08'       PASS DELETED RECORDS                         
         MVI   DMOUTBTS,X'FD'      AND IGNORE ERROR IF DELETED                  
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+1(3),1(R8)      SET A-M/CLT                                  
         GOTO1 HIGH                                                             
         B     XZ64                                                             
*                                                                               
XZ62     GOTO1 SEQ                                                              
*                                                                               
XZ64     CLC   KEY(4),KEYSAVE      SAME TYPE/A-M/CLT                            
         BNE   XZ70                                                             
*                                                                               
         CLI   KEY+4,X'FF'         TEST POL (CPP GUIDE)                         
         BE    XZ70                YES - DONE                                   
*                                                                               
         ZIC   R0,KEY+7            GET EST NUMBER                               
         BAS   RE,XZEST            TEST POL ESTIMATE OPEN                       
         BNE   XZ62                                                             
*                                                                               
         L     R2,ADGOAL                                                        
         USING GOALRECD,R2                                                      
*                                                                               
         ST    R2,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         TM    15(R2),X'80'        TEST DELETED                                 
         BO    XZ62                                                             
*                                                                               
         MVI   PRDERRSW,0          RESET FATAL ERROR SWITCH                     
         LA    R7,GOALREC+4        POINT TO PRD 1                               
         BAS   RE,FINDPRD                                                       
         BNE   XZ62                                                             
*                                                                               
         LA    R7,GOALREC+12                                                    
         CLI   0(R7),0                                                          
         BE    *+12                                                             
         BAS   RE,FINDPRD                                                       
         BNE   XZ62                                                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,13(R2)         GET REC LENGTH                               
         AH    R0,=H'4'                                                         
         L     R4,ADGOAL                                                        
         SH    R4,=H'4'                                                         
         SLL   R0,16                                                            
         ST    R0,0(R4)            SET REC LENGTH FOR SORT                      
         EJECT                                                                  
*======================================================*                        
* GENERATE RECORD FOR CLIENT PCX                       *                        
*======================================================*                        
         SPACE 1                                                                
         MVC   1(1,R2),BAGYMD      MOVE AGY/MED TO REC                          
         MVC   2(2,R2),PCX         SET CLIENT = PCX                             
         MVC   GOALREC+20(2),=C'PE' AGYALPHA BECOMES PC                         
         MVC   GDXFRAGY,XFRAGY     SET TRANSFER AGY NUMBER                      
*                                                                               
         AP    AGYCNT,=P'1'                                                     
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)   AND PUT RECORD 1                  
*                                                                               
         MVC   2(2,R2),SVPCCLT     SET CLIENT CODE FOR PC/AGY                   
         AP    AGYCNT,=P'1'                                                     
         GOTO1 (RF),(R1)                      AND PUT RECORD 2                  
*                                                                               
         CLI   QOPT1,C'Y'          SEE IF TEST RUN                              
         BNE   XZ62                                                             
         CP    AGYCNT,=P'100'                                                   
         BL    XZ62                                                             
         EJECT                                                                  
*=======================================================*                       
* READ ALL POL BUY RECORDS FOR THIS CLIENT              *                       
*=======================================================*                       
         SPACE 1                                                                
XZ70     XC    KEY,KEY                                                          
         MVC   KEY(3),1(R8)        SET A-M/CLT                                  
         MVI   KEY+3,X'FF'         SET PRD=POL                                  
         GOTO1 HIGH                                                             
         B     XZ74                                                             
*                                                                               
XZ72     GOTO1 SEQ                                                              
*                                                                               
XZ74     CLC   KEY(3),KEYSAVE      SAME A-M/CLT                                 
         BNE   XZ76                                                             
*                                                                               
         ZIC   R0,KEY+9                                                         
         BAS   RE,XZEST            VALIDATE POL EST OPEN                        
         BNE   XZ72                                                             
         CLI   KEY+6,X'E8'         CABLE BUY?                                   
         BNL   XZ72                 YES - SKIP IT                               
*                                                                               
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         ST    R2,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         CLC   KEY+4(2),BUYREC+4   TEST SPILL POINTER                           
         BNE   XZ72                                                             
*                                                                               
         MVI   PRDERRSW,0          RESET FATAL ERROR SWITCH                     
         BAS   RE,XZPRD            TRANSLATE PRD CODES                          
         CLI   PRDERRSW,C'Y'       TEST ERROR                                   
         BE    XZ72                YES - SKIP RECORD                            
*                                                                               
**NOP    BAS   RE,XZCML            TRANSLATE CMML CODES                         
*                                                                               
         OC    MGREQCNT,MGREQCNT                                                
         BZ    *+8                                                              
         BAS   RE,FINDMGEQ         SEE IF NEED MARKET OVERRIDE                  
*                                                                               
         MVC   BDXFRAGY,XFRAGY     SET TRANSFER FLAG IN BUYREC                  
         MVC   BUYALPHA,=C'PE'     AGYALPHA BECOMES PE                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,13(R2)         GET REC LENGTH                               
         AH    R0,=H'4'                                                         
         L     R4,ADBUY                                                         
         SH    R4,=H'4'                                                         
         SLL   R0,16               LEFT ALIGN                                   
         ST    R0,0(R4)            SET REC LENGTH FOR SORT                      
         EJECT                                                                  
*=========================================================*                     
* GENERATE PC/PCX RECORD                                  *                     
*=========================================================*                     
         SPACE 1                                                                
         MVC   0(1,R2),BAGYMD      MOVE PC AGY/MED TO REC                       
         MVC   1(2,R2),PCX         SET CLIENT = PCX                             
*                                                                               
         AP    AGYCNT,=P'1'                                                     
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)    AND PUT RECORD 1                 
         SPACE 1                                                                
* NOW GENERATE PC/AGY RECORD *                                                  
         SPACE 1                                                                
         MVC   1(2,R2),SVAGYCLT    MOVE CLIENT CODE                             
*                                                                               
         AP    AGYCNT,=P'1'                                                     
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)                                     
         CLI   QOPT1,C'Y'          SEE IF TEST RUN                              
         BNE   XZ72                                                             
         CP    AGYCNT,=P'200'                                                   
         BNH   XZ72                                                             
*                                                                               
XZ76     DS    0H                                                               
         BAS   RE,XZAGYTOT                                                      
*                                                                               
XZ77     L     RE,SVPCAGY                                                       
         LR    RF,RE               SAVE PREVIOUS ADDRESS                        
         LA    RE,AGYTABL(RE)      NEXT ENTRY                                   
         USING AGYTABD,RE                                                       
*                                                                               
XZ78     CLI   0(RE),X'FF'         TEST EOL                                     
         BE    XZ80                                                             
         ST    RE,SVPCAGY                                                       
         MVC   SVAGYA,AGYAGY       SET NEW AGYALPHA                             
         MVI   SVAGYA+2,C' '                                                    
         MVC   WORK(3),AGYCLTA     MOVE EBCDIC CLIENT CODE                      
         MVC   SVAGYCLT,AGYCLTAP   AND PACKED CLIENT CODE                       
         MVC   SVPCCLT,AGYCLTGP    AND SET AGENCY CODE FOR CLT PC               
*                                                                               
         CLC   SVAGYA(2),AGYAGY-AGYTABD(RF)  TEST NEXT IS SAME AGENCY           
         BE    XZ61                          YES - SKIP OPEN/CLOSE              
         B     XZ60                                                             
*                                                                               
         DROP RE                                                                
         EJECT                                                                  
*  AGYCLTA FIELDS ARE THE ALPHA CLIENT CODES USED ON THE PC FILE                
*          TO HOLD AGENCY DATA                                                  
*  AGYCLTG FIELDS ARE THE ALPHA CLIENT CODES USED BY THE AGENCY                 
*          TO HOLD CLIENT PC DATA                                               
*                                                                               
*  E.G., IN THE TABLE BELOW, TRACY LOCKE (TR) HAS PEPSI BUYS UNDER              
*        CLIENT PC AND CLIENT TRI.                                              
         SPACE 2                                                                
AGYTABD  DSECT                                                                  
AGYAGY   DS    CL2                 ALPHA AGENCY CODE                            
AGYCLTA  DS    CL3                 PC FILE CLIENT CODE                          
AGYCLTAP DS    XL2                 PC FILE PACKED CLIENT CODE                   
AGYCLTG  DS    CL3                 AGY FILE CLIENT CODE (FOR PC)                
AGYCLTGP DS    XL2                 AGY FILE PACKED CLIENT CODE (FOR PC)         
AGYTABL  EQU   *-AGYTABD                                                        
         SPACE 1                                                                
SPXZ02   CSECT                                                                  
         SPACE 1                                                                
PCAGYTAB DS    0CL9                                                             
         DC    C'TR',C'TR ',AL2(0),C'PC ',AL2(0)                                
         DC    C'TR',C'TR ',AL2(0),C'PCN',AL2(0)                                
         DC    C'TR',C'TR ',AL2(0),C'PGB',AL2(0)                                
         DC    X'FF'                 EOL FLAG                                   
*                                                                               
FLIST    DC    CL8' SPTFILE'                                                    
         DC    CL8' SPTDIR '                                                    
FTRFDIR  DC    CL8'ZSTRFDR '                                                    
FTRFFIL  DC    CL8'ZSTRFFL '                                                    
         DC    CL8' CTFILE '                                                    
         DC    CL8'X       '                                                    
         EJECT                                                                  
* ALL AGENCY FILES HAVE BEEN PROCESSED - SORT RECORDS AND WRITE TO TAPE         
         SPACE 1                                                                
XZ80     MVI   TOTIND,C'+'         INDICATE ADDING RECORDS                      
*                                                                               
XZ82     DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,4(R1)                                                      
         BZ    XZ90                                                             
         LR    R0,R2                                                            
         PUT   FILEOUT,(R0)                                                     
         AP    OUTCNT,=P'1'                                                     
         AP    ADDCNT,=P'1'                                                     
* MOVE RECORD *                                                                 
         L     R1,ADBUY                                                         
         SH    R1,=H'4'                                                         
         LH    RE,0(R2)            GET LENGTH                                   
XZ84     CH    RE,=H'256'                                                       
         BNH   XZ86                                                             
         MVC   0(256,R1),0(R2)                                                  
         LA    R1,256(R1)                                                       
         LA    R2,256(R2)                                                       
         SH    RE,=H'256'                                                       
         B     XZ84                                                             
XZ86     BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R2)  *EXECUTED*                                        
*                                                                               
         LA    R1,1(RE,R1)         POINT TO END OF REC                          
         XC    0(2,R1),0(R1)       CLEAR NEXT ELEM CODE/LENGTH                  
*                                                                               
         BAS   RE,XZTOT                                                         
         B     XZ82                                                             
*                                                                               
XZ90     DS    0H                  PRINT FINAL TOTALS                           
         CLOSE FILEOUT                                                          
*                                                                               
         L     R2,ADBUY                                                         
         MVI   0(R2),X'FF'         PASS EOF TO TOT ROUTINES                     
         BAS   RE,XZTOT                                                         
*                                                                               
         BAS   RE,XZTOTPRT                                                      
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
XZ92     LA    R4,CTRS                                                          
*                                                                               
XZ94     MVC   P(20),6(R4)                                                      
         EDIT  (P6,0(R4)),(12,P+22)                                             
         GOTO1 REPORT                                                           
         LA    R4,L'CTRS(R4)                                                    
         LA    R0,CTRX                                                          
         CR    R4,R0                                                            
         BL    XZ94                                                             
         B     EXIT                                                             
         EJECT                                                                  
*******************************************************                         
* COPY CLT/PRD/EST/CML RECORDS FROM PC TO PCX/TR/GA   *                         
*                                                     *                         
* BUILD TABLE OF PC ESTIMATE HEADERS                  *                         
* PCESTTAB HAS NON-ZERO VALUE FOR EACH OPEN ESTIMATE  *                         
* PCPOLTAB HAS 4 BYTE PACKED DATE OF POL ESTIMATE     *                         
*******************************************************                         
         SPACE 1                                                                
*LOWSTART DC    C'841231'         IGNORE ESTIMATES PRIOR TO THIS DATE           
LOWSTART DC    C'031228'         IGNORE ESTIMATES PRIOR TO THIS DATE            
         SPACE 1                                                                
BLDPCX   NTR1                                                                   
* CONVERT LOWSTART TO 'FUNNY' DATE                                              
         GOTO1 DATCON,DMCB,(0,LOWSTART),(0,LOWSTART)                            
*                                                                               
         L     R1,=A(PCESTTAB)                                                  
         L     R0,=A(PCESTTBX)                                                  
         BAS   RE,CLEAR                                                         
*                                                                               
         L     R1,=A(PCPOLTAB)                                                  
         L     R0,=A(PCPOLTBX)                                                  
         BAS   RE,CLEAR                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         GOTO1 HIGH                                                             
         B     BLDPCX4                                                          
*                                                                               
BLDPCX2  GOTO1 SEQ                                                              
*                                                                               
BLDPCX4  CLC   KEY(4),KEYSAVE      SAME A-M/CLT                                 
         BNE   BLDPCX30                                                         
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   BLDPCX2                                                          
*                                                                               
         L     R2,ADBUY                                                         
         ST    R2,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         USING ESTHDRD,R2                                                       
         CLI   KEY+7,0             TEST ESTIMATE HEADER                         
         BE    BLDPCX6             NO                                           
         CLC   ESTART,LOWSTART     COMPARE TO XFR START DATE                    
         BL    BLDPCX2                                                          
* ZERO ALL THE BUCKETS                                                          
         LHI   R0,26                                                            
         LA    R1,EORD                                                          
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         LHI   R0,26                                                            
         LA    R1,EPAID                                                         
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         LHI   R0,13                                                            
         LA    R1,EAUTH                                                         
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         ZAP   ECURPDN,=P'0'                                                    
         DROP  R2                                                               
*                                                                               
BLDPCX6  MVC   2(2,R2),TR          SET CLIENT = TR                              
         BAS   RE,PUTSORT                                                       
*                                                                               
         USING CLTHDRD,R2                                                       
         MVC   2(2,R2),PCX         SET CLIENT = PCX                             
         OC    KEY+4(9),KEY+4      TEST CLIENT HEADER                           
         BNZ   *+14                                                             
         MVI   CEXTRA+2,C'F'       INDICATE BUYID REQUIRED                      
         MVC   CTITLE(10),=CL10'MARKET'                                         
         DROP  R2                                                               
         BAS   RE,PUTSORT                                                       
*                                                                               
         CLI   KEY+7,0             TEST ESTIMATE HEADER                         
         BE    BLDPCX2             NO                                           
*                                                                               
         CLC   =C'POL',KEY+4       TEST POL ESTIMATE                            
         BE    BLDPCX10                                                         
*                                                                               
         USING ESTHDRD,R2                                                       
BLDPCX8  ZIC   RE,EPRDCD+1         GET PRD NUMBER                               
         DROP  R2                                                               
         SLL   RE,8                X 256                                        
         A     RE,=A(PCESTTAB)                                                  
         ZIC   R0,KEY+7            GET EST NUMBER                               
         AR    RE,R0               INDEX TO CORRECT BYTE                        
         MVC   0(1,RE),KEY+7       MOVE EST NUMBER TO TABLE                     
         B     BLDPCX2                                                          
         SPACE 1                                                                
* POL ESTHDR - SAVE START/END DATES *                                           
         SPACE 1                                                                
         USING ESTHDRD,R2                                                       
BLDPCX10 DS    0H                                                               
         GOTO1 DATCON,DMCB,ESTART,(2,FULL)                                      
         GOTO1 (RF),(R1),EEND,(2,FULL+2)                                        
         ZIC   RE,KEY+7            GET ESTIMATE NUMBER                          
         SLL   RE,2                X 4                                          
         A     RE,=A(PCPOLTAB)                                                  
         MVC   0(4,RE),FULL        MOVE DATES TO TABLE                          
         B     BLDPCX2                                                          
         DROP  R2                                                               
         EJECT                                                                  
*******************************************************                         
* NOW READ PC CMML RECORDS AND GENERATE FOR PCX/TR/GA *                         
* AND BUILD TABLE OF COMMERCIALS IN PCCMLTAB          *                         
*******************************************************                         
         SPACE 1                                                                
BLDPCX30 L     R1,=A(PCCMLTAB)                                                  
         L     R0,=A(PCCMLTBX)                                                  
         BAS   RE,CLEAR                                                         
*                                                                               
         L     R6,=A(PCCMLTAB)     SET TABLE POINTER                            
         SR    R7,R7               CLEAR TABLE COUNTER                          
         B     BLDPCX36            ***** NOP *****                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         GOTO1 HIGH                                                             
         B     BLDPCX34                                                         
*                                                                               
BLDPCX32 GOTO1 SEQ                                                              
*                                                                               
BLDPCX34 CLC   KEY(5),KEYSAVE      SAME TYPE/A-M/CLT                            
         BNE   BLDPCX36                                                         
*                                                                               
         L     R2,ADBUY                                                         
         ST    R2,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         MVC   3(2,R2),PCX         SET CLIENT = PCX                             
         BAS   RE,PUTSORT                                                       
         MVC   3(2,R2),TR          SET CLIENT = TR                              
         BAS   RE,PUTSORT                                                       
*                                                                               
         USING CMLRECD,R2                                                       
         MVC   0(8,R6),KEY+5       SAVE COMMERCIAL CODE                         
         MVC   8(2,R6),CMLSEQ+1    AND 2 BYTES OF CMML SEQ                      
         DROP  R2                                                               
         LA    R6,10(R6)                                                        
         BCTR  R7,0                                                             
         L     R0,=A(PCCMLTBX)                                                  
         CR    R6,R0                                                            
         BL    BLDPCX32                                                         
         B     CMLTBERR                                                         
*                                                                               
BLDPCX36 LPR   R7,R7                                                            
         ST    R7,PCCMLCNT                                                      
         B     EXIT                                                             
         EJECT                                                                  
PUTSORT  NTR1                                                                   
         SR    R0,R0                                                            
         ICM   R0,3,13(R2)                                                      
         AH    R0,=H'4'                                                         
         LR    R4,R2                                                            
         SH    R4,=H'4'                                                         
         SLL   R0,16                                                            
         ST    R0,0(R4)                                                         
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)                                     
         AP    AGYCNT,=P'1'                                                     
         B     EXIT                                                             
         EJECT                                                                  
* READ AND SAVE ALL COMMERCIALS AND SEQ NUMBERS FOR CLIENT IN 'KEY'             
         SPACE 1                                                                
BLDCML NTR1                                                                     
         L     R1,=A(SVCMLTAB)                                                  
         L     R0,=A(SVCMLTBX)                                                  
         BAS   RE,CLEAR                                                         
*                                                                               
         L     R4,=A(SVCMLTAB)                                                  
         SR    R5,R5               CLEAR COUNTER                                
         B     BLDCML10            ************ NOP *************               
*                                                                               
         GOTO1 HIGH                                                             
         B     BLDCML4                                                          
*                                                                               
BLDCML2  GOTO1 SEQ                                                              
*                                                                               
BLDCML4  CLC   KEY(5),KEYSAVE      0A21/A-M/CLT                                 
         BNE   BLDCML10                                                         
*                                                                               
         L     R6,=A(XZIO)                                                      
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         USING CMLRECD,R6                                                       
*                                                                               
         MVC   0(8,R4),KEY+5       SAVE COMMERCIAL CODE                         
         MVC   8(2,R4),CMLSEQ+1    AND 2 BYTES OF CMML SEQ                      
         BCTR  R5,0                BUMP COUNTER                                 
         LA    R4,10(R4)           NEXT ENTRY                                   
         L     R0,=A(SVCMLTBX)                                                  
         CR    R4,R0                                                            
         BL    BLDCML2                                                          
         B     CMLTBERR                                                         
*                                                                               
BLDCML10 LPR   R5,R5                                                            
         ST    R5,SVCMLCNT                                                      
         B     EXIT                                                             
         EJECT                                                                  
*=====================================================*                         
* SET UP EQUIVALENCE TABLE BETWEEN AGY/PC AND PC/PCX  *                         
* FORCE ALL NON-MATCHES TO PRD XX ON PC TO GET AROUND *                         
* PROBLEMS OF PIGGYBACKS WITH NON-PEPSI BRANDS        *                         
*=====================================================*                         
         SPACE 1                                                                
BLDPRD   NTR1                                                                   
         L     R5,ADCLT            POINT TO PC CLTHDR                           
         LA    R5,CLIST-CLTHDRD(R5)                                             
BLDPRD1A CLC   0(3,R5),=C'XX '                                                  
         BE    BLDPRD1X                                                         
         LA    R5,4(R5)                                                         
         CLI   0(R5),C'A'                                                       
         BNL   BLDPRD1A                                                         
         B     BLDPRD2                                                          
BLDPRD1X ST    R5,APRDXX           SAVE SLOT ADDRESS                            
*                                                                               
BLDPRD2  L     R4,=A(AGCLTHDR)     POINT TO NEW CLTHDR                          
         LA    R4,CLIST-CLTHDRD(R4)                                             
         L     RF,=A(PRDTAB)                                                    
         XC    0(256,RF),0(RF)                                                  
*                                                                               
BLDPRD3  L     R5,ADCLT            POINT TO PC CLTHDR                           
         LA    R5,CLIST-CLTHDRD(R5)                                             
*                                                                               
BLDPRD4  CLC   0(3,R4),0(R5)       MATCH ALPHA PRD CODE                         
         BE    BLDPRD6                                                          
         LA    R5,4(R5)                                                         
         CLI   0(R5),C'A'                                                       
         BNL   BLDPRD4                                                          
         ICM   R5,15,APRDXX        POINT TO PRD XX                              
         BZ    BLDPRD10            IGNORE ERROR IF NO XX                        
*                                                                               
BLDPRD6  ZIC   RE,3(R4)            GET PRD NUM                                  
         AR    RE,RF               POINT TO PROPER SLOT                         
         MVC   0(1,RE),3(R5)       MOVE PC PRD CODE TO SLOT                     
*                                                                               
BLDPRD10 LA    R4,4(R4)                                                         
         CLI   0(R4),C'A'                                                       
         BNL   BLDPRD3                                                          
         B     EXIT                                                             
         EJECT                                                                  
*===================================================================*           
* BUILD A TABLE OF MGR EQUIVALENCE RECORDS STA(3)/MGRP(5)/MKT(2)    *           
*===================================================================*           
         SPACE 1                                                                
BLDMGREQ NTR1                                                                   
         L     R1,=A(MGREQTAB)                                                  
         L     R0,=A(MGREQTBX)                                                  
         BAS   RE,CLEAR                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D44'     STA EQU ID                                   
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),PCX        READ RECORDS FOR CLIENT PC                   
         L     R3,=A(MGREQTAB)                                                  
         SR    R4,R4               RESET COUNTER                                
         GOTO1 HIGH                                                             
         B     BLDMG12                                                          
*                                                                               
BLDMG10  GOTO1 SEQ                                                              
*                                                                               
BLDMG12  CLC   KEY(5),KEYSAVE      SAME TYPE/A-M/CLT                            
         BNE   BLDMGX                                                           
*                                                                               
         L     R6,ADBUY            READ REC INTO BUY AREA                       
         ST    R6,AREC                                                          
         USING STERECD,R6                                                       
         GOTO1 GET                                                              
         MVC   WORK(4),=4C'0'                                                   
         MVC   WORK+4(5),STEKSTA                                                
         MVC   WORK+9(3),=C'   '                                                
         GOTO1 MSPACK,DMCB,WORK,WORK+4,WORK+12                                  
         SPACE                                                                  
* READ AND STORE ALL 03 ELEMENTS                                                
         LA    R6,24(R6)                                                        
         MVI   ELCODE,3                                                         
         BAS   RE,FIRSTEL                                                       
         B     *+8                                                              
BLDMG20  BAS   RE,NEXTEL                                                        
         BNE   BLDMG10                                                          
*                                                                               
         MVC   0(3,R3),WORK+14     STORE PACKED STA                             
         USING STEEL03,R6                                                       
         MVC   3(1,R3),STEMGID     MGRPID                                       
         UNPK  4(5,R3),STEMGRP(3)  MGRP (UNPK 1 EXTRA BYTE)                     
         MVC   8(2,R3),STEMGMKT    BINARY MARKET                                
* PRINT IT OUT SO WE HAVE A CLUE ABOUT WHAT'S GOING ON                          
         MVC   P(11),=C'** MGREQ **'                                            
         MVC   P+12(4),WORK+4      STATION                                      
         MVC   P+17(5),3(R3)       MGRPID/MGRP                                  
         SR    R0,R0                                                            
         ICM   R0,3,STEMGMKT                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+23(4),DUB         OVERRIDE MARKET                              
         GOTO1 REPORT                                                           
*                                                                               
         LA    R3,10(R3)                                                        
         BCTR  R4,0                                                             
         B     BLDMG20                                                          
         DROP  R6                                                               
*                                                                               
BLDMGX   LPR   R4,R4                                                            
         ST    R4,MGREQCNT         SAVE COUNTER                                 
         XIT1                                                                   
         SPACE                                                                  
* IF TABLE EXCEEDED COME HERE *                                                 
         SPACE                                                                  
BLDMGERR MVC   ERRSTEQ(35),=CL35'** ERROR ** MGREQ TABLE TOO SMALL'             
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*==========================================================*                    
* SEARCH MKTGRP EQUIVALENCE TABLE FOR OVERRIDE MARKET      *                    
*==========================================================*                    
         SPACE 1                                                                
FINDMGEQ NTR1                                                                   
         L     R6,ADBUY                                                         
         LR    R2,R6                                                            
         LA    R6,24(R6)                                                        
         MVI   ELCODE,X'70'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   EXIT                                                             
         L     R3,=A(MGREQTAB)                                                  
         L     R4,MGREQCNT         # OF ENTRIES                                 
*                                                                               
FMGEQ10  CLC   0(3,R3),6(R2)       STATION MATCH                                
         BNE   FMGEQ12                                                          
         CLC   3(5,R3),3(R6)       MGRP MATCH                                   
         BE    FMGEQ20                                                          
*                                                                               
FMGEQ12  LA    R3,10(R3)           NEXT ENTRY                                   
         BCT   R4,FMGEQ10                                                       
         B     EXIT                                                             
*                                                                               
FMGEQ20  MVC   4(2,R2),8(R3)       IF EQUAL CHANGE MKT                          
         MVC   P(9),=C'MGRP OVRD'                                               
         MVC   P+10(5),3(R3)                                                    
         LA    R4,P+16                                                          
         B     XZPRT               PRINT BUY AND RETURN TO CALLER               
         EJECT                                                                  
* TRANSLATE ALL PRODUCT CODES IN BUYREC TO PC PRD CODES                         
         SPACE 1                                                                
XZPRD    NTR1                                                                   
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         LA    R7,BDMASPRD                                                      
         CLI   0(R7),0                                                          
         BE    *+8                                                              
         BAS   RE,FINDPRD                                                       
         LA    R7,1(R7)                                                         
         CLI   0(R7),0                                                          
         BE    *+8                                                              
         BAS   RE,FINDPRD                                                       
*                                                                               
         SR    R0,R0                                                            
         LA    R6,BDELEM                                                        
XZPRD2   ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    EXIT                                                             
         CLI   0(R6),6                                                          
         BL    XZPRD2                                                           
         CLI   0(R6),13                                                         
         BH    XZPRD2                                                           
*                                                                               
         TM    6(R6),X'C0'         TEST MINUS OR MINUSSED                       
         BNZ   XZPRD3              SKIP DATE TEST FOR SOPHIA                    
* HOPE SHE REMEMBERS HOW KIND THIS IS                                           
         CLC   2(2,R6),POLESDTS    TEST ELEM PRIOR TO PC EST START              
         BL    XZPERERR                                                         
         CLC   2(2,R6),POLESDTS+2    OR AFTER PC EST END                        
         BH    XZPERERR                                                         
*                                                                               
XZPRD3   ZIC   R0,1(R6)                                                         
         SH    R0,=H'10'                                                        
         BNP   XZPRD2                                                           
         SRL   R0,2                SET FOR BCT                                  
         LA    R7,10(R6)                                                        
*                                                                               
XZPRD4   BAS   RE,FINDPRD                                                       
*                                                                               
         SLL   RF,8                PRD CODE X 256                               
         ZIC   RE,BUYREC+9         ESTIMATE NUMBER                              
         AR    RF,RE                                                            
         A     RF,=A(PCESTTAB)     POINT TO EST TAB ENTRY                       
         CLI   0(R1),0             TEST PC EST OPEN                             
         BE    XZESTERR                                                         
XZPRD6   LA    R7,4(R7)            NEXT PRD CODE                                
         BCT   R0,XZPRD4                                                        
         B     XZPRD2                                                           
         EJECT                                                                  
FINDPRD  ZIC   RF,0(R7)            GET PRD CODE                                 
         A     RF,APRDTAB                                                       
         CLI   0(RF),0                                                          
         BE    FINDPRD2                                                         
         MVC   0(1,R7),0(RF)                                                    
         BR    RE                                                               
*                                                                               
FINDPRD2 NTR1 ,                     SET UP TO 'ENTER' ERROR ROUTINES            
         B     XZPRDERR                                                         
         SPACE 2                                                                
******************************************************                          
* TEST AGENCY ESTIMATE NUMBER IS VALID ON CLIENT PCX *                          
* R0 CONTAINS ESTIMATE NUMBER ON ENTRY               *                          
******************************************************                          
         SPACE 1                                                                
XZEST    NTR1                                                                   
         LR    RE,R0               GET EST NUMBER                               
         SLL   RE,2                X 4                                          
         A     RE,=A(PCPOLTAB)                                                  
         MVC   POLESDTS,0(RE)      MOVE TO SAVE AREA                            
         OC    0(4,RE),0(RE)       TEST POL EST OPEN                            
         BNZ   EQXIT                                                            
         B     NEQXIT                                                           
         EJECT                                                                  
* TRANSLATE BUYREC CMML CODES TO PC CMML CODES                                  
         SPACE 1                                                                
XZCML    NTR1                                                                   
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
XZCML2   ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    EXIT                                                             
         CLI   0(R6),X'12'         TEST FILM ELEMENT                            
         BNE   XZCML2                                                           
*                                                                               
         ZIC   R0,1(R6)                                                         
         SH    R0,=H'3'                                                         
         SRL   R0,1                DIVIDE BY 2                                  
         LA    R1,3(R6)                                                         
*                                                                               
XZCML4   BAS   RE,GETCML                                                        
         BNE   XZCML2    <====== TEMP PATCH FOR DELETED COMMERCIALS             
         BAS   RE,GETCMLPC                                                      
         LA    R1,2(R1)                                                         
         BCT   R0,XZCML4                                                        
         B     XZCML2                                                           
*                                                                               
CMLERR1  DC    H'0'                CMML NOT FOUND IN AGYLIST                    
         SPACE 1                                                                
* FIND ALPHA EQUIVALENT OF CMLSEQ IN SVCMLTAB AND SAVE IN SVCMML *              
         SPACE 1                                                                
GETCML   L     R4,=A(SVCMLTAB)                                                  
         L     R5,SVCMLCNT                                                      
*                                                                               
GETCML2  CLC   8(2,R4),0(R1)       MATCH CMML SEQ                               
         BE    GETCML4                                                          
         LA    R4,10(R4)                                                        
         BCT   R5,GETCML2                                                       
         LTR   RE,RE               SET CC NOT EQ                                
         BR    RE                                                               
*                                                                               
GETCML4  MVC   SVCMML,0(R4)        SAVE CMLTAB ENTRY                            
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
         EJECT                                                                  
* FIND ALPHA CMML ENTRY IN PCTAB *                                              
         SPACE 1                                                                
GETCMLPC L     R4,=A(PCCMLTAB)                                                  
         L     R5,PCCMLCNT                                                      
*                                                                               
CMLPC2   CLC   0(8,R4),SVCMML      MATCH CMML CODE                              
         BE    CMLPC4                                                           
         LA    R4,10(R4)                                                        
         BCT   R5,CMLPC2                                                        
         XC    0(2,R1),0(R1)       CLEAR MISSING CMML SEQ                       
         B     CMLPCERR            AND PRINT ERROR MESSAGE                      
*                                                                               
CMLPC4   MVC   0(2,R1),8(R4)       MOVE PC CML SEQ NUMBER                       
         CR    RE,RE               SET PC EQ                                    
         BR    RE                                                               
*                                                                               
CMLPCERR NTR1                                                                   
         MVC   ERRMSG(33),=C'**ERROR ** MISSING PCX COMMERCIAL'                 
         MVC   ERRMSG+34(8),SVCMML                                              
*                                                                               
         CLC   SVBUYERR,BUYREC     TEST SAME BUYREC                             
         BNE   CMLPCER2                                                         
         CLC   SVCMLERR,SVCMML     SAME BUY - TEST SAME CMML                    
         BE    CMLPCER2            SAME CMML - XZERR WILL SUPPRESS MSG          
         XC    SVBUYERR,SVBUYERR   NEW CMML - FORCE TO PRINT                    
*                                                                               
CMLPCER2 MVC   SVCMLERR,SVCMML     SAVE CMML CODE                               
         B     XZERR                                                            
         EJECT                                                                  
MKTERR   MVC   ERRMSG(31),=C'**ERROR ** PCX MARKET NOT FOUND'                   
         B     XZERR                                                            
*                                                                               
XZPERERR MVC   ERRMSG(36),=C'**ERROR ** BUY NOT IN PCX EST PERIOD'              
         MVI   PRDERRSW,C'Y'       INDICATE FATAL ERROR                         
         B     XZERR                                                            
*                                                                               
XZPRDERR MVC   ERRMSG(35),=C'**ERROR ** MISSING PCX PRODUCT CODE'               
         MVI   PRDERRSW,C'Y'       INDICATE FATAL ERROR                         
*                                                                               
* NEED TO PUT ALPHA CODE IN MESSAGE                                             
*                                                                               
         L     RE,=A(AGCLTHDR)                                                  
         LA    RE,CLIST-CLTHDRD(RE)                                             
*                                                                               
XZPRERR2 CLC   0(1,R7),3(RE)       MATCH PRD NUMBERS                            
         BE    XZPRERR4                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   XZPRERR2                                                         
         ZIC   R0,0(R7)            ANYTHING IS BETTER THAN BLOWING UP           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL(3),DUB                                                      
         LA    RE,FULL                                                          
XZPRERR4 MVC   ERRMSG+38(3),0(RE)                                               
*                                                                               
         B     XZERR         ***** PRINT ERROR MESSAGES *****                   
*************************************************************                   
         LA    R4,BADPRDS          A(SPECIAL PRODUCTS)                          
XZPRERR6 CLC   0(3,R4),ERRMSG+38   TEST PRODUCT IS IN LIST                      
         BNE   *+14                NO                                           
         MVC   P,SPACES            YES - CLEAR PRINT LINE                       
         B     EXIT                DON'T PRINT ERROR MESSAGE                    
*                                                                               
         LA    R4,3(R4)            BUMP LIST POINTER                            
         CLI   0(R4),X'FF'         TEST END OF TABLE                            
         BE    XZERR               YES - PRINT ERROR MESSAGE                    
         B     XZPRERR6            NO - TRY NEXT TABLE ENTRY                    
         SPACE 3                                                                
BADPRDS  DS    0CL3                                                             
         DC    C'AG '                                                           
         DC    C'BG '                                                           
         DC    C'CFD'                                                           
         DC    C'CFP'                                                           
         DC    C'DW '                                                           
         DC    C'FLD'                                                           
         DC    C'FLY'                                                           
         DC    C'IHS'                                                           
         DC    C'MBP'                                                           
         DC    C'MI '                                                           
         DC    C'OGR'                                                           
         DC    C'TGL'                                                           
         DC    C'TLA'                                                           
         DC    C'TOS'                                                           
         DC    C'TSW'                                                           
         DC    X'FF'                                                            
         EJECT                                                                  
XZESTERR MVC   ERRMSG(37),=C'**ERROR ** NO PCX BRAND ESTIMATE PRD='             
         L     RE,ADCLT            POINT TO PCX CLTHDR                          
         LA    RE,CLIST-CLTHDR(RE)                                              
*                                                                               
XZESERR2 CLC   0(1,R7),3(RE)       MATCH PRD NUMBERS                            
         BE    XZESERR4                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   XZESERR2                                                         
         ZIC   R0,0(R1)            ANYTHING IS BETTER THAN BLOWING UP           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL(3),DUB                                                      
         LA    RE,FULL                                                          
XZESERR4 MVC   ERRMSG+38(3),0(RE)                                               
         B     XZERR                                                            
         EJECT                                                                  
XZERR    LA    R4,ERRKEY                                                        
*                                                                               
         CLC   SVBUYERR,BUYREC                                                  
         BNE   XZPRT                                                            
         MVC   ERRMSG,SPACES                                                    
         B     EXIT                                                             
*                                                                               
XZPRT    MVC   SVBUYERR,BUYREC                                                  
         MVC   0(3,R4),SVAGYA                                                   
         LA    R4,4(R4)                                                         
         GOTO1 CLUNPK,DMCB,BUYKEY+1,(R4)                                        
         LA    R4,4(R4)                                                         
         GOTO1 MSUNPK,DMCB,BUYKEY+4,(R4),5(R4)                                  
         LA    R4,11(R4)                                                        
         ZIC   R0,BUYKEY+9         EST                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R4),DUB                                                      
         MVI   3(R4),C'-'                                                       
         ZIC   R0,BUYREC+10        LINE                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R4),DUB                                                      
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
* PRINT TOTALS FOR AN AGENCY                                                    
         SPACE 1                                                                
XZAGYTOT NTR1                                                                   
         CLI   XFRAGY,0            TEST FIRST TIME                              
         BE    AGYTOTX                                                          
*                                                                               
         MVC   P(3),SVAGYA         AGENCY ALPHA                                 
         MVI   P+3,C'='                                                         
         ZIC   R0,XFRAGY                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+4(2),DUB                                                       
         OI    AGYCNT+3,X'0F'                                                   
         UNPK  P+10(7),AGYCNT                                                   
         MVC   P+18(7),=C'RECORDS'                                              
         GOTO1 REPORT                                                           
*                                                                               
AGYTOTX  IC    RE,XFRAGY           BUMP XFR AGENCY NUMBER                       
         LA    RE,1(RE)                                                         
         STC   RE,XFRAGY                                                        
         ZAP   AGYCNT,=P'0'        RESET AGENCY RECORD COUNTER                  
         B     EXIT                                                             
         EJECT                                                                  
*========================================================*                      
* SUBROUTINE TO CLEAR FROM R1 TO R0. RF IS DESTROYED.    *                      
*========================================================*                      
         SPACE 1                                                                
CLEAR    LA    RF,256                                                           
*                                                                               
CLEAR2   SR    R0,R1               GIVES LENGTH TO CLEAR                        
         CR    R0,RF                                                            
         BNH   CLEAR4                                                           
         XC    0(256,R1),0(R1)                                                  
         AR    R1,RF                                                            
         SR    R0,RF                                                            
         BZR   RE                                                               
         B     CLEAR2                                                           
CLEAR4   LR    RF,R0                                                            
         BCTR  RF,0                                                             
         EX    RF,CLEARXC                                                       
         BR    RE                                                               
CLEARXC  XC    0(0,R1),0(R1)  ** EXECUTED **                                    
         SPACE 2                                                                
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
*                                                                               
FIRSTEL  CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTELX  LTR   RE,RE               SET CC NEQ                                   
         BR    RE                                                               
         EJECT                                                                  
**************************************************                              
* ACCUMULATE ORDERED/PAID TOTALS FOR BUY RECORDS *                              
* R2 POINTS TO BUY RECORD                        *                              
* TOTIND = C'-' TO POST TO REMOVED               *                              
* TOTIND = C'+' TO POST TO ADDED                 *                              
**************************************************                              
         SPACE 1                                                                
XZTOT    NTR1                                                                   
*                                                                               
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         CLI   0(R2),X'FF'         TEST EOF                                     
         BE    XZTOT10                                                          
*                                                                               
         TM    15(R2),X'80'        TEST DELETED                                 
         BNZ   EXIT                                                             
*                                                                               
         CLI   0(R2),X'10'         TEST HEADER                                  
         BL    EXIT                IGNORE FOR NOW !                             
*                                                                               
         CLI   OLDCLT,0            TEST FIRST TIME                              
         BE    XZTOT2                                                           
*                                                                               
         CLC   OLDCLT,1(R2)        TEST SAME CLIENT                             
         BE    XZTOT4              YES                                          
         L     R5,NEXTBUF          NO - MOVE CLT TOT TO BUFFER                  
         MVC   0(1,R5),TOTIND      MOVE +/-                                     
         MVC   1(2,R5),OLDCLT                                                   
         MVC   4(24,R5),CTOTS                                                   
         LA    R5,28(R5)                                                        
         ST    R5,NEXTBUF                                                       
         MVI   0(R5),0             SET END OF BUFFER FLAG                       
*                                                                               
XZTOT2   LA    R1,CTOTS                                                         
         BAS   RE,XZCLR            CLEAR CTOTS                                  
*                                                                               
         MVC   OLDCLT,1(R2)        MOVE CLIENT CODE                             
*                                                                               
XZTOT4   SR    R4,R4               GROSS ORD                                    
         SR    R5,R5               GROSS PAID                                   
         SR    R6,R6               NET PAID                                     
         IC    R7,38(R2)           SAVE BDTIME BYTE                             
         MVI   38(R2),0            MAKE ZERO FOR GETRATE (PB'S)                 
*                                                                               
         L     RF,GETRATE                                                       
*                                                                               
         LA    R3,24(R2)           PROCESS BUYREC                               
         SR    R0,R0                                                            
XZTOT6   ICM   R0,1,1(R3)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    XZTOT8                                                           
         CLI   0(R3),6                                                          
         BL    XZTOT6                                                           
         CLI   0(R3),14                                                         
         BH    XZTOT6                                                           
         LA    R1,SPOTS                                                         
         STM   R1,R3,DMCB                                                       
         LA    R1,DMCB                                                          
         MVC   0(1,R1),3(R2)       MOVE PRD CODE                                
         BASR  RE,RF                                                            
*                                                                               
         A     R4,GROSS                                                         
         OC    4(2,R3),4(R3)       TEST PAID                                    
         BZ    XZTOT6                                                           
         A     R5,GROSS                                                         
         A     R6,NET                                                           
         B     XZTOT6                                                           
*                                                                               
XZTOT8   STC   R7,38(R2)           RESTORE BDTIME                               
         CVD   R4,DUB                                                           
         AP    CTOTS(8),DUB                                                     
         CVD   R5,DUB                                                           
         AP    CTOTS+8(8),DUB                                                   
         CVD   R6,DUB                                                           
         AP    CTOTS+16(8),DUB                                                  
         B     EXIT                                                             
*                                                                               
XZTOT10  L     R5,NEXTBUF          MOVE LAST CLT TOT TO BUFFER                  
         MVC   0(1,R5),TOTIND                                                   
         MVC   1(2,R5),OLDCLT                                                   
         MVC   4(24,R5),CTOTS                                                   
         MVI   28(R5),0            SET END OF BUFFER FLAG                       
         LA    R5,28(R5)           AND ADVANCE POINTER                          
         ST    R5,NEXTBUF                                                       
         XC    OLDCLT,OLDCLT       SET FIRST TIME FLAG                          
         B     EXIT                                                             
         EJECT                                                                  
* PRINT DOLLAR TOTALS FROM BUFFER *                                             
         SPACE 1                                                                
XZTOTPRT NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   P(21),=C'** DELETED RECORDS **'                                  
         GOTO1 REPORT                                                           
*                                                                               
         L     R5,=A(TOTBUFF)                                                   
         ST    R5,NEXTBUF                                                       
*                                                                               
         CLI   0(R5),C'-'                                                       
         BNE   XZTP8                                                            
*                                                                               
XZTP2    DS    0H                                                               
         GOTO1 CLUNPK,DMCB,1(R5),P+27                                           
*                                                                               
XZTP4    LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,4(R5)                                                         
         LA    R6,AGYTOTS                                                       
*                                                                               
XZTP6    AP    0(8,R6),0(8,R5)     BUMP AGY TOTALS                              
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XZTP6                                                         
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         CLI   0(R5),C'-'                                                       
         BE    XZTP2                                                            
         ST    R5,NEXTBUF          SAVE BUFFER POINTER                          
*                                                                               
         GOTO1 REPORT              SKIP A LINE                                  
         SPACE 1                                                                
* PRINT THE - TOTALS *                                                          
         SPACE 1                                                                
XZTP8    MVC   P+27(5),=C'TOTAL'                                                
         LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,AGYTOTS                                                       
         LA    R6,FILTOTS                                                       
*                                                                               
XZTP10   SP    0(8,R6),0(8,R5)     SUBTRACT FROM APCUMS                         
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XZTP10                                                        
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         LA    R1,AGYTOTS                                                       
         BAS   RE,XZCLR                                                         
         SPACE 2                                                                
* PRINT THE TOTALS OF INSERTED RECORDS *                                        
         SPACE 1                                                                
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         L     R5,NEXTBUF          RESTORE BUFFER POINTER                       
*                                                                               
         MVC   P(21),=C'** INSERTED RECORDS **'                                 
         GOTO1 REPORT                                                           
*                                                                               
         CLI   0(R5),0             TEST END OF BUFFER                           
         BE    XZTP20                                                           
*                                                                               
XZTP12   DS    0H                                                               
         GOTO1 CLUNPK,DMCB,1(R5),P+27                                           
*                                                                               
XZTP14   LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,4(R5)                                                         
         LA    R6,AGYTOTS                                                       
*                                                                               
XZTP16   AP    0(8,R6),0(8,R5)     BUMP AGY TOTALS                              
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XZTP16                                                        
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         CLI   0(R5),0                                                          
         BNE   XZTP12                                                           
*                                                                               
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
         MVC   P+27(5),=C'TOTAL'                                                
         LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,AGYTOTS                                                       
         LA    R6,FILTOTS                                                       
*                                                                               
XZTP18   AP    0(8,R6),0(8,R5)     BUMP FILE TOTALS                             
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XZTP18                                                        
*                                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
         LA    R1,AGYTOTS                                                       
         BAS   RE,XZCLR                                                         
         SPACE 1                                                                
XZTP20   DS    0H                                                               
         MVC   P(22),=C'* GRAND TOTALS (NET) *'                                 
*                                                                               
         LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,FILTOTS                                                       
*                                                                               
XZTP22   MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR PENNIES                                
         MVI   15(R3),C'+'         SET SIGN                                     
         CP    0(8,R5),=P'0'                                                    
         BNM   *+8                                                              
         MVI   15(R3),C'-'                                                      
         LA    R5,8(R5)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XZTP22                                                        
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
XZCLR    LA    R0,3                                                             
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
PC       DC    X'BC5F'            =PC                                           
PCX      DC    X'BC57'            =PCX                                          
TR       DC    X'CE3F'            =TR                                           
*                                                                               
NEXTBUF  DC    A(TOTBUFF)                                                       
APRDTAB  DC    A(PRDTAB)                                                        
APRDXX   DC    A(0)                                                             
TOTIND   DC    X'00'                                                            
OLDCLT   DC    XL2'00'                                                          
CTOTS    DS    0CL24                                                            
         DC    3PL8'0'             GROSS ORD/GROSS PAID/NET PAID                
AGYTOTS  DC    3PL8'0'                                                          
FILTOTS  DC    3PL8'0'                                                          
POLESDTS DS    F                                                                
SVPCAGY  DC    A(0)                                                             
BUYSTART DS    XL2                                                              
BUYEND   DS    XL2                                                              
SVAGYA   DS    CL3                                                              
SVAGYCLT DS    XL2                                                              
SVPCCLT  DS    XL2                                                              
SVAGYB   DS    XL1                                                              
XFRAGY   DC    X'00'                                                            
SVCMML   DS    CL10                                                             
CNDATA   DS    XL14                                                             
SVBUYERR DS    CL13                                                             
SVCMLERR DS    CL8                                                              
PRDERRSW DC    X'00'                                                            
ELCODE   DS    X                                                                
*                                                                               
STATBERR DC    H'0'                                                             
CMLTBERR DC    H'0'                                                             
*                                                                               
AGYCNT   DC    PL4'0'                                                           
*                                                                               
PCSTACNT DC    A(0)                                                             
PCCMLCNT DC    A(0)                                                             
SVCMLCNT DC    A(0)                                                             
PCGOLCNT DC    A(0)                                                             
MGREQCNT DC    A(0)                                                             
*                                                                               
CTRS     DS    0CL26                                                            
INCNT    DC    PL6'0',CL20'RECORDS IN'                                          
OUTCNT   DC    PL6'0',CL20'RECORDS OUT'                                         
DELCNT   DC    PL6'0',CL20'RECORDS DELETED'                                     
ADDCNT   DC    PL6'0',CL20'RECORDS INSERTED'                                    
CTRX     EQU   *-1                                                              
         EJECT                                                                  
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,MACRF=GM,               X        
               EODAD=XZ50                                                       
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,MACRF=PM                        
*                                                                               
         LTORG                                                                  
         DS    0D                                                               
         DC    C'**XZIO**'                                                      
XZIO     DS    4000C                                                            
*                                                                               
         DS    0D                                                               
         DC    C'*PRDTAB*'                                                      
PRDTAB   DS    XL256               USED TO CONVERT AGY/PC TO PC/PC              
*                                                                               
         DS    0D                                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'PCPOLTAB'                                                    
PCPOLTAB DS    256XL4                                                           
PCPOLTBX EQU   *-1                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'PCESTTAB'                                                    
PCESTTAB DS    210XL256                                                         
PCESTTBX EQU   *-1                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'AGCLTHDR'                                                    
AGCLTHDR DS    1500C               THIS AREA FOR AGY/PC CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'PCCMLTAB'                                                    
PCCMLTAB DS    40000C              4000 10 BYTE ENTRIES                         
PCCMLTBX EQU   *-1                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'SVCMLTAB'                                                    
SVCMLTAB DS    40000C                                                           
SVCMLTBX EQU   *-1                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'PCSTATAB'                                                    
PCSTATAB DS    12000C              2000 6 BYTE ENTRIES                          
PCSTATBX EQU   *-1                                                              
         DS    0D                                                               
         DC    CL8'MGREQTAB'                                                    
MGREQTAB DS    2000C               200 10 BYTE ENTRIES                          
MGREQTBX EQU   *-1                                                              
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'TOTBUFF'                                                     
TOTBUFF  DS    800D                                                             
         EJECT                                                                  
       ++INCLUDE DDCNTRL                                                        
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
STAMASTD DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENSTEQ                                                      
       ++INCLUDE SPTRCMML                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         PRINT ON                                                               
SPWORKD  DSECT                                                                  
         ORG   P                                                                
ERRKEY   DS    CL30                                                             
ERRMSG   DS    CL102                                                            
         ORG   P                                                                
ERRSTEQ  DS    CL132                                                            
         ORG   P                                                                
STEQMKT  DS    CL4                                                              
         DS    CL1                                                              
STEQSTA  DS    CL5                                                              
         DS    CL1                                                              
STEQACN  DS    CL5                                                              
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPREPXZ02 05/08/07'                                      
         END                                                                    
