*          DATA SET PPREPDA02  AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET PPREPXK02  AT LEVEL 006 AS OF 10/02/90                      
*PHASE PPXK02A,*                                                                
*INCLUDE SORTER                                                                 
*INCLUDE CONFID                                                                 
         SPACE 2                                                                
*===========================================================*                   
* THIS VERSION IS USED FOR DAILY  FILE TRANSFERS            *                   
*===========================================================*                   
         TITLE 'PPXK02 - DUPONT FILE DATA TRANSFER'                             
PPXK02   CSECT                                                                  
*          DATA SET PPREPL102  AT LEVEL 033 AS OF 02/05/90                      
********************************************************************            
         SPACE 3                                                                
PPXK02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPXK02,R8,RR=R9                                                
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     R7,PPWORK2C                                                      
         USING PPWORK2D,R7                                                      
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    XK10                                                             
         CLI   MODE,CLTFRST                                                     
         BE    XK30                                                             
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*===============================================================*               
* RUNFRST PROCESSING                                            *               
*===============================================================*               
         SPACE 1                                                                
XK10     DS    0H                                                               
*                                                                               
         OPEN  (FILEIN,(INPUT))                                                 
         OPEN  (FILEOUT,(OUTPUT))                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         B     EXIT                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,13,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2004'                                  
         EJECT                                                                  
*===============================================================*               
* CLTFRST PROCESSING                                            *               
*===============================================================*               
         SPACE 1                                                                
XK30     DS    0H                                                               
         MVI   TOTIND,C'-'         SET FLAG FOR DELETING RECORDS                
         SPACE 1                                                                
*==========================================================*                    
* READ XXX CLIENT HEADER ON DUPONT FILE                    *                    
* THE CLIENT NAME CONTAINS UP TO 6 AGENCY CODES AA,AA,AA,  *                    
* THE FIRST AGENCY IN THE NAME IS THE AGENCY RESPONSIBLE   *                    
* FOR MAINTAINING CONTRACTS. IF ADDITIONAL AGENCIES ARE    *                    
* NEEDED, USE ADDRESS. 10 ADDITIONAL AGENCIES.                                  
*==========================================================*                    
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,2                   RECORD CODE                            
         MVC   KEY+4(3),=C'XXX'          DUMMY CLIENT                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AREC,=A(CCXXX)                                                   
         GOTO1 GETPRT                                                           
*                                                                               
         BAS   RE,XKBLDEST         BUILD CC ESTIMATE LIST                       
*                                                                               
         BAS   RE,XKBLDSTA         BUILD CC MKT/STA LIST                        
*                                                                               
         BAS   RE,XKBLDSEQ         BUILD CC ACN # LIST                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         BAS   RE,XKBLDCML         BUILD CC COMMERCIAL LIST                     
*                                                                               
         MVC   CCCMLCNT,SVCMLCNT   MOVE TABLE TO SAVE AREA                      
         L     R0,SVCMLCNT                                                      
         L     R1,=A(SVCMLTAB)                                                  
         L     RF,=A(CCCMLTAB)                                                  
*                                                                               
XK32     MVC   0(10,RF),0(R1)                                                   
         LA    R1,10(R1)                                                        
         LA    RF,10(RF)                                                        
         BCT   R0,XK32                                                          
*                                                                               
XK34     DS    0H                                                               
         L     R0,ADBUY                                                         
         SH    R0,=H'4'                                                         
         GET   FILEIN,(R0)                                                      
         AP    INCNT,=P'1'                                                      
*                                                                               
XK34A    L     RE,ADBUY                                                         
         SH    RE,=H'4'            POINT TO RECLEN                              
         AH    RE,0(RE)            POINT TO END OF REC                          
         XC    0(2,RE),0(RE)       CLEAR END OF RECORD                          
*                                                                               
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         CLC   0(1,R2),BAGYMD      TEST AGENCY CC BUY                           
         BE    XK35                YES                                          
         CLC   =X'FFFFFFFF',0(R2)  TEST FILE TRAILER REC                        
         BE    XK34                YES - SKIP                                   
         L     R0,ADBUY                                                         
         SH    R0,=H'4'                                                         
         PUT   FILEOUT,(R0)        PUT ALL NON-CC RECS TO OUTPUT                
         AP    OUTCNT,=P'1'                                                     
         B     XK34                AND CONTINUE                                 
         SPACE 1                                                                
*=================================================================*             
* THE FOLLOWING TEST DELETES ALL BUY RECORDS UNDER CLIENT CODE CC *             
* AND ALL BUYS COPIED FROM OTHER SPTFILES                         *             
*=================================================================*             
         SPACE 1                                                                
XK35     DS    0H                                                               
         CLI   BDXFRAGY,0          TEST A COPIED RECORD                         
         BE    XK36                NO - KEEP IT                                 
         BAS   RE,XKTOT            EXTRACT DOLLAR VALUE                         
         AP    DELCNT,=P'1'        BUMP DELETE COUNTER                          
         B     XK34                AND READ NEXT RECORD                         
*                                                                               
XK36     DS    0H                                                               
         L     R0,ADBUY                                                         
         SH    R0,=H'4'                                                         
         PUT   FILEOUT,(R0)        PUT ALL NON-CC BUYS TO OUTPUT                
         AP    OUTCNT,=P'1'                                                     
*                                                                               
         L     R5,=A(SVCLTHDR)     POINT TO CURRENT CLTHDR                      
         CLC   1(3,R8),0(R2)       SAME A-M/CLT                                 
         BE    XK38                                                             
*                                                                               
         BAS   RE,XKAGYTOT         PRINT AGENCY TOTALS                          
         EJECT                                                                  
*====================================================*                          
* SET UP FOR NEW CLIENT                              *                          
*====================================================*                          
         SPACE 1                                                                
         MVC   KEY(13),0(R2)             SAVE BUY KEY                           
         GOTO1 CLUNPK,DMCB,1(R2),SVAGYA  SAVE ALPHA CLT FROM BUY KEY            
*                                                                               
         L     RE,=A(CCXXX)                                                     
         LA    RE,CLIST-CLTHDRD(RE)                                             
*                                                                               
XK37A    CLC   0(3,RE),SVAGYA            MATCH ALPHA CODE                       
         BE    XK37B                                                            
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   XK37A                                                            
         MVC   ERRMSG(23),=C'UNKNOWN CCUSA AGENCY = '                           
         MVC   ERRMSG+23(3),SVAGYA                                              
         GOTO1 REPORT                                                           
         MVI   XFRAGY,0            SET THE XFR CODE TO 0                        
         B     XK37X               AND BY ALL MEANS CONTINUE                    
*                                                                               
XK37B    MVC   XFRAGY,3(RE)        SAVE XFR AGY CODE                            
*                                                                               
XK37X    XC    KEY,KEY             READ NEW CLIENT HEADER                       
         MVC   KEY+1(3),0(R2)      MOVE A-M/CLT                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R8,=A(SVCLTHDR)                                                  
         ST    R8,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         BAS   RE,XKBLDPRD         BUILD PRODUCT TRANSLATE TABLE                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),1(R8)      MOVE A-M/CLT                                 
         BAS   RE,XKBLDCML         BUILD CMML LIST                              
*                                                                               
XK38     CLC   BUYREC+9(1),SVPROF12 TEST TO INCLUDE EST IN XFR                  
         BH    XK34                 NO                                          
*                                                                               
         TM    BUYREC+15,X'80'     TEST BUY DELETED                             
         BO    XK34                                                             
*                                                                               
         BAS   RE,XKEST             TEST ESTIMATE VALID                         
         BNE   XK34                                                             
*                                                                               
         BAS   RE,SEACNMTH         THIS INSTRUCTION ONLY HERE BECAUSE           
*                                  CCUSA FILE CLTHDRS WERE SET UP               
*                                  INCORRECTLY - SO FIX MKT NUMBERS NOW         
*                                                                               
         MVC   1(2,R2),=X'885F'    SET CLIENT = CC                              
         MVC   BDXFRAGY,XFRAGY     SET 'COPIED BUY' IND                         
         AP    AGYCNT,=P'1'        ADD TO COPIED COUNTER                        
*                                                                               
         MVI   PRDERRSW,0          RESET ERROR SWITCH                           
         MVC   APRDTAB,=A(PRDTAB1) SET PRDTAB ADDRESS                           
         BAS   RE,XKPRD            TRANSLATE PRD CODES                          
*                                                                               
         CLI   PRDERRSW,C'Y'       TEST FATAL ERROR                             
         BE    XK34                                                             
*                                                                               
         BAS   RE,XKCML            TRANSLATE CMML CODES                         
*                                                                               
XK40     DS    0H                                                               
         L     R0,ADBUY                                                         
         SH    R0,=H'4'            POINT TO LEN                                 
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R0)                                     
         B     XK34                                                             
         EJECT                                                                  
*============================================================*                  
* NOW PROCESS DATA FROM OTHER SPTFILES                       *                  
* EACH BUY RECORD WILL GENERATE TWO RECORDS ON THE CC FILE   *                  
* ONE UNDER CLIENT CC AND ONE UNDER THE CLIENT=AGENCY        *                  
*============================================================*                  
         SPACE 1                                                                
XK50     DS    0H                                                               
         BAS   RE,XKAGYTOT         PRINT AGENCY TOTALS                          
         CLOSE FILEIN               AND CLOSE INPUT TAPE                        
*                                                                               
         L     R2,ADBUY                                                         
         MVI   0(R2),X'FF'         SET EOF REC                                  
         BAS   RE,XKTOT            FORCE LAST - CLT TO BUFFER                   
         SPACE 1                                                                
*==========================================================*                    
* MUST READ CLIENT HEADERS FOR DDS CCCUSA AGENCIES NOW     *                    
* WHILE STILL ON THE CCUSA SPTFILE AND SAVE THEM           *                    
*==========================================================*                    
         SPACE 1                                                                
XK51     XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         LA    R4,CCAGYLST                                                      
*                                                                               
XK51A    DS    0H                                                               
         GOTO1 CLPACK,DMCB,2(R4),KEY+2                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AREC,5(R4)          SET SAVE AREA ADDRESS                        
         GOTO1 GETPRT                                                           
*                                                                               
         LA    R4,L'CCAGYLST(R4)                                                
         CLI   0(R4),X'FF'                                                      
         BNE   XK51A                                                            
*                                                                               
         LA    RE,CCAGYLST                                                      
         B     XK62                                                             
         EJECT                                                                  
XK52     DS    0H                  CLOSE PREVIOUS SPOT SYSTEM                   
         GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'SPOT'                                 
*                                                                               
         XC    CNDATA,CNDATA                                                    
         LA    R4,CNDATA                                                        
         USING CND,R4                                                           
         MVC   CNAGY(2),SVAGYA     MOVE ALPHA AGENCY CODE                       
*                                                                               
         GOTO1 =V(CONFID),DMCB,(R4),(1,FULL)                                    
         OC    FULL,FULL           TEST ID FOUND                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,UTL                                                           
         MVC   4(1,RE),CNSSE       MOVE SPOT SYSTEM NUMBER                      
         MVC   SVAGYB,CNSCD        AND SAVE SPOT AGENCY NUMBER                  
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',FLIST,ADBUY                     
*                                                                               
         XC    KEY,KEY                                                          
         IC    R0,SVAGYB                                                        
         STC   R0,KEY+1            SET AGENCY IN KEY (LEFT ALIGN)               
         OI    KEY+1,X'01'         SET MEDIA = SPOT TV                          
         MVC   KEY+2(2),=X'885F'                                                
         GOTO1 HIGH                READ CC CLTHDR                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R8,=A(SVCLTHDR)                                                  
         ST    R8,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         BAS   RE,XKBLDPRD         BUILD PRODUCT TRANSLATE TABLE                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),1(R8)      A-M/CLT                                      
         BAS   RE,XKBLDCML         BUILD COMMERCIAL TRANSLATE TABLE             
         EJECT                                                                  
* READ ALL CLIENT CC POL BUY RECORDS *                                          
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(3),1(R8)        SET A-M/CLT                                  
         MVI   KEY+3,X'FF'         SET PRD=POL                                  
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BE    XK56                                                             
         B     XK60                SKIP IF NO BUYS                              
*                                                                               
XK54     GOTO1 SEQ                                                              
*                                                                               
XK56     CLC   KEY(3),KEYSAVE      SAME A-M/CLT                                 
         BNE   XK60                                                             
*                                                                               
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         ST    R2,AREC                                                          
         GOTO1 GETPRT                                                           
         CLC   KEY+4(2),BUYREC+4   TEST SPILL POINTER                           
         BNE   XK54                                                             
*                                                                               
         CLC   BUYREC+9(1),SVPROF12 TEST TO INCLUDE EST IN XFR                  
         BH    XK54                 NO                                          
         BAS   RE,XKEST             VALIDATE POL EST OPEN                       
         BNE   XK54                                                             
*                                                                               
         BAS   RE,XKMKT            TRANSLATE MKT NUMBER                         
*                                                                               
         BAS   RE,SEACNMTH         CHECK FOR ACN # MATCH                        
*                                                                               
         MVI   PRDERRSW,0          RESET FATAL ERROR SWITCH                     
         MVC   APRDTAB,=A(PRDTAB1) SET PRDTAB TABLE ADDRESS                     
         BAS   RE,XKPRD            TRANSLATE PRD CODES                          
         CLI   PRDERRSW,C'Y'       TEST ERROR                                   
         BE    XK54                YES - SKIP RECORD                            
         BAS   RE,XKCML            TRANSLATE CMML CODES                         
*                                                                               
         MVC   BDXFRAGY,XFRAGY     SET TRANSFER FLAG IN BUYREC                  
         MVC   BUYALPHA,=C'CC'     AGYALPHA BECOMES CC                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,13(R2)         GET REC LENGTH                               
         AH    R0,=H'4'                                                         
         L     R4,ADBUY                                                         
         SH    R4,=H'4'                                                         
         SLL   R0,16               LEFT ALIGN                                   
         ST    R0,0(R4)            SET REC LENGTH FOR SORT                      
         EJECT                                                                  
*======================================================*                        
* GENERATE RECORD FOR CLIENT CC                        *                        
*======================================================*                        
         SPACE 1                                                                
         MVC   0(1,R2),BAGYMD      MOVE CC AGY/MED TO REC                       
         MVC   1(2,R2),=X'885F'    SET CLIENT = CC                              
*                                                                               
         AP    AGYCNT,=P'1'                                                     
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)    AND PUT RECORD 1                 
         SPACE 1                                                                
*======================================================*                        
* NOW GENERATE RECORD FOR CC/AGY                       *                        
*======================================================*                        
         SPACE 1                                                                
         MVC   APRDTAB,=A(PRDTAB2) SET PRDTAB ADDRESS                           
         MVI   APRDTAB,X'FF'       SET FOR NO ERRORS                            
         BAS   RE,XKPRD            AND TRANSLATE PRD CODES                      
         MVC   1(2,R2),SVAGYCLT    MOVE CORRESPONDING AGY-CLT CODE              
*                                                                               
         AP    AGYCNT,=P'1'                                                     
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)                                     
         CLI   QOPT1,C'Y'          SEE IF TEST RUN                              
         BNE   XK54                                                             
         CP    AGYCNT,=P'100'                                                   
         BNH   XK54                                                             
*                                                                               
XK60     DS    0H                                                               
         BAS   RE,XKAGYTOT                                                      
*                                                                               
         L     RE,SVCCAGY                                                       
         LA    RE,L'CCAGYLST(RE)   NEXT ENTRY                                   
*                                                                               
XK62     CLI   0(RE),X'FF'         TEST EOL                                     
         BE    XK70                                                             
         ST    RE,SVCCAGY                                                       
         MVC   SVAGYA,0(RE)        SET NEW AGYALPHA                             
         MVI   SVAGYA+2,C' '                                                    
         MVC   WORK(3),2(RE)       MOVE EBCDIC CLIENT CODE                      
         GOTO1 CLPACK,DMCB,WORK,SVAGYCLT                                        
* NEED TO LOOK UP XFR AGENCY NUMBER                                             
         L     RE,=A(CCXXX)                                                     
         LA    RE,CLIST-CLTHDR(RE)                                              
*                                                                               
XK64     CLC   WORK(3),0(RE)                                                    
         BE    XK66                                                             
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   XK64                                                             
         DC    H'0'                                                             
*                                                                               
XK66     MVC   XFRAGY,3(RE)                                                     
         B     XK52                                                             
         EJECT                                                                  
* ENTRIES ARE ALPHA AGENCY CODE       (2)                                       
*             CCUSAS FILE CLIENT CODE (3)                                       
*             CLTHDR SAVE AREA ADDRESS(4)                                       
*                                                                               
CCAGYLST DS    0CL9                                                             
         DC    C'PMPM ',AL4(CCPM)    PRO-MEDIA (PMWA)                           
         DC    C'BJBJM',AL4(CCBJM)   BOZELLE/JACOBS (BJMN)                      
         DC    C'APAPA',AL4(CCAPA)   APPLE (APNY)                               
         DC    C'LILAI',AL4(CCLAI)   LIBERTY ADVTSG (LIPA)                      
         DC    C'RRRR ',AL4(CCRR)    ROSS ROY (ROSS)                            
         DC    C'MCMC ',AL4(CCMC)    MCANN ERICKSON (MCNY)                      
         DC    C'WVWV ',AL4(CCWV)    WAVE ADVTSG (WVME)                         
         DC    X'FF'                 EOL FLAG                                   
*                                                                               
FLIST    DC    CL8' SPTFILE'                                                    
         DC    CL8' SPTDIR '                                                    
         DC    CL8' DEMDIRN'                                                    
         DC    CL8' DEMFILN'                                                    
         DC    CL8' CTFILE '                                                    
         DC    CL8'X       '                                                    
         EJECT                                                                  
*=====================================================================*         
* AGENCY FILES HAVE BEEN PROCESSED - SORT RECORDS AND WRITE TO TAPE   *         
*=====================================================================*         
         SPACE 1                                                                
XK70     MVI   TOTIND,C'+'         INDICATE ADDING RECORDS                      
*                                                                               
XK72     DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,4(R1)                                                      
         BZ    XK80                                                             
         LR    R0,R2                                                            
         PUT   FILEOUT,(R0)                                                     
         AP    OUTCNT,=P'1'                                                     
         AP    ADDCNT,=P'1'                                                     
* MOVE RECORD *                                                                 
         L     R1,ADBUY                                                         
         SH    R1,=H'4'                                                         
         LH    RE,0(R2)            GET LENGTH                                   
XK74     CH    RE,=H'256'                                                       
         BNH   XK76                                                             
         MVC   0(256,R1),0(R2)                                                  
         LA    R1,256(R1)                                                       
         LA    R2,256(R2)                                                       
         SH    RE,=H'256'                                                       
         B     XK74                                                             
*                                                                               
XK76     BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R2)  *EXECUTED*                                        
*                                                                               
         LA    R1,1(RE,R1)         POINT TO END OF REC                          
         XC    0(2,R1),0(R1)       CLEAR NEXT ELEM CODE/LENGTH                  
*                                                                               
         BAS   RE,XKTOT                                                         
         B     XK72                                                             
*                                                                               
XK80     DS    0H                  PRINT FINAL TOTALS                           
         CLOSE FILEOUT                                                          
*                                                                               
         L     R2,ADBUY                                                         
         MVI   0(R2),X'FF'         PASS EOF TO TOT ROUTINES                     
         BAS   RE,XKTOT                                                         
*                                                                               
         BAS   RE,XKTOTPRT                                                      
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
XK82     LA    R4,CTRS                                                          
*                                                                               
XK84     MVC   P(20),4(R4)                                                      
         EDIT  (P4,0(R4)),(8,P+22)                                              
         GOTO1 REPORT                                                           
         LA    R4,L'CTRS(R4)                                                    
         LA    R0,CTRX                                                          
         CR    R4,R0                                                            
         BL    XK84                                                             
         B     EXIT                                                             
         EJECT                                                                  
*============================================================*                  
* BUILD TABLE OF ALL CLIENT CC ESTIMATE HEADERS              *                  
* CCESTTAB HAS NON-ZERO VALUE FOR EACH OPEN ESTIMATE         *                  
* CCPOLTAB HAS 4 BYTE PACKED DATE OF POL ESTIMATE            *                  
*============================================================*                  
         SPACE 1                                                                
XKBLDEST NTR1                                                                   
*                                                                               
         L     R1,=A(CCESTTAB)                                                  
         L     R0,=A(CCESTTBX)                                                  
         BAS   RE,CLEAR                                                         
*                                                                               
         L     R4,=A(CCESTTAB)                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+4(3),QCLIENT                                                 
         GOTO1 HIGH                SKIP CLTHDR                                  
         B     BLDEST4                                                          
*                                                                               
BLDEST2  GOTO1 SEQ                                                              
*                                                                               
BLDEST4  CLC   KEY(7),KEYSAVE      SAME AGM REC CLI                             
         BNE   EXIT                                                             
*                                                                               
*==============================================*                                
* READ ESTHDR TO GET DATES                     *                                
*==============================================*                                
         SPACE 1                                                                
BLDEST10 MVC   AREC,ADEST                                                       
         GOTO1 GETPRT                                                           
*                                                                               
         L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
         MVC   0(3,R4),PESTKPRD         PRODUCT                                 
         MVC   3(2,R4),PESTKEST                                                 
         GOTO1 DATCON,DMCB,PESTST,(3,5(R4))                                     
         GOTO1 (RF),(R1),PESTEND,(3,8(R4))                                      
         LA    R4,11(R4)                                                        
         B     BLDEST2                                                          
         DROP  R6                                                               
         EJECT                                                                  
*=======================================*                                       
* BUILD A TABLE OF 6 BYTE PUB CODES     *                                       
*=======================================*                                       
         SPACE 1                                                                
XKBLDSTA NTR1                                                                   
         L     R4,=A(CCSTATAB)                                                  
         SR    R5,R5               CLEAR COUNTER                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=C'ST'                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PUBMAS',KEY,ADSTAT                    
*                                                                               
         L     R6,ADSTAT                                                        
         USING STAMASTD,R6                                                      
         B     BLDSTA4                                                          
*                                                                               
BLDSTA2  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'STATION'                              
*                                                                               
BLDSTA4  CLC   =C'ST',0(R6)        STILL IN TV MASTER RECORDS                   
         BNE   BLDSTA10                                                         
         CLC   =C'CC',7(R6)        TEST AGENCY CC                               
         BNE   BLDSTA2                                                          
         L     R0,=A(CCSTATBX)                                                  
         CR    R4,R0               TEST PAST END OF TABLE                       
         BNL   STATBERR                                                         
*                                                                               
         GOTO1 MSPACK,DMCB,SMKT,2(R6),(R4)                                      
*                                                                               
         MVI   5(R4),C'A'          SET AFFILIATE IND                            
         CLC   =C'ABC',SNETWRK                                                  
         BE    BLDSTA6                                                          
         CLC   =C'CBS',SNETWRK                                                  
         BE    BLDSTA6                                                          
         CLC   =C'NBC',SNETWRK                                                  
         BE    BLDSTA6                                                          
         MVI   5(R4),C'I'          SET INDEPENDENT IND                          
*                                                                               
BLDSTA6  LA    R4,6(R4)                                                         
         BCTR  R5,0                BUMP COUNTER                                 
         B     BLDSTA2                                                          
*                                                                               
BLDSTA10 LPR   R5,R5                                                            
         ST    R5,CCSTACNT         SAVE STATION COUNT                           
         SPACE 1                                                                
* THANKS TO NEW MSPACK WE NOW HAVE TO SORT THE FUCKING TABLE *                  
         SPACE 1                                                                
         GOTO1 XSORT,DMCB,A(CCSTATAB),(R5),6,3,2                                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*===================================================================*           
* READ AND SAVE ALL COMMERCIALS AND SEQ NUMBERS FOR CLIENT IN 'KEY' *           
*===================================================================*           
         SPACE 1                                                                
XKBLDCML NTR1                                                                   
         L     R1,=A(SVCMLTAB)                                                  
         L     R0,=A(SVCMLTBX)                                                  
         BAS   RE,CLEAR                                                         
*                                                                               
         L     R4,=A(SVCMLTAB)                                                  
         SR    R5,R5               CLEAR COUNTER                                
*                                                                               
         GOTO1 HIGH                                                             
         B     BLDCML4                                                          
*                                                                               
BLDCML2  GOTO1 SEQ                                                              
*                                                                               
BLDCML4  CLC   KEY(5),KEYSAVE      0A21/A-M/CLT                                 
         BNE   BLDCML10                                                         
*                                                                               
         L     R6,=A(XKIO)                                                      
         ST    R6,AREC                                                          
         GOTO1 GETPRT                                                           
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
*========================================================*                      
* SET UP EQUIVALENCE TABLE BETWEEN AGY/CC AND CC/CC      *                      
*========================================================*                      
         SPACE 1                                                                
XKBLDPRD NTR1                                                                   
         L     R4,=A(SVCLTHDR)     POINT TO NEW CLTHDR                          
         LA    R4,CLIST-CLTHDRD(R4)                                             
         L     RF,=A(PRDTAB1)                                                   
         XC    0(256,RF),0(RF)                                                  
*                                                                               
BLDPRD2  L     R5,ADCLT            POINT TO CC CLTHDR                           
         LA    R5,CLIST-CLTHDRD(R5)                                             
*                                                                               
BLDPRD4  CLC   0(3,R4),0(R5)       MATCH ALPHA PRD CODE                         
         BE    BLDPRD6                                                          
         LA    R5,4(R5)                                                         
         CLI   0(R5),C'A'                                                       
         BNL   BLDPRD4                                                          
         B     BLDPRD10            IGNORE PRD NOT FOUND                         
*                                                                               
BLDPRD6  ZIC   RE,3(R4)            GET PRD NUM                                  
         AR    RE,RF               POINT TO PROPER SLOT                         
         MVC   0(1,RE),3(R5)       MOVE CC PRD CODE TO SLOT                     
*                                                                               
BLDPRD10 LA    R4,4(R4)                                                         
         CLI   0(R4),C'A'                                                       
         BNL   BLDPRD2                                                          
         EJECT                                                                  
*=============================================================*                 
* NOW SET UP EQUIVALENCES TO TRANSLATE CC/CC TO CC/AGY        *                 
*=============================================================*                 
         SPACE 1                                                                
         ICM   RE,15,SVCCAGY       POINT TO DDS CC AGY LIST                     
         BZ    EXIT                NOT SET WHEN PROCESSING TAPE                 
         ICM   RE,15,5(RE)         GET CLTHDR SAVE AREA ADDRESS                 
         LA    RE,CLIST-CLTHDRD(RE)                                             
         ST    RE,FULL                                                          
*                                                                               
         L     RF,=A(PRDTAB2)      CLEAR THE TABLE                              
         XC    0(256,RF),0(RF)                                                  
*                                                                               
         L     R4,ADCLT            POINT TO CC CLTHDR                           
         LA    R4,CLIST-CLTHDRD(R4)                                             
*                                                                               
BLDPRD12 L     R5,FULL             POINT TO CC/AGY PRDLIST                      
*                                                                               
BLDPRD14 CLC   0(3,R4),0(R5)       MATCH ALPHA PRD CODE                         
         BE    BLDPRD16                                                         
         LA    R5,4(R5)                                                         
         CLI   0(R5),C'A'                                                       
         BNL   BLDPRD14                                                         
         B     BLDPRD20            IGNORE PRD NOT FOUND                         
*                                                                               
BLDPRD16 ZIC   RE,3(R4)            GET PRD NUM                                  
         AR    RE,RF               POINT TO PROPER SLOT                         
         MVC   0(1,RE),3(R5)       MOVE CC PRD CODE TO SLOT                     
*                                                                               
BLDPRD20 LA    R4,4(R4)                                                         
         CLI   0(R4),C'A'                                                       
         BNL   BLDPRD12                                                         
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
*===================================================================*           
* BUILD A TABLE OF STATION EQUIVALENCE RECORDS CLI/STA/ACN#/MKT     *           
*===================================================================*           
         SPACE 1                                                                
XKBLDSEQ NTR1                                                                   
         L     R1,=A(CCSTEQTB)                                                  
         L     R0,=A(CCSTEQBX)                                                  
         BAS   RE,CLEAR                                                         
         MVI   NOSTAEQU,1          = NO RECORDS DO NO CHECKS                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D44'     STA EQU ID                                   
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),=X'885F'                                                
         LA    R4,CCSTEQNB         # OF ENTRIES                                 
         L     R3,=A(CCSTEQTB)                                                  
         GOTO1 HIGH                                                             
         B     BLDSE120                                                         
*                                                                               
BLDSE100 GOTO1 SEQ                                                              
*                                                                               
BLDSE120 CLC   KEY(5),KEYSAVE                                                   
         BNE   EXIT                                                             
         LTR   R4,R4                                                            
         BZ    BLDSEERR                                                         
*                                                                               
         L     R6,ADBUY            READ REC INTO BUY AREA                       
         ST    R6,AREC                                                          
         USING STERECD,R6                                                       
         GOTO1 GETPRT                                                           
         MVC   WORK(4),=4C'0'      NO MARKET 00 BLOW UP                         
         MVC   WORK+4(5),STEKSTA                                                
         GOTO1 MSPACK,DMCB,WORK,WORK+4,WORK+9                                   
         SPACE                                                                  
* READ AND STORE ALL 2 ELEMENTS                                                 
         LA    R6,24(R6)                                                        
         MVI   ELCODE,2                                                         
         BAS   RE,FIRSTEL                                                       
         B     *+8                                                              
BLDSE200 BAS   RE,NEXTEL                                                        
         BNE   BLDSE100                                                         
*                                                                               
         MVC   0(3,R3),WORK+11     STORE PACKED STA                             
         USING STEEL02,R6                                                       
         MVC   3(7,R3),STEACN      ACN#/MKT#                                    
         MVI   NOSTAEQU,0          HAVE A STA ACN #                             
         LA    R3,10(R3)                                                        
         BCTR  R4,0                                                             
         B     BLDSE200                                                         
         DROP  R6                                                               
         SPACE                                                                  
* IF TABLE EXCEEDED COME HERE                                                   
BLDSEERR MVC   ERRSTEQ(35),=C'** ERROR ** STA EQU TABLE TOO SMALL'              
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*==========================================================*                    
* LOOK FOR AN ACN # MATCH-IF FOUND CHANGE MARKET           *                    
*==========================================================*                    
         SPACE 1                                                                
SEACNMTH NTR1                                                                   
         CLI   NOSTAEQU,1          ANY ACN # ENTRED                             
         BE    EXIT                                                             
*                                                                               
         L     R6,ADBUY                                                         
         LR    R2,R6                                                            
         LA    R6,24(R6)                                                        
         MVI   ELCODE,X'70'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   EXIT                                                             
         L     R3,=A(CCSTEQTB)                                                  
         LA    R4,CCSTEQNB         # OF ENTRIES                                 
*                                                                               
ACNMT100 CLC   0(3,R3),6(R2)       STATION MATCH                                
         BNE   ACNMT120                                                         
         CLC   3(3,R3),3(R6)       ACN # MATCH                                  
         BE    ACNMT200                                                         
*                                                                               
ACNMT120 LA    R3,10(R3)           NEXT ENTRY                                   
         CLI   0(R3),0             END OF TABLE                                 
         BE    EXIT                                                             
         BCT   R4,ACNMT100                                                      
         B     EXIT                                                             
*                                                                               
ACNMT200 MVC   4(2,R2),8(R3)       IF EQUAL CHANGE MKT                          
         B     EXIT                                                             
         EJECT                                                                  
*==================================================================*            
* TRANSLATE ALL PRODUCT CODES IN BUYREC TO CC PRD CODES            *            
* APRDTAB CONTAINS APPROPRIATE PRDTAB ADDRESS                      *            
*==================================================================*            
         SPACE 1                                                                
XKPRD    NTR1                                                                   
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         LA    R1,BDMASPRD                                                      
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         BAS   RE,FINDPRD                                                       
         LA    R1,1(R1)                                                         
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         BAS   RE,FINDPRD                                                       
*                                                                               
         SR    R0,R0                                                            
         LA    R6,BDELEM                                                        
XKPRD2   ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    EXIT                                                             
         CLI   0(R6),6                                                          
         BL    XKPRD2                                                           
         CLI   0(R6),13                                                         
         BH    XKPRD2                                                           
*                                                                               
         CLC   2(2,R6),POLESDTS    TEST ELEM PRIOR TO CC EST START              
         BL    XKPERERR                                                         
         CLC   2(2,R6),POLESDTS+2    OR AFTER CC EST END                        
         BH    XKPERERR                                                         
*                                                                               
         ZIC   R0,1(R6)                                                         
         SH    R0,=H'10'                                                        
         BNP   XKPRD2                                                           
         SRL   R0,2                SET FOR BCT                                  
         LA    R1,10(R6)                                                        
*                                                                               
XKPRD4   BAS   RE,FINDPRD                                                       
*                                                                               
         CLI   APRDTAB,0           TEST TO CHECK FOR ERRORS                     
         BNE   XKPRD6              NO                                           
         SLL   RF,8                PRD CODE X 256                               
         ZIC   RE,BUYREC+9         ESTIMATE NUMBER                              
         AR    RF,RE                                                            
         A     RF,=A(CCESTTAB)     POINT TO EST TAB ENTRY                       
         CLI   0(R1),0             TEST CC EST OPEN                             
         BE    XKESTERR                                                         
XKPRD6   LA    R1,4(R1)            NEXT PRD CODE                                
         BCT   R0,XKPRD4                                                        
         B     XKPRD2                                                           
         EJECT                                                                  
FINDPRD  ZIC   RF,0(R1)            GET PRD CODE                                 
         A     RF,APRDTAB                                                       
         CLI   0(RF),0                                                          
         BE    FINDPRD2                                                         
         MVC   0(1,R1),0(RF)                                                    
         BR    RE                                                               
*                                                                               
FINDPRD2 NTR1 ,                     SET UP TO 'ENTER' ERROR ROUTINES            
         B     XKPRDERR                                                         
         SPACE 2                                                                
* TEST AGENCY ESTIMATE NUMBER IS VALID ON CLIENT CC                             
         SPACE 1                                                                
XKEST    NTR1                                                                   
         ZIC   RE,BUYREC+9                                                      
         SLL   RE,2                X 4                                          
         A     RE,=A(CCPOLTAB)                                                  
         MVC   POLESDTS,0(RE)      MOVE TO SAVE AREA                            
         OC    0(4,RE),0(RE)       TEST POL EST OPEN                            
         BNZ   EQXIT                                                            
         B     NEQXIT                                                           
         SPACE 2                                                                
*====================================================================*          
* SEARCH CC MKT-STA LIST FOR CORRECT MARKET NUMBER                   *          
* REMEMBER THAT TABLE IS IN STATION ORDER BUT ENTRIES ARE MKT/STA    *          
*====================================================================*          
         SPACE 1                                                                
XKMKT    NTR1                                                                   
         LA    R4,BUYREC+4         POINT TO MARKET CODE                         
         GOTO1 BINSRCH,DMCB,(R4),A(CCSTATAB),CCSTACNT,6,(2,3),CCSTACNT          
         CLI   0(R1),1             TEST MARKET NOT FOUND                        
         BE    MKTERR                                                           
         L     RE,0(R1)            GET ENTRY ADDRESS                            
         MVC   BUYREC+4(2),0(RE)   MOVE NEW MARKET NUMBER                       
         MVC   LASTAFF,5(RE)       SET NEW AFFILIATE IND                        
         SPACE 1                                                                
* OVERWRITE SPILL DEMO ELEMENT CODES *                                          
         SPACE 1                                                                
         LA    R6,BUYREC+24                                                     
         MVI   ELCODE,3                                                         
XKMKT2   BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         OI    0(R6),X'80'         SET ELCODE = X'83'                           
         B     XKMKT2                                                           
         EJECT                                                                  
*==============================================================*                
* TRANSLATE BUYREC CMML CODES TO CC CMML CODES                 *                
*==============================================================*                
         SPACE 1                                                                
XKCML    NTR1                                                                   
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
XKCML2   ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    EXIT                                                             
         CLI   0(R6),X'12'         TEST FILM ELEMENT                            
         BNE   XKCML2                                                           
*                                                                               
         ZIC   R0,1(R6)                                                         
         SH    R0,=H'3'                                                         
         SRL   R0,1                DIVIDE BY 2                                  
         LA    R1,3(R6)                                                         
*                                                                               
XKCML4   BAS   RE,GETCML                                                        
         BNE   CMLERR1                                                          
         BAS   RE,GETCMLCC                                                      
         LA    R1,2(R1)                                                         
         BCT   R0,XKCML4                                                        
         B     XKCML2                                                           
*                                                                               
CMLERR1  DC    H'0'                CMML NOT FOUND IN AGYLIST                    
         SPACE 1                                                                
*================================================================*              
* FIND ALPHA EQUIVALENT OF CMLSEQ IN SVCMLTAB AND SAVE IN SVCMML *              
*================================================================*              
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
* FIND ALPHA CMML ENTRY IN CCTAB *                                              
         SPACE 1                                                                
GETCMLCC L     R4,=A(CCCMLTAB)                                                  
         L     R5,CCCMLCNT                                                      
*                                                                               
CMLCC2   CLC   0(8,R4),SVCMML      MATCH CMML CODE                              
         BE    CMLCC4                                                           
         LA    R4,10(R4)                                                        
         BCT   R5,CMLCC2                                                        
         XC    0(2,R1),0(R1)       CLEAR MISSING CMML SEQ                       
         B     CMLCCERR            AND PRINT ERROR MESSAGE                      
*                                                                               
CMLCC4   MVC   0(2,R1),8(R4)       MOVE CC CML SEQ NUMBER                       
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
*                                                                               
CMLCCERR NTR1                                                                   
         MVC   ERRMSG(33),=C'** ERROR ** MISSING CC COMMERCIAL'                 
         MVC   ERRMSG+34(8),SVCMML                                              
*                                                                               
         CLC   SVBUYERR,BUYREC     TEST SAME BUYREC                             
         BNE   CMLCCER2                                                         
         CLC   SVCMLERR,SVCMML     SAME BUY - TEST SAME CMML                    
         BE    CMLCCER2            SAME CMML - XKERR WILL SUPPRESS MSG          
         XC    SVBUYERR,SVBUYERR   NEW CMML - FORCE TO PRINT                    
*                                                                               
CMLCCER2 MVC   SVCMLERR,SVCMML     SAVE CMML CODE                               
         B     XKERR                                                            
         EJECT                                                                  
MKTERR   MVC   ERRMSG(31),=C'** ERROR ** CC MARKET NOT FOUND'                   
         B     XKERR                                                            
*                                                                               
XKPERERR MVC   ERRMSG(36),=C'** ERROR ** BUY NOT IN CC EST PERIOD'              
         MVI   PRDERRSW,C'Y'       INDICATE FATAL ERROR                         
         B     XKERR                                                            
*                                                                               
XKPRDERR MVC   ERRMSG(35),=C'** ERROR ** MISSING CC PRODUCT CODE'               
         MVI   PRDERRSW,C'Y'       INDICATE FATAL ERROR                         
*                                                                               
* NEED TO PUT ALPHA CODE IN MESSAGE                                             
*                                                                               
XKPRERR0 L     RE,=A(SVCLTHDR)                                                  
         LA    RE,CLIST-CLTHDRD(RE)                                             
*                                                                               
XKPRERR2 CLC   0(1,R1),3(RE)       MATCH PRD NUMBERS                            
         BE    XKPRERR4                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),C'A'                                                       
         BNL   XKPRERR2                                                         
         ZIC   R0,0(R1)            ANYTHING IS BETTER THAN BLOWING UP           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL(3),DUB                                                      
         LA    RE,FULL                                                          
XKPRERR4 MVC   ERRMSG+38(3),0(RE)                                               
         B     XKERR                                                            
*                                                                               
XKESTERR MVC   ERRMSG(37),=C'** ERROR ** NO CC BRAND ESTIMATE PRD='             
         L     RE,ADCLT            POINT TO CC CLTHDR                           
         LA    RE,CLIST-CLTHDR(RE)                                              
         B     XKPRERR2                                                         
         EJECT                                                                  
XKERR    LA    R4,ERRKEY                                                        
*                                                                               
         CLC   SVBUYERR,BUYREC                                                  
         BNE   XKERR2                                                           
         MVC   ERRMSG,SPACES                                                    
         B     EXIT                                                             
*                                                                               
XKERR2   MVC   SVBUYERR,BUYREC                                                  
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
*================================================================*              
* PRINT TOTALS FOR AN AGENCY                                     *              
*================================================================*              
         SPACE 1                                                                
XKAGYTOT NTR1                                                                   
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
AGYTOTX  DS    0H                                                               
         ZAP   AGYCNT,=P'0'        RESET AGENCY RECORD COUNTER                  
         B     EXIT                                                             
         EJECT                                                                  
*===================================================================*           
* SUBROUTINE TO CLEAR FROM R1 TO R0. RF IS DESTROYED.               *           
*===================================================================*           
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
         EJECT                                                                  
*=============================================================*                 
* ACCUMULATE ORDERED/PAID TOTALS FOR BUY RECORDS              *                 
* R2 POINTS TO BUY RECORD                                     *                 
* TOTIND = C'-' TO POST TO REMOVED                            *                 
* TOTIND = C'+' TO POST TO ADDED                              *                 
*=============================================================*                 
         SPACE 1                                                                
XKTOT    NTR1                                                                   
*                                                                               
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         CLI   0(R2),X'FF'         TEST EOF                                     
         BE    XKTOT10                                                          
*                                                                               
         TM    15(R2),X'80'        TEST DELETED                                 
         BNZ   EXIT                                                             
*                                                                               
         CLI   OLDCLT,0            TEST FIRST TIME                              
         BE    XKTOT2                                                           
*                                                                               
         CLC   OLDCLT,1(R2)        TEST SAME CLIENT                             
         BE    XKTOT4              YES                                          
         L     R5,NEXTBUF          NO - MOVE CLT TOT TO BUFFER                  
         MVC   0(1,R5),TOTIND      MOVE +/-                                     
         MVC   1(2,R5),OLDCLT                                                   
         MVC   4(24,R5),CTOTS                                                   
         LA    R5,28(R5)                                                        
         ST    R5,NEXTBUF                                                       
         MVI   0(R5),0             SET END OF BUFFER FLAG                       
*                                                                               
XKTOT2   LA    R1,CTOTS                                                         
         BAS   RE,XKCLR            CLEAR CTOTS                                  
*                                                                               
         MVC   OLDCLT,1(R2)        MOVE CLIENT CODE                             
*                                                                               
XKTOT4   SR    R4,R4               GROSS ORD                                    
         SR    R5,R5               GROSS PAID                                   
         SR    R6,R6               NET PAID                                     
         IC    R4,38(R2)           SAVE BDTIME BYTE                             
         MVI   38(R2),0            MAKE ZERO FOR GETRATE (PB'S)                 
*                                                                               
         L     RF,GETRATE                                                       
*                                                                               
         LA    R3,24(R2)           PROCESS BUYREC                               
         SR    R0,R0                                                            
XKTOT6   ICM   R0,1,1(R3)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    XKTOT8                                                           
         CLI   0(R3),6                                                          
         BL    XKTOT6                                                           
         CLI   0(R3),14                                                         
         BH    XKTOT6                                                           
         LA    R1,SPOTS                                                         
         STM   R1,R3,DMCB                                                       
         LA    R1,DMCB                                                          
         MVC   0(1,R1),3(R2)       MOVE PRD CODE                                
         BASR  RE,RF                                                            
*                                                                               
         A     R4,GROSS                                                         
         OC    4(2,R3),4(R3)       TEST PAID                                    
         BZ    XKTOT6                                                           
         A     R5,GROSS                                                         
         A     R6,NET                                                           
         B     XKTOT6                                                           
*                                                                               
XKTOT8   STC   R7,38(R2)           RESTORE BDTIME                               
         CVD   R4,DUB                                                           
         AP    CTOTS(8),DUB                                                     
         CVD   R5,DUB                                                           
         AP    CTOTS+8(8),DUB                                                   
         CVD   R6,DUB                                                           
         AP    CTOTS+16(8),DUB                                                  
         B     EXIT                                                             
*                                                                               
XKTOT10  L     R5,NEXTBUF          MOVE LAST CLT TOT TO BUFFER                  
         MVC   0(1,R5),TOTIND                                                   
         MVC   1(2,R5),OLDCLT                                                   
         MVC   4(24,R5),CTOTS                                                   
         MVI   28(R5),0            SET END OF BUFFER FLAG                       
         LA    R5,28(R5)           AND ADVANCE POINTER                          
         ST    R5,NEXTBUF                                                       
         XC    OLDCLT,OLDCLT       SET FIRST TIME FLAG                          
         B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* PRINT DOLLAR TOTALS FROM BUFFER                                 *             
*=================================================================*             
         SPACE 1                                                                
XKTOTPRT NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   P(21),=C'** DELETED RECORDS **'                                  
         GOTO1 REPORT                                                           
*                                                                               
         L     R5,=A(TOTBUFF)                                                   
         ST    R5,NEXTBUF                                                       
*                                                                               
         CLI   0(R5),C'-'                                                       
         BNE   XKTP8                                                            
*                                                                               
XKTP2    DS    0H                                                               
         GOTO1 CLUNPK,DMCB,1(R5),P+27                                           
*                                                                               
XKTP4    LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,4(R5)                                                         
         LA    R6,AGYTOTS                                                       
*                                                                               
XKTP6    AP    0(8,R6),0(8,R5)     BUMP AGY TOTALS                              
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XKTP6                                                         
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         CLI   0(R5),C'-'                                                       
         BE    XKTP2                                                            
         ST    R5,NEXTBUF          SAVE BUFFER POINTER                          
*                                                                               
         GOTO1 REPORT              SKIP A LINE                                  
         SPACE 1                                                                
* PRINT THE - TOTALS *                                                          
         SPACE 1                                                                
XKTP8    MVC   P+27(5),=C'TOTAL'                                                
         LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,AGYTOTS                                                       
         LA    R6,FILTOTS                                                       
*                                                                               
XKTP10   SP    0(8,R6),0(8,R5)     SUBTRACT FROM ACCUMS                         
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XKTP10                                                        
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         LA    R1,AGYTOTS                                                       
         BAS   RE,XKCLR                                                         
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
         BE    XKTP20                                                           
*                                                                               
XKTP12   DS    0H                                                               
         GOTO1 CLUNPK,DMCB,1(R5),P+27                                           
*                                                                               
XKTP14   LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,4(R5)                                                         
         LA    R6,AGYTOTS                                                       
*                                                                               
XKTP16   AP    0(8,R6),0(8,R5)     BUMP AGY TOTALS                              
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XKTP16                                                        
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         CLI   0(R5),0                                                          
         BNE   XKTP12                                                           
*                                                                               
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
         MVC   P+27(5),=C'TOTAL'                                                
         LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,AGYTOTS                                                       
         LA    R6,FILTOTS                                                       
*                                                                               
XKTP18   AP    0(8,R6),0(8,R5)     BUMP FILE TOTALS                             
         MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR THE PENNIES                            
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XKTP18                                                        
*                                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
         LA    R1,AGYTOTS                                                       
         BAS   RE,XKCLR                                                         
         SPACE 1                                                                
XKTP20   DS    0H                                                               
         MVC   P(22),=C'* GRAND TOTALS (NET) *'                                 
*                                                                               
         LA    R3,P+33                                                          
         LA    R4,3                                                             
         LA    R5,FILTOTS                                                       
*                                                                               
XKTP22   MVC   3(14,R3),=X'402020206B2020206B2020202020'                        
         ED    3(14,R3),2(R5)                                                   
         XC    15(2,R3),15(R3)     CLEAR PENNIES                                
         MVI   15(R3),C'+'         SET SIGN                                     
         CP    0(8,R5),=P'0'                                                    
         BNM   *+8                                                              
         MVI   15(R3),C'-'                                                      
         LA    R5,8(R5)                                                         
         LA    R3,14(R3)                                                        
         BCT   R4,XKTP22                                                        
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
XKCLR    LA    R0,3                                                             
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         BR    RE                                                               
         EJECT                                                                  
NEXTBUF  DC    A(TOTBUFF)                                                       
APRDTAB  DS    A                                                                
TOTIND   DC    X'00'                                                            
OLDCLT   DC    XL2'00'                                                          
CTOTS    DS    0CL24                                                            
         DC    3PL8'0'             GROSS ORD/GROSS PAID/NET PAID                
AGYTOTS  DC    3PL8'0'                                                          
FILTOTS  DC    3PL8'0'                                                          
POLESDTS DS    F                                                                
SVCCAGY  DC    A(0)                                                             
BUYSTART DS    XL2                                                              
BUYEND   DS    XL2                                                              
LASTMKT  DS    XL2                                                              
LASTAFF  DS    XL1                                                              
SVAGYA   DS    CL3                                                              
SVAGYCLT DS    XL2                                                              
SVAGYB   DS    XL1                                                              
XFRAGY   DC    X'00'                                                            
SVCMML   DS    CL10                                                             
CNDATA   DS    XL14                                                             
MKTCD    DS    CL3                 NSI MARKET CODE                              
SVBUYERR DS    CL13                                                             
SVCMLERR DS    CL8                                                              
NOSTAEQU DS    XL1                                                              
ELCODE   DS    XL1                                                              
PRDERRSW DC    X'00'                                                            
*                                                                               
SVPROF   DS    CL16                                                             
SVPROF12 EQU   SVPROF+11           HIGH EST FOR XFR                             
*                                                                               
STATBERR DC    H'0'                                                             
CMLTBERR DC    H'0'                                                             
*                                                                               
AGYCNT   DC    PL4'0'                                                           
*                                                                               
CCSTACNT DC    A(0)                                                             
CCCMLCNT DC    A(0)                                                             
SVCMLCNT DC    A(0)                                                             
CCGOLCNT DC    A(0)                                                             
*                                                                               
CTRS     DS    0CL24                                                            
INCNT    DC    PL4'0',CL20'RECORDS IN'                                          
OUTCNT   DC    PL4'0',CL20'RECORDS OUT'                                         
DELCNT   DC    PL4'0',CL20'RECORDS DELETED'                                     
ADDCNT   DC    PL4'0',CL20'RECORDS INSERTED'                                    
CTRX     EQU   *-1                                                              
         EJECT                                                                  
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,MACRF=GM,               X        
               EODAD=XK50                                                       
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,MACRF=PM                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
         DC    C'**XKIO**'                                                      
XKIO     DS    2000C                                                            
*                                                                               
         DS    0D                                                               
         DC    C'*PRDTAB1'                                                      
PRDTAB1  DS    XL256               USED TO CONVERT AGY/CC TO CC/CC              
*                                                                               
         DS    0D                                                               
         DC    C'*PRDTAB2'                                                      
PRDTAB2  DS    XL256               USED TO CONVERT CC/CC TO CC/AGY              
*                                                                               
         DS    0D                                                               
         DC    C'CCPRDEQ*'                                                      
CCPRDEQ  DS    XL256               USED TO EQUIVALENCE DIET BRANDS              
*                                                                               
         DS    0D                                                               
         DC    CL8'CCPOLTAB'                                                    
CCPOLTAB DS    256XL4                                                           
CCPOLTBX EQU   *-1                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'CCESTTAB'                                                    
CCESTTAB DS    210XL256                                                         
CCESTTBX EQU   *-1                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'SVCLTHDR'                                                    
SVCLTHDR DS    1000C               THIS AREA FOR AGY/CC CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'***CCBJM'                                                    
CCBJM   DS     1000C               THIS AREA FOR CC/AGY CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'***CCAPA'                                                    
CCAPA   DS     1000C               THIS AREA FOR CC/AGY CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'***CCLAI'                                                    
CCLAI   DS     1000C               THIS AREA FOR CC/AGY CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'**CCPM**'                                                    
CCPM    DS     1000C               THIS AREA FOR CC/AGY CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'**CCRR**'                                                    
CCRR    DS     1000C               THIS AREA FOR CC/AGY CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'**CCMC**'                                                    
CCMC    DS     1000C               THIS AREA FOR CC/AGY CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'**CCWV**'                                                    
CCWV    DS     1000C               THIS AREA FOR CC/AGY CLTHDR                  
*                                                                               
         DS    0D                                                               
         DC    CL8'**CCXXX*'                                                    
CCXXX   DS     1000C               THIS AREA FOR XFRAGY CODES                   
*                                                                               
         DS    0D                                                               
         DC    CL8'CCCMLTAB'                                                    
CCCMLTAB DS    20000C              2000 10 BYTE ENTRIES                         
CCCMLTBX EQU   *-1                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'SVCMLTAB'                                                    
SVCMLTAB DS    20000C                                                           
SVCMLTBX EQU   *-1                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'CCSTATAB'                                                    
CCSTATAB DS    12000C              2000 6 BYTE ENTRIES                          
CCSTATBX EQU   *-1                                                              
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'CCGOLMKT'                                                    
CCGOLMKT DS    10240C              2500+ 4 BYTE ENTRIES                         
CCGOLMKX EQU   *                                                                
CCGOLMAX EQU   (CCGOLMKX-CCGOLMKT)/4                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'CCSTEQTB'                                                    
CCSTEQTB DS    200XL10                                                          
CCSTEQBX EQU   *-1                                                              
CCSTEQNB EQU   (*-CCSTEQTB)/10                                                  
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'TOTBUFF'                                                     
TOTBUFF  DS    800D                                                             
         EJECT                                                                  
       ++INCLUDE DDCNTRL                                                        
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
CLTHDRD  DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
ESTHDRD  DSECT                                                                  
       ++INCLUDE PESTREC                                                        
PUBMASTD DSECT                                                                  
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBREPEL                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE PBUYREC                                                        
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
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
**PAN#1  DC    CL21'002PPREPDA02 05/01/02'                                      
         END                                                                    
