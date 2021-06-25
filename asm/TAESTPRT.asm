*          DATA SET TAESTPRT   AT LEVEL 087 AS OF 01/16/14                      
*PHASE T70206C,*                                                                
         TITLE 'T70206 - PRINTS AN ESTIMATE MAINTENANCE STYLE'                  
T70206   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 ESTLNQ,T70206,RA,R6,RR=R2                                        
*                                                                               
         LR    R7,RC               R7=A(WORKING STORAGE)                        
         USING ESTD,R7                                                          
         MVC   TRACEOPT,0(R1)      SAVE TRACE VALUE                             
         L     R4,4(R1)            A(HYPO PERFORMER ESTIMATE ELEMENT)           
*                                                                               
         L     RC,0(R1)            RC=CONTROLLER STORAGE AREA                   
         USING GEND,RC                                                          
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         ST    R2,RELO                                                          
         EJECT                                                                  
         LA    R2,P+1              R2=A(PRINT LINE)                             
         USING PRTD,R2                                                          
         MVI   COUNTOPT,C'N'                                                    
         LTR   R4,R4               IF A(HYPO PERF ESTIMATE ELE PAST)            
         BZ    MAIN20                                                           
         MVI   COUNTOPT,C'Y'                                                    
         MVI   HLINES,0                                                         
         BAS   RE,PRTUNSCN                                                      
         MVC   4(1,R1),HLINES      RETURN # OF LINES PERF TAKES                 
         B     XIT                                                              
*                                                                               
MAIN20   L     R4,AIO              R4=A(ESTIMATE RECORD)                        
         ST    R4,AESTIO           SAVE IT                                      
         USING TLESD,R4                                                         
         MVC   SVHDHOOK,HEADHOOK   SAVE A(HEADHOOK)                             
         MVC   SVMDHOOK,MIDHOOK    SAVE A(MIDHOOK)                              
         MVC   SVSPECS,SPECS       SAVE A(SPECS)                                
         MVC   SVRCSUB,RCSUBPRG    SAVE RCSUBPRG                                
         SPACE 1                                                                
         MVI   FORCEHED,C'Y'       SET TO START NEW PAGE                        
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS            SET A(SPECS)                                 
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK         SET A(HEADING ROUTINE)                       
         LA    R1,MDHOOK                                                        
         ST    R1,MIDHOOK          SET A(MIDLINE ROUTINE)                       
         LA    R1,WRKIO                                                         
         ST    R1,AWRKIO           SET A(WORKING IO)                            
         SPACE 1                                                                
         BAS   RE,PROCESS          PROCESS THE RECORD                           
         BAS   RE,TRACETAB         TRACE OUT THE TAPE                           
*                                  SORT THE TABLE                               
         LA    R5,TABKEYLQ         LENGTH OF KEY IN TABLE ENTRY                 
         LA    RF,ESTD                       TABLE                              
         AHI   RF,ESTTAB-ESTD                                                   
         L     R4,TABCNT           R4=NUMBER OF TABLE ENTRIES TO SORT           
         LA    R3,TABLNQ           LENGTH OF TABLE ENTRY                        
         GOTO1 XSORT,DMCB,(RF),(R4),(R3),(R5),0                                 
*                                                                               
         BAS   RE,PRTEST           PRINT THE RECORD                             
*                                                                               
         MVC   HEADHOOK,SVHDHOOK   RESTORE A(HEADHOOK)                          
         MVC   MIDHOOK,SVMDHOOK    RESTORE A(MIDHOOK)                           
         MVC   SPECS,SVSPECS       RESTORE A(SPECS)                             
         MVC   RCSUBPRG,SVRCSUB    RESTORE RCSUBPRG                             
         MVI   FORCEHED,C'Y'       SET TO START ON NEW PAGE                     
         MVI   FORCEMID,C'N'       RESET MIDLINE                                
         NI    SPOOLIND,X'FF'-SPNSPACE RESET SPOOLIND                           
         SPACE 1                                                                
XIT      XIT1                                                                   
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO LOOP THROUGH ESTIMATE RECORD AND                      
*              SET NECESSARY INFO INTO TABLE                                    
*                                                                               
PROCESS  NTR1                                                                   
         XR    R1,R1               PRE CLEAR TABLE COUNT                        
         LA    RE,ESTD                       TABLE                              
         AHI   RE,ESTTAB-ESTD                                                   
         LH    RF,=Y(MAXETAB*TABLNQ)                                            
         XCEFL ,                                                                
         XC    SVCLI,SVCLI                   SAVED CLIENT                       
         XC    SVPRD,SVPRD                   SAVED PRODUCT                      
*                                                                               
         LA    R3,ESTD             R3=A(ESTIMATE TABLE)                         
         AHI   R3,ESTTAB-ESTD                TABLE                              
         USING TABD,R3                                                          
*                                                                               
         MVI   ELCODE,TAESELQ      GET FIRST/NEXT ESTIMATE ELEMENT              
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PRC10    BAS   RE,NEXTEL                                                        
         BNE   PRCX                                                             
         SPACE 1                                                                
         USING TAESD,R4            R4=A(ESTIMATE DETAILS)                       
         CLI   TAESTYPE,TAESTCLI   IF CLIENT                                    
         BNE   PRC20                                                            
         MVC   SVCLI,TAESCLI       SAVE CLIENT CODE                             
         XC    SVPRD,SVPRD         CLEAR PRODUCT CODE                           
         B     PRC80                                                            
*                                                                               
PRC20    CLI   TAESTYPE,TAESTPRD   IF PRODUCT                                   
         BNE   PRC30                                                            
         MVC   SVPRD,TAESPRD       SAVE PRODUCT CODE                            
         B     PRC80                                                            
*                                                                               
PRC30    TM    TAESTYPE,TAESTCOM   IF COMMERCIAL TYPE ELEMENT                   
         BNO   PRC10                                                            
         MVC   TABCOM,TAESDATA     SAVE COMMERCIAL DATA                         
         B     PRC80                                                            
*                                                                               
PRC80    MVC   TABCLI,SVCLI        SET CLIENT CODE IN TABLE                     
         MVC   TABPRD,SVPRD        SET PRODUCT CODE IN TABLE                    
         MVC   TABTYPE,TAESTYPE    SET ELEMENT TYPE IN TABLE                    
         MVC   TABSEQ,TAESSEQ      SET ELEMENT SEQUENCE NUMBER IN TABLE         
         LA    R3,TABLNQ(R3)       BUMP TO NEXT TABLE ENTRY                     
         LA    R1,1(R1)            INCREMENT TABLE COUNT                        
         CH    R1,=Y(MAXETAB)                                                   
         BL    PRC10                                                            
         DC    H'0'                ESTTAB FULL                                  
*                                                                               
PRCX     ST    R1,TABCNT           SAVE NUMBER OF TABLE ENTRIES                 
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE TO PROCESS TABLE AND PRINT ESTIMATE                      
*                                                                               
PRTEST   NTR1                                                                   
         LA    R3,ESTD             R3=A(SORTED ESTIMATE TABLE)                  
         AHI   R3,ESTTAB-ESTD                TABLE                              
         USING TABD,R3                                                          
*                                                                               
PRTE5    OC    TABD(TABLNQ),TABD   TEST END OF TABLE                            
         BZ    PRTEX                                                            
*                                                                               
         BAS   RE,SEARCH           RETURNS R4=A(ELEMENT)                        
         USING TAESD,R4                                                         
*                                                                               
         CLI   TABTYPE,TAESTCLI    IF CLIENT                                    
         BNE   PRTE20                                                           
         BAS   RE,PRTCLIE          PRINT CLIENT INFO                            
         B     PRTE80                                                           
*                                                                               
PRTE20   CLI   TABTYPE,TAESTPRD    IF PRODUCT                                   
         BNE   PRTE30                                                           
         BAS   RE,PRTPROD          PRINT PRODUCT INFORMATION                    
         B     PRTE80                                                           
*                                                                               
PRTE30   TM    TABTYPE,TAESTCOM    IF COMMERCIAL TYPE                           
         BO    *+6                                                              
         DC    H'0'                                                             
         TM    TAESTYPE,TAESTHYP   IF HYPO COMMERCIAL                           
         BZ    *+12                                                             
         BAS   RE,PRTHCOM          PRINT HYPO COMMERCIAL INFO                   
         B     PRTE50                                                           
         BAS   RE,PRTCOM           ELSE, PRINT ACTUAL COMMERCIAL INFO           
         BNE   PRTE80              IF NOT ON FILE SKIP TO NEXT TAB NTRY         
*                                                                               
PRTE50   MVI   ELCODE,TAESELQ      RESET ELCODE                                 
         BAS   RE,NEXTEL           GET NEXT ESTIMATE ELEMENT                    
         BNE   PRTE80                                                           
         TM    TAESTYPE,TAESTPER   IF PERFORMER TYPE ESTIMATE ELEMENT           
         BZ    PRTE80                                                           
         BAS   RE,PRTPERF          PRINT ALL ACTUAL PERFORMER INFO              
         BAS   RE,PRTHPERF         PRINT ALL HYPO PERFORMER INFO                
*                                                                               
PRTE80   LA    R3,TABLNQ(R3)       BUMP TO NEXT TABLE ENTRY                     
         B     PRTE5                                                            
*                                                                               
PRTEX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO RETURN R4 POINTING TO ELE IN RECORD                   
*                                                                               
         USING TABD,R3             R3=A(CURRENT TABLE ENTRY)                    
SEARCH   NTR1                                                                   
         L     R4,AIO              R4=A(ESTIMATE RECORD)                        
         MVI   ELCODE,TAESELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SEARCH5  BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TAESD,R4            R4=A(ESTIMATE DETAILS ELEMENT)               
         CLC   TABTYPE,TAESTYPE    MATCH ON ELEMENT TYPE                        
         BNE   SEARCH5                                                          
         CLC   TABSEQ,TAESSEQ      MATCH ON SEQUENCE NUMBER                     
         BNE   SEARCH5                                                          
         LA    R1,L'TABCLI-1                                                    
         LA    RE,TABCLI                                                        
         CLI   TABTYPE,TAESTCLI                                                 
         BE    SEARCH8                                                          
         LA    R1,L'TABPRD-1                                                    
         LA    RE,TABPRD                                                        
         CLI   TABTYPE,TAESTPRD                                                 
         BE    SEARCH8                                                          
         LA    R1,L'TABCOM-1                                                    
         LA    RE,TABCOM                                                        
*                                                                               
SEARCH8  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),TAESDATA    MATCH ON DATA                                
         BNE   SEARCH5                                                          
         XIT1  REGS=(R4)           RETURN R4=A(ELEMENT)                         
         EJECT                                                                  
*              ROUTINE TO PRINT CLIENT INFORMATION                              
         SPACE 1                                                                
         USING TAESD,R4            R4=A(ESTIMATE DETAILS ELEMENT)               
PRTCLIE  NTR1                                                                   
         MVI   PROCSW,PROCCLI      SET PROCESSING CLIENT                        
         MVC   PRTLCLI,LTCLI       LITERAL 'CLIENT'                             
         MVC   PRTCLI,TAESCLI      CLIENT CODE                                  
         MVC   AIO,AWRKIO          READ CLIENT RECORD INTO WRKIO                
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'AC',TAESCLI),0                            
         MVC   AIO,AESTIO          RESTORE IO TO ESTIMATE RECORD                
         MVC   PRTCNAME,=CL36'** NOT FOUND **'                                  
         BNE   *+10                                                             
         MVC   PRTCNAME,TGNAME                                                  
         BAS   RE,PRTUNSCN         UNSCAN THE OPTIONS IF ANY                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT PRODUCT INFORMATION                             
         SPACE 1                                                                
         USING TAESD,R4            R4=A(ESTIMATE DETAILS ELEMENT)               
PRTPROD  NTR1                                                                   
         CLI   PROCSW,PROCCLI      IF PREVIOUSLY NOT CLIENT MODE                
         BE    PRTPROD5                                                         
         MVI   PROCSW,PROCPRD      SET PROCESSING PRODUCT                       
         BAS   RE,PRINTIT          PRINT A BLANK LINE FIRST                     
*                                                                               
PRTPROD5 MVI   PROCSW,PROCPRD      SET PROCESSING PRODUCT                       
         MVC   PRTLPRD,LTPRD       LITERAL 'PRODUCT'                            
         MVC   PRTPRD,TAESPRD      PRODUCT CODE                                 
         MVC   AIO,AWRKIO          READ PRODUCT RECORD IN WRKIO                 
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'AC',TAESPRD),0                            
         MVC   AIO,AESTIO          RESTORE IO TO ESTIMATE RECORD                
         MVC   PRTCNAME,=CL36'** NOT FOUND **'                                  
         BNE   *+10                                                             
         MVC   PRTPNAME,TGNAME                                                  
         BAS   RE,PRTUNSCN         UNSCAN THE OPTIONS IF ANY                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT COMMERCIAL INFORMATION                          
         SPACE 1                                                                
         USING TAESD,R4            R4=A(ESTIMATE DETAILS ELEMENT)               
PRTCOM   NTR1                                                                   
         CLI   PROCSW,PROCCOM      IF PREVIOUSLY NOT COMML MODE                 
         BE    PRTCOM5                                                          
         MVI   PROCSW,PROCCOM      SET PROCESSING MODE                          
         CLI   LINE,55             IF NOT ENOUGH LINES                          
         BL    *+12                                                             
         MVI   FORCEHED,C'Y'       FORCE NEW PG & MIDLINES                      
         B     PRTCOM5                                                          
*                                                                               
         BAS   RE,PRTBLANK         ELSE, PRINT BLANK LINE BEFORE                
         MVI   FORCEMID,C'Y'       MID-LINES                                    
*                                                                               
PRTCOM5  MVC   TGCOM,TAESCOM       SET INTERNAL COMMERCIAL NUMBER               
         MVC   TGCID(L'TAESCID),TAESCID SET COMMERCIAL ID                       
         ST    R4,SAVER4           SAVE R4                                      
*                                                                               
         MVC   AIO,AWRKIO          R4=A(COMMERCIAL RECORD)                      
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',0)                                   
         BNE   PRTCOMNO            SKIP IF COMMERCIAL NOT ON FILE               
*                                                                               
         L     R4,AWRKIO                                                        
         USING TLCOD,R4                                                         
         CLC   TLCOAGY,TGAGY       IF AGENCY HAS CHANGED                        
         BNE   PRTCOMNO            SKIP TO NEXT ELEMENT IN ESTIMATE             
*                                                                               
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING TACOD,R4            R4=A(COMMERCIAL DETAILS EL.)                 
         CLC   TGCID(L'TAESCID),TACOCID  IF CID HAS CHANGED                     
         BE    *+8                                                              
         MVI   PRTCCIDL,C'*'             INDICATE SO                            
*                                                                               
         MVC   PRTCCID,TACOCID                 COMMERCIAL ID                    
         MVC   PRTCMED(L'TACOMED),TACOMED      MEDIA                            
         GOTO1 MEDVAL,DMCB,TACOMED                                              
         BNE   *+10                                                             
         MVC   PRTCMED,TGMENAME                                                 
         EDIT  TACOSEC,(3,PRTCSEC),ALIGN=LEFT  LENGTH                           
         GOTO1 CTYPVAL,DMCB,TACOTYPE           COMMERCIAL TYPE                  
         BNE   *+10                                                             
         MVC   PRTCTYPE,TGCTNAME                                                
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   PRTCTTLE,TGNAME                 TITLE                            
         SPACE 1                                                                
         L     R4,AWRKIO                                                        
         MVI   ELCODE,TALFELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PRTCOM10                                                         
         SPACE 1                                                                
         USING TALFD,R4            R4=A(LIFT DETAILS ELEMENT)                   
         MVC   PRTCLID,TALFLID                 LIFT ID                          
         EDIT  TALFSEC,(3,PRTCLSEC),ALIGN=LEFT LIFT LENGTH                      
         B     PRTCOM30                                                         
         SPACE 1                                                                
         USING TAVRD,R4                                                         
PRTCOM10 L     R4,AWRKIO           R4=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TAVRELQ      READ VERSION ELEMENTS                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PRTCOM20 BAS   RE,NEXTEL                                                        
         BNE   PRTCOM30                                                         
         CLI   TAVRVERS,2          IF ELEMENT FOR VERSION 2                     
         BNE   PRTCOM20            IS FOUND ...                                 
         MVC   PRTCLID,TAVRCID     SAVE ID AND LENGTH AS LIFT                   
         EDIT  TAVRSEC,(3,PRTCLSEC),ALIGN=LEFT                                  
         DROP  R4                                                               
         SPACE 1                                                                
PRTCOM30 MVC   AIO,AESTIO          RESTORE IO TO ESTIMATE RECORD                
         L     R4,SAVER4           RESTORE R4 TO A(ESTIMATE ELEMENT)            
         BAS   RE,PRTUNSCN         UNSCAN THE OPTIONS IF ANY                    
         B     YES                                                              
         SPACE 2                                                                
PRTCOMNO MVC   AIO,AESTIO          RESTORE IO TO ESTIMATE RECORD                
         B     NO                                                               
         EJECT                                                                  
*        ROUTINE TO PRINT HYPOTHETICAL COMMERCIAL INFORMATION                   
*                                                                               
         USING TAESD,R4            R4=A(ESTIMATE DETAILS ELEMENT)               
PRTHCOM  NTR1                                                                   
         CLI   PROCSW,PROCCOM      IF PREVIOUSLY NOT COMML MODE                 
         BE    PRTHCOM5                                                         
         MVI   PROCSW,PROCCOM      SET PROCESSING MODE                          
*                                                                               
         CLI   LINE,55             IF NOT ENOUGH LINES                          
         BL    *+12                                                             
         MVI   FORCEHED,C'Y'       FORCE NEW PG AND MID-LINES                   
         B     PRTHCOM5                                                         
*                                                                               
         BAS   RE,PRTBLANK         PRINT BLANK LINE BEFORE MIDLINES             
         MVI   FORCEMID,C'Y'       MID-LINES PRINT                              
*                                                                               
PRTHCOM5 MVC   PRTHCID(L'TAESHCOM),TAESHCOM      HYPO COMML CODE                
         CLI   TAESHCOM+9,0                      HYPO CODE PACKED?              
         BNE   PRTHCOM6                                                         
         GOTO1 =A(CHPACK),DMCB,(C'U',TAESHCOM),PRTHCID,RR=RELO                  
*                                                                               
PRTHCOM6 GOTO1 MEDVAL,DMCB,(X'80',TAESHMED)                                     
         MVC   PRTHMED,TGMENAME                  MEDIA                          
         EDIT  (1,TAESHLEN),(2,PRTHSEC)          LENGTH                         
         MVC   PRTHTYPE,=CL8'**HYPO**'                                          
         MVC   PRTHNME,SPACES                                                   
         BAS   RE,PRTUNSCN         UNSCAN THE OPTIONS IF ANY                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT ACTUAL PERFORMER INFORMATION                    
         SPACE 1                                                                
         USING TAESD,R4            R4=A(ESTIMATE DETAILS ELEMENT)               
PRTPERF  NTR1                                                                   
         MVI   PROCSW,PROCPRF      SET PROCESSING PERFORMER                     
         MVI   FRSTIME,C'Y'        SET FIRST TIME FLAG                          
         B     PRTPRF5             CHECK FIRST PERFORMER                        
*                                                                               
PRTPRF2  MVI   ELCODE,TAESELQ                                                   
         BAS   RE,NEXTEL                                                        
         BNE   PRTPRFX                                                          
         TM    TAESTYPE,TAESTPER   MAKE SURE STILL PERFORMER                    
         BZ    PRTPRFX                                                          
*                                                                               
PRTPRF5  TM    TAESTYPE,TAESTHYP   IF ACTUAL PERFORMER                          
         BO    PRTPRF2                                                          
         CLI   TAESLEN,TAESLNQ     AND IF PERF HAS DETAILS                      
         BNH   PRTPRF2                                                          
*                                                                               
         CLI   FRSTIME,C'Y'        IF FIRST TIME FOR ACTUAL PERFOMER            
         BNE   PRTPRF6                                                          
         MVI   FRSTIME,C'N'        RESET INDICATOR                              
         CLI   LINE,56             IF NOT ENOUGH LINES ON CURR PG               
         BL    *+12                                                             
         MVI   FORCEHED,C'Y'       FORCE HEAD & MID-LINES                       
         B     *+8                                                              
         MVI   FORCEMID,C'Y'       ELSE, FORCE MID-LINES                        
*                                                                               
PRTPRF6  MVC   TGCSORT,TAESSORT    SET GLOBALS                                  
         EDIT  (4,TAESSSN),(9,TGSSN),FILL=0                                     
         GOTO1 RECVAL,DMCB,TLCACDQ,(X'40',0)                                    
         GOTO1 HIGH                                                             
         CLC   KEY(TLCACAT-TLCAKEY),KEYSAVE                                     
         BNE   PRTPRF2                                                          
*                                                                               
         ST    R4,SAVER4           SAVE R4                                      
         MVC   AIO,AWRKIO          READ CAST RECORD IN WRKIO                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AWRKIO                                                        
         USING TLCAD,R4                                                         
         MVC   PRTSSN,TLCASSN      S/S NUMBER                                   
         TM    TGSYSTAT,TASYSPID   USING PID?                                   
         BZ    PRTPRF7                                                          
         MVC   PRTSSN,SPACES                                                    
         GOTO1 SSNPACK,DMCB,TLCASSN,PRTSSN   CONVERT SSN TO PID                 
PRTPRF7  MVC   PRTPCAT,TLCACAT     CATEGORY                                     
         SPACE 1                                                                
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
         MVC   PRTPDBL,TACADBL     N'DOUBLES                                    
         MVI   PRTPCAM,C'Y'        ON CAMERA                                    
         CLI   TACAONOF+1,C'F'                                                  
         BNE   *+8                                                              
         MVI   PRTPCAM,C'N'        OFF CAMERA                                   
         CLI   TACACORP,C' '       IF CORP CODE ON CAST RECORD                  
         BNH   *+8                                                              
         MVI   PRTPW4TY,TAW4TYCO   SET W4 TYPE IS CORPORATION                   
*                                                                               
         BAS   RE,DSPOV            DISPLAY OVERSCALE 1,2                        
*                                                                               
         TM    TACASTAT,TACASTLO   IF ONLY ON LIFT                              
         BZ    *+12                                                             
         MVI   PRTPLIFT,C'O'                                                    
         B     PRTPRF8                                                          
         TM    TACASTAT,TACASTLF   IF ON LIFT                                   
         BZ    PRTPRF8                                                          
         MVI   PRTPLIFT,C'Y'       LIFT                                         
*                                                                               
PRTPRF8  GOTO1 XNAME,DMCB,(X'80',TLW4CDQ),PRTPNME,0  GET W4 NAME                
*                                                                               
         MVC   AIO,AESTIO          RESTORE IO TO ESTIMATE RECORD                
         L     R4,SAVER4           RESTORE R4=A(ESTIMATE ELEMENT)               
         BAS   RE,PRTUNSCN         UNSCAN THE OPTIONS IF ANY                    
         B     PRTPRF2                                                          
*                                                                               
PRTPRFX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY PERFORMER OVERSCALES                          
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
         SPACE 1                                                                
DSPOV    NTR1                                                                   
         GOTO1 SETOV2,DMCB,(R4),AIO,SPACES                                      
         SPACE 1                                                                
         GOTO1 GETOV1,DMCB,SPACES,FULL  GET OVERSCALE 1                         
         CLI   0(R1),X'FF'                                                      
         BNE   *+10                                                             
         XC    FULL,FULL           RETURNED AMOUNT - IGNORE                     
         SPACE 1                                                                
         LA    R1,PRTPOV1          OVERSCALE PRINT FIELD                        
         XR    RE,RE               HANDLE 1ST OVERSCALE                         
         L     RF,FULL                                                          
         D     RE,=F'100'          PUSH OUT PENNIES                             
         EDIT  (RF),(3,(R1)),ALIGN=LEFT                                         
         AR    R1,R0               BUMP PAST AMOUNT                             
         SPACE 1                                                                
         ICM   RF,15,TACAOV2       DO WE HAVE 2ND OVERSCALE                     
         BZ    DSPOVX                                                           
         MVI   0(R1),C','          ADD COMMA                                    
         XR    RE,RE                                                            
         D     RE,=F'100'          PUSH OUT PENNIES                             
         EDIT  (RF),(3,1(R1)),ALIGN=LEFT                                        
DSPOVX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT HYPOTHETICAL PERFORMER INFORMATION              
*                                                                               
         USING TAESD,R4            R4=A(ESTIMATE DETAILS ELEMENT)               
PRTHPERF NTR1                                                                   
         MVI   PROCSW,PROCHPRF     SET PROCESSING PERFORMER                     
         MVI   FRSTIME,C'Y'        SET FIRST TIME FLAG                          
         BAS   RE,CALCLPG          CALCULATE LAST PAGE FOR HYPO PRFS            
*                                                                               
         LA    R1,0                LOOP THROUGH HYPO PERFS                      
PRTHPRF5 BAS   RE,SNPHPERF         SEARCH & PRT HYPO PERFS FOR THIS PG          
         LA    R1,1(R1)            BUMP TO NEXT POSSIBLE PAGE                   
         CLM   R1,1,LSTPG          CHECK NOT LAST LAST PAGE                     
         BNH   PRTHPRF5                                                         
*                                                                               
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SEARCH FOR AND PRINT                                  
*              HYPO PERFORMERS FOR CURRENT PAGE                                 
*                                  NTRY --- R1=PAGE NUMBER                      
         USING TAESD,R4            R4=A(HYP PERF ESTIMATE ELE)                  
         SPACE 1                                                                
SNPHPERF NTR1                                                                   
         B     SNPHPRF5                                                         
*                                                                               
SNPHPRF2 MVI   ELCODE,TAESELQ                                                   
         BAS   RE,NEXTEL                                                        
         BNE   SNPHPRFX                                                         
         TM    TAESTYPE,TAESTPER   MAKE SURE STILL PERFORMER                    
         BZ    SNPHPRFX                                                         
*                                                                               
SNPHPRF5 TM    TAESTYPE,TAESTHYP   IF HYPO PERFORMER                            
         BZ    SNPHPRF2                                                         
         CLM   R1,1,TAESHPG        TAKE SURE CORRECT PAGE                       
         BNE   SNPHPRF2                                                         
*                                                                               
         CLI   FRSTIME,C'Y'        IF FIRST FOR HYPO PERFORMER                  
         BNE   SNPHPRF6                                                         
         MVI   FRSTIME,C'N'        RESET INDICATOR                              
         CLI   LINE,56             IF NOT ENOUGH LINES LEFT ON CURR PG          
         BL    *+12                                                             
         MVI   FORCEHED,C'Y'       FORCE HEAD AND MID-LINES                     
         B     *+8                                                              
         MVI   FORCEMID,C'Y'       ELSE, FORCE MID-LINES                        
*                                                                               
SNPHPRF6 BAS   RE,PHPERF           PRINT THE LINE                               
         B     SNPHPRF2                                                         
*                                                                               
SNPHPRFX B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CALCULATES THE LAST PAGE NUMBER FOR THIS SET             
*              OF HYPO PERFORMERS                                               
*                                  XIT LSTPG= LAST PAGE NUMBER                  
         SPACE                                                                  
CALCLPG  NTR1                                                                   
         MVI   LSTPG,0                                                          
CALCLPG5 CLI   TAESTYPE,TAESTHPE                                                
         BNE   XIT                                                              
         CLC   LSTPG,TAESHPG       SAVE BIGGEST PAGE NUMBER                     
         BNL   *+10                                                             
         MVC   LSTPG,TAESHPG                                                    
         BAS   RE,NEXTEL                                                        
         BE    CALCLPG5                                                         
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT HYPO PERFORMER LINE                             
*                                                                               
         USING TAESD,R4            R4=A(HYP PERF ESTIMATE ELE)                  
PHPERF   NTR1                                                                   
         EDIT  (1,TAESHNPR),(3,PRTHNUM),ALIGN=LEFT                              
         GOTO1 CATVAL,DMCB,(X'80',TAESHCAT)  LOOK UP CATEGORY                   
         MVC   PRTHCAT,TGCACDE               AND DISPLAY IT                     
*                                                                               
         MVC   PRTHCAM(3),=C'ON '                                               
         TM    TAESHSTA,X'80'      ON/OFF CAMERA                                
         BO    *+10                                                             
         MVC   PRTHCAM,=C'OFF'                                                  
*                                                                               
         GOTO1 UNIVAL,DMCB,(X'20',TAESHUNI)  LOOK UP UNION                      
         MVC   PRTHUNI,TGUNCDE               AND DISPLAY IT                     
         SPACE 1                                                                
         MVC   PRTHLFT(1),TAESHLFT  LIFT STATUS                                 
         MVC   PRTHDBL(1),TAESHDBL  N'DOUBLES                                   
         SPACE 1                                                                
         TM    TAESHSTA,X'40'      CORP STATUS                                  
         BZ    *+8                                                              
         MVI   PRTHCRP,C'Y'                                                     
         BAS   RE,PRTUNSCN         UNSCAN THE OPTIONS IF ANY                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY ESTIMATE SUB-ELEMENTS                         
*                                                                               
         USING TAESD,R4            R4=A(ESTIMATE DETAILS ELEMENT)               
PRTUNSCN NTR1                                                                   
         MVI   HLINES,0            INITIALIZE LINE COUNT                        
         CLI   TAESSBNO,0          IF SOMETHING TO DISPLAY                      
         BE    PRTUNX                                                           
         CLI   PROCSW,PROCCLI      IF CLIENT OR PRODUCT OPTIONS                 
         BE    *+12                                                             
         CLI   PROCSW,PROCPRD                                                   
         BNE   PRTUN1                                                           
         MVC   PRTGDTLS,=CL14'GLOBAL DETAILS'                                   
*                                                                               
PRTUN1   ZIC   R5,TAESSBNO         R5=(NUMBER OF SUB-ELEMENTS)                  
         LA    R4,TAESSBCD         R4=A(FIRST SUB-ELEMENT)                      
         USING TAESSBCD,R4                                                      
*                                                                               
         LA    R3,ESTD             INITIALIZE UNSCAN BLOCK ENTRY                
         AH    R3,=Y(MYBLOCK-ESTD)                                              
         XR    R0,R0                                                            
PRTUN2   MVC   0(LHSLNQ+RHSLNQ,R3),SPACES  17 AND 13                            
*                                                                               
         LA    R1,OPTTAB           LOOK FOR ENTRY IN TABLE                      
         USING OPTD,R1                                                          
PRTUN3   CLI   0(R1),X'FF'         TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   TAESSBCD,OPTCODE    MATCH ON SUB-ELEMENT                         
         BE    PRTUN4                                                           
         TM    OPTSTAT,OPTNOLHS                                                 
         BZ    *+12                                                             
         LA    R1,OPTNEXT                                                       
         B     *+8                                                              
         LA    R1,OPTNEXT2                                                      
         B     PRTUN3                                                           
*                                                                               
PRTUN4   XR    RF,RF                                                            
         ICM   RF,3,OPTODSP        RF=DISPLACEMENT TO O/P ROUTINE               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RF,RB                                                            
         LA    RE,PRTUN5           RE=A(RETURN)                                 
         NTR1  ,                   COMMON NTR1                                  
         BR    RF                                                               
         SPACE 2                                                                
PRTUN5   DS    0H                  HERE AFTER EXECUTING ROUTINE                 
         ZIC   RE,TAESSBDT         ASSUME LENGTH IS IN FIRS DATA BYTE           
         CLI   TAESSBCD,OPCOMNM    IF THIS ISN'T COMMERCIAL NAME                
         BE    *+16                                                             
         LA    R3,LHSLNQ+RHSLNQ(R3) BUMP UNSCAN BLOCK ENTRY                     
         AH    R0,=H'1'            AND ADD 1 TO COUNT                           
         IC    RE,OPTSUBLN         L'SUB EL IS IN TABLE                         
*                                                                               
         LA    R4,1(RE,R4)         BUMP TO NEXT SUB-ELEMENT                     
         BCT   R5,PRTUN2           AND PROCESS                                  
*                                                                               
         LTR   R0,R0               ANYTHING TO DO                               
         BZ    PRTUNX                                                           
*                                                                               
         LA    R1,ESTD             SET A(MYBLOCK)                               
         AH    R1,=Y(MYBLOCK-ESTD)                                              
         ST    R1,DMCB                                                          
         STC   R0,DMCB             SET NUMBER OF ACTUAL ENTRIES                 
PRTUN10  LA    RF,LHSLNQ           NON-STANDARD L'LEFT HAND SIDE                
         LA    R0,RHSLNQ           NON-STANDARD L'RIGHT HAND SIDE               
         XC    DUMSCRNH(8+L'PRTDETLS),DUMSCRNH                                  
         MVI   DUMSCRNH,L'PRTDETLS+8                                            
         MVI   DUMSCRNH+5,L'PRTDETLS                                            
*                                                                               
         GOTO1 UNSCAN,DMCB,,((R0),DUMSCRNH),0,((RF),C' $LT')                    
         MVC   PRTDETLS,DUMSCRN                                                 
         CLI   0(R1),0                                                          
         BE    PRTUNX                                                           
         MVC   SVPARM1,DMCB        SAVE INFO FROM UNSCAN                        
         BAS   RE,PRINTIT                                                       
         MVC   DMCB,SVPARM1        RESTORE INFO FROM UNSCAN                     
         B     PRTUN10             UNSCAN NEXT PRINT LINE                       
*                                                                               
PRTUNX   BAS   RE,PRINTIT                                                       
         B     XIT                                                              
         SPACE 2                                                                
MYOCOMNM DS    0H                  MY HYPO COMMERCIAL NAME                      
         ZIC   RE,TAESSBDT                                                      
         CH    RE,=AL2(L'HCOMNAME)                                              
         BNH   *+8                                                              
         LA    RE,L'HCOMNAME                                                    
         B     OUTCOMN2                                                         
         EJECT                                                                  
       ++INCLUDE TAESTOPTR         OPTION ROUTINES                              
         SPACE 2                                                                
UXIT     B     XIT                                                              
         SPACE 1                                                                
UXITRF   XIT1  REGS=(RF)                                                        
         SPACE 1                                                                
LHSLNQ   EQU   17                  NON-STANDARD L'LHS OF UNSCAN BLOCK           
RHSLNQ   EQU   13                  NON-STANDARD L'RHS OF UNSCAN BLOCK           
         EJECT                                                                  
*              HEAD HOOK ROUTINE                                                
*                                                                               
HDHOOK   NTR1                                                                   
         L     R4,AIO              R4=A(ESTIMATE RECORD)                        
         USING TLESD,R4                                                         
*                                  AGENCY CODE                                  
         MVC   H4+9(L'TLESAGY),TLESAGY                                          
*                                  ESTIMATE NUMBER                              
         MVC   H4+105(L'TLESEST),TLESEST                                        
*                                  REVISION NUMBER                              
         GOTO1 CHAROUT,DMCB,TANUELQ,0,TANUTREV                                  
         CLC   TGNAME,SPACES                                                    
         BNH   HDHOOK5                                                          
         MVC   H4+117(8),=C'REVISION'                                           
         MVC   H4+126(L'TGNAME),TGNAME                                          
*                                  ESTIMATE NAME                                
HDHOOK5  GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   H5+96(L'TGNAME),TGNAME                                           
         NI    SPOOLIND,X'FF'-SPNSPACE                                          
         B     XIT                                                              
         SPACE 2                                                                
*              MID HOOK ROUTINE                                                 
*                                                                               
MDHOOK   NTR1                                                                   
         OI    SPOOLIND,SPNSPACE   SET NO BLANK AFTER MIDLINES                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT A LINE                                          
*                                                                               
PRINTIT  NTR1                                                                   
         CLI   COUNTOPT,C'Y'       IF ONLY COUNTING LINES                       
         BNE   PRINTIT5                                                         
         ZIC   R1,HLINES           INCREMENT COUNT ONLY                         
         LA    R1,1(R1)                                                         
         STC   R1,HLINES                                                        
         B     XIT                                                              
*                                                                               
PRINTIT5 MVI   RCSUBPRG,0          SET BASIC HEADLINES                          
         CLI   PROCSW,PROCCOM      IF COMMERCIAL MODE (ACTUAL OR HYPO)          
         BNE   *+8                                                              
         MVI   RCSUBPRG,10         SET COMMERCIAL MIDLINES                      
         CLI   PROCSW,PROCPRF      IF ACTUAL PERFORMER                          
         BNE   *+8                                                              
         MVI   RCSUBPRG,20         SET PERFORMER MIDLINES                       
         CLI   PROCSW,PROCHPRF     IF HYPO PERFORMER                            
         BNE   *+8                                                              
         MVI   RCSUBPRG,30         SET HYPO PERFORMER MIDLINES                  
*                                                                               
         CLI   RCSUBPRG,20         IF USING PERFORMER MIDLINES                  
         BNE   PRINTIT8                                                         
         TM    TGSYSTAT,TASYSPID   AND USING PID#                               
         BZ    PRINTIT8                                                         
         MVI   RCSUBPRG,25         USE PID PERFORMER MIDLINES                   
*                                                                               
PRINTIT8 GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT A BLANK LINE                                    
         SPACE 1                                                                
PRTBLANK NTR1                                                                   
         CLI   COUNTOPT,C'Y'       IF NOT COUNTING LINES                        
         BE    XIT                                                              
         MVI   RCSUBPRG,0          SET BASIC HEADLINES                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT OUT ESTTAB                                      
*                                                                               
TRACETAB NTR1                                                                   
         CLI   TRACEOPT,C'N'       IF TRACING                                   
         BE    XIT                                                              
         LA    R2,ESTD                                                          
         AHI   R2,ESTTAB-ESTD                                                   
         L     RF,TABCNT                                                        
         MH    RF,=Y(TABLNQ)                                                    
         GOTO1 MYTRACE,DMCB,=C'ESTTAB',(R2),(RF)                                
         B     XIT                                                              
         SPACE 2                                                                
MYTRACE  NTR1                                                                   
         LM    R2,R4,0(R1)                                                      
         ZIC   RF,0(R1)                                                         
         GOTO1 TRACE,DMCB,(R3),(R4),(R2),(RF)                                   
         B     XIT                                                              
         EJECT                                                                  
         GETEL (R4),DATADISP,ELCODE                                             
         SPACE 2                                                                
LTCLI    DC    C'CLIENT'                                                        
LTPRD    DC    C'PRODUCT'                                                       
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              SPECS TO COVER PRINTED ESTIMATE                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SPROG 0,1,10,20,25,30                                                  
         SSPEC H1,2,RUN                                                         
         SSPEC H1,58,C'TALENT ESTIMATE'                                         
         SSPEC H2,58,15X'BF'                                                    
         SSPEC H1,97,REPORT                                                     
         SSPEC H1,111,PAGE                                                      
         SSPEC H2,97,REQUESTOR                                                  
         SSPEC H4,2,C'AGENCY'                                                   
         SSPEC H4,97,C'ESTIMATE'                                                
*                                                                               
         SPROG 10                                                               
         SSPEC M1,10,C'COMML ID     MED LEN TYPE'                               
         SSPEC M2,10,C'--------     --- --- ----'                               
         SSPEC M1,40,C'LIFT ID      LEN TITLE'                                  
         SSPEC M2,40,C'-------      --- -----'                                  
         SSPEC M1,82,C'COMMERCIAL DETAILS'                                      
         SSPEC M2,82,C'------------------'                                      
*                                                                               
         SPROG 20                                                               
         SSPEC M1,23,C'S/S NUMB  NAME'                                          
         SSPEC M2,23,C'--------  ----'                                          
         SSPEC M1,57,C'CAT DWC LFT OV1'                                         
         SSPEC M2,57,C'--- --- --- --- '                                        
         SSPEC M1,82,C'PERFORMER DETAILS'                                       
         SSPEC M2,82,C'-----------------'                                       
*                                                                               
         SPROG 25                                                               
         SSPEC M1,23,C'PID NUMB  NAME'                                          
         SSPEC M2,23,C'--------  ----'                                          
         SSPEC M1,57,C'CAT DWC LFT OV1'                                         
         SSPEC M2,57,C'--- --- --- --- '                                        
         SSPEC M1,82,C'PERFORMER DETAILS'                                       
         SSPEC M2,82,C'-----------------'                                       
*                                                                               
         SPROG 30                                                               
         SSPEC M1,23,C'NUM CAT CAM UNI LFT DBL CRP'                             
         SSPEC M2,23,C'--- --- --- --- --- --- ---'                             
         SSPEC M1,82,C'DETAILS'                                                 
         SSPEC M2,82,C'-------'                                                 
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE TAGENEOPTS                                                     
         EJECT                                                                  
*              OPTION TABLE  (SEE DSECT OPTD)                                   
*              IF ADDING ENTRY HERE, MUST ADD TO OPTTAB IN TAGEN2C              
*              AND IN TAGENDF                                                   
         SPACE 1                                                                
OPTTAB   DS    0C                                                               
         DC    AL1(OPADDL,LOPADDL,0)                                            
         DC    AL2(OUTPERF-T70206)                                              
         DC    CL10'ADDITIONAL'          ADD PERFORMERS                         
*                                                                               
         DC    AL1(OPPERF,LOPPERF,0)                                            
         DC    AL2(OUTPERF-T70206)                                              
         DC    CL10'PERFORMERS'          DISPLAY PERFORMER LIST                 
*                                                                               
         DC    AL1(OPX,LOPX,0)                                                  
         DC    AL2(OUTLHS1-T70206)                                              
         DC    CL10'XCLUDE'              EXCLUDE PERFORMER                      
*                                                                               
         DC    AL1(OPYES,LOPYES,0)                                              
         DC    AL2(OUTLHS1-T70206)                                              
         DC    CL10'YES'                 INCLUDE COMMERCIAL                     
*                                                                               
         DC    AL1(OPBROAD,LOPBROAD,0)                                          
         DC    AL2(OUTPRBR-T70206)                                              
         DC    CL10'BROADCAST'           WHERE BROADCAST                        
*                                                                               
         DC    AL1(OPCANTX,LOPCANTX,0)                                          
         DC    AL2(OUTAMT3-T70206)                                              
         DC    CL10'CANTAX'              CANADIAN TAX RATE OVERRIDE             
*                                                                               
         DC    AL1(OPCOMM,LOPCOMM,0)                                            
         DC    AL2(OUTAMT3-T70206)                                              
         DC    CL10'COMMISSION'          COMMISSION RATE OVERRIDE               
*                                                                               
         DC    AL1(OPEORTX,LOPEORTX,0)                                          
         DC    AL2(OUTAMT3-T70206)                                              
         DC    CL10'EORTAX'              E-O-R TAX RATE OVERRIDE                
*                                                                               
         DC    AL1(OPEXCH,LOPEXCH,0)                                            
         DC    AL2(OUTEXCH-T70206)                                              
         DC    CL10'EXCHANGE'            EXCHANGE RATE                          
*                                                                               
         DC    AL1(OPEXP,LOPEXP,0)                                              
         DC    AL2(OUTEXP-T70206)                                               
         DC    CL10'EXPIRY'              EXPIRY DATE OVERRIDE                   
*                                                                               
         DC    AL1(OPINCTX,LOPINCTX,0)                                          
         DC    AL2(OUTAMT3-T70206)                                              
         DC    CL10'INCTAX'              INCORPORATED TAX RATE OVERRIDE         
*                                                                               
         DC    AL1(OPINTEG,LOPINTEG,0)                                          
         DC    AL2(OUTAMT3-T70206)                                              
         DC    CL10'INTEGRATE'           INTEGRATION FEES                       
*                                                                               
         DC    AL1(OPLLIFT,LOPLLIFT,0)                                          
         DC    AL2(OUTBINR-T70206)                                              
         DC    CL10'LLIFT'               L'LIFT                                 
*                                                                               
         DC    AL1(OPLLIF2,LOPLLIF2,0)                                          
         DC    AL2(OUTBINR-T70206)                                              
         DC    CL10'LLIF2'               L'2ND LIFT                             
*                                                                               
         DC    AL1(OPMULT,LOPMULT,0)                                            
         DC    AL2(OUTAMT3-T70206)                                              
         DC    CL10'MULTIPLIER'          MULTIPLIER                             
*                                                                               
         DC    AL1(OPOVER,LOPOVER,0)                                            
         DC    AL2(OUTAMT2-T70206)                                              
         DC    CL10'OVERSCALE'           USE OVERSCALE PERCENTAGE               
*                                                                               
         DC    AL1(OPOVER2,LOPOV2,0)                                            
         DC    AL2(OUTOVER2-T70206)                                             
         DC    CL10'OVERSCALE'           SECOND USE OVERSCALE PCT               
*                                                                               
         DC    AL1(OPOV12,LOPOV12,0)                                            
         DC    AL2(OUTOV12-T70206)                                              
         DC    CL10'OVERSCALE'           USE OVERSCALE PCT 1 & 2                
*                                                                               
         DC    AL1(OPYRAS,LOPYRAS,OPTNOLHS)                                     
         DC    AL2(OUTYRAS-T70206)       YEAR AS/OF DATE                        
*                                                                               
         DC    AL1(OPOVAS,LOPOVAS,OPTNOLHS)                                     
         DC    AL2(OUTOVAS-T70206)       OVERSCALE AS/OF DATE                   
*                                                                               
         DC    AL1(OPOV2AS,LOPOV2AS,OPTNOLHS)                                   
         DC    AL2(OUTOV2AS-T70206)      OV2 AS/OF DATE                         
*                                                                               
         DC    AL1(OPOV12AS,LOPOV12A,OPTNOLHS)                                  
         DC    AL2(OUTOV12A-T70206)      OV 1&2 AS/OF DATE                      
*                                                                               
         DC    AL1(OPPROD,LOPPROD,0)                                            
         DC    AL2(OUTPRBR-T70206)                                              
         DC    CL10'PRODUCED'            WHERE PRODUCED                         
*                                                                               
         DC    AL1(OPCTYP,LOPCTYP,0)                                            
         DC    AL2(OUTRHS2-T70206)                                              
         DC    CL10'TYPE'                COMMERCIAL TYPE                        
*                                                                               
         DC    AL1(OPYEAR,LOPYEAR,0)                                            
         DC    AL2(OUTYEAR-T70206)                                              
         DC    CL10'YEAR'                CONTRACT YEAR OVERRIDE                 
*                                                                               
         DC    AL1(OPUSE,LOPUSE,OPTNOLHS)                                       
         DC    AL2(OUTUSE-T70206)        USE TYPES                              
*                                                                               
         DC    AL1(OPASOF,LOPASOF,OPTNOLHS)                                     
         DC    AL2(OUTASOF-T70206)       AS OF DATES                            
*                                                                               
         DC    AL1(OPGRTNO,LOPGRTNO,OPTNOLHS)                                   
         DC    AL2(OUTGRTNO-T70206)      GUARANTEE NUMBER                       
*                                                                               
         DC    AL1(OPPRI,LOPPRI,0)                                              
         DC    AL2(OUTLHS3-T70206)       PRIMARY COMML IND.                     
         DC    CL10'PRIMARY'                                                    
*                                                                               
         DC    AL1(OPSSN,LOPSSN,OPTNOLHS)                                       
         DC    AL2(OUTSSN-T70206)        SOCIAL SECURITY NO.                    
*                                                                               
         DC    AL1(OPGUAR,LOPGUAR,0)                                            
         DC    AL2(OUTRHS1-T70206)       GRT CREDITTING STAT                    
         DC    CL10'GUARANTEE'                                                  
*                                                                               
         DC    AL1(OPMAJOR,LOPMAJOR,OPTNOLHS)                                   
         DC    AL2(OUTMAJ-T70206)        MAJORS                                 
*                                                                               
         DC    AL1(OPUNITS,LOPUNITS,OPTNOLHS)                                   
         DC    AL2(OUTBIN-T70206)        UNITS                                  
*                                                                               
         DC    AL1(OPUSES,LOPUSES,OPTNOLHS)                                     
         DC    AL2(OUTBIN-T70206)        NUMBER OF USES                         
*                                                                               
         DC    AL1(OPUSES2,LOPUSES2,OPTNOLHS)                                   
         DC    AL2(OUTUSES2-T70206)      SPECIFIC USE NOS.                      
*                                                                               
         DC    AL1(OPLCLAU,LOPLCLAU,OPTNOLHS)                                   
         DC    AL2(OUTBIN-T70206)        LAST CLA USE                           
*                                                                               
         DC    AL1(OPINS,LOPINS,OPTNOLHS)                                       
         DC    AL2(OUTBIN-T70206)        NUMBER OF INSERTS                      
*                                                                               
         DC    AL1(OPTAG,LOPTAG,OPTNOLHS)                                       
         DC    AL2(OUTBIN1-T70206)       NUMBER OF TAGS                         
*                                                                               
         DC    AL1(OPDEMO,LOPDEMO,OPTNOLHS)                                     
         DC    AL2(OUTBIN1-T70206)       NUMBER OF DEMOS                        
*                                                                               
         DC    AL1(OPPCT,LOPPCT,OPTNOLHS)                                       
         DC    AL2(OUTPCT-T70206)        PERCENTAGES                            
*                                                                               
         DC    AL1(OPAMT,LOPAMT,OPTNOLHS)                                       
         DC    AL2(OUTAMTL-T70206)       AMOUNTS                                
*                                                                               
         DC    AL1(OPCYC,LOPCYC,OPTNOLHS)                                       
         DC    AL2(OUTCYC-T70206)        CYCLE DATES                            
*                                                                               
         DC    AL1(OPDATE,LOPDATE,OPTNOLHS)                                     
         DC    AL2(OUTDATEL-T70206)      DATES                                  
*                                                                               
         DC    AL1(OPHIST,LOPHIST,0)                                            
         DC    AL2(OUTLHS1-T70206)                                              
         DC    CL10'HISTORY'             HISTORY ONLY - DON'T PAY               
*                                                                               
         DC    AL1(OPL,LOPL,0)                                                  
         DC    AL2(OUTLHS1-T70206)                                              
         DC    CL10'L'                   PAYMENT TO LIFT                        
*                                                                               
         DC    AL1(OPS,LOPS,0)                                                  
         DC    AL2(OUTLHS1-T70206)                                              
         DC    CL10'S'                   PAYMENT TO 2ND LIFT                    
*                                                                               
         DC    AL1(OPLIFT,LOPLIFT,0)                                            
         DC    AL2(OUTLHS2-T70206)                                              
         DC    CL10'LIFT'                PERFORMER ON LIFT                      
*                                                                               
         DC    AL1(OPSLIFT,LOPSLIFT,0)                                          
         DC    AL2(OUTLHS2-T70206)                                              
         DC    CL10'SLIFT'               PERFORMER ON 2ND LIFT                  
*                                                                               
         DC    AL1(OPALLIN,LOPALLIN,0)                                          
         DC    AL2(OUTLHS2-T70206)                                              
         DC    CL10'ALL'                 PERFORMER ON ALL                       
*                                                                               
         DC    AL1(OPLONLY,LOPLONLY,0)                                          
         DC    AL2(OUTLHS2-T70206)                                              
         DC    CL10'LONLY'               PERFORMER ON LIFT ONLY                 
*                                                                               
         DC    AL1(OPSONLY,LOPSONLY,0)                                          
         DC    AL2(OUTLHS2-T70206)                                              
         DC    CL10'SONLY'               PERFORMER ON 2ND LIFT ONLY             
*                                                                               
         DC    AL1(OPFSONLY,LOPFSNLY,0)                                         
         DC    AL2(OUTLHS2-T70206)                                              
         DC    CL10'FSONLY'              PERFORMER ON 1ST AND 2ND LIFT          
*                                                                               
         DC    AL1(OPPNH,LOPPNH,0)                                              
         DC    AL2(OUTAMT3-T70206)                                              
         DC    CL10'P&&H'                P&H AMOUNT                             
*                                                                               
         DC    AL1(OPSPNH,LOPSPNH,0)                                            
         DC    AL2(OUTAMT4-T70206)                                              
         DC    CL10'SP&&H'               SUBJECT TO P&H AMOUNT                  
*                                                                               
         DC    AL1(OPPNHR,LOPPNHR,0)                                            
         DC    AL2(OUTAMT4-T70206)                                              
         DC    CL10'P&&HR'               P&H RATE                               
*                                                                               
         DC    AL1(OPNO,LOPNO,0)                                                
         DC    AL2(OUTLHS1-T70206)                                              
         DC    CL10'NO'                  DON'T INCLUDE                          
*                                                                               
         DC    AL1(OPUK,LOPUK,0)                                                
         DC    AL2(OUTLHS2-T70206)                                              
         DC    CL10'UK'                  UK FOR FOREIGN USE                     
*                                                                               
         DC    AL1(OPAPPLY,LOPAPPLY,0)                                          
         DC    AL2(OUTRHS1-T70206)                                              
         DC    CL10'APPLY'               APPLY SESSION/HLD                      
*                                                                               
         DC    AL1(OPLAST,LOPLAST,0)                                            
         DC    AL2(OUTDATER-T70206)                                             
         DC    CL10'LAST SERV'           LAST SERVICES DATE                     
*                                                                               
         DC    AL1(OPSTATE,LOPSTATE,0)                                          
         DC    AL2(OUTRHS2-T70206)                                              
         DC    CL10'STATE'               ADDENDUM STATE                         
*                                                                               
         DC    AL1(OPAFM,LOPAFM,0)                                              
         DC    AL2(OUTRHS3-T70206)                                              
         DC    CL10'AFM RATE'            AFM RATE                               
*                                                                               
         DC    AL1(OPCOMNM,LOPCOMNM,OPTNOLHS)                                   
         DC    AL2(OUTCOMNM-T70206)      HYPO COMML NAME                        
*                                                                               
         DC    AL1(OPNAP,LOPNAP,0)                                              
         DC    AL2(OUTDATER-T70206)                                             
         DC    CL10'NAP'                 NO AUTO PAYMENTS                       
*                                                                               
         DC    AL1(OPATYP,LOPATYP,0)                                            
         DC    AL2(OUTATYPE-T70206)                                             
         DC    CL10'ATYPE'               COMMERCIAL ACTRA TYPE                  
*                                                                               
         DC    AL1(OPCSF,LOPCSF,0)                                              
         DC    AL2(OUTCSF-T70206)                                               
         DC    CL10'CSF'                 CONTRACT SERVICE FEE                   
*                                                                               
         DC    AL1(OPHLD,LOPHLD,0)                                              
         DC    AL2(OUTAMT3-T70206)                                              
         DC    CL10'HLD'                 HOLDING FEE AMOUNT                     
*                                                                               
         DC    AL1(OPHLDR,LOPHLDR,0)                                            
         DC    AL2(OUTWPCTR-T70206)                                             
         DC    CL10'HLD'                 HOLDING FEE RATE                       
*                                                                               
         DC    AL1(OPHLDAS,LOPHLDAS,OPTNOLHS)                                   
         DC    AL2(OUTHLDAS-T70206)      HOLDING FEE AMT AS/OF DATE             
*                                                                               
         DC    AL1(OPHLDRAS,LOPHLDRA,OPTNOLHS)                                  
         DC    AL2(OUTHLDRA-T70206)      HOLDING FEE RATE AS/OF DATE            
*                                                                               
         DC    AL1(OP1ST,LOP1ST,0)                                              
         DC    AL2(OUTDATER-T70206)                                             
         DC    CL10'1ST'                 FIRST FIXED CYCLE                      
*                                                                               
         DC    AL1(OPHNDTX,LOPHNDTX,0)                                          
         DC    AL2(OUTAMT3-T70206)                                              
         DC    CL10'HNDTAX'              INCORPORATED TAX AMT OVERRIDE          
*                                                                               
         DC    AL1(OPPAY1,LOPPAY1,0)                                            
         DC    AL2(OUTBIN-T70206)        NUMBER OF USES                         
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE TACHPACK                                                       
*              DSECT TO COVER LOCAL SAVED STORAGE FOR PROGRAM                   
         SPACE 1                                                                
ESTD     DSECT                                                                  
*                                                                               
AESTIO   DS    A                   A(ESTIMATE RECORD)                           
AWRKIO   DS    A                   A(WRKIO)                                     
SVHDHOOK DS    A                   A(HEADHOOK)                                  
SVMDHOOK DS    A                   A(MIDHOOK)                                   
SVSPECS  DS    A                   A(SPECS)                                     
SVRCSUB  DS    CL1                 RCSUBPRG                                     
*                                                                               
SAVER4   DS    F                   REGISTER 4                                   
SVPARM1  DS    F                   SAVED DMCB(4) FROM UNSCAN                    
TABCNT   DS    F                   COUNT OF TABLE ENTRIES                       
LSTPG    DS    XL1                 LAST PAGE OF HYPO PERFORMER SET              
TRACEOPT DS    CL1                 N = DON'T TRACE                              
PROCSW   DS    XL1                 PROCCESSING SWITCH                           
PROCPRF  EQU   X'80'               PERFORMER MODE                               
PROCHPRF EQU   X'40'               HYPO PERFORMER MODE                          
PROCCOM  EQU   X'20'               COMMERCIAL MODE (HYPO OR ACTUAL)             
PROCCLI  EQU   X'10'               CLIENT MODE                                  
PROCPRD  EQU   X'08'               PRODUCT MODE                                 
*                                                                               
FRSTIME  DS    CL1                 FIRST TIME INDICATOR                         
STATUS2  DS    XL1                 STATUS NEEDED FOR TAESTOPTR                  
DOPPERF  EQU   X'02'               TAESTPRT DOESN'T NEED IT                     
SVCLI    DS    CL6                 SAVED CLIENT                                 
SVPRD    DS    CL6                 SAVED PRODUCT                                
HLINES   DS    XL1                 NUMBER OF LINES USED FOR OPTIONS             
COUNTOPT DS    XL1                 Y=SET HLINES FOR TAESD/TAESTHPE ONLY         
*                                                                               
DUMSCRNH DS    CL8                 DUMMY SCREEN HEADER FOR UNSCAN               
DUMSCRN  DS    CL(L'PRTDETLS)                                                   
*                                                                               
RELO     DS    F                                                                
MYWORK   DS    CL20                MISC. WORKING AREA                           
         DS    0D                                                               
WRKIO    DS    CL4000              WORKING IO                                   
*                                                                               
MAXETAB  EQU   200                 MAXIMUM NUMBER OF ENTRIES                    
ESTTAB   DS    (MAXETAB*TABLNQ)C   ESTIMATE TABLE                               
*                                                                               
MYBLOCK  DS    7650C               BLOCK TO UNSCAN USE DETAILS                  
*                                  (255 X 30 - MAX N'USES X LENGTH)             
ESTLNQ   EQU   *-ESTD                                                           
         EJECT                                                                  
*              DSECT TO COVER ESTIMATE TABLE                                    
TABD     DSECT                                                                  
TABCLI   DS    CL6                 CLIENT CODE                                  
TABPRD   DS    CL6                 PRODUCT CODE                                 
TABTYPE  DS    XL1                 ESTIMATE ELEMENT TYPE                        
TABKEYLQ EQU   *-TABD              LENGTH OF KEY IN TABLE                       
TABCOM   DS    CL12                ACTUAL/HYPO DATA INFO                        
TABSEQ   DS    XL1                 ELEMENT SEQUENCE NUMBER                      
         ORG                                                                    
TABLNQ   EQU   *-TABD              LENGTH OF TABLE ENTRY                        
         SPACE 2                                                                
*              DSECT TO COVER OPTION TABLE                                      
         SPACE 1                                                                
OPTD     DSECT                                                                  
OPTCODE  DS    XL1                 EQUATE IDENTIFYING THIS OPTION               
OPTSUBLN DS    XL1                 L'SUB-ELEMENT DATA                           
OPTSTAT  DS    XL1                                                              
OPTNOLHS EQU   X'80'               NO LHS IN TABLE-USE I/P RTN TO CHECK         
OPTODSP  DS    AL2                 DISPLACEMENT TO OUTPUT ROUTINE               
OPTNEXT  EQU   *                                                                
OPTLHS   DS    CL10                LHS OF OPTION  (OPTIONAL)                    
OPTNEXT2 EQU   *                                                                
         EJECT                                                                  
*              DSECT TO COVER PRINT LINES                                       
         SPACE 1                                                                
PRTD     DSECT                                                                  
*                                                                               
         ORG   PRTD                CLIENT PRINT LINE                            
PRTLCLI  DS    CL6                 LITERAL 'CLIENT'                             
         DS    CL2                                                              
PRTCLI   DS    CL6                 CLIENT CODE                                  
         DS    CL1                                                              
PRTCNAME DS    CL36                CLIENT NAME                                  
         SPACE 1                                                                
         ORG   PRTD                PRODUCT PRINT LINE                           
PRTLPRD  DS    CL7                 LITERAL 'PRODUCT'                            
         DS    CL1                                                              
PRTPRD   DS    CL6                 PRODUCT CODE                                 
         DS    CL1                                                              
PRTPNAME DS    CL36                PRODUCT NAME                                 
         SPACE 1                                                                
         ORG   PRTD+8              COMMERCIAL PRINT LINE                        
PRTCCID  DS    CL12                COMMERCIAL ID                                
PRTCCIDL DS    CL1                 C'*' IF CID HAS CHANGED                      
PRTCMED  DS    CL3                 MEDIA                                        
         DS    CL1                                                              
PRTCSEC  DS    CL3                 LENGTH                                       
         DS    CL1                                                              
PRTCTYPE DS    CL7                 TYPE                                         
         DS    CL2                                                              
PRTCLID  DS    CL12                LIFT ID                                      
         DS    CL1                                                              
PRTCLSEC DS    CL3                 LENGTH OF LIFT ID                            
         DS    CL1                                                              
PRTCTTLE DS    CL24                TITLE                                        
         SPACE 1                                                                
         ORG   PRTD+8              HYPO COMMERCIAL PRINT LINE                   
PRTHCID  DS    CL12                HYPO COMMERCIAL ID                           
         DS    CL1                                                              
PRTHMED  DS    CL3                 COMMERCIAL MEDIA                             
         DS    CL1                                                              
PRTHSEC  DS    CL3                 LENGTH                                       
         DS    CL1                                                              
PRTHTYPE DS    CL8                 **HYPO**                                     
         DS    CL18                                                             
PRTHNME  DS    CL24                NAME                                         
         ORG   PRTHNME                                                          
HCOMNAME DS    CL24                LABEL NEEDED FOR TAESTOPTR                   
         SPACE 1                                                                
         ORG   PRTD+21             ACTUAL PERFORMER PRINT LINE                  
PRTSSN   DS    CL9                 SSN                                          
         DS    CL1                                                              
PRTPNME  DS    CL23                NAME                                         
         DS    CL1                                                              
PRTPCAT  DS    CL3                 CATEGORY                                     
         DS    CL1                                                              
PRTPDBL  DS    CL1                 DOUBLES                                      
PRTPCAM  DS    CL1                 ON/OFF CAMERA                                
PRTPW4TY DS    CL1                 W4 TYPE                                      
         DS    CL1                                                              
PRTPLIFT DS    CL3                 PERFORMER ON LIFT                            
         DS    CL1                                                              
PRTPOV1  DS    CL7                 OVERSCALE 1 AND TWO                          
         SPACE 1                                                                
         ORG   PRTD+21             HYPO PERFORMER PRINT LINE                    
PRTHNUM  DS    CL3                 NUMBER                                       
         DS    CL1                                                              
PRTHCAT  DS    CL3                 CATEGORY                                     
         DS    CL1                                                              
PRTHCAM  DS    CL3                 CAMERA                                       
         DS    CL1                                                              
PRTHUNI  DS    CL3                 UNION                                        
         DS    CL1                                                              
PRTHLFT  DS    CL3                 LIFT                                         
         DS    CL1                                                              
PRTHDBL  DS    CL3                 DOUBLES                                      
         DS    CL1                                                              
PRTHCRP  DS    CL3                 CORP                                         
         SPACE 2                                                                
         ORG   PRTD+55             CLIENT/PRODUCT DETAILS LITERAL               
PRTGDTLS DS    CL14                                                             
         ORG   PRTD+80             COMML/PERF DETAILS                           
PRTDETLS DS    CL48                                                             
*                                                                               
         EJECT                                                                  
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* TAGENFILE                                                                     
* TAGENEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'087TAESTPRT  01/16/14'                                      
         END                                                                    
