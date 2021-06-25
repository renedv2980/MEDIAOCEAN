*          DATA SET WKBUFFER   AT LEVEL 007 AS OF 05/06/09                      
*PHASE WKBUFFA                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE DDWTO                                                                  
         TITLE 'WKBUFFER - BUFFER WORKER FILE INDEX RECORDS'                    
         PRINT NOGEN                                                            
WKBUFF   CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE WORKL,WKBUFFER,=A(WORKAREA),RA,R9                                
         USING WORKD,RC                                                         
         USING DMSPACED,DSPHD                                                   
*                                                                               
         ST    RD,SAVERD                                                        
         BRAS  RE,INIT             INITIALISE                                   
*                                                                               
         CLI   RUN,C'T'                                                         
         BNE   MAIN02                                                           
         L     RF,=A(RPTAREA)                                                   
         STCM  RF,15,DSPTFRST                                                   
         A     RF,=A(RPTAREAL-1)                                                
         STCM  RF,15,DSPTEND                                                    
         B     MAIN03                                                           
*                                                                               
MAIN02   XC    DUB,DUB             LOCK TABLE IN TABS DATASPACE                 
         MVC   DUB(4),=AL4(DTWRKR)                                              
         MVI   DUB,X'80'           SET LONG ALLOCATE                            
         GOTO1 VLOCKSPC,DUB                                                     
*                                                                               
         L     RF,4(R1)            SAVE DSPACE HEADER                           
         MVC   DSPHD,0(RF)                                                      
         NC    DSPTFRST,=XL4'0FFFFFFF'                                          
*                                                                               
         CLI   RUN,C'R'                                                         
         BE    MAIN10                                                           
*                                                                               
MAIN03   ICM   R2,15,DSPTFRST      CLEAR OUT DATASPACE BLOCK                    
         ICM   R3,15,DSPTEND                                                    
         AHI   R3,1                                                             
         SR    R3,R2                                                            
         BRAS  RE,R2ALET                                                        
         XR    RF,RF                                                            
         MVCL  R2,RE                                                            
         BRAS  RE,ARSOFF                                                        
*                                                                               
         BRAS  RE,FREEMAIN         FREE STORAGE IF ANY ALLOCATED                
         BRAS  RE,WKBUILD          BUILD INDEX IN XA AS REQUIRED                
         BRAS  RE,WKSETOK          SET INDEX READY TO USE                       
*                                                                               
         CLI   RUN,C'T'            TEST JUST EXITS TO MVS                       
         BE    MAIN10                                                           
*                                                                               
         XC    DUB,DUB             FREE TABLE IN TABS DATASPACE                 
         MVC   DUB(4),=AL4(DTWRKR)                                              
         MVI   DUB,X'10'                                                        
         GOTO1 VLOCKSPC,DUB                                                     
*                                                                               
MAIN04   BRAS  RE,WAIT             SLEEPY TIME                                  
*                                                                               
         TM    JOBSTEP,JOBSTOP     FORCE STOP POSTED?                           
         BO    MAIN06              YES                                          
         TM    JOBSTEP,JOBEOJ      FORCE EOJ POSTED?                            
         BO    MAIN06              YES                                          
*                                                                               
         TM    JOBSTEP,JOBINIT     FORCE REBUILD POSTED?                        
         BNO   MAIN04              NO                                           
         BRAS  RE,RPTONIO          OUTPUT COUNTS OF I/O                         
         B     MAIN02                                                           
*                                                                               
MAIN06   BRAS  RE,ARSOFF                                                        
         LAM   AR4,AR4,ALET          CLEAR HEADER INFO IN DATASPACE             
         ICM   R4,15,DSPTFRST                                                   
         SAC   512                                                              
         USING WKBUFFD,R4                                                       
         XC    WKBUFFID,WKBUFFID   THIS FLAGS BUFFER AVAILABLE                  
         BRAS  RE,ARSOFF                                                        
*                                                                               
MAIN10   BRAS  RE,RPTONIO          OUTPUT COUNTS OF I/O                         
         B     XBASE                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         EXTRACT RESULTS,'S',FIELDS=(TIOT,COMM,ASID)                            
         ALESERV EXTRACTH,STOKEN=ASTOKEN                                        
*                                                                               
         L     RF,RXTIOT                                                        
         MVC   JOBNAME,0(RF)       GET JOBNAME FOR THIS IMAGE                   
         MVC   XGMFNME,0(RF)       AND SAVE IT IN VARIOUS USEFUL PLACES         
         MVC   XFMFNME,0(RF)                                                    
         MVC   XGMLNME,0(RF)                                                    
*                                                                               
         BRAS  RE,PRINTI           INITIALISE PRINTING                          
         BRAS  RE,CARDIN           READ AND VALIDATE INPUT CARDS                
         BL    EXITL               CARD VALIDATION ERROR                        
*                                                                               
         CLI   RUN,C'T'                                                         
         BE    INIT02                                                           
*                                                                               
         BRAS  RE,GETSPC           BIND TO REQUESTED DATASPACE                  
         BRAS  RE,SETOPS           SET UP OPERATOR COMMS                        
*                                                                               
INIT02   GOTO1 VDMGR,DMCB,=C'OPEN',=C'SER',=C'NCTFILE X'                        
*                                                                               
         LHI   R0,4                MAKE NON-SWAPPABLE                           
         LNR   R0,R0                                                            
         SVC   247                                                              
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* READ AND VALIDATE INPUT CARDS                                       *         
***********************************************************************         
         SPACE 1                                                                
CARDIN   NTR1  ,                                                                
         LHI   R1,1                STARTED PROCESSING CARDS                     
         BRAS  RE,DOMSG                                                         
*                                                                               
CARD02   GOTO1 VCARDS,DMCB,CARD,=C'RE00'                                        
         CLC   =C'/*',CARD         END OF CARDS?                                
         BE    CARDX               NO                                           
*                                                                               
         MVC   PLINE+12(L'CARD),CARD  PRINT PARAMETER CARD                      
         XR    R1,R1                                                            
         BRAS  RE,DOMSG                                                         
*                                                                               
         L     R1,ACARDTAB                                                      
         BRAS  RE,CARDPRC          PROCESS INPUT CARD                           
         BE    CARD02                                                           
         LH    R1,ERRCNT                                                        
         AHI   R1,1                                                             
         STH   R1,ERRCNT                                                        
         B     CARD02                                                           
*                                                                               
CARDX    LHI   R1,2                FINISHED PROCESSING CARDS                    
         BRAS  RE,DOMSG                                                         
         OC    ERRCNT,ERRCNT       ERRORS ON PROCESSING?                        
         BZ    EXITOK              NO                                           
         B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD INDICES IN CORE FOR ALL WORKER FILES               *         
***********************************************************************         
         SPACE 1                                                                
WKBUILD  NTR1  ,                                                                
         LHI   R1,9                SET BUILDING WKFILE INDICES                  
         BRAS  RE,DOMSG                                                         
*                                                                               
         XC    UKEY,UKEY           GET LIST OF PQ FILES                         
         GOTO1 VDMGR,DMCB,(0,GLIST),WRKFLST,UKEY,ACXREC,ACIREC                  
         MVC   AWRKFLST,UKEY+(UKUSRINF-UKRECD)                                  
*                                                                               
         ICM   R2,15,DSPTFRST      GET A(FIRST INDEX ENTRY)                     
         AHI   R2,WKBUFFPL                                                      
*                                                                               
WKBLD02  BRAS  RE,ARSOFF           BUILD AREA IS IN TABS DSPACE                 
         XC    LCLDATA,LCLDATA     RESET LOCAL CIDATA                           
*                                                                               
         L     RF,AWRKFLST         NEXT PQ FILE IN LIST                         
         AHI   RF,8                                                             
         ST    RF,AWRKFLST                                                      
         CLI   0(RF),0             TEST END OF LIST                             
         BE    WKBLD16                                                          
*                                                                               
         USING WFINDEXD,R2                                                      
         USING CIDATAD,LCLDATA                                                  
         XC    BLOCK,BLOCK         RESET RELATIVE BLOCK NUMBER                  
         MVI   FSTBLK,C'N'         RESET FIRST BLOCK IN PQ PROCESSED            
*                                                                               
         BRAS  RE,R2ALET                                                        
         L     RF,AWRKFLST         NEXT PQ FILE IN LIST                         
         MVC   WFNINUM,0(RF)       SAVE WK FILE INTERNAL NUMBER                 
         MVC   LCLINUM,0(RF)       SAVE WK FILE INTERNAL NUMBER                 
         MVC   WFNENUM,4(RF)       SAVE WK FILE EXTERNAL NUMBER                 
         MVC   LCLADTF,4(RF)       SAVE A(DTF)                                  
         MVC   WFNID,WRKFLST       SAVE WK FILE NAME                            
         MVC   WFNID+4(1),1(RF)    SET WRKF FILE DMCB/ENQ NAME                  
         MVC   LCLID,WFNID                                                      
         BRAS  RE,ARSOFF                                                        
*                                                                               
         XC    CXADDR,CXADDR                                                    
         XC    P1(32),P1           P1 DADDS PARAM LIST FOR INDEX READS          
         MVC   P1,VDAOPEN                                                       
         MVC   P2,ACXREC                                                        
         MVC   P4,LCLADTF                                                       
         LA    RE,CXADDR                                                        
         ST    RE,P5                                                            
         GOTO1 VDADDS,P1           OPEN FILE                                    
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P1,VRDID                                                         
         MVC   CXADDR,=X'00010100'                                              
         GOTO1 VDADDS,P1           READ 1ST RECORD AND INIT EXTENTS             
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,R23ALET                                                       
         MVC   LCLDATA+00(20),CXREC      SET PART 1 INDEX DATA                  
         TM    LCLDATA,X'80'                                                    
         BZ    *+14                                                             
         MVC   LCLDATA+20(20),CXREC+L'W_INDEX  SET PART 2 INDEX DATA            
         NI    LCLDATA,X'7F'                                                    
         MVC   LCLDATA+14(1),LCLINUM     SET WRKR FILE NUM                      
         MVC   LCLDATA+15(5),LCLID       SET WRKR FILE NAME                     
*                                                                               
         LHI   RE,L'W_INDEX                                                     
         STH   RE,CINDXLN                                                       
         L     RF,LCLADTF          SET F/L RECORD SIZE IN DTF                   
         MVC   DBLKSZ-DTFPHD(L'DBLKSZ,RF),CIBLKLN                               
         OI    DBLKSZ-DTFPHD(RF),X'80'                                          
*                                                                               
         LA    R3,WFBLKFST         R3=A(INDEX BLOCK TABLE)                      
         USING WFKTABD,R3                                                       
         B     WKBLD06                                                          
*                                                                               
WKBLD04  XR    RE,RE               READ IN NEXT PAGE                            
         LH    RF,CXPAGE           XADT=CXP/TRKS + 1                            
         LH    R0,CIHIREC          XADB=REM + 1                                 
         DR    RE,R0                                                            
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         STH   RF,CXADDR                                                        
         STC   RE,CXADDR+2                                                      
         MVI   CXADDR+3,0                                                       
*                                                                               
         GOTO1 VDADDS,P1,VRDID     READ IN PAGE                                 
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AHI   R3,WFKTABLQ         FF FILL NEXT TABLE ENTRY                     
*                                                                               
WKBLD06  BRAS  RE,ARSOFF           GET OUT OF AR MODE                           
         BRAS  RE,ON31                                                          
         LH    R0,CIBLKLN                                                       
         AHI   R0,16                                                            
         BRAS  RE,GETMAIN          GET STORAGE FOR BLOCK + HEADER               
*                                                                               
         MVC   0(5,R1),LCLID                                                    
         MVC   5(11,R1),=CL11' BLCK HDRXX'                                      
*                                                                               
         BRAS  RE,R23ALET          BACK INTO AR MODE                            
         L     RF,BLOCK                                                         
         AHI   RF,1                                                             
         ST    RF,BLOCK                                                         
         STH   RF,WFKNUM           SET RELATIVE BLOCK NUMBER                    
*                                                                               
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  14(2,R1),DUB                                                     
         AHI   R1,16               GO PAST HEADER                               
         ST    R1,WFKADD           SET REAL ADDRESS                             
*                                                                               
         MVC   WFKDA,CXADDR        SET BLOCK D/A                                
         MVC   WFKLEN,CIBLKLN      SET BLOCK LENGTH                             
*                                                                               
         L     RE,WFKADD           MOVE BUFFER INTO LOCAL BLOCK                 
         LH    RF,WFKLEN                                                        
         L     R0,ACXREC                                                        
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,OFF31                                                         
*                                                                               
         CLI   FSTBLK,C'Y'         FIRST BLOCK FOR THIS WORKER FILE             
         BE    WKBLD08             NO                                           
         MVI   FSTBLK,C'Y'         SET PROCESSED                                
*                                                                               
         XC    CXPAGE,CXPAGE                                                    
         LH    R5,CICINDX                                                       
         STH   R5,CXENTRY                                                       
         MH    R5,CINDXLN                                                       
         A     R5,ACXREC                                                        
         USING W_RECD,R5           R5=A(PRTQUE INDEX RECORD)                    
*                                                                               
WKBLD08  CLC   W_RECD(2),=X'FFFF'  END OF THE INDEX?                            
         BE    WKBLD12             YES                                          
*                                                                               
         LH    RF,CXENTRY          BUMP TO NEXT INDEX ENTRY                     
         LA    RF,1(RF)                                                         
         STH   RF,CXENTRY                                                       
         CH    RF,CIENTRYS                                                      
         BNL   WKBLD10                                                          
         AH    R5,CINDXLN                                                       
         B     WKBLD08                                                          
*                                                                               
WKBLD10  XC    CXENTRY,CXENTRY                                                  
         LH    RF,CXPAGE                                                        
         LA    RF,1(RF)                                                         
         STH   RF,CXPAGE                                                        
         L     R5,ACXREC                                                        
         B     WKBLD04           END OF INDEX PAGE                              
*                                                                               
WKBLD12  MVC   HALF,CXPAGE       SAVE LAST INDEX PAGE NUMBER                    
         CLC   CXPAGE(4),CJPAGE                                                 
         BH    WKBLD14           EXIT IF END OF PART 2 INDEX                    
         OC    CJCITOT,CJCITOT                                                  
         BZ    WKBLD14           EXIT IF NO PART 2 INDEX                        
*                                                                               
         MVC   CXPAGE,CJPAGE                                                    
         LH    R5,CJENTRY                                                       
         STH   R5,CXENTRY                                                       
         MH    R5,CINDXLN                                                       
         A     R5,ACXREC                                                        
         CLC   CXPAGE,HALF                                                      
         BE    WKBLD08             STARTS IN SAME PAGE AS END OF PART 1         
         B     WKBLD04                                                          
*                                                                               
WKBLD14  BRAS  RE,R23ALET          REACHED END FOR THIS WRKR FILE               
         AHI   R3,WFKTABLQ                                                      
         MVC   0(16,R3),FF32       SET LAST ENTRY TO FF'S                       
         AHI   R3,WFKTABLQ                                                      
         ST    R2,LASTWKB          SAVE LAST WRKR FILE PROCESSED                
         MVC   WFNCNT,BLOCK                                                     
         ST    R3,WFNXID           SET A(NEXT WRKR FILE)                        
         LR    R2,R3               GO TO NEXT FREE SPACE                        
         B     WKBLD02             AND CONTINUE WITH THE NEXT ONE               
*                                                                               
WKBLD16  BRAS  RE,R2ALET                                                        
         L     R2,LASTWKB                                                       
         MVC   WFNXID,FF32         FLAG THIS AS LAST WRKR FILE                  
         BRAS  RE,ARSOFF                                                        
*                                                                               
         LHI   R1,10               DONE BUILDING WKFILE INDICES                 
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SET BUFFER OK TO USE INFORMATION                                    *         
***********************************************************************         
         SPACE 1                                                                
WKSETOK  NTR1  ,                                                                
         XR    R0,R0                                                            
         BRAS  RE,TIMER            GET CURRENT DATE AND TIME                    
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(5,0),(21,WORK),0                                    
*                                                                               
         ICM   R2,15,DSPTFRST                                                   
         BRAS  RE,R2ALET           SET HEADER INFO IN DATASPACE                 
         USING WKBUFFD,R2                                                       
         MVC   WKBUFFID,=CL16'*WKBUFFER INFO**'                                 
         MVC   WKBUFFST,=CL08'Stoken= '                                         
         MVC   WKBUFFLT,=CL12'Last Action='                                     
         MVC   WKBUFFDT,=CL05'Date='                                            
         MVC   WKBUFFTM,=CL05'Time='                                            
         MVC   WKBUFFNL,=CL08'I/O Onl='                                         
         MVC   WKBUFFFL,=CL08'I/O Ofl='                                         
         MVC   WKBUFFIX,=CL16'*WRKR FILE INDEX'                                 
*                                                                               
         MVC   WKBUFFLV,=CL04'Init'                                             
         TM    JOBSTEP,JOBINIT     REBUILD?                                     
         BZ    *+10                NO                                           
         MVC   WKBUFFLV,=CL04'Rbld'                                             
         MVC   WKBUFFSV,ASTOKEN                                                 
         MVC   WKBUFFDV,WORK                                                    
         MVC   WKBUFFTV,TIME                                                    
         ZAP   WKBUFFNV,PZERO                                                   
         ZAP   WKBUFFFV,PZERO                                                   
         BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FREE ALL STORAGE OBTAINED IN GMLIST                                 *         
***********************************************************************         
         SPACE 1                                                                
FREEMAIN NTR1  ,                                                                
         L     R2,AGMLIST                                                       
         OC    0(L'GMLIST,R2),0(R2)                                             
         BNZ   FMN02                                                            
         LHI   R1,11               FREEMAIN NOT NECESSARY                       
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
*                                                                               
FMN02    CLI   0(R2),EOT                                                        
         BE    EXITOK              END OF TABLE                                 
         OC    0(L'GMLIST,R2),0(R2)                                             
         BZ    EXITOK              LAST ENTRY PROCESSED                         
*                                                                               
         LM    R0,R1,0(R2)                                                      
         FREEMAIN RC,A=(1),LV=(0)  FREE STORAGE                                 
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         LA    R0,XFMFAIL                                                       
         BRAS  RE,WTODIE                                                        
*                                                                               
         XC    0(L'GMLIST,R2),0(R2)                                             
         AHI   R2,L'GMLIST                                                      
         B     FMN02                                                            
         EJECT                                                                  
***********************************************************************         
* REPORT ON I/O SAVINGS FROM DATASPACE                                *         
***********************************************************************         
         SPACE 1                                                                
RPTONIO  NTR1  ,                                                                
         LHI   R1,20               BEGAN REPORTING ON IO COUNTS                 
         BRAS  RE,DOMSG                                                         
*                                                                               
         XC    TOTALS,TOTALS                                                    
         CLI   RUN,C'T'                                                         
         BE    RPP04                                                            
*                                                                               
         BRAS  RE,ON31                                                          
         BRAS  RE,ARSOFF                                                        
         ICM   R2,15,DSPTFRST                                                   
         ICM   R3,15,DSPTEND                                                    
         AHI   R3,1                                                             
         SR    R3,R2                                                            
         C     R3,=A(RPTAREAL)                                                  
         BNH   RPP02                                                            
*                                                                               
         BRAS  RE,ARSOFF                                                        
         MVC   PLINE+15(40),=CL40'CANNOT REPORT - INCREASE RPTAREA'             
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+15(40),=CL40'CANNOT REPORT - INCREASE RPTAREA'             
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
*                                                                               
RPP02    BRAS  RE,R2ALET                                                        
         L     RE,=A(RPTAREA)      COPY ENTIRE AREA LOCALLY                     
         L     RF,=A(RPTAREAL)                                                  
         MVCL  RE,R2                                                            
         BRAS  RE,ARSOFF                                                        
*                                                                               
RPP04    BRAS  RE,OFF31                                                         
         MVC   PLINE+13(H1LINEL),H1LINE                                         
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+13(H2LINEL),H2LINE                                         
         BRAS  RE,PRINTL                                                        
*                                                                               
         L     R4,=A(RPTAREA)                                                   
         AHI   R4,WKBUFFPL                                                      
         USING WFINDEXD,R4                                                      
*                                                                               
RPP06    CLC   WFNXID,FF32         TABLE TERMINATOR                             
         BE    RPP10               YES                                          
*                                                                               
         MVC   PLINE+13(L'WFNID),WFNID                                          
         LA    R5,WFINDEXL(R4)                                                  
         USING WFKTABD,R5                                                       
*                                                                               
RPP08    LA    R0,WFKDA                                                         
         GOTO1 VHEXOUT,DMCB,(R0),PLINE+19,4,0                                   
         L     R0,WFKONRD                                                       
         EDIT  (R0),(12,PLINE+28),0,COMMAS=YES,ZERO=NOBLANK                     
         L     R0,WFKOFRD                                                       
         EDIT  (R0),(12,PLINE+41),0,COMMAS=YES,ZERO=NOBLANK                     
         L     R0,WFKONWT                                                       
         EDIT  (R0),(12,PLINE+54),0,COMMAS=YES,ZERO=NOBLANK                     
         L     R0,WFKOFWT                                                       
         EDIT  (R0),(12,PLINE+67),0,COMMAS=YES,ZERO=NOBLANK                     
         BRAS  RE,PRINTL                                                        
*                                                                               
         L     R0,TOTONR           UPDATE GRAND TOTALS                          
         A     R0,WFKONRD                                                       
         ST    R0,TOTONR                                                        
         L     R0,TOTOFR                                                        
         A     R0,WFKOFRD                                                       
         ST    R0,TOTOFR                                                        
         L     R0,TOTONW                                                        
         A     R0,WFKONWT                                                       
         ST    R0,TOTONW                                                        
         L     R0,TOTOFW                                                        
         A     R0,WFKOFWT                                                       
         ST    R0,TOTOFW                                                        
         AHI   R5,WFKTABLQ                                                      
         CLC   WFKNUM,FF32                                                      
         BNE   RPP08                                                            
*                                                                               
         L     R4,WFNXID           NEXT WORKER FILE                             
         S     R4,DSPTFRST                                                      
         A     R4,=A(RPTAREA)                                                   
         B     RPP06                                                            
*                                                                               
RPP10    MVC   PLINE+13(H3LINEL),H3LINE                                         
         BRAS  RE,PRINTL                                                        
         MVC   PLINE+13(6),=CL06'Totals'                                        
         EDIT  (B4,TOTONR),(12,PLINE+28),0,COMMAS=YES,ZERO=NOBLANK              
         EDIT  (B4,TOTOFR),(12,PLINE+41),0,COMMAS=YES,ZERO=NOBLANK              
         EDIT  (B4,TOTONW),(12,PLINE+54),0,COMMAS=YES,ZERO=NOBLANK              
         EDIT  (B4,TOTOFW),(12,PLINE+67),0,COMMAS=YES,ZERO=NOBLANK              
         BRAS  RE,PRINTL                                                        
*                                                                               
         LHI   R1,21               ENDED REPORTING ON IO COUNTS                 
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
*                                                                               
*                   12345678901234567890123456789012345678901234567890          
H1LINE   DC    CL50'File  --D/A--  -Online------------------ -Offline-'         
         DC    CL16'----------------'                                           
H1LINEL  EQU   *-H1LINE                                                         
H2LINE   DC    CL50'                       Read        Write         R'         
         DC    CL16'ead        Write'                                           
H2LINEL  EQU   *-H2LINE                                                         
H3LINE   DC    CL50'--------------------------------------------------'         
         DC    CL16'----------------'                                           
H3LINEL  EQU   *-H3LINE                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DO GETMAIN CALLS AND SAVE A() AND L' OF AREA IN GMLIST   *         
* NTRY: R0 = LENGTH OF AREA REQUIRED                                  *         
* EXIT: R0/R1 RETURNED BY GETMAIN                                  *            
*       GMLIST UPDATED AS L' FOLLOWED BY A() OF AREA                  *         
***********************************************************************         
         SPACE 1                                                                
GETMAIN  NTR1  ,                                                                
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=DBLWD                            
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         LA    R0,XGMFAIL                                                       
         BRAS  RE,WTODIE                                                        
*                                                                               
         L     RF,AGMLIST                                                       
GMN02    CLI   0(RF),EOT                                                        
         BNE   *+12                                                             
         LA    R0,XGMLIST                                                       
         BRAS  RE,WTODIE                                                        
*                                                                               
         OC    0(L'GMLIST,RF),0(RF)                                             
         BZ    *+12                                                             
         AHI   RF,L'GMLIST                                                      
         B     GMN02                                                            
*                                                                               
         STM   R0,R1,0(RF)                                                      
         XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
***********************************************************************         
* OUTPUT TIME FROM MVS TIME MACRO                                     *         
* NTRY R0 = ZERO USE CURRENT TIME (FROM MVS TIME MACRO)               *         
*      R0 = NZ   USE TIME IN HHMMSSXX IN R0                           *         
* EXIT TIME HOLDS HH:MM:SS:XX WHERE XX IS 1/100 SECS                  *         
***********************************************************************         
         SPACE 1                                                                
TIMER    NTR1  ,                                                                
         LTR   R0,R0                                                            
         BNZ   TIME02                                                           
         TIME  DEC                 R0=TIME                                      
*                                                                               
TIME02   STC   R0,TIME+10                                                       
         OI    TIME+10,X'F0'                                                    
         SRL   R0,4                                                             
         STC   R0,TIME+9           XX PORTION                                   
         OI    TIME+9,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+7                                                        
         OI    TIME+7,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+6           SS PORTION                                   
         OI    TIME+6,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+4                                                        
         OI    TIME+4,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+3           MM PORTION                                   
         OI    TIME+3,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+1                                                        
         OI    TIME+1,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+0           HH PORTION                                   
         OI    TIME+0,X'F0'                                                     
*                                                                               
         MVI   TIME+2,C':'         SET COLONS FOR HH:MM:SS:XX                   
         MVI   TIME+5,C':'                                                      
         MVI   TIME+8,C':'                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DATASPACE ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
GETSPC   NTR1  ,                                                                
         LHI   R1,3                BEGAN ATTEMPTING BIND                        
         BRAS  RE,DOMSG                                                         
*                                                                               
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'GETA'                                                 
         MVC   WORK+4(12),DSPACE                                                
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    GETSPC2                                                          
*                                                                               
         LHI   R1,5                DATASPACE BIND FAILED                        
         BRAS  RE,DOMSG                                                         
         ABEND 912,DUMP                                                         
*                                                                               
GETSPC2  MVC   OFFS,WORK+20        EXTRACT VALUES                               
         MVC   ALET,WORK+24                                                     
         MVC   STOKN,WORK+28                                                    
         OC    ALET,ALET                                                        
         BNZ   GETSPC4                                                          
*                                                                               
         LHI   R1,6                                                             
         BRAS  RE,DOMSG                                                         
         ABEND 912,DUMP                                                         
*                                  SET IT IN SSB ALSO                           
         USING SSOOFF,SSB                                                       
GETSPC4  MVC   SSODSPAC,DSPACE+3                                                
*&&US*&& CLI   SSODSPAC,C'P'                                                    
*&&US*&& BNE   *+8                                                              
*&&US*&& MVI   SSODSPAC,C'A'       Convert to file stamp standard               
         MVC   SSOTBLET,ALET                                                    
         LHI   R1,4                                                             
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET WAIT UNTIL POSTED EITHER BY OPERATOR OR FACPAK                  *         
***********************************************************************         
         SPACE 1                                                                
WAIT     NTR1  ,                                                                
         BRAS  RE,ARSOFF                                                        
         XC    JOBSTEP,JOBSTEP                                                  
         L     R1,AOPERECB         OPERATOR ECB COMES LAST                      
         WAIT  ECB=(R1)                                                         
         BRAS  RE,OPSCOMS                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SET UP OPERATOR COMMUNICATIONS                                      *         
***********************************************************************         
         SPACE 1                                                                
SETOPS   NTR1  ,                                                                
         LHI   R1,7                OUTPUT INITIALISING OPERATOR MSG             
         BRAS  RE,DOMSG                                                         
*                                                                               
         L     RF,RXCOM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         L     R2,COMCIBPT         GET A(CIB)                                   
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT         SET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTART    TEST FOR 'S JOBNAME' (IE NOT BATCH)          
         BNE   SETOP2                                                           
         QEDIT ORIGIN=(R3),BLOCK=(R2) RELEASE THE CIB                           
*                                                                               
SETOP2   QEDIT ORIGIN=(R3),CIBCTR=1   ACCEPT MODIFY COMMANDS                    
*                                                                               
         LHI   R1,8                                                             
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* PROCESS OPERATOR COMMUNICATIONS                                     *         
***********************************************************************         
         SPACE 1                                                                
OPSCOMS  NTR1  ,                                                                
         LHI   R1,12                                                            
         BRAS  RE,DOMSG                                                         
*                                                                               
         L     RF,RXCOM            SET RF TO COMM BLOCK                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         USING CIBNEXT,R2                                                       
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BNE   OPSC02                                                           
         MVI   JOBSTEP,JOBSTOP     SET EOJ FLAG                                 
         LHI   R1,13               STOP JOB MESSAGE                             
         BRAS  RE,DOMSG                                                         
         B     OPSC14                                                           
*                                                                               
OPSC02   CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    OPSC04              YES                                          
         LHI   R1,14               BAD COMMAND (UNKNOWN VERB)                   
         BRAS  RE,DOMSG                                                         
         B     OPSC14                                                           
*                                                                               
OPSC04   XR    R1,R1               GET DATA LEN IN R1                           
         ICM   R1,3,CIBDATLN                                                    
         CHI   R1,8                                                             
         BNH   OPSC06                                                           
         LHI   R1,15               BAD COMMAND (TOO LONG)                       
         BRAS  RE,DOMSG                                                         
         B     OPSC14                                                           
*                                                                               
OPSC06   CHI   R1,3                                                             
         BNE   OPSC08                                                           
         CLC   =C'EOJ',CIBDATA                                                  
         BNE   OPSC08                                                           
         MVI   JOBSTEP,JOBEOJ      SET EOJ FLAG                                 
         LHI   R1,16               EOJ MESSAGE                                  
         BRAS  RE,DOMSG                                                         
         B     OPSC14                                                           
*                                                                               
OPSC08   CHI   R1,4                                                             
         BNE   OPSC10                                                           
         CLC   =C'STOP',CIBDATA                                                 
         BNE   OPSC10                                                           
         MVI   JOBSTEP,JOBEOJ      SET EOJ FLAG                                 
         LHI   R1,16               EOJ MESSAGE                                  
         BRAS  RE,DOMSG                                                         
         B     OPSC14                                                           
*                                                                               
OPSC10   CHI   R1,7                                                             
         BNE   OPSC12                                                           
         CLC   =C'REBUILD',CIBDATA                                              
         BNE   OPSC12                                                           
         MVI   JOBSTEP,JOBINIT                                                  
         LHI   R1,17               REBUILD MESSAGE                              
         BRAS  RE,DOMSG                                                         
         B     OPSC14                                                           
*                                                                               
OPSC12   LHI   R1,18               INVALID MESSAGE                              
         BRAS  RE,DOMSG                                                         
         MVC   PLINE(22),=C'**Input message was - '                             
         XR    R1,R1                                                            
         ICM   R1,3,CIBDATLN                                                    
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   PLINE+22(0),CIBDATA                                              
         BRAS  RE,PRINTL                                                        
         MVI   JOBSTEP,JOBBAD                                                   
*                                                                               
         MVC   PLINE(32),=C'INVALID KEYWORD               //'                   
         XR    R1,R1                                                            
         ICM   R1,3,CIBDATLN                                                    
         CHI   R1,13                                                            
         BNH   *+8                                                              
         LHI   R1,13                                                            
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   PLINE+16(0),CIBDATA                                              
         GOTO1 =V(DDWTO),DMCB,PLINE,0                                           
*                                                                               
OPSC14   L     RF,RXCOM            CLEAR OPERATOR COMMAND                       
         USING COMLIST,RF                                                       
         ICM   R2,15,COMCIBPT      A(CIB)                                       
         BZ    OPSC16                                                           
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)                                           
         DROP  RF                                                               
*                                                                               
OPSC16   LHI   R1,19                                                            
         BRAS  RE,DOMSG                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS CARD VALUE FROM TABLE                                       *         
* NTRY: CARD = CARD VALUE TO PROCESS                                  *         
*       R1   = A(CORRECT TABLE)                                       *         
***********************************************************************         
         SPACE 1                                                                
CARDPRC  NTR1  ,                                                                
         LR    R3,R1                                                            
         USING CARDTABD,R3                                                      
         CLI   CARD,C'*'           * IN COL 1 IS A COMMENT                      
         BE    EXITOK                                                           
*                                                                               
         GOTO1 VSCANNER,DMCB,(C'C',CARD),(1,SCNBLK)                             
         CLI   4(R1),0                                                          
         BE    CEINVLIN            INVALID LINE                                 
*                                                                               
         LA    R2,SCNBLK                                                        
         USING SCANBLKD,R2                                                      
         XR    RF,RF                                                            
         LHI   R1,CARDTABL                                                      
*                                                                               
CPRC02   CLI   CNAME,EOT           END OF TABLE                                 
         BE    CEINVKEY            INVALID KEYWORD                              
         ICM   RF,1,CXLEN                                                       
         EX    RF,CARDCMP                                                       
         BE    CPRC04                                                           
         BXH   R3,R1,CPRC02                                                     
*                                                                               
CARDCMP  CLC   SC1STFLD(0),CNAME                                                
*                                                                               
CPRC04   CLI   CTYPE,CTNUM         NUMERIC INPUT?                               
         BNE   CPRC06              NO                                           
         TM    SC2NDVAL,SCNUMQ                                                  
         BNO   CENOTNUM                                                         
         CLC   SC2NDNUM,CMIN       SCOPE FOR MAX/MIN VALUES                     
         BL    CETOOLOW                                                         
         CLC   SC2NDNUM,CMAX                                                    
         BH    CETOOBIG                                                         
         ICM   RF,15,COUT                                                       
         MVC   0(4,RF),SC2NDNUM    SET NUMERIC VALUE INTO OUTPUT                
         B     EXITOK                                                           
*                                                                               
CPRC06   CLI   CTYPE,CTCHR         CHARACTER INPUT                              
         BNE   CPRC08              NO                                           
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BZ    CENOINP                                                          
         C     RF,CMIN             SCOPE FOR LENGTH                             
         BL    CETOOSHT                                                         
         C     RF,CMAX                                                          
         BH    CETOOLNG                                                         
         ICM   RE,15,COUT          MOVE IN FIELD                                
         ICM   RF,1,CLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),SC2NDFLD                                                 
         B     EXITOK                                                           
*                                                                               
CPRC08   CLI   CTYPE,CTNOP         NO SECOND INPUT                              
         BNE   CPRC10              NO                                           
         B     EXITOK                                                           
*                                                                               
CPRC10   CLI   CTYPE,CTNOPF        NO SECOND INPUT / TURN ON FLAG               
         BNE   CPRC12              NO                                           
         ICM   RF,15,COUT                                                       
         OC    0(L'CFLAG,RF),CFLAG                                              
         B     EXITOK                                                           
*                                                                               
CPRC12   DC    H'0'                EXTRA TYPES HERE                             
*                                                                               
CEINVLIN LA    R1,CINVLIN          INVALID LINE                                 
         B     CERR                                                             
CEINVKEY LA    R1,CINVKEY          INVALID KEYWORD                              
         B     CERR                                                             
CENOTNUM LA    R1,CNOTNUM          NOT A NUMBER                                 
         B     CERR                                                             
CENOTCHR LA    R1,CNOTCHR          NOT CHARACTER                                
         B     CERR                                                             
CETOOSHT LA    R1,CTOOSHT          TOO SHORT                                    
         B     CERR                                                             
CETOOLNG LA    R1,CTOOLNG          TOO LONG                                     
         B     CERR                                                             
CETOOLOW LA    R1,CTOOLOW          TOO SMALL                                    
         B     CERR                                                             
CETOOBIG LA    R1,CTOOBIG          TOO BIG                                      
         B     CERR                                                             
CENOINP  LA    R1,CNOINP           NO INPUT                                     
         B     CERR                                                             
*                                                                               
CERR     XR    R0,R0                                                            
         BRAS  RE,TIMER                                                         
         MVC   PLINE(L'TIME),TIME                                               
         MVC   PLINE+L'TIME+1(L'CERRHDR),CERRHDR                                
         MVC   PLINE+L'TIME+CERRHDRL+2(CERRML),0(R1)                            
         XR    R1,R1                                                            
         BRAS  RE,DOMSG                                                         
         B     EXITL                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                      *         
***********************************************************************         
         SPACE 1                                                                
CERRML   EQU   40                                                               
CERRHDRL EQU   20                                                               
*                                                                               
CERRHDR  DC    CL20'**INPUT CARD ERROR**'                                       
CINVLIN  DC    CL40'Invalid Line Format'                                        
CINVKEY  DC    CL40'Invalid Keyword'                                            
CNOTNUM  DC    CL40'Value not a valid number'                                   
CNOTCHR  DC    CL40'Value not a valid character string'                         
CTOOSHT  DC    CL40'Length of input string too short'                           
CTOOLNG  DC    CL40'Length of input string too long'                            
CTOOLOW  DC    CL40'Numeric value too small'                                    
CTOOBIG  DC    CL40'Numeric value too large'                                    
CNOINP   DC    CL40'Invalid/missing value'                                      
         EJECT                                                                  
***********************************************************************         
* INITIALISE PRINTER                                                  *         
***********************************************************************         
         SPACE 1                                                                
PRINTI   NTR1  ,                                                                
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         PUT   SYSPRINT,PTITLE     PRINT TITLES                                 
         PUT   SYSPRINT,PTITLEU                                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT LINE                                                          *         
***********************************************************************         
         SPACE 1                                                                
PRINTL   NTR1  ,                                                                
         PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         B     EXIT                EXIT                                         
         EJECT                                                                  
***********************************************************************         
* CLOSE PRINTER                                                       *         
***********************************************************************         
         SPACE 1                                                                
PRINTX   NTR1  ,                                                                
         CLOSE SYSPRINT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OUTPUT INFORMATION MESSAGE                                          *         
* NTRY: R1  NZ      INDEX TO MESSAGE                                  *         
*       R1  ZERO    MESSAGE IS ALREADY ON PRINT LINE                  *         
***********************************************************************         
         SPACE 1                                                                
DOMSG    NTR1  ,                                                                
         LTR   R1,R1                                                            
         BZ    DOMSG02                                                          
         XR    R0,R0                                                            
         BRAS  RE,TIMER                                                         
         MVC   PLINE(L'TIME),TIME                                               
*                                                                               
         BCTR  R1,0                                                             
         MHI   R1,L'MESSTAB                                                     
         A     R1,AMESSTAB                                                      
         MVC   PLINE+15(L'MESSTAB),0(R1)                                        
*                                                                               
DOMSG02  BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* EXITS AND OTHER USEFUL ROUTINES                                     *         
***********************************************************************         
         SPACE 1                                                                
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    BRAS  RE,PRINTX           CLOSE PRINTING AND EXIT TO MVS               
         L     RD,SAVERD                                                        
         XBASE                                                                  
*                                                                               
         SPACE 1                                                                
WTODIE   NTR1  ,                   OUTPUT MESSAGE AND DIE                       
         STAM  AR0,ARF,ARDIE                                                    
         BRAS  RE,ARSOFF                                                        
         WTO   TEXT=(R0)                                                        
         ABEND 922,DUMP                                                         
*                                                                               
ON31     O     RE,=XL4'80000000'                                                
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=XL4'7FFFFFFF'                                                
         BSM   0,RE                                                             
*                                                                               
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         BR    RE                                                               
*                                                                               
R2ALET   LAM   AR2,AR2,ALET                                                     
         SAC   512                                                              
         BR    RE                                                               
*                                                                               
R23ALET  LAM   AR2,AR2,ALET                                                     
         CPYA  AR3,AR2                                                          
         SAC   512                                                              
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
EOT      EQU   X'FF'                                                            
*                                                                               
ARZERO   DC    16F'0'                                                           
FF32     DC    32X'FF'                                                          
*                                                                               
BLOCK    DC    A(0)                                                             
*                                                                               
VHEXOUT  DC    V(HEXOUT)                                                        
VSCANNER DC    V(SCANNER)                                                       
VISDDS   DC    V(ISDDS)                                                         
VCARDS   DC    V(CARDS)                                                         
VLOCKSPC DC    V(LOCKSPC)                                                       
VDMGR    DC    V(DATAMGR)                                                       
VDADDS   DC    V(DADDS)                                                         
VRDID    DC    A(00000001)         DADDS RDID ROUTINE                           
VWTID    DC    A(00000004)         DADDS WTID ROUTINE                           
VDAOPEN  DC    A(00000014)         DADDS DAOPEN ROUTINE                         
VHELLO   DC    V(HELLO)                                                         
DATCON   DC    V(DATCON)                                                        
VDDSIO   DC    V(DDSIO)                                                         
*                                                                               
AGMLIST  DC    A(GMLIST)                                                        
AMESSTAB DC    A(MESSTAB)                                                       
ACARDTAB DC    A(CARDTAB)                                                       
ACIREC   DC    A(CIREC)                                                         
ACXREC   DC    A(CXREC)                                                         
*                                                                               
DMOPEN   DC    CL8'DMOPEN'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DTFAD    DC    CL8'DTFAD '                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
GLIST    DC    CL8'GLIST   '                                                    
WRKFLST  DC    CL8'WRKF1   '                                                    
*                                                                               
PTITLE   DC    CL132'WKBUFFER output information'                               
PTITLEU  DC    CL132'-------- ------ -----------'                               
*                                                                               
ALET     DS    A                   ALET                                         
OFFS     DS    A                   DATASPACE OFFSET                             
STOKN    DS    CL8                 STOKEN                                       
*                                                                               
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
*                                                                               
         DC    CL8'JOBNAME='                                                    
JOBNAME  DC    CL8' '              JOBNAME                                      
*                                                                               
         DC    CL8'JOBSTEP='                                                    
JOBSTEP  DC    X'00',XL7'00'       OPERATOR COMMAND FLAGS                       
JOBEOJ   EQU   X'80'               TERMINATE                                    
JOBINIT  EQU   X'40'               RE-INITIALISE PQ                             
JOBSTOP  EQU   X'20'               FORCE CANCEL                                 
JOBBAD   EQU   X'10'               BAD INPUT                                    
*                                                                               
         DC    CL8'REBUILD='                                                    
REBUILD  DC    X'00'               SET TO C'Y' IF REBUILDING INDICES            
         DC    XL7'00'                                                          
         DC    CL8'ARDIE==='                                                    
ARDIE    DC    16F'0'                                                           
*                                                                               
         DC    CL8'EXTRACTR'                                                    
RESULTS  DS    0F                  RESULTS FROM EXTRACT MACRO                   
RXTIOT   DC    F'0'                TIOT                                         
RXCOM    DC    F'0'                COMMS BLOCK                                  
RXASID   DC    F'0'                ASID                                         
         DC    F'0'                N/D                                          
*                                                                               
         DC    CL8'STOKEN=>'       STOKEN                                       
ASTOKEN  DC    XL8'00'                                                          
         DC    CL12'AOPERECB===>'  A(OPERATOR ECB)                              
AOPERECB DC    A(0)                                                             
*                                                                               
         DC    CL8'PLINE***'                                                    
PLINE    DC    CL166' '                                                         
*                                                                               
DSPACE   DC    CL12' '                                                          
RUN      DC    CL8' '                                                           
*                                                                               
XGMFAIL  DC    AL2(53)                                                          
XGMFNME  DC    CL8' ',CL45' ERROR - GETMAIN failed (Increase region)'           
*                                                                               
XFMFAIL  DC    AL2(53)                                                          
XFMFNME  DC    CL8' ',CL45' ERROR - FREEMAIN failed (Call programmer)'          
*                                                                               
XGMLIST  DC    AL2(53)                                                          
XGMLNME  DC    CL8' ',CL45' ERROR - Increase GMLIST (Call programmer)'          
*                                                                               
* FATABSDEQU                                                                    
       ++INCLUDE FATABSDEQU                                                     
*                                                                               
SPACES   DC    132C' '                                                          
         EJECT                                                                  
***********************************************************************         
* DCBS                                                                *         
***********************************************************************         
         SPACE 1                                                                
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FB,LRECL=132             
         EJECT                                                                  
***********************************************************************         
* INPUT CARDS TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
         DC    CL16'CARDTAB*CARDTAB*'                                           
         SPACE 1                                                                
CARDTAB  DC    CL8'DSPACE  ',F'001',F'0000012'                                  
         DC    AL1(05,CTCHR,L'DSPACE,0),AL4(DSPACE)                             
         DC    CL8'DDSIO   ',F'001',F'0000012'                                  
         DC    AL1(04,CTCHR,8,0),V(DDSIO)                                       
         DC    CL8'RUN     ',F'001',F'0000008'                                  
         DC    AL1(02,CTCHR,1,0),AL4(RUN)                                       
CARDTABX DC    AL1(CARDEOT)                                                     
         DS    0L                                                               
         DC    CL16'CARDTABXCARDTABX'                                           
         EJECT                                                                  
***********************************************************************         
* CARD TABLE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
CARDTABD DSECT                                                                  
CNAME    DS    CL8                 INPUT CARD                                   
CMIN     DS    F                   MINIMUM (VALUE OR LENGTH)                    
CMAX     DS    F                   MAXIMUM (VALUE OR LENGTH)                    
CXLEN    DS    AL1                 LEN-1 OF CNAME VALUE FOR COMPARE             
CTYPE    DS    AL1                 INPUT TYPE                                   
CTNUM    EQU   1                   NUMERIC                                      
CTCHR    EQU   2                   CHARACTER                                    
CTNOP    EQU   3                   NO SECOND INPUT FIELD                        
CTNOPF   EQU   4                   NO SECOND INPUT FIELD/TURN ON FLAG           
CLEN     DS    AL1                 OUTPUT AREA LENGTH (CHAR ONLY)               
CFLAG    DS    AL1                 FLAG VALUE (CTNOPF)                          
COUT     DS    AL4                 A(OUTPUT AREA)                               
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
CARDEOT  EQU   X'FF'                                                            
*                                                                               
WKBUFF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* UTL AND SSB FOR DDSIO                                               *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL16'UTL*UTL*UTL*UTL*'                                           
UTL      DC    F'0'                                                             
         DC    X'0A'                                                            
         DC    XL251'00'                                                        
*                                                                               
         DC    CL16'SSB*SSB*SSB*SSB*'                                           
SSB      DC    H'0'                                                             
         DC    X'FF'                                                            
         DC    X'02'               SUPPRESS RECOVERY                            
         DC    1020X'00'                                                        
         EJECT                                                                  
***********************************************************************         
* BUFFERS                                                             *         
***********************************************************************         
         SPACE 1                                                                
CXHDR    DC    CL16'**CXREC**CXREC**'                                           
CXREC    DS    18432C                                                           
*                                                                               
         DS    0L                                                               
CIHDR    DC    CL16'**CIREC**CIREC**'                                           
CIREC    DS    18432C                                                           
         EJECT                                                                  
***********************************************************************         
* GETMAIN ADDRESS LIST FORMAT XL4(ADDRESS),AL4(LENGTH)                *         
***********************************************************************         
         SPACE 1                                                                
         DC    CL16'**GETMAIN LIST**'                                           
GMLIST   DC    2000XL8'00'                                                      
GMLISTX  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* MESSAGE TABLE                                                       *         
***********************************************************************         
         SPACE 1                                                                
MESSTAB  DS    0CL60                                                            
  DC CL60'Began Processing Input Parameters from Cards                '         
  DC CL60'Ended Processing Input Parameters from Cards                '         
  DC CL60'Attempting Dataspace bind                                   '         
  DC CL60'Completed  Dataspace bind                                   '         
  DC CL60'Dataspace bind failed - check cards then check dump         '         
  DC CL60'ALET for dataspace is not valid - check TABS available      '         
  DC CL60'Began Initialising Operator Communications                  '         
  DC CL60'Ended Initialising Operator Communications                  '         
  DC CL60'Began building WRKF indices in core                         '         
  DC CL60'Ended building WRKF indices in core                        '          
  DC CL60'FREEMAIN not necessary for this build                      '          
  DC CL60'Began processing operator command                           '         
  DC CL60'Operator cancel command issued                              '         
  DC CL60'Unknown operator command verb                               '         
  DC CL60'Operator command too long to process (8 byte maximum)       '         
  DC CL60'Operator console post - Application Terminating             '         
  DC CL60'Operator requested WKBUFFER rebuild                         '         
  DC CL60'Unknown operator command                                    '         
  DC CL60'Ended processing operator command                           '         
  DC CL60'Began reporting on i/o counts                               '         
  DC CL60'Ended reporting on i/o counts                               '         
*22                                                                             
         EJECT                                                                  
***********************************************************************         
* REPORT AREA FOR BUFFER BUILD                                        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
RPTAREA  DC    (60*1024)X'00'      60K                                          
RPTAREAL EQU   *-RPTAREA                                                        
         DC    64X'FF'                                                          
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DC                                                  *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
         DC    CL16'WORKAREAWORKAREA'                                           
WORKAREA DC    120000X'00'                                                      
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
XDUB     DS    D                                                                
DSPHD    DS    XL64                                                             
*                                                                               
SAVERD   DS    A                                                                
CARDRD   DS    A                                                                
CARDEND  DS    A                                                                
UKEY     DS    XL40                USER INDEX/KEY FOR PRTQUE                    
*                                                                               
LASTWKB  DS    A                                                                
FULL     DS    F                                                                
*                                                                               
AWRKFLST DS    A                   A(NEXT ENTRY IN LIST OF PRTQ FILES)          
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
P7       DS    F                                                                
P8       DS    F                                                                
*                                                                               
TOTALS   DS    0XL16                                                            
TOTONR   DS    F                                                                
TOTONW   DS    F                                                                
TOTOFR   DS    F                                                                
TOTOFW   DS    F                                                                
*                                                                               
LCLDATA  DS    10D                 CI DATA (COVERED BY DMPRTQW)                 
LCLADTF  DS    A                   A(DTF) FOR THIS WORKER FILE                  
*                                                                               
HALF     DS    H                                                                
ERRCNT   DS    H                                                                
BYTE     DS    X                                                                
FSTBLK   DS    X                                                                
*                                                                               
LCLID    DS    CL5                                                              
LCLINUM  DS    X                                                                
TIME     DS    CL11                                                             
WORK     DS    CL64                                                             
CARD     DS    CL80                                                             
*                                                                               
SCNBLK   DS    3CL(SCBLKLQ)                                                     
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* SCANNER CLONE FOR INPUT CARD VALIDATION                             *         
***********************************************************************         
         SPACE 1                                                                
SCANNER  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 SWORKL,**SCAN**                                                  
         USING SWORKD,RC                                                        
         LR    R9,R1               R9=A(PARAMETER LIST)                         
         LM    R2,R3,0(R9)         R2=A(DATA STRING) R3=A(BLOCK)                
         MVC   MAXLINES,4(R9)                                                   
         XC    SDISP,SDISP                                                      
         XR    R4,R4                                                            
         IC    R4,5(R2)            L'DATA IF SCREEN FIELD                       
         LA    R2,8(R2)                                                         
         MVC   LROW,=H'42'         PRESET DEFAULT LENGTHS                       
         MVC   LRIGHT,=H'20'                                                    
         MVC   LBOTH,=H'30'                                                     
         CLI   0(R9),C'C'                                                       
         BE    SCAN1                                                            
*                                                                               
SCAN1    AHI   R2,-8                                                            
         LA    R4,80                                                            
         CLC   0(80,R2),SSPACES                                                 
         BE    ERROR2                                                           
         LA    R5,79(R2)                                                        
*                                                                               
SCAN2    CLI   0(R5),C' '                                                       
         BNE   SCAN4                                                            
         BCTR  R5,0                                                             
         BCT   R4,SCAN2                                                         
*                                                                               
SCAN4    LA    R5,0(R2,R4)         L'DATA IN R4                                 
         MVC   BORROW,0(R5)        SAVE THE NEXT CHARACTER                      
         MVC   0(1,R5),COMMA       AND POP IN A COMMA TO SIMPLIFY               
         SR    R6,R6               R6=NUMBER OF LINES USED                      
         EJECT                                                                  
*                                                                               
*HANDLE LINES OF DATA                                                           
*                                                                               
SCAN6    XC    0(12,R3),0(R3)      PRESET A LINE                                
         LH    RF,LBOTH                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),SSPACES                                                 
         MVC   2(2,R3),=X'E0E0'                                                 
         BRAS  RE,GETL                                                          
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         MVI   2(R3),0                                                          
         CLI   1(R3),0                                                          
         BNE   *+8                                                              
         MVI   3(R3),0                                                          
         CLC   0(1,R3),LBOTH+1                                                  
         BH    ERROR                                                            
         CLC   1(1,R3),LRIGHT+1                                                 
         BH    ERROR                                                            
         CLI   1(R3),0                                                          
         BE    SCAN8                                                            
         CLI   0(R3),10                                                         
         BH    ERROR                                                            
*                                                                               
SCAN8    SR    R7,R7                                                            
         IC    R7,0(R3)                                                         
         LTR   R7,R7                                                            
         BZ    SCAN18                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),0(R2)                                                   
         TM    2(R3),X'80'                                                      
         BZ    SCAN10                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN10                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,5(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING DISPS.               
         BO    SCAN10                                                           
         ST    R8,4(R3)                                                         
*                                                                               
SCAN10   LA    R2,2(R2,R7)                                                      
         IC    R7,1(R3)                                                         
         LTR   R7,R7                                                            
         BZ    SCAN20                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   22(0,R3),0(R2)                                                   
         TM    3(R3),X'80'                                                      
         BZ    SCAN12                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN12                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,9(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING SDISPS.              
         BO    SCAN12                                                           
         ST    R8,8(R3)                                                         
*                                                                               
SCAN12   LA    R2,2(R2,R7)                                                      
         B     SCAN20                                                           
*                                                                               
VARPAK   PACK  SDUB,0(0,R2)                                                     
*                                                                               
SCAN18   LA    R2,1(R2)                                                         
         CLI   1(R3),0                                                          
         BNE   ERROR                                                            
*                                                                               
SCAN20   LA    R6,1(R6)            BUMP N'LINES                                 
         AH    R3,LROW             BUMP TO NEXT LINE IN BLOCK                   
         CR    R2,R5               ARE WE NOW PAST LAST 'COMMA'                 
         BH    OK                                                               
         IC    R7,MAXLINES                                                      
         LTR   R7,R7                                                            
         BZ    SCAN6                                                            
         CR    R6,R7               HAVE WE REACHED MAX N'LINES                  
         BNE   SCAN6                                                            
*                                                                               
OK       MVC   0(1,R5),BORROW      RETURN THE BYTE                              
         STC   R6,4(R9)            SET NUMBER OF LINES USED                     
         B     XIT                                                              
*                                                                               
ERROR    MVI   4(R9),0                                                          
         MVC   0(1,R5),BORROW                                                   
         MVC   2(2,R3),=X'FFFF'                                                 
         B     XIT                                                              
*                                                                               
ERROR2   MVI   4(R9),0                                                          
         MVC   2(2,R3),=X'FFFF'                                                 
*                                                                               
XIT      XMOD1 1                                                                
         EJECT                                                                  
*VALIDATE AND GET LENGTHS                                                       
*                                                                               
GETL     NTR1                                                                   
         LR    R4,R3                                                            
         SR    R5,R5                                                            
         TM    4(R9),X'80'                                                      
         BZ    GETL2                                                            
         MVC   4(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
*                                                                               
GETL2    CLC   0(1,R2),COMMA       TEST FIELD SEPERATOR                         
         BE    GETL12                                                           
         CLC   0(1,R2),EQUAL                                                    
         BE    GETL14                                                           
*                                                                               
GETL3    LA    R5,1(R5)                                                         
         CLI   0(R2),C'9'                                                       
         BNH   *+8                                                              
         MVI   2(R4),0             (ALL INVALID)                                
         CLI   0(R2),C'0'                                                       
         BL    GETL4                                                            
         NI    2(R4),X'BF'         (INVALID ALPHA)                              
         B     GETL10                                                           
*                                                                               
GETL4    NI    2(R4),X'7F'         (INVALID NUM)                                
         CLI   0(R2),C'Z'                                                       
         BNH   GETL6                                                            
         MVI   2(R4),0             Z-0 = ALL INVALID                            
         B     GETL10                                                           
*                                                                               
GETL6    CLI   0(R2),C'A'          LESS THAN A = ALL INVALID                    
         BNL   GETL8                                                            
         MVI   2(R4),0                                                          
         B     GETL10                                                           
*                                                                               
GETL8    CLI   0(R2),C'F'          OK FOR ALPHA                                 
         BNH   GETL10                                                           
         NI    2(R4),X'DF'         G-Z = INVALID HEX                            
*                                                                               
GETL10   LA    R2,1(R2)                                                         
         B     GETL2                                                            
*                                                                               
GETL12   STC   R5,0(R4)            COMMA FOUND                                  
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         B     XIT                                                              
*                                                                               
GETL14   CR    R4,R3               EQUAL FOUND - IS THIS THE FIRST ONE?         
         BNE   GETL3               TREAT AS NORMAL CHARACTER IF NOT             
         STC   R5,0(R4)            NOW STORE L1                                 
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         TM    4(R9),X'80'                                                      
         BZ    GETL16                                                           
         MVC   8(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
*                                                                               
GETL16   LA    R4,1(R4)            POINT TO FIELD2 DATA                         
         SR    R5,R5               CLEAR L2                                     
         LA    R2,1(R2)            POINT PAST EQUAL SIGN                        
         B     GETL2                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
COMMA    DC    C','                                                             
EQUAL    DC    C'='                                                             
SSPACES  DC    CL80' '                                                          
         EJECT                                                                  
***********************************************************************         
* SCANNER WORKING STORAGE DSECT                                       *         
***********************************************************************         
         SPACE 1                                                                
SWORKD   DSECT                                                                  
SDUB     DS    D                                                                
SWORK    DS    CL32                                                             
LASTSTOP DS    F                                                                
BORROW   DS    CL1                                                              
MAXLINES DS    CL1                                                              
LROW     DS    H                                                                
LRIGHT   DS    H                                                                
LBOTH    DS    H                                                                
SDISP    DS    H                                                                
SWORKL   EQU   *-SWORKD                                                         
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
         EJECT                                                                  
***********************************************************************         
* DATASPACE BUFFER DSECT                                              *         
***********************************************************************         
         SPACE 1                                                                
* WKBUFFERD                                                                     
       ++INCLUDE WKBUFFERD                                                      
         EJECT                                                                  
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* FATABSD                                                                       
         PRINT OFF                                                              
       ++INCLUDE FATABSD                                                        
         PRINT ON                                                               
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* DMDSHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
CIDATAD  DSECT                                                                  
         PRINT  OFF                                                             
       ++INCLUDE DMPRTQW                                                        
         PRINT  ON                                                              
* DMWRKFD                                                                       
         PRINT  OFF                                                             
       ++INCLUDE DMWRKFD                                                        
         PRINT  ON                                                              
* DMDTFPH                                                                       
         PRINT  OFF                                                             
       ++INCLUDE DMDTFPH                                                        
         PRINT  ON                                                              
* DMWRKFK                                                                       
         PRINT  OFF                                                             
       ++INCLUDE DMWRKFK                                                        
         PRINT  ON                                                              
* FAPRQ                                                                         
         PRINT  OFF                                                             
       ++INCLUDE FAPRQ                                                          
         PRINT  ON                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007WKBUFFER  05/06/09'                                      
         END                                                                    
