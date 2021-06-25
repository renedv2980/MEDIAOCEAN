*          DATA SET DELNK13    AT LEVEL 068 AS OF 02/26/21                      
*PHASE TF2F13C                                                                  
*INCLUDE DUMPOUT                                                                
*INCLUDE UNBOOK                                                                 
*INCLUDE NSIWEEK                                                                
DELNK13  TITLE '- SPOT PRISMA DEMO SYSTEM SERVER'                               
***********************************************************************         
* THIS VERSION FORCES NCM UPGRADES TO LOOKUP STANDARD BOOK                      
***********************************************************************         
SVRDEF   CSECT                                                                  
         LKSVR IDF=Y,REQUEST=*,CODE=CODE,SYSTEM=SPTSYSQ,FACS=FACS,     *        
               APPEND=Y,SERVERTYPE=TSTDEMC,AUTOCLEAR=Y,WORKERKEY=PRLK, *        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED),FILES=FILESPOT,    *        
               SYSPHASE=SYSPHASE,TYPE=D,LOADFACSOFF=Y                           
*                                                                               
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 IUNWRKL,**LK13*,CLEAR=YES,RR=RE                                  
         USING LP_D,R1                                                          
         L     RF,LP_ARUNP                                                      
         USING RUNPARMD,RF         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         OC    RMASTC,RMASTC       TEST RUNNING ONLINE                          
         BNZ   INIT02                                                           
         ICM   R9,15,LP_ABLK1      ROOT SHOULD PASS A(WORKD)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R8,15,RSVRSAVE      R8=A(4K SAVE AREA)                           
         B     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE         A(64K SAVE AREA)                             
B#WORKD  EQU   1                   WORKD                                        
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         SR    R8,R8                                                            
         ICM   R8,3,=AL2(WORKL)                                                 
         LA    R8,WORKD(R8)        R8=A(46K SAVE AREA)                          
         USING SAVED,R8,R7         R8=A(SAVE W/S)                               
                                                                                
INIT04   LR    R7,R8                                                            
         AHI   R7,4096                                                          
         ST    R1,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         ST    RC,AIUNWRK                                                       
                                                                                
         MVC   VERSNUM,LP_VRSN1                                                 
         MVC   AGYALPH,LP_AGY                                                   
         MVC   OFFLFLAG,LP_FLAG    SAVE ONLINE/OFFLINE FLAG                     
         MVC   USERID,LP_USRID                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   CLI   RUNPMODE,RRUNSTRQ   TEST FIRST FOR RUN                           
         BNE   PRCWRK                                                           
                                                                                
         BASR  RE,0                                                             
         AHI   RE,LVALUES-*                                                     
         LA    R0,SVVALUES                                                      
         LHI   R1,SVVALUESL                                                     
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    RE,RELOLST2                                                      
         LHI   R0,RLOLSTN2                                                      
RUNSTR01 L     RF,0(RE)            RELOCATE ADCONS                              
         A     RF,SRVRRELO                                                      
         ST    RF,0(RE)                                                         
         AHI   RE,L'RELOLST2                                                    
         BCT   R0,RUNSTR01                                                      
                                                                                
                                                                                
         LA    R0,REQVALS          CLEAR DOWN SAVE AREA                         
         LHI   R1,REQVALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RF,RCOMFACS         YES - LOAD FACILITIES OVERLAYS               
         ST    RF,ACOMFACS                                                      
         L     RE,CGETPROF-COMFACSD(RF)                                         
         ST    RE,VGETPROF                                                      
                                                                                
         L     RE,CGETDAY-COMFACSD(RF)                                          
         ST    RE,VGETDAY                                                       
         L     RE,CADDAY-COMFACSD(RF)                                           
         ST    RE,VADDAY                                                        
         L     RE,CDEFINE-COMFACSD(RF)                                          
         ST    RE,VDEFINE                                                       
                                                                                
         L     RE,CHELLO-COMFACSD(RF)                                           
         ST    RE,VHELLO                                                        
                                                                                
         L     RE,CDEMTABS-COMFACSD(RF)                                         
         ST    RE,VDEMTABS                                                      
                                                                                
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,(1,0),0,0                                              
         MVC   AROUT1,0(R1)                                                     
         GOTOR (RF),DMCB,(2,0),0,0                                              
         MVC   AROUT2,0(R1)                                                     
                                                                                
         GOTO1 (RF),(R1),0,X'D9000A24'       SPGETIUN                           
         MVC   VSPGTIUN,0(R1)                                                   
         GOTO1 (RF),(R1),0,X'D9000A21'       SPGETDEMF                          
         MVC   VSPDEMLK,0(R1)                                                   
         GOTO1 (RF),(R1),0,X'D9000A00'       BOOKVAL                            
         MVC   VBOOKVAL,0(R1)                                                   
         GOTO1 (RF),(R1),0,X'D9000AE0'       DEMOCON                            
         MVC   VDEMOCON,0(R1)                                                   
         GOTO1 (RF),(R1),0,X'D9000A11'       UNTIME                             
         MVC   VUNTIME,0(R1)                                                    
         GOTO1 (RF),(R1),0,X'D9000A22'       SPDEMUP                            
         MVC   VSPDEMUP,0(R1)                                                   
         GOTO1 (RF),(R1),0,X'D9000A7A'       STAPACK                            
         MVC   VSTAPACK,0(R1)                                                   
                                                                                
         GOTOR (#WRKINI,AWRKINI)   INITIALISE WORKING STORAGE                   
                                                                                
         L     R1,ALP                                                           
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
                                                                                
RUNSTR02 L     R1,ALP                                                           
         LA    R0,SAVED                                                         
B#SAVED  EQU   2                   SAVED                                        
         ST    R0,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   CLI   RUNPMODE,RPRCWRKQ   TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         XC    REQVALS(REQVALSL),REQVALS                                        
                                                                                
         L     RE,=V(DUMPOUT)                                                   
         A     RE,SRVRRELO                                                      
         ST    RE,VDUMPOUT                                                      
         LA    R0,REQVALS          CLEAR DOWN SAVE AREA                         
         LHI   R1,REQVALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         J     EXITY                                                            
                                                                                
*=====================================================================*         
* RUN A DOWNLOAD REQUEST                                              |         
*=====================================================================*         
                                                                                
RUNREQ   CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
                                                                                
         L     RF,ALP                                                           
         L     RF,LP_ARUNP-LP_D(RF)                                             
         L     RF,RUNPMODE-RUNPARMD(RF)                                         
         L     RF,RMASTC-RUNFACSD(RF)                                           
         MVC   DEPRINT,MCVPRINT-MASTD(RF)      ADDRESS OF PRINT ROUT            
         TM    OFFLFLAG,LP_FOFFL              IF OFFLINE THEN SET OVSYS         
         BZ    *+10                           ELSE IT IS ALREADY SETTED         
         MVC   OVSYS,MCOVSYS-MASTD(RF)         ADDRESS OF SYSTEM NUMBER         
                                                                                
         CLC   LP_QMAPN,=AL2(M#PGDET)          PROCESS REPORT                   
         JE    PROGDET                                                          
         CLC   LP_QMAPN,=AL2(M#CANLK)          CANADIAN LOOKUP                  
         JE    SDCANLK                         FOR SPOT DESKTOP                 
         J     EXITY                                                            
                                                                                
PROGDET  GOTOR RUNPGDET,DMCB,(RC)                                               
         J     EXITY                                                            
SDCANLK  GOTOR CANLOOK                                                          
         J     EXITY                                                            
         DROP  R6                                                               
         LTORG                                                                  
                                                                                
**********************************************************************          
*                                                                    *          
*     START PROGRAM DETAILS                                          *          
*                                                                    *          
**********************************************************************          
RUNPGDET NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    TMPFILE,TMPFILE                                                  
         XC    TMPBOOK,TMPBOOK                                                  
         XC    TMPBKTYP,TMPBKTYP    CLEAR OTHERWISE PROBLEM IN TRAMKT           
         XC    TMPBKWK,TMPBKWK                                                  
         XC    TMPDAY,TMPDAY                                                    
         XC    TMPPCID,TMPPCID                                                  
         XC    TMPUINDX,TMPUINDX                                                
                                                                                
         XC    PREVMKT,PREVMKT                                                  
         MVI   DBSRC,C'N'                                                       
         MVI   DBMED,C'T'                                                       
         XC    DEMOPTR,DEMOPTR                                                  
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
                                                                                
                                                                                
*    FOR OFF-LINE NO RELO                                                       
         TM    OFFLFLAG,LP_FOFFL                                                
         JZ    RUNPG02                                                          
         XC    SRVRRELO,SRVRRELO                                                
         GOTOR INITBUFF                                                         
         J     RUNPG03                                                          
                                                                                
RUNPG02  MVI   DMCB,2                                                           
         MVC   DMCB+1(2),=X'0002'                                               
         MVC   ATSIOREC,AIO3                                                    
         GOTOR (#INITTSR,AINITTSR),DMCB                                         
                                                                                
RUNPG03  XC    WORK,WORK                                                        
         MVC   WORK+0(4),=C'S01W'                                               
         MVC   WORK+4(2),AGYALPH                                                
         MVI   WORK+6,C'T'                                                      
         GOTOR VGETPROF,DMCB,WORK,PROF1W,VDATAMGR                               
                                                                                
* SEE IF WE WANT 2 DECIMAL                                                      
         SR    R4,R4                             GET DECIMAL                    
         XC    OPTDEC,OPTDEC                                                    
         ICM   R4,7,ADECIMAL                                                    
         BZ    *+14                                                             
         LA    R4,LW_DATA1-LW_D(R4)                                             
         MVC   OPTDEC,0(R4)                                                     
                                                                                
* SEE IF WE WANT 2 DECIMAL IMPRESSION                                           
         SR    R4,R4                                                            
         XC    OPTIPRE,OPTIPRE                                                  
         ICM   R4,7,AIPRE                                                       
         BZ    *+14                                                             
         LA    R4,LW_DATA1-LW_D(R4)                                             
         MVC   OPTIPRE,0(R4)                                                    
                                                                                
* SEE IF PROGRAM SHARES AND PUTS INSTEAD OF TSA SHARE AND TOTALS                
         SR    R4,R4                                                            
         XC    PROGSHRF,PROGSHRF                                                
         ICM   R4,7,APSHR                                                       
         BZ    *+14                                                             
         LA    R4,LW_DATA1-LW_D(R4)                                             
         MVC   PROGSHRF,0(R4)                                                   
                                                                                
* SEE IF WE HAVE A PRISMA AGENCY                                                
         SR    R4,R4                                                            
         XC    OPTPAGY,OPTPAGY                                                  
         ICM   R4,7,APAGY                                                       
         BZ    *+14                                                             
         LA    R4,LW_DATA1-LW_D(R4)                                             
         MVC   OPTPAGY,0(R4)                                                    
                                                                                
* SEE IF RADIO HOME MARKET WAS PASSED IN                                        
         SR    R4,R4                             GET DECIMAL                    
         XC    RHOMEMKT,RHOMEMKT                                                
         ICM   R4,7,ARHMKIND                                                    
         BZ    *+14                                                             
         LA    R4,LW_DATA1-LW_D(R4)                                             
         MVC   RHOMEMKT,0(R4)                                                   
                                                                                
                                                                                
* SEE IF WE HAVE UNIVERSAL DEFAULT BOOKTYPE PASSED IN                           
         SR    R4,R4                                                            
         XC    FORCEBT,FORCEBT                  FORCE BOOKTYPE                  
         ICM   R4,7,ADEFBKTP                                                    
         BZ    *+14                                                             
         LA    R4,LW_DATA1-LW_D(R4)                                             
         MVC   FORCEBT,0(R4)                                                    
                                                                                
*                                                                               
* SET FLAG TO FORCE DOWNLOAD OF DEMO HEADER RECORD BELOW                        
RUNPG05  MVI   DEMOHDRF,DMHDR_FROMSTART                                         
         XC    DEMOPTR,DEMOPTR                                                  
*                                                                               
RUNPG06  SR    R4,R4                             PROCESS FILES                  
         ICM   R4,7,AFILE                                                       
         LA    R2,1                                                             
         TM    FILEIND,LQ_TSINQ                                                 
         BNO   RUNPG08                                                          
         LA    R4,LW_DATA1-LW_D(R4)                                             
         ST    R4,FILBKPTR                                                      
         J     RUNPG09                                                          
RUNPG08  TM    FILEIND,LQ_TLSTQ                                                 
         JO    *+8                               IF NO BOOKS AT ALL             
         J     EXITY                             JUST EXIT                      
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
         ST    R4,FILBKPTR                                                      
RUNPG09  DS    0H                                                               
         ST    R2,TOTALFBK                                                      
                                                                                
                                                                                
RUNPG15  XC    ROWCOUNT,ROWCOUNT                INITIALIZE COUNTS               
         XC    FBKCOUNT,FBKCOUNT                                                
                                                                                
RUNPG17  L     R5,=A(DBLOCK1-WORKD)             GET STATION                     
         LA    R5,WORKD(R5)                                                     
         USING DBLOCK,R5                                                        
         SR    R4,R4                                                            
         ICM   R4,7,ASTAT                                                       
         LA    R2,1                                                             
         TM    STAIND,LQ_TSINQ                                                  
         BNO   RUNPG20                                                          
         LA    R4,LW_DATA1-LW_D(R4)                                             
         ST    R4,MYSTAPTR                                                      
         J     RUNPG30                                                          
RUNPG20  TM    STAIND,LQ_TLSTQ                                                  
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
         ST    R4,MYSTAPTR                                                      
RUNPG30  XC    ALFMKTS,ALFMKTS                                                  
                                                                                
         XC    DBLOCK,DBLOCK                                                    
         XC    DBSELALF,DBSELALF                                                
         ST    R4,MYSTAPTR                                                      
         L     R4,MYSTAPTR                   *** DON'T BLOW AWAY R4 ***         
         USING STATTABD,R4                                                      
                                                                                
         L     R2,FILBKPTR                                                      
         USING FILBKD,R2                                                        
                                                                                
                                                                                
         MVC   INPFIL,FBKFILE                    FILE                           
         MVI   DBMED,C'T'                        DEFAULT MED                    
*******  MVC   OUTPCID,FBKPCID                   PCID                           
         L     RE,=A(DFILTAB)                                                   
         A     RE,SRVRRELO                                                      
         USING DFILTABD,RE                                                      
         OC    INPFIL,SPACES                                                    
RUNPG31A CLI   0(RE),0                         SHOULD ALWAYS HAVE               
         JNE   *+6                             A MATCH TO TABLE                 
         DC    H'0'                                                             
         CLC   INPFIL,DINFILE                                                   
         JE    RUNPG31C                                                         
         AHI   RE,DFILTABL                                                      
         J     RUNPG31A                                                         
RUNPG31C MVC   TMPFILE,DTMPFILE                                                 
         MVC   DBSRC,DFILSRC                                                    
         MVC   DBMED,DFILMED                                                    
         MVC   TMPDBFIL,DFILDBF                                                 
         MVC   FLAG2DEC,DFIL2DEC                                                
                                                                                
         CLI   DEMOHDRF,DMHDR_RELEASED                                          
         BE    RUNPG33                                                          
         GOTOR =A(BLDDEMOL),RR=SRVRRELO      BUILD DEMO HEADER RECORD           
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
                                                                                
RUNPG33  MVI   DEMOHDRF,DMHDR_RELEASED       DEMO HEADER RECORD STATUS          
         DROP  RE                                                               
                                                                                
         XC    AUNIVDEM,AUNIVDEM                                                
         XC    AIMPSDEM,AIMPSDEM                                                
         XC    RDEMFLAG,RDEMFLAG                                                
         CLI   DBMED,C'R'                        RADIO?                         
         BNE   RUNPG35                           NO                             
         LA    R0,UNIVDEMS                       COPY DEMO REQUEST              
         LA    R1,UNIVDEML                                                      
         LA    RE,DEMODEMS                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    RF,UNIVDEMS                                                      
*                                                                               
RUNPG34  CLI   1(RF),C'X'                       CHECK IF ORIGINAL               
         BNE   *+8                              USER DEMO LIST                  
         OI    RDEMFLAG,RDEM_X_MODIFIER         ASKS FOR X,O OR F               
         CLI   1(RF),C'O'                                                       
         BNE   *+8                                                              
         OI    RDEMFLAG,RDEM_O_MODIFIER                                         
         CLI   1(RF),C'F'                                                       
         BNE   *+8                                                              
         OI    RDEMFLAG,RDEM_F_MODIFIER                                         
         MVI   1(RF),C'U'                        AND FORCE UNIVERSE             
         AHI   RF,3                                                             
         CLI   0(RF),X'FF'                                                      
         BNE   RUNPG34                                                          
         LA    R1,UNIVDEMS                                                      
         ST    R1,AUNIVDEM                                                      
         ST    R1,AIMPSDEM                                                      
         ST    R1,ACUMSDEM                                                      
                                                                                
RUNPG35  MVC   TMPSTA,STATION                                                   
         MVC   TMPDYTIM(6),STADYTM                                              
         MVC   TMPSTATE,STASTATE                                                
         MVC   TMPCNTY,STACNTY                                                  
         MVC   TOTALROW,STATNROW                                                
* CONVERT BUY END DATE TO BINARY BOOK                                           
         XC    BUYENDT,BUYENDT                                                  
         OC    STABEND,STABEND                                                  
         BZ    RUNPG36                                                          
         GOTO1 VDATCON,DMCB,(2,STABEND),(3,BUYENDT)                             
RUNPG36  XC    LPMSTRT,LPMSTRT                                                  
         OC    STABLPMD,STABLPMD                                                
         BZ    RUNPG37                                                          
         GOTO1 VDATCON,DMCB,(2,STABLPMD),(3,LPMSTRT)                            
*                                                                               
RUNPG37  DS    0C                                                               
         MVC   TMPSYSC,STASYSC                                                  
*  CHECK # OF ROW FOR THIS STAT/DAYTIME TO BE ZERO                              
         OC    TOTALROW,TOTALROW                                                
         BNZ   *+14                                                             
         MVC   ROWCOUNT,EFFS                    FUDGE TO -1 FOR LATER           
         B     RUNPG170                         COMPARISION                     
                                                                                
         MVC   TOTALROW,STATNROW                                                
RUNPG40  DS    0H                                                               
         XC    DUMSTATH,DUMSTATH                                                
         XC    DUMSTAT,DUMSTAT                                                  
         ZIC   RE,STALEN                         LENGTH OF INPUT STRING         
         STC   RE,DUMSTATH+5                                                    
         MVI   DUMSTATH,L'DUMSTATH+L'DUMSTAT                                    
         ZIC   RE,STALEN                         LENGTH OF INPUT STRING         
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DUMSTAT(0),STATION                                               
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         L     R0,AIO1                                                          
         GOTO1 VSCANNER,DMCB,DUMSTATH,(R0),C',=,/'                              
         L     R1,AIO1                                                          
         USING SCANBLKD,R1                                                      
         MVC   DBSELSTA,SC1STFLD                                                
         OC    DBSELSTA(5),=C'     '                                            
         CLI   DBSELSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
         MVC   TMPSTA,DBSELSTA                                                  
         MVC   STATION,DUMSTAT                                                  
         MVC   ALFMKTS,SC2NDFLD                                                 
                                                                                
         LA    RF,SC2NDFLD                                                      
         ST    RF,DMCB                                                          
         ZIC   RF,SC2NDLEN                                                      
         OR    RF,RF                                                            
         BZ    RUNPG45                                                          
         ST    RF,DMCB+4                                                        
         LA    RF,WORK                                                          
         ST    RF,DMCB+8                                                        
         XC    WORK,WORK                                                        
                                                                                
         GOTOR (#VALAMKT,AVALAMKT),DMCB                                         
         XC    SPILLMKT,SPILLMKT                                                
         OC    WORK(2),WORK                      BINARY MKT RETURNED?           
         BZ    RUNPG45                           IF SO MKT IS NUMERIC           
         MVC   SPILLMKT,WORK                                                    
         XC    ALFMKTS,ALFMKTS                                                  
         B     RUNPG50                                                          
*                                                                               
         DROP  R1                                                               
RUNPG45  DS    0C                                                               
         GOTOR =A(TRAMKT),DMCB,RR=SRVRRELO                                      
*                                                                               
RUNPG50  DS    0H                                                               
********************************************************************* *         
* DUE TO SBTK LOOKUP BUG, SBTK IS INCORRECTLY LOOKING AT TT FILETYPE  *         
* WHEN IT SHOULD BE LOOKING AT FUSION, TF FILETYPE                    *         
* IF WE HAVE A CABLE STATION, WITH AN ALPHA MARKET                    *         
* AND THAT MARKET IS NOT LPM, CHECK THE MARKET RECORD FOR FUSION      *         
* OR CHECK THE 00A PROFILE                                            *         
***********************************************************************         
         XC    FORCEFT,FORCEFT     FORCE FILE TYPE                    *         
***      OC    VERSNUM,VERSNUM                                        *         
***      JZ    RUNPG75                                                *         
***      CLC   VERSNUM,=AL1(4,5,0,102)                                *         
***      JL    RUNPG75                                                *         
***      CLC   VERSNUM,=AL1(4,6,0,2)                                  *         
***      JH    RUNPG75                                                *         
         SR    RE,RE                                                  *         
         OC    STASYSC,STASYSC     CABLE?                             *         
         JZ    RUNPG75                                                *         
         OC    ALFMKTS,ALFMKTS     HAVE ALPHA?                        *         
         JZ    RUNPG75                                                *         
         CLC   INPFIL,=C'TT '      ASKING FOR TT FILETYPE?            *         
         JNE   RUNPG75                                                *         
*                                                                               
* for ox agencies do not check market record                                    
*we do not need to see if we need to override to tf source                      
* since this is sbtk related code                                               
         CLC   =C'OR',AGYALPH                                                   
         JE    RUNPG75                                                          
         CLC   =C'OK',AGYALPH                                                   
         JE    RUNPG75                                                          
         CLC   =C'OB',AGYALPH                                                   
         JE    RUNPG75                                                          
         CLC   =C'OI',AGYALPH                                                   
         JE    RUNPG75                                                          
         CLC   =C'OS',AGYALPH                                                   
         JE    RUNPG75                                                          
         CLC   =C'OE',AGYALPH                                                   
         JE    RUNPG75                                                          
         CLC   =C'OF',AGYALPH                                                   
         JE    RUNPG75                                                          
         CLC   =C'OZ',AGYALPH                                                   
         JE    RUNPG75                                                          
         CLC   =C'OL',AGYALPH                                                   
         JE    RUNPG75                                                          
* ox test agency codes                                                          
         CLC   =C'OX',AGYALPH                                                   
         JE    RUNPG75                                                          
         CLC   =C'O3',AGYALPH                                                   
         JE    RUNPG75                                                          
*                                                                               
         GOTOR VDEMTABS,DMCB,FUSNENDP                                 *         
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1            *         
         JZ    *+2                 BAD TABLEID PASSED                 *         
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2       *         
         LR    RF,RE               SAVE A(FIRST ENTRY)                *         
         USING FUSENDPD,RE                                            *         
RUNPG55  CLC   0(2,RE),EFFS        EOF - NOT LPM                      *         
         JE    RUNPG60                                                *         
         CLC   FUSAMKT,ALFMKTS     LPM?                               *         
         JE    RUNPG75             YES, THEN TT FILETYPE IS CORRECT   *         
         AR    RE,R0                                                  *         
         J     RUNPG55                                                *         
* IF WE GET HERE, THEN WE KNOW MARKET IS NOT LPM                      *         
* NEED TO READ THE MARKET RECORD TO CHECK CABLE DEMO FLAG             *         
RUNPG60  BRAS  RE,GETMKT                                              *         
         CLI   FORCEFT,0           MARKET CABLE DEMO FLAG?            *         
         JNE   RUNPG70                                                *         
* IF WE GET HERE, MARKET RECORD CABLE DEMO FIELD WAS NULL             *         
* NEED TO CHECK THE 00A PROFILE, 'CABLE DEMO LOOKUP' VALUE            *         
         XC    WORK,WORK                                              *         
         MVC   WORK+0(4),=C's00A'  <-- must be lowercase s            *         
         MVC   WORK+4(2),AGYALPH                                      *         
         MVC   WORK+6(1),DBMED                                        *         
         GOTOR VGETPROF,DMCB,WORK,WORK+16,VDATAMGR                    *         
         MVC   FORCEFT,WORK+16                                        *         
RUNPG70  CLI   FORCEFT,C'F'        SETUP TO READ FUSION?              *         
         JNE   RUNPG75             NO                                 *         
         MVC   DBSRC,FORCEFT       YES, OVERRIDE TO FUSION            *         
         MVC   INPFIL,=C'TF '                                         *         
         L     RE,FILBKPTR                                            *         
         MVC   FBKFILE-FILBKD(L'FBKFILE,RE),=C'TF '                   *         
*                                                                     *         
***********************************************************************         
*                                                                               
RUNPG75  XC    DBSELRMK,DBSELRMK                 CLEAR FIELD IN CASE            
         MVI   DBERROR,0                         'TWAS FUDGED                   
                                                                                
         MVC   DBSELAGY,AGYALPH                                                 
         MVC   DBSELMK,SPILLMKT                                                 
                                                                                
         MVC   TMPDYTIM(L'STADYTM),STADYTM                                      
                                                                                
*    NOW FIGURE OUT WHICH ENTRY OF THE FILE BK ENTRY WE SHOULD PROCESS          
*    BASED ON OUR COUNTERS                                                      
*    R2 POINTS TO CURRENT FIL/BK TABLE ENTRY                                    
         LA    R1,FILBKL                                                        
         SR    RE,RE                                                            
         L     RF,ROWCOUNT                       CURRENT FILBK TABLE            
         MR    RE,R1                             ENTRY POINTED TO FOR           
         LR    RE,R2                             STATION POINTED TO             
         AR    R2,RF                             FILEBK ENTRY                   
RUNPG80  MVC   TMPBOOK,FBKBOOK                   BOOK                           
         MVC   TMPBKTYP,FBKBKTY                  BOOKTYPE                       
         CLI   TMPBKTYP,X'40'                   ANY BOOKTYPE?                   
         BH    *+18                             DONT USE FORCE BOOKTYPE         
         CLI   FORCEBT,X'40'                    ANY FORCE BOOKTYPE ?            
         BNH   *+10                                                             
         MVC   TMPBKTYP,FORCEBT                                                 
*                                                                               
* RELOOK UP SPILLMKT FOR RADIO FOR THIS BOOKTYPE                                
* RADIO COULD HAVE DIFFERENT MKT NUMBERS FOR DIFFERENT BOOKTYPES                
         CLI   DBMED,C'R'                                                       
         BNE   RUNPG81                                                          
         GOTOR =A(TRAMKT),DMCB,RR=SRVRRELO                                      
*                                                                               
                                                                                
RUNPG81  MVC   TMPBKWK,FBKWEEK                   WEEK SPECIFIED                 
         MVC   TMPMBKS,FBKMULBK                  MULTIBOOK AVERAGE BKS          
         MVC   TMPUINDX,FBKUPIND                 UPGRADE INDEX NUMBER           
         MVI   TMPLATBN,0                                                       
         MVI   TMPLATBT,0                                                       
         MVI   NUMLATBK,0                                                       
         CLI   FBKLATBN,0                        LATEST BOOKS FEATURE           
         BE    RUNPG82B                                                         
         CLI   FBKLATWN,C'W'                     ALL WEEKS FOR NUMBER           
         BNE   *+8                               OF LATEST BOOKS?               
         MVI   TMPBKWK,C'W'                                                     
         MVC   TMPLATBN,FBKLATBN                                                
         CLI   FBKLATBT,0                        BKTYPE FOR LATEST BK?          
         BE    RUNPG82D                                                         
         MVC   DBBTYPE,FBKLATBT                                                 
         MVC   TMPLATBT,FBKLATBT                                                
         B     RUNPG82D                                                         
RUNPG82B DS    0X                                                               
* ---- GET BOOK FOR LATEST BOOK ONLY                                            
                                                                                
         MVI   LATFLAG,C'N'                                                     
         CLI   TMPBOOK+1,X'FF'                 LAT2,LAT3,LAT4..LATN ?           
         BE    RUNPG82C                                                         
         OC    TMPBOOK,TMPBOOK                                                  
         BNZ   RUNPG82L                                                         
         OC    TMPUINDX,TMPUINDX                                                
         BNZ   RUNPG85                                                          
                                                                                
* ALSO ALLOW BOOKTYPE TO BE PASSED IN FOR LATEST BOOK LOOKUP                    
RUNPG82C MVC   DBBTYPE,FBKLATBT                                                 
         MVC   TMPBKTYP,FBKLATBT                                                
         CLI   TMPBKTYP,X'40'                  ANY BOOKTYPE SPECIFIED?          
         BH    *+18                                                             
         CLI   FORCEBT,X'40'                   ANY FORCE BOOKTYPE?              
         BNH   *+10                                                             
         MVC   TMPBKTYP,FORCEBT                                                 
         CLI   TMPBOOK+1,X'FF'                 LAT2,LAT3,LAT4..LATN ?           
         BE    RUNPG82L                                                         
                                                                                
*                                                                               
RUNPG82D MVI   LATFLAG,C'Y'                      SET LASTEST FLAG               
         OC    BUYENDT,BUYENDT                   LATEST CAN NOT BE              
         BZ    *+10                              AFTER BUYEND DATE              
         MVC   DBSELDAT,BUYENDT                  IF BUYEND DATE PASSED          
                                                                                
RUNPG82E L     R0,AIO1                                                          
         ST    R0,DBAREC                                                        
         MVC   DBFILE,=C'TP '                    FILE                           
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         MVI   DBFUNCT,DBGETTLB                                                 
         MVC   DBSELMK,SPILLMKT                                                 
         XC    DBSELBK,DBSELBK                                                  
         CLI   DBSRC,C'N'                                                       
         BNE   *+18                                                             
         OC    TMPSYSC,TMPSYSC                                                  
         BZ    *+8                                                              
         MVI   DBBTYPE,C'W'                                                     
                                                                                
* LATEST BOOK CAN NOT BE PASSED BUY END DATE                                    
* CHECK IF BUY START DATE AGAINST LPM START DATE                                
                                                                                
                                                                                
RUNPG82H GOTOR (#SETUSID,ASETUSID)                                              
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
         CLI   DBERROR,0                                                        
         BNE   *+16                                                             
         XC    TMPBOOK,TMPBOOK                                                  
         MVC   TMPBOOK+1(2),DBACTBK                                             
*                                                                               
         CLI   LATFLAG,C'Y'          IF NOTHING FOUND FOR LATEST                
         BNE   RUNPG82J              LOOKUP FOR THIS STATION                    
         CLI   DBERROR,0                                                        
         BE    RUNPG82J                                                         
         MVC   TMPPCID,FBKPCID                                                  
         L     RE,FBKCOUNT                   IF WE ARE READING TOO FAR          
         AHI   RE,1                          BACK THEN JUST UPDATE              
         ST    RE,FBKCOUNT                   COUNT AND GO TO NEXT SET           
         MVC   OUTPCID,TMPPCID               PASS PCID                          
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
***      B     RUNPG170                                                         
         B     RUNPG88A              PRINT OUT NO DOMINANT PROGRAM              
*                                                                               
RUNPG82J CLI   LATFLAG,C'Y'          DONT CLEAR OUT BOOKTYPE FOR LAT            
         BE    *+10                  LAT2,LAT3..LATN LOOKUPS                    
         XC    TMPBKTYP,TMPBKTYP                                                
         XC    TMPBKWK,TMPBKWK                                                  
                                                                                
         CLI   TMPLATBN,0            THE NUMBER OF LATEST BOOKS FEATURE         
         BE    RUNPG82L              IS NOT OUR TRADITIONAL LATEST              
         MVI   LATFLAG,C'N'          LOOKUP- FLAG IT AS SO                      
                                                                                
         MVC   SVLATBK,DBACTBK                                                  
         MVC   SVLATBT,TMPLATBT                                                 
         XC    TMPBOOK,TMPBOOK                                                  
         MVC   TMPBOOK+1(2),SVLATBK                                             
         MVC   TMPBKTYP,TMPLATBT                                                
         CLI   TMPBKTYP,X'40'                  ANY BOOKTYPE SPECIFIED?          
         BH    *+18                                                             
         CLI   FORCEBT,X'40'                   ANY FORCE BOOKTYPE?              
         BNH   *+10                                                             
         MVC   TMPBKTYP,FORCEBT                                                 
                                                                                
RUNPG82L DS    0C                                                               
* IF LPM START DATE PASSED. CHECK BOOK AGAINST LPM START DATE                   
* BECAUSE WE WANT TO GIVE THEM THE PARALLEL LPM BOOKTYPE IS                     
* BOOK IS ON OR AFTER LPM START                                                 
         CLI   DBMED,C'T'                                                       
         BNE   RUNPG82N                                                         
         OC    TMPUINDX,TMPUINDX                 DONT DO THIS FOR               
         BNZ   RUNPG82N                          UPGRADES                       
         OC    LPMSTRT,LPMSTRT                                                  
         BZ    RUNPG82N                                                         
         CLC   TMPBOOK+1(L'DBSELBK),LPMSTRT                                     
         BL    RUNPG82N                                                         
         CLI   TMPBKTYP,0                                                       
         BNE   *+8                                                              
         MVI   TMPBKTYP,C'P'                                                    
         CLI   TMPBKTYP,C'H'                                                    
         BNE   *+8                                                              
         MVI   TMPBKTYP,C'I'                                                    
                                                                                
RUNPG82N MVC   INPWEEK,TMPBKWK                                                  
         CLI   FBKLATWN,C'W'                     IF ALL WEEKS FOR NUM           
         BNE   RUNPG82O                          OF LATEST BOOKS                
         CLI   TMPBKWK,0                         INITIALIZE AS FIRST            
         BNE   RUNPG82O                          AS WEEK 0 SO WE GET            
         MVI   INPWEEK,0                         MONTHLY AVG FIRST              
         B     RUNPG85                                                          
*                                                                               
RUNPG82O CLI   FBKWEEK,C'W'                      IF ALL WEEKS WANTED            
         BNE   RUNPG84                           DONT STRIP HIGH ORDER          
         MVI   TMPBKWK,1                                                        
         B     RUNPG85                                                          
RUNPG84  NI    TMPBKWK,X'0F'                     NIBBLE                         
********************************************************                        
RUNPG85  DS    0H                                                               
*                                                                               
         MVC   INPFIL,FBKFILE                    FILE                           
         MVI   DBMED,C'T'                        DEFAULT MED                    
         MVC   TMPPCID,FBKPCID                                                  
                                                                                
         L     RE,=A(DFILTAB)                                                   
         A     RE,SRVRRELO                                                      
         USING DFILTABD,RE                                                      
         OC    INPFIL,SPACES                                                    
RUNPG86  CLI   0(RE),0                         SHOULD ALWAYS HAVE               
         JNE   *+6                             A MATCH TO TABLE                 
         DC    H'0'                                                             
         CLC   INPFIL,DINFILE                                                   
         JE    RUNPG87                                                          
         AHI   RE,DFILTABL                                                      
         J     RUNPG86                                                          
RUNPG87  MVC   TMPFILE,DTMPFILE                                                 
         MVC   DBSRC,DFILSRC                                                    
         MVC   DBMED,DFILMED                                                    
         MVC   FLAG2DEC,DFIL2DEC                                                
         MVC   TMPDBFIL,DFILDBF                                                 
         MVI   ANYTPFLG,C'N'         INITIALIZE                                 
         DROP  RE                                                               
*                                                                               
RUNPG88  CLI   TMPLATBN,0            FOR SPOT DESKTOP FEATURE LATEST            
         BE    RUNPG88H              NEW                                        
         CLI   DBACTBK,X'5A'         DONT READ PASS 1996                        
         BNL   RUNPG88H                                                         
         L     RE,FBKCOUNT                   IF WE ARE READING TOO FAR          
         AHI   RE,1                          BACK THEN JUST UPDATE              
         ST    RE,FBKCOUNT                   COUNT AND GO TO NEXT SET           
         MVC   OUTPCID,TMPPCID               PASS PCID                          
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
* PRISMA WANTS NO DOMINANT PROGRAM NAME RETURNED WHEN LATEST BOOK               
* LOOKUP FAILS                                                                  
RUNPG88A MVC   SUMMNUM,=H'1'                                                    
         LA    R3,SDEMVALS                                                      
         USING SDEMVALS,R3                                                      
         ZICM  R0,NUMDEMO,(3)                                                   
RUNPG88C MVI   SDEMSEND,C'Y'                                                    
         XC    SDEMOS,SDEMOS                                                    
         AHI   R3,SDEMVALL                                                      
         BCT   R0,RUNPG88C                                                      
         MVC   SDEMNUM,NUMDEMO                                                  
         MVC   SPROG(15),=C'NO DOMINANT PGM'                                    
         L     R1,AO#DDEMO                                                      
         MVI   1(R1),O#SDEMO                                                    
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
*                                                                               
         B     RUNPG170                                                         
*                                                                               
RUNPG88H CLI   NUMLATBK,0                       DONT UPDATE THE TOTAL           
         BNE   RUNPG89                          COUNT OF FILE/BK COMBOS         
         L     RE,FBKCOUNT                      PROCESSED IN MIDST OF           
         AHI   RE,1                             SPOT DESKTOPS LATEST            
         ST    RE,FBKCOUNT                      BOOK FEATURE                    
RUNPG89  MVI   ALLTPFLG,C'N'                     INITIALIZE  FLAG               
*                                                                               
* ONLY RADIO ALLOW TO PROCESS LAT4 OR MORE.                                     
         CLI   DBMED,C'R'                                                       
         BE    RUNPG90                                                          
         CLI   TMPBOOK+1,X'FF'                                                  
         BNE   RUNPG90                                                          
         CLI   TMPBOOK+2,X'F0'          ALLOW 'F0' OR HIGHER -BACKWARD          
         BH    RUNPG90                  COMPATIBILITY WITH OLD VALDLDB          
         CLI   TMPBOOK+2,4              CODE IN SPLNK02                         
         BH    RUNPG170                                                         
*                                                                               
RUNPG90  OC    TMPMBKS,TMPMBKS                                                  
         BZ    RUNPG96                                                          
         CLI   DBMED,C'R'                                                       
         BE    RUNPG96                                                          
         B     RUNPG97                                                          
                                                                                
                                                                                
RUNPG96  OC    TMPUINDX,TMPUINDX                 ANY UPGRADES??                 
         BZ    RUNPG100                                                         
RUNPG97  GOTOR DEMUPGD                                                          
         J     RUNPG140                                                         
RUNPG100 DS    0X                                                               
         LHI   R0,1                                                             
         MVI   ALLSTATF,C'N'                                                    
         CLC   =C'0000',DBSELSTA   IS IT ALL STATIONS?                          
         BNE   RUNPG130                                                         
         MVI   ALLSTATF,C'Y'                                                    
         CLI   DBSRC,C'N'          NIELSEN                                      
         BE    RUNPG110                                                         
         CLI   DBSRC,C'C'          NCM                                          
         BE    *+8                                                              
         CLI   DBSRC,C'V'          VIDEOLOGY                                    
         BE    RUNPG110                                                         
         CLI   DBMED,C'U'          RADIO COUNTY COVERAGE DONT HAVE              
         BE    RUNPG110            SPILL MARKETS -STORED IN TMPCNTY             
                                                                                
         CLC   PREVMKT,SPILLMKT    KEEP SAME UNIVERSE VALUE                     
         BNE   RUNPG105            AS PRIOR LOOKUP IF SAME MKT                  
         CLI   DBMED,C'R'                                                       
         BNE   RUNPG118            ONLY FOR RADIO FOR NOW,                      
***      CLI   NSTATS,0            KEEP LOOKING IF PREV BOOK HAD NO             
         CLC   NSTATS,=H'0'        KEEP LOOKING IF PREV BOOK HAD NO             
         BNE   RUNPG118            STATIONS                                     
*                                                                               
RUNPG105 OC    SPILLMKT,SPILLMKT   MUST HAVE SPILL MARKET PASSED                
         BZ    RUNPG170                                                         
***PG110 MVI   NSTATS,0                                                         
RUNPG110 MVC   NSTATS,=H'0'                                                     
         L     RE,AIO5                                                          
         ST    RE,ATHISTAT                                                      
                                                                                
         MVC   DBAREC,AIO1                                                      
         XC    DBSELSTA,DBSELSTA                                                
         MVI   DBFUNCT,DBGETMS                                                  
         MVC   DBFILE,TMPFILE                                                   
         MVC   DBSELMED,DBMED                                                   
                                                                                
         XC    DBSELBK,DBSELBK                                                  
         XC    PREVSTAT,PREVSTAT                                                
         MVC   DBSELBK,TMPBOOK+1                                                
         MVC   DBBTYPE,TMPBKTYP                                                 
         MVC   DBSELRMK,SPILLMKT                                                
         MVC   DBSELSRC,DBSRC                                                   
         MVC   PREVMKT,DBSELRMK                                                 
*                                                                               
         MVI   CNTYINDX,0                                                       
         CLI   DBSELMED,C'U'       RADIO COUNTY COVERAGE                        
         JNE   RUNPG115                                                         
RUNPG113 ZIC   RE,CNTYINDX                                                      
         MHI   RE,L'DBCNTY                                                      
         LA    RF,TMPCNTY          GET COUNTY                                   
         AR    RF,RE                                                            
         CLI   0(RF),X'FF'                                                      
         JE    RUNPG118                                                         
         MVC   DBSELRMK,0(RF)                                                   
         ZIC   RE,CNTYINDX                                                      
         MHI   RE,L'DBCNTYST                                                    
         LA    RF,TMPSTATE        GET STATE                                     
         AR    RF,RE                                                            
         MVC   DBBTYPE,0(RF)                                                    
                                                                                
RUNPG115 GOTO1 VDEMAND,DMCB,DBLOCK,A(EXPHOOK)                                   
         CLI   DBSELMED,C'U'       RADIO COUNTY COVERAGE                        
         JNE   RUNPG118                                                         
         ZIC   RE,CNTYINDX                                                      
         AHI   RE,1                                                             
         STC   RE,CNTYINDX                                                      
         J     RUNPG113                                                         
                                                                                
RUNPG118 SR    R0,R0                                                            
***      ICM   R0,1,NSTATS         LIST OF STATIONS?                            
         ICM   R0,3,NSTATS         LIST OF STATIONS?                            
         BZ    RUNPG170             NO                                          
         XC    DUMSTAT,DUMSTAT                                                  
*                                                                               
         CLI   DBSELMED,C'U'       RADIO COUNTY COVERAGE HAS NO SPILL           
         JNE   RUNPG119            MARKET                                       
RUNPG118A DS   0C                                                               
         XC    TEMP,TEMP                                                        
         GOTOR GETBUFF                                                          
         JNE   RUNPG140                                                         
         MVC   TMPSTA,TEMP                                                      
         MVC   DUMSTAT(5),TMPSTA                                                
         J     RUNPG130                                                         
*                                                                               
*                                                                               
RUNPG119 DS    0C                                                               
* NIELSEN DO NOT SET SPILL OR ALPHA MKT FOR 0000 STATION                        
         CLI   DBSRC,C'N'                                                       
         BNE   *+12                                                             
         CLI   ALLSTATF,C'Y'                                                    
         BE    RUNPG119B                                                        
*                                                                               
         MVI   DUMSTAT+5,C'/'                                                   
         MVC   DUMSTAT+6(3),ALFMKTS                                             
RUNPG119B DS    0C                                                              
         L     R3,AIO5                                                          
         B     RUNPG123                                                         
*                                                                               
RUNPG120 CLI   DBSELMED,C'U'                                                    
         JNE   RUNPG122                                                         
         GOTOR SEQBUFF                                                          
         JNE   RUNPG140                                                         
         MVC   TMPSTA,TEMP                                                      
         MVC   DUMSTAT(5),TMPSTA                                                
         J     RUNPG130                                                         
*                                                                               
RUNPG122 LA    R3,L'TMPSTA(R3)                                                  
RUNPG123 MVC   TMPSTA,0(R3)                                                     
         MVC   DUMSTAT(5),TMPSTA                                                
                                                                                
RUNPG130 MVI   OUTPUTF,OUTPUTON                                                 
         GOTOR DEMLK,DMCB,(RC),RR=SRVRRELO                                      
         MVI   DPTINDEX,0                                                       
         MVC   TMPDYTIM(L'STADYTM),STADYTM       RESTORE DAYTIME                
         BCT   R0,RUNPG120                                                      
                                                                                
RUNPG140 DS    0H                                                               
         CLI   FBKLATWN,C'W'                     IF ALL WEEKS FOR NUM           
         BE    *+8                               of latest book                 
         CLI   FBKWEEK,C'W'                                                     
         BNE   RUNPG150                                                         
         ZIC   RE,TMPBKWK                                                       
         AHI   RE,1                                                             
         STC   RE,TMPBKWK                                                       
* WE NEVER SUPPORTED W WEEK OPTION CORRECTLY IN SPLNK17 BOOK VALIDATION         
* WE SHOULD ALWAYS SET INPWEEK TO TMPBKWK SO IT GETS UPDATED TO CURR            
* WEEK NUMBER BEING PROCESSED SO IT GETS DISPLAYED CORRECTLY                    
* ONLY THIS IF LATEST WEEK NUMBER MAPCODE IS SET FOR NOW                        
         CLI   FBKLATWN,C'W'                     IF ALL WEEKS FOR NUM           
         BNE   *+14                              of latest book                 
         MVC   INPWEEK,TMPBKWK                                                  
         OI    INPWEEK,X'F0'                                                    
*                                                                               
         CLI   TMPBKWK,5                                                        
         BL    RUNPG100                                                         
         MVI   TMPBKWK,1                         RESET FOR NEXT FILE            
*                                                                               
         CLI   FBKLATWN,C'W'                     IF ALL WEEKS FOR NUM           
         BNE   RUNPG150                          OF LATEST BOOKS                
         MVI   INPWEEK,0                         RESET TO WEEK 0 TO             
         MVI   TMPBKWK,0                         READ MONTHLY 1ST               
*                                                                               
RUNPG150 CLC   INPFIL,TMPFILE                                                   
         BE    RUNPG170                                                         
*                                                                               
         CLC   INPFIL,=C'TRT'       THIS DOESN'T APPLY TO TRITON,               
         BE    RUNPG170             WHICH HAS INPFIL DIFFERENT THAN             
         CLC   INPFIL,=C'TRD'       TMPFILE                                     
         BE    RUNPG170                                                         
*                                                                               
         MVC   INPFIL,TMPFILE                                                   
         MVI   ALLTPFLG,C'Y'                     SET DID ALL TP FLAG            
         J     RUNPG90                                                          
                                                                                
* NOW CHECK IF STATION CHANGED. IF IT IS THE SAME JUST KEEP PROCESSING          
* SAME STAION SAME BOOK WITH THE NEXT DAYTIME                                   
* IF IT CHANGES , PROCESS THE NEXT BOOK FOR THE STATION/DAYTIME AGAIN           
                                                                                
RUNPG170 LA    RE,STATTABL                                                      
         AR    R4,RE                             DON'T BLOW AWAY R4!!           
         LA    R1,FILBKL                                                        
         L     RF,TOTALROW                                                      
         MR    RE,R1                                                            
         L     R2,FILBKPTR                                                      
         AR    R2,RF                                                            
                                                                                
         TM    OFFLFLAG,LP_FOFFL                                                
         JZ    RUNPG180                                                         
         XC    SRVRRELO,SRVRRELO                                                
         GOTOR INITBUFF                                                         
         J     RUNPG182                                                         
                                                                                
RUNPG180 MVI   DMCB,5                            RESET TSAR BUFF                
         MVC   DMCB+1(2),=X'0005'                                               
         MVC   ATSIOREC,AIO3                                                    
         GOTOR (#INITTSR,AINITTSR),DMCB                                         
                                                                                
* TEST TO SEE IF WE ARE PROCESSING SPOT DESKTOP'S LATEST BOOKS FEATURE          
RUNPG182 DS    0X                                                               
         CLI   TMPLATBN,0                     NOT PROCESSING SPOT               
         JE    RUNPG187                       DESKTOP LATEST BK FEATURE         
         CLI   ANYTPFLG,C'Y'                                                    
         JE    RUNPG184                                                         
*DIDNT GET THE BOOK BACK FOR LAST READ - DONT UPDATE THE COUNT                  
*OF LATEST BOOKS WE HAVE READ - KEEP SAME COUNT AND REREAD A MONTH BACK         
         J     RUNPG185                                                         
                                                                                
RUNPG184 ZIC   RE,NUMLATBK                                                      
         AHI   RE,1                                                             
         STC   RE,NUMLATBK                                                      
         CLC   NUMLATBK,TMPLATBN               READ ALL THE LATEST BKS          
         JNL   RUNPG187                        REQUIRED?                        
RUNPG185 XC    DMCB,DMCB                                                        
         MVC   DMCB(2),TMPBOOK+1                                                
         GOTOR =A(GETNLATB),DMCB,RR=SRVRRELO   GET NEXT LATEST BOOK             
         MVC   TMPBOOK+1(2),DMCB+4             NEXT BOOK TO READ                
* FOR LAT NUM BOOKS FEATURE ALSO RESET THE WEEKS BACK TO 0                      
* WHEN READING NEXT LATEST BOOK                                                 
*                                                                               
         CLI   TMPBOOK+1,X'5A'                 DONT READ PASS 90 FOR            
         BNL   RUNP185B                                                         
         B     RUNPG187     IF LOWER THAN JUST EXIT                             
*&&DO                                                                           
*----------------------------------------------------------------               
**** THIS IS NOT NEEDED ANYMORE?                                                
RUNP185A L     RE,FBKCOUNT                   IF WE ARE READING TOO FAR          
         AHI   RE,1                          BACK THEN JUST UPDATE              
         ST    RE,FBKCOUNT                   COUNT AND GO TO NEXT SET           
         MVC   OUTPCID,TMPPCID               PASS PCID                          
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         B     RUNPG187                                                         
*&&                                                                             
*----------------------------------------------------------------               
*                                                                               
RUNP185B CLI   NUMLATBK,0                    READJUST THE FBKCOUNT              
         BNE   RUNPG186                      BACK 1 SINCE WE DIDNT              
         L     RE,FBKCOUNT                   FIND ANYTHING FOR THIS             
         SHI   RE,1                          LATEST BOOK AND WE ARE             
         ST    RE,FBKCOUNT                   GOING TO READ MONTH BACK           
*                                                                               
RUNPG186 L     R2,FILBKPTR                                                      
*    NOW FIGURE OUT WHICH ENTRY OF THE FILE BK ENTRY WE SHOULD PROCESS          
*    BASED ON OUR COUNTERS                                                      
*    R2 POINTS TO CURRENT FIL/BK TABLE ENTRY                                    
         LA    R1,FILBKL                                                        
         SR    RE,RE                                                            
         L     RF,ROWCOUNT                       CURRENT FILBK TABLE            
         MR    RE,R1                             ENTRY POINTED TO FOR           
         LR    RE,R2                             STATION POINTED TO             
         AR    R2,RF                             FILEBK ENTRY                   
         L     R4,MYSTAPTR                                                      
         J     RUNPG82B                                                         
*                                                                               
* TEST TO SEE OF WE HAVE PROCESS ALL THE ROWS FOR A COLUMN                      
* PROCESS ALL FIL/BKS FOR EACH STATION/DAYTIME COMBINATION                      
RUNPG187 L     RE,ROWCOUNT                                                      
         AHI   RE,1                                                             
         ST    RE,ROWCOUNT                                                      
         CLC   ROWCOUNT,TOTALROW                                                
         BE    RUNPG190                                                         
         L     R4,MYSTAPTR                       POINT TO BACK TO STAT          
         L     R2,FILBKPTR                       AND PROCESS NEXT BK            
         B     RUNPG30                                                          
*  IF WE PROCESSED THE ALL ROWS FOR THE STATION THEN PROCESS NEXT               
*  STATION UNTIL ALL THE FILE BKS ARE PROCESSED                                 
RUNPG190 ST    R4,MYSTAPTR                        KEEP POINTED TO NEXT          
         ST    R2,FILBKPTR                       STATION AND FIL/BK             
         XC    ROWCOUNT,ROWCOUNT                 ENTRY ASSOCIATED WITH          
         CLC   FBKCOUNT,TOTALFBK                 IT AND DO ALL ROWS             
         BNE   RUNPG30                           FOT THAT STATION               
                                                                                
         ZICM  R2,NUMDEMO2,(3)                ANY REMAINING DEMOS?              
         BZ    RUNPGX                                                           
         MVI   DEMOHDRF,DMHDR_CONTINUE                                          
         B     RUNPG06                                                          
                                                                                
RUNPGX   J     EXITY                                                            
         DROP  R4                                                               
*--------------------------------------------------------------------           
         LTORG                                                                  
                                                                                
UNIVDEMS DS    (MAXDEMS*3)XL3,X        UNIV-DEMO VALUES (+LIST TERMIN)          
UNIVDEML EQU   *-UNIVDEMS                                                       
         ORG    UNIVDEMS                                                        
IMPSDEMS DS    (MAXDEMS*3)XL3,X        IMP-DEMO VALUES (+LIST TERMIN)           
IMPSDEML EQU   *-IMPSDEMS                                                       
         ORG    UNIVDEMS                                                        
CUMSDEMS DS    (MAXDEMS*3)XL3,X        CUM-DEMO VALUES (+LIST TERMIN)           
CUMSDEML EQU   *-IMPSDEMS                                                       
*                                                                               
         EJECT                                                                  
YESQ     EQU   C'Y'                                                             
SPSYSQ   EQU   2                                                                
RESYSQ   EQU   8                                                                
EXITN    CR    RB,RE                                                            
         J     EXIT                                                             
EXITY    CR    RE,RE                                                            
EXIT     XIT1  ,                                                                
                                                                                
***********************************************************************         
* LIST OF MEDIA FILES TO OPEN IN ALL SYSTEMS                          *         
***********************************************************************         
*                                                                               
FILESPOT DS    0X                                ** FILE INFO **                
         DC    C'SPOT   '                        SYSTEM NAME FOR OPEN           
                                                                                
         DC    C'NSPTDIR '                                                      
         DC    C'NSPTFIL '                                                      
         DC    C'NDEMDIRA'                                                      
         DC    C'NDEMDIRN'                                                      
         DC    C'NDEMDIRR'                                                      
         DC    C'NL=DEMFA'                                                      
         DC    C'NL=DEMFN'                                                      
         DC    C'NL=DEMFR'                                                      
         DC    C'NNTIDIR '                                                      
         DC    C'NL=NTIFL'                                                      
         DC    C'NPAVDIR '                                                      
         DC    C'NL=PAVFL'                                                      
         DC    C'NCTFILE '                                                      
         DC    C'NSTAFILE'                                                      
         DC    C'X'                                                             
                                                                                
         EJECT                                                                  
***********************************************************************         
* LIST OF FILES WE CAN LOOKUP FROM  THE DEMO FILES                              
* MAPS TO DFILTABD                                                              
***********************************************************************         
DFILTAB  DS    0X                                                               
         DC    C'TT ',C'T',C'N',C'RTN',C'T4 ',C'Y',C'T'                         
         DC    C'T4 ',C'T',C'N',C'RTN',C'TT ',C'Y',C'T'                         
         DC    C'TF ',C'T',C'F',C'RTF',C'TF ',C'Y',C'T'                         
         DC    C'OTP',C'O',C'N',C'RON',C'OTP',C'Y',C'T'                         
         DC    C'WTP',C'W',C'N',C'RWN',C'WTP',C'Y',C'T'                         
         DC    C'RTP',C'R',C'A',C'RRA',C'RTP',C'N',C'T'                         
         DC    C'RTP',C'R',C'A',C'DRA',C'RTP',C'N',C'T'                         
         DC    C'RDP',C'R',C'A',C'DRA',C'RDP',C'N',C'R'                         
         DC    C'TDP',C'D',C'N',C'RDN',C'TDP',C'N',C'T'                         
         DC    C'RUA',C'U',C'A',C'RUA',C'RUA',C'N',C'R'                         
         DC    C'TRT',C'R',C'T',C'RRT',C'RTP',C'N',C'T'   TRITON TP             
         DC    C'TRT',C'R',C'T',C'DRT',C'RTP',C'N',C'T'   TRITON TP             
         DC    C'TRD',C'R',C'T',C'DRT',C'RDP',C'N',C'R'   TRITON DPT            
         DC    C'TVD',C'T',C'V',C'RTV',C'RTV',C'Y',C'T'   VIDEOLOGY  TP         
         DC    C'RTH',C'R',C'H',C'RRH',C'RTP',C'N',C'T'   IHEART TP             
         DC    C'RTD',C'R',C'H',C'DRH',C'RDP',C'N',C'T'   IHEART DPT            
         DC    C'XT ',C'T',C'N',C'RTX',C'TT ',C'Y',C'T'   FAKE MEDX             
         DC    C'NCM',C'T',C'C',C'RTC',C'NCM',C'Y',C'T'   NCM                   
DFILTABX DC    AL1(0)                                                           
***********************************************************************         
* LIST OF CORE RESIDENT FACILITIES (MAPS TO SYSADDR IN WORKD)         *         
***********************************************************************         
FACS     DS    0X                                                               
         DC    AL1(QDEMAND),AL2(CDEMAND-COMFACSD,VDEMAND-SYSADDR)               
         DC    AL1(QDEMOUT),AL2(CDEMOUT-COMFACSD,VDEMOUT-SYSADDR)               
         DC    AL1(QDEMAINT),AL2(CDEMAINT-COMFACSD,VDEMAINT-SYSADDR)            
         DC    AL1(QDEMADDR),AL2(CDEMADDR-COMFACSD,0)                           
         DC    AL1(QDEMEL),AL2(CDEMEL-COMFACSD,0)                               
         DC    AL1(QDDISP),AL2(CT00AD0-COMFACSD,0)                              
         DC    AL1(QDEMOMTH),AL2(CDEMOMTH-COMFACSD,0)                           
         DC    AL1(QDEFINE),AL2(CDEFINE-COMFACSD,VDEFINE-SYSADDR)               
         DC    AL1(QDEMTABS),AL2(CDEMTABS-COMFACSD,0)                           
         DC    AL1(0),AL2(CDATAMGR-COMFACSD,VDATAMGR-SYSADDR)                   
         DC    AL1(0),AL2(CDATAMGR-COMFACSD,VDATAMGR-SYSADDR)                   
FACSX    DC    AL1(0)                                                           
         EJECT                                                                  
                                                                                
         DROP  RB                                                               
                                                                                
**********************************************************************          
*                                                                               
**********************************************************************          
EXPHOOK  NTR1  BASE=*,LABEL=*                                                   
         L     R4,DBAREC                                                        
         USING MLKEY,R4                                                         
         CLI   DBSELMED,C'U'                                                    
         JE    EXPHK30                                                          
                                                                                
         CLI   MLSTAT,C'Z'         CUT OUT MARKET TOTALS                        
         JH    EXITY                                                            
                                                                                
         L     R2,ATHISTAT                                                      
         CLC   MLSTAT,PREVSTAT                                                  
         JE    EXITY                                                            
         MVC   0(5,R2),MLSTAT                                                   
         MVC   PREVSTAT,MLSTAT                                                  
         LA    R2,5(R2)                                                         
         ST    R2,ATHISTAT                                                      
***      AI    NSTATS,1                                                         
         ZICM  RE,NSTATS,(3)                                                    
         AHI   RE,1                                                             
         STCM  RE,3,NSTATS                                                      
***      CLI   NSTATS,100                                                       
         CLC   NSTATS,=H'300'                                                   
         JNH   EXITY                                                            
         DC    H'0'                OUT OF SPACE IN STATION LIST                 
         SPACE 2                                                                
*   RADIO COUNTY COVERAGE                                                       
EXPHK30  DS    0C                                                               
         CLI   MLSTAT,X'F0'        CUT OUT COUNTY TOTALS                        
         JNL   EXITY                                                            
         XC    TEMP,TEMP                                                        
         MVC   TEMP(L'MLSTAT),MLSTAT                                            
         MVC   TEMP2,TEMP                                                       
         GOTOR GETBUFF                                                          
         JE    EXITY                                                            
         MVC   TEMP,TEMP2                                                       
         GOTOR PUTBUFF                                                          
**       AI    NSTATS,1                                                         
         ZICM  RE,NSTATS,(3)                                                    
         AHI   RE,1                                                             
         STCM  RE,3,NSTATS                                                      
         J     EXITY                                                            
         DROP  R4                                                               
         LTORG                                                                  
**********************************************************************          
*     CANADIAN LOOKUP FOR  SPOTDESKTOP                               *          
**********************************************************************          
CANLOOK  NTR1  BASE=*,LABEL=*                                                   
         ICM   R4,7,ARSTAMKT            RATING SERVICE/STATION/AMKT             
         LA    R2,1                                                             
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
         ST    R4,MYSTAPTR                                                      
         STCM  R2,3,NUMSTAT                                                     
                                                                                
         USING CANRSRVD,R4                                                      
CANLK02  MVC   NMAMKTS,CANNMKTS                  NUMBER OF ALPHA MKTS           
                                                                                
         SR    R5,R5                             GET DEMO                       
         ICM   R5,7,ADEMO                                                       
         LA    R2,1                                                             
         TM    DEMOIND,LQ_TSINQ                                                 
         ICM   R2,3,LW_NUMN-LW_D(R5)                                            
         LA    R5,LW_DATA2-LW_D(R5)                                             
         LA    R6,DEMODEMS                                                      
         STCM  R2,3,NUMDEMO                                                     
                                                                                
CANLK05  MVC   0(3,R6),0(R5)                                                    
         AHI   R6,3                                                             
         AHI   R5,3                                                             
         BCT   R2,CANLK05                                                       
         MVI   0(R6),X'FF'                        EOL                           
                                                                                
                                                                                
CANLK06  L     R5,=A(DBLOCK1-WORKD)                                             
         LA    R5,WORKD(R5)                                                     
         USING SPDEMLKD,R5                                                      
                                                                                
* PASS DOWN THE DEMO HEADER RECORD ONCE                                         
                                                                                
         XC    SPDEMLK(SPDEMLKL),SPDEMLK                                        
* SET RESEARCH APPLICATION FLAG                                                 
         LA    RE,SYSCEXT                                                       
         USING SPLKXTD,RE                                                       
         XC    SPLKXTND,SPLKXTND                                                
         OI    SPXTFLAG,SPXTRAPP       SET CALLING RESEARCH APPL FLAG           
         ST    RE,SPLKXTND                                                      
                                                                                
         L     R0,AIO1                                                          
         ST    R0,SPLKAREC                                                      
         MVC   SPLKAFAC,ACOMFACS                                                
         LA    R0,DEMODEMS                                                      
                                                                                
         ST    R0,SPLKALST                                                      
         LA    R0,THISDEMS                                                      
         ST    R0,SPLKAVAL                                                      
         LA    R0,CANLKHK                                                       
         ST    R0,SPLKHOOK                                                      
         MVC   MYPROF1W,CANPROF                  THIS IS TO INCORPORATE         
         MVI   MYPROF1W+5,C'I'                   PRECISION PROFILE IF           
                                                                                
                                                                                
         LA    R0,MYPROF1W                       CONNECTED TO REP               
         ST    R0,SPLKA1W                        ALWAYS DMA=I                   
                                                                                
         MVI   SPLKFIL,C'T'                                                     
                                                                                
         MVI   SPLKMED,C'C'                      ALWAYS C FOR NOW               
         MVI   SPLKSRC,C'A'                      0=CSI  1=BBM                   
         CLI   CANRSRV,C'0'                                                     
         BNE   *+8                                                              
         MVI   SPLKSRC,C'N'                                                     
                                                                                
         MVC   SPLKAGY,AGYALPH                                                  
         MVC   SPLKDBK,CANBOOK                                                  
                                                                                
                                                                                
         MVC   SPLKSTA,CANSTAT                                                  
         MVI   SPLKSTA+4,C'T'                                                   
                                                                                
         MVC   SPLKDAY,CANDAY                                                   
         MVC   SPLKSTIM,CANSTIM                                                 
         MVC   SPLKETIM,CANETIM                                                 
                                                                                
         MVI   SPLKSVI,X'FF'                                                    
         MVI   SPLKBEST,C'M'                                                    
         LA    RE,CANAMKTS                                                      
         ST    RE,AMKTPTR                                                       
         MVC   NMAMKTS,CANNMKTS                  NUMBER OF ALPHA MKTS           
         MVC   SPLKALF,0(RE)                                                    
         AHI   RE,3                                                             
         ST    RE,AMKTPTR                                                       
         ZIC   RE,NMAMKTS                                                       
         SHI   RE,1                                                             
         STC   RE,NMAMKTS                                                       
CANLK20  MVC   SPLKUID,USERID                                                   
         CLI   CAN2DEC,C'Y'                                                     
         BNE   CANLK21                                                          
         XC    DUB,DUB                                                          
         LA    R0,WORK2                                                         
         ST    R0,DUB+4                                                         
         MVC   DUB+0(4),=C'SPOT'                                                
         MVI   SDBXF,C'S'                                                       
         ST    R5,DUB1                                                          
         GOTOR =A(STDBXLNK),RR=SRVRRELO                                         
                                                                                
         ICM   R1,15,FULL1                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXTTID,R1                                                       
         MVI   DBXTSCTL,C'2'                                                    
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         MVI   TMPWHOLE,WHOLERTG                                                
         OI    TMPWHOLE,WHOLESHR                                                
         OI    TMPWHOLE,WHOLEPUT                                                
         TM    TMPWHOLE,WHOLERTG           ROUND RATINGS TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTRC2T,C'Y'                YEP                                 
         TM    TMPWHOLE,WHOLESHR           ROUND SHARES  TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTSC2T,C'Y'                YEP                                 
         TM    TMPWHOLE,WHOLEPUT           ROUND PUTS    TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTPC2T,C'Y'                YEP                                 
         DROP  R1                                                               
                                                                                
CANLK21  MVI   ANYTPFLG,C'N'                      ASSUME NO DATA                
         GOTO1 VSPDEMLK,DMCB,(X'FF',SPDEMLKD)                                   
         CLI   0(R1),X'80'                                                      
         BE    CANLK33                                                          
                                                                                
CANLK22  LA    RF,DROPSVI2                                                      
         BASR  RE,RF                                                            
                                                                                
CANLK33  MVC   CANPROG(L'SPLKPRG),SPLKPRG                                       
         CLI   ANYTPFLG,C'N'                                                    
         BNE   *+10                                                             
         XC    CANPROG,CANPROG                                                  
                                                                                
         L     R3,SPLKDBLK                                                      
         USING DBLOCKD,R3                                                       
         OC    DBACTBK,DBACTBK                                                  
         BZ    CALK30                                                           
         MVC   TMPBOOK+1(L'DBACTBK),DBACTBK                                     
         MVI   MYBKH,18                          CREATE FAKE FIELD              
         GOTOR =V(UNBOOK),DMCB,(1,TMPBOOK),MYBKH,0,(C'+',=CL6' '),     +        
               RR=SRVRRELO                                                      
CALK23   MVC   CANLKBK(L'MYBK),MYBK                                             
         DROP  R3                                                               
                                                                                
CALK30   LA    R3,SDEMVALS                                                      
         USING SDEMVALS,R3                                                      
         LA    R2,THISDEMS                                                      
         ZICM  R0,NUMDEMO,(3)                                                   
CALK40   MVI   SDEMSEND,C'Y'                                                    
         MVC   SDEMOS,0(R2)                                                     
         CLI   ANYTPFLG,C'N'                                                    
         BNE   *+10                                                             
         XC    SDEMOS,SDEMOS                                                    
                                                                                
         AHI   R3,SDEMVALL                                                      
         AHI   R2,4                                                             
         BCT   R0,CALK40                                                        
                                                                                
         MVC   SDEMNUM,NUMDEMO                                                  
                                                                                
         L     R1,AO#DDEMO                                                      
         MVI   1(R1),O#SDEMO                                                    
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         DROP  R3                                                               
                                                                                
*  NOW PROCESS ALL THE ALPHA MARKETS                                            
         CLI   NMAMKTS,0                                                        
         JE    CANLK80                                                          
         L     RE,AMKTPTR                                                       
         MVC   SPLKALF,0(RE)                                                    
         AHI   RE,3                                                             
         ST    RE,AMKTPTR                                                       
         ZIC   RE,NMAMKTS                                                       
         SHI   RE,1                                                             
         STC   RE,NMAMKTS                                                       
         XC    SPLKSPL,SPLKSPL                                                  
*******  XC    THISDEMS,THISDEMS                                                
         LA    R0,THISDEMS                       CLEAR THISDEMS                 
         LHI   R1,THISDEML                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     CANLK21                                                          
* GO BACK AND PROCESS REST OF RATING SERVICE/STATIONS                           
CANLK80  XC    SPLKSPL,SPLKSPL                                                  
         XC    SPLKALF,SPLKALF                                                  
         AHI   R4,CANRSRVL                                                      
         ZICM  RE,NUMSTAT,(3)                                                   
         SHI   RE,1                                                             
         STCM  RE,3,NUMSTAT                                                     
         OC    NUMSTAT,NUMSTAT                   IF NO MORE STATION             
         JZ    CANLKX                                                           
         MVC   NMAMKTS,CANNMKTS                  NUMBER OF ALPHA MKTS           
******   XC    THISDEMS,THISDEMS                                                
         LA    R0,THISDEMS                       CLEAR THISDEMS                 
         LHI   R1,THISDEML                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     CANLK06                                                          
*                                                                               
CANLKX   J     EXITY                                                            
                                                                                
***********************************************************************         
* ROUTINE TO PROCESS VALUES RETURNED FROM SPDEMLK                               
* BE CAREFUL NOT TO CLOBBER DMCB, SINCE SPDEMLK'S CALLER DEPENDS ON IT          
***********************************************************************         
CANLKHK  NTR1                                                                   
         MVC   SAVEDMCB(24),DMCB                                                
         L     R4,SPLKDBLK                                                      
         USING DBLOCKD,R4                                                       
         TM    SPLKDAY,X'90'                       CANT HAVE VAR,AGN            
         JO    EXITY                               FOR TT,T4                    
                                                                                
         MVI   ANYTPFLG,C'Y'                                                    
         LA    RF,DROPSVI2                                                      
         BASR  RE,RF                                                            
         CLC   DBFACTOR,=H'1'                                                   
         BE    CALKHK30                                                         
         ZICM  R0,NUMDEMO,(3)                                                   
         LH    R1,DBFACTOR                                                      
         LA    R2,THISDEMS                                                      
CALKHK20 L     RF,0(R2)                                                         
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,CALKHK20                                                      
                                                                                
CALKHK30 GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         MVC   DMCB(24),SAVEDMCB                                                
         J     EXITY                                                            
                                                                                
DROPSVI2 LA    R1,THISDEMS                                                      
         LA    RF,8(R1)                                                         
         ZICM  R0,NUMDEMO,(3)                                                   
         MVC   4(4,R1),0(RF)                                                    
         LA    R1,4(R1)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,*-14                                                          
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB,R4,R5                                                         
***********************************************************************         
* ROUTINE INITIALIZES BUFFERIN CALL                                   *         
***********************************************************************         
INITBUFF NTR1  BASE=*,LABEL=*                                                   
         L     R1,ALP                                                           
         USING LP_D,R1                                                          
         L     RF,LP_ARUNP                                                      
         DROP  R1                                                               
         USING RUNPARMD,RF         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         SR    RE,RE                                                            
         ICM   RE,7,RUNPARUN                                                    
         DROP  RF                                                               
         USING RUNFACSD,RE         R6=A(RUNFACS)                                
         L     RF,RBUFFRIN                                                      
         DROP  RE                                                               
         GOTOR (RF),DMCB,('BUFFAINI',ADEMBUFF),TEMP,ACOMFACS                    
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* ROUTINE ADDS RECORD TO ARRAY IN BUFFER                              *         
***********************************************************************         
PUTBUFF  NTR1  BASE=*,LABEL=*                                                   
         L     R1,ALP                                                           
         USING LP_D,R1                                                          
         L     RF,LP_ARUNP                                                      
         DROP  R1                                                               
         USING RUNPARMD,RF         R7=A(RUNPARMS)                               
         SR    RE,RE                                                            
         ICM   RE,7,RUNPARUN                                                    
         DROP  RF                                                               
         USING RUNFACSD,RE         R6=A(RUNFACS)                                
         L     RF,RBUFFRIN                                                      
         DROP  RE                                                               
         GOTOR (RF),DMCB,('BUFFAPUT',ADEMBUFF),TEMP,ACOMFACS                    
         BE    *+6                                                              
         DC    H'0'                                                             
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
***********************************************************************         
* ROUTINE CHECKS IF RECORD EXIST IN BUFFER                            *         
***********************************************************************         
GETBUFF  NTR1  BASE=*,LABEL=*                                                   
         L     R1,ALP                                                           
         USING LP_D,R1                                                          
         L     RF,LP_ARUNP                                                      
         DROP  R1                                                               
         USING RUNPARMD,RF         R7=A(RUNPARMS)                               
         SR    RE,RE                                                            
         ICM   RE,7,RUNPARUN                                                    
         DROP  RF                                                               
         USING RUNFACSD,RE         R6=A(RUNFACS)                                
         L     RF,RBUFFRIN                                                      
         DROP  RE                                                               
         MVC   TEMP2(5),TEMP                                                    
         GOTOR (RF),DMCB,('BUFFARDH',ADEMBUFF),TEMP,ACOMFACS                    
         OC    TEMP2,TEMP2                                                      
         JZ    EXIT                                                             
         CLC   TEMP(5),TEMP2                                                    
         JE    GETBUF10                                                         
         OI    BUFFERRS-BUFFPARM(R1),BUFFERNF                                   
                                                                                
GETBUF10 CLI   BUFFERRS-BUFFPARM(R1),0        SET  CONDITION CODE               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
***********************************************************************         
* ROUTINE TO READ SEQ THE BUFFER                                      *         
***********************************************************************         
SEQBUFF  NTR1  BASE=*,LABEL=*                                                   
         L     R1,ALP                                                           
         USING LP_D,R1                                                          
         L     RF,LP_ARUNP                                                      
         DROP  R1                                                               
         USING RUNPARMD,RF         R7=A(RUNPARMS)                               
         SR    RE,RE                                                            
         ICM   RE,7,RUNPARUN                                                    
         DROP  RF                                                               
         USING RUNFACSD,RE         R6=A(RUNFACS)                                
         L     RF,RBUFFRIN                                                      
         DROP  RE                                                               
         GOTOR (RF),DMCB,('BUFFASEQ',ADEMBUFF),TEMP,ACOMFACS                    
                                                                                
SEQBUF10 CLI   BUFFERRS-BUFFPARM(R1),0        SET  CONDITION CODE               
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
***********************************************************************         
* ROUTINE TO GET DEMO PRECISION                                                 
* ENTRY   DMCB+0(1)=MODIFIER TO LOOK UP                                         
* EXIT    DMCB+4(1)=PRECISION                                                   
***********************************************************************         
GETPREC  NTR1  BASE=*,LABEL=*                                                   
         L     RE,=A(PRECTAB)                                                   
         A     RE,SRVRRELO                                                      
         MVI   DMCB+4,0                        DEFAULT                          
*                                                                               
         USING PRECTABD,RE                                                      
GETP10   CLI   0(RE),X'FF'                     DEMO MOD BETTER BE IN            
         BE    GETPRCY                                                          
         CLC   DBMED,PTABMED       MEDIA?                                       
         BNE   *+14                                                             
         CLC   PTABMOD,DMCB+0      MODIFIER?                                    
         BE    GETP20                                                           
         AHI   RE,PTABLNQ                                                       
         J     GETP10                                                           
*                                                                               
GETP20   MVC   DMCB+4(1),PTABPRE   SET PRECISION                                
*                                                                               
         CLI   FLAG2DEC,C'Y'       AUTHORIZED FOR 2 DECIMAL PREC?               
         JNE   GETPRCY                                                          
         CLI   PTABMOD,C'I'        IMPRESSIONS?                                 
         JNE   GETP30                                                           
         CLI   OPTIPRE,C'2'        2 DECIMAL IMPRESSIONS?                       
         JNE   GETP30                                                           
         MVI   DMCB+4,X'02'                                                     
         J     GETPRCY                                                          
*                                                                               
GETP30   CLI   OPTDEC,C'2'                                                      
         JNE   *+10                                                             
         MVC   DMCB+4(1),PTABOVR1  2 DECIMAL OVERRIDE PRECISION                 
*                                                                               
GETPRCY  J     EXITY                                                            
                                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
PRECTAB  DS    0XL4                             DEMO PRECISION TABLE            
         DC    C'R',X'01',C'T',X'02'                                            
         DC    C'S',X'01',C'T',X'01'                                            
         DC    C'P',X'01',C'T',X'01'                                            
         DC    C'I',X'01',C'T',X'01'                                            
         DC    C'T',X'01',C'T',X'01'                                            
         DC    C'Q',X'01',C'T',X'01'                                            
         DC    C'D',X'01',C'T',X'01'                                            
         DC    C'X',X'01',C'T',X'01'                                            
         DC    C'U',X'00',C'T',X'00'                                            
*                                                                               
         DC    C'R',X'01',C'W',X'02'            DEMOMOD/PRECISION               
         DC    C'S',X'01',C'W',X'01'                                            
         DC    C'P',X'01',C'W',X'01'                                            
         DC    C'I',X'01',C'W',X'01'                                            
         DC    C'T',X'01',C'W',X'01'                                            
         DC    C'Q',X'01',C'W',X'01'                                            
         DC    C'D',X'01',C'W',X'01'                                            
         DC    C'X',X'01',C'W',X'01'                                            
         DC    C'U',X'00',C'W',X'00'                                            
*                                                                               
         DC    C'R',X'01',C'O',X'02'            DEMOMOD/PRECISION               
         DC    C'S',X'01',C'O',X'01'                                            
         DC    C'P',X'01',C'O',X'01'                                            
         DC    C'I',X'01',C'O',X'01'                                            
         DC    C'T',X'01',C'O',X'01'                                            
         DC    C'Q',X'01',C'O',X'01'                                            
         DC    C'D',X'01',C'O',X'01'                                            
         DC    C'X',X'01',C'O',X'01'                                            
         DC    C'U',X'00',C'O',X'00'                                            
*                                                                               
         DC    C'R',X'01',C'C',X'02'            DEMOMOD/PRECISION               
         DC    C'S',X'01',C'C',X'01'                                            
         DC    C'P',X'01',C'C',X'01'                                            
         DC    C'I',X'01',C'C',X'01'                                            
         DC    C'T',X'01',C'C',X'01'                                            
         DC    C'Q',X'01',C'C',X'01'                                            
         DC    C'D',X'01',C'C',X'01'                                            
         DC    C'X',X'01',C'C',X'01'                                            
         DC    C'U',X'00',C'C',X'00'                                            
*RADIO                                                                          
         DC    C'C',X'00',C'R',X'00'                                            
         DC    C'F',X'01',C'R',X'01'                                            
         DC    C'O',X'01',C'R',X'01'                                            
         DC    C'X',X'02',C'R',X'02'                                            
         DC    C'R',X'01',C'R',X'02'            DEMOMOD/PRECISION               
         DC    C'S',X'01',C'R',X'01'                                            
         DC    C'P',X'01',C'R',X'01'                                            
         DC    C'I',X'00',C'R',X'00'                                            
         DC    C'T',X'01',C'R',X'01'                                            
         DC    C'Q',X'01',C'R',X'01'                                            
         DC    C'D',X'01',C'R',X'01'                                            
         DC    C'X',X'01',C'R',X'01'                                            
         DC    C'U',X'00',C'R',X'00'                                            
         DC    C'FF'                                                            
********************************************************************            
*              DEMLOOK ROUTINE                                     *            
********************************************************************            
DEMLK    NMOD1 0,**DEMLK*,RA                                                    
*                                     FOR TRITON,                               
         GOTOR TRITONST               FOR UNIVERSE REQUESTS, REPLACE            
*                                     TMPSTA WITH ANY STATION IN THE            
*                                     MARKET/BOOK                               
*                                                                               
         L     R5,=A(DBLOCK1-WORKD)                                             
         LA    R5,WORKD(R5)                                                     
         USING SPDEMLKD,R5                                                      
                                                                                
         MVI   ALLDAYPT,C'N'                                                    
         CLI   DBMED,C'D'                                                       
         BE    *+8                                                              
         CLI   DBMED,C'R'                                                       
         BNE   *+10                                                             
         CLC   TMPDYTIM(5),EFFS                   ALL DAYPARTS LOOKUP           
         BNE   *+8                                FOR RADIO                     
         MVI   ALLDAYPT,C'Y'                                                    
* FOR NO REACH AND FREQUENCY RADIO LOOKUPS READJUST IMPS,CUMES                  
* BACK TO HUNDREDS PRECISION MODIFIERS                                          
         CLI   ALLDAYPT,C'Y'                                                    
         BE    DMLK01B                           FOR REACH AND FREQ             
         LA    RE,DEMODEMS                                                      
DMLK01A  CLI   0(RE),X'FF'                                                      
         BE    DMLK01B                                                          
         CLI   1(RE),C'W'                        USE FULL PREC                  
         BNE   *+8                               DEMO MODFIFIERS                
         MVI   1(RE),C'C'                        FOR CUMES AND IMPS             
         CLI   1(RE),C'B'                        NON R&F USE                    
         BNE   *+8                               C AND I MODIFIERS              
         MVI   1(RE),C'I'                                                       
         AHI   RE,3                                                             
         B     DMLK01A                                                          
*                                                                               
DMLK01B  XC    ADAYPART,ADAYPART                                                
         XC    RADIOFLAG,RADIOFLAG                                              
         XC    RDAYPFLAG,RDAYPFLAG                                              
         L     RE,=A(BKSREAD-WORKD)                                             
         LA    RE,WORKD(RE)                                                     
         XC    0(L'BKSREAD,RE),0(RE)                                            
         MVI   L'BKSREAD(RE),X'FF'                                              
                                                                                
* PASS DOWN THE DEMO HEADER RECORD ONCE                                         
                                                                                
         XC    SPDEMLK(SPDEMLKL),SPDEMLK                                        
         L     R0,AIO1                                                          
         ST    R0,SPLKAREC                                                      
         MVC   SPLKAFAC,ACOMFACS                                                
                                                                                
         LA    R0,DEMODEMS                       SET RQST DEMO LIST             
                                                                                
         OC    AUNIVDEM,AUNIVDEM                                                
         BZ    *+8                                                              
         L     R0,AUNIVDEM                                                      
         ST    R0,SPLKALST                                                      
         LA    R0,THISDEMS                                                      
         ST    R0,SPLKAVAL                                                      
         LA    R0,DEMLKHK                                                       
         ST    R0,SPLKHOOK                                                      
         MVC   MYPROF1W,PROF1W                   THIS IS TO INCORPORATE         
         MVI   MYPROF1W+5,C'I'                   PRECISION PROFILE IF           
         LA    R0,MYPROF1W                       CONNECTED TO REP               
         ST    R0,SPLKA1W                        ALWAYS DMA=I                   
                                                                                
***      MVC   SPLKFIL,INPFIL                                                   
***      MVI   SPLKFIL,C'T'                                                     
         MVC   SPLKFIL,TMPDBFIL                                                 
*                                                                               
         MVC   SPLKMED,DBMED                                                    
         MVC   SPLKSRC,DBSRC                     USTV SO FAR AND NSI            
         MVC   SPLKTPTT,DBTPTTS                                                 
         CLC   =C'T4',INPFIL                     T4                             
         BNE   *+8                                                              
         MVI   SPLKTPTT,C'P'                                                    
                                                                                
         MVC   SPLKAGY,AGYALPH                                                  
         OC    OPTPAGY,OPTPAGY     ANY PRIMSA AGENCY?                           
         JZ    *+10                                                             
         MVC   SPLKAGY,OPTPAGY                                                  
                                                                                
         MVC   SPLKDBK,TMPBOOK+1                                                
         MVC   SPLKWKN,TMPBKWK                                                  
         MVC   SPLKBTYP,TMPBKTYP                                                
                                                                                
         MVC   SPLKSTA,TMPSTA                                                   
         CLI   SPLKSTA+3,C'+'                                                   
         BNE   *+8                                                              
         MVI   SPLKSTA+3,C' '                                                   
         CLI   SPLKSTA+4,C'+'                                                   
         BNE   *+8                                                              
         MVI   SPLKSTA+4,C' '                                                   
         MVC   SPLKSPL,SPILLMKT                                                 
                                                                                
                                                                                
         MVI   DPTINDEX,0                                                       
         MVI   TIMEZONE,C'E'                                                    
         CLI   DBMED,C'D'          IF TV DAYPARTS RF LOOKUP                     
         BE    DMLK01G                                                          
         CLI   DBMED,C'R'          IF RADIO,                                    
         BNE   DMLK04                                                           
         MVI   SPLKBEST,C'P'        SUPPORT OVERNIGHT DAYPART POSTING           
                                                                                
         OC    AUNIVDEM,AUNIVDEM                                                
         BZ    *+8                               RADIO                          
         MVI   SPLKFIL,C'T'                      UNIV LOOK AT RTP FILE          
*                                                                               
DMLK01G  CLC   =C'AAAAA',SPLKSTA                 MKT LEVEL LOOKUPS              
         BNE   DMLK02                                                           
         SR    R1,R1                                                            
         ICM   R1,3,SPILLMKT                                                    
         CVD   R1,DUB                                                           
         UNPK  SPLKSTA(4),DUB                                                   
         OI    SPLKSTA+3,X'F0'                                                  
         MVI   SPLKSTA+4,C'A'                                                   
*                                                                               
         CLI   DBSRC,C'H'                       IHEART HAS BAND C               
         BNE   *+8                                                              
         MVI   SPLKSTA+4,C'C'                                                   
*                                                                               
         CLI   DBMED,C'D'                                                       
         BNE   *+14                             DAYPART MKT RECORDS             
         MVI   SPLKSTA+4,C'T'                   DONT HAVE SPILL MKT             
         XC    SPLKSPL,SPLKSPL                  IN KEY                          
*                                                                               
*  CHECK IF WE ARE PROCEESING ALL DAYPARTS REQUEST FOR                          
*  RADIO REACH AND FREQUENCY                                                    
                                                                                
DMLK02   XC    SPLKAAFD,SPLKAAFD                 CLEAR OUT  ALWAYS              
         CLI   ALLDAYPT,C'N'                                                    
         BE    DMLK04                                                           
         GOTOR =A(GETDAYPT),DMCB,RR=SRVRRELO                                    
                                                                                
         CLI   DBMED,C'D'                                                       
         BE    DMLK2B                                                           
         L     RE,ADAYPART                                                      
         USING STAND,RE                                                         
         OR    RE,RE                                                            
         BZ    DEMLK60                                                          
         CLI   STAARBID,0                                                       
         JE    DEMLK60                                                          
         CLI   SPLKFIL,C'R'         ALWAYS SET DAYPART CODE IF                  
         BNE   *+10                 LOOKING UP DAYPART RADIO FILE               
         MVC   SPLKSELP,TMPSELDP+1                                              
         XC    SPLKSPL,SPLKSPL      CLEAR OUT SPILL MARKET NUMBER               
*                                   WE ARE ALWAYS USING ALPHA MKT               
         B     DMLK04                                                           
DMLK2B   MVC   SPLKSELP,TMPSELDP+1                                              
         MVC   SPLKDAY,TMPSELDP                                                 
         OC    ADAYPART,ADAYPART                                                
         BZ    DEMLK60                                                          
         DROP  RE                                                               
*                                                                               
                                                                                
                                                                                
DMLK04   LA    R3,TMPDYTIM                                                      
         ST    R3,DYTMPTR                                                       
DMLK05   DS    0C                                                               
* THIS BRANCH SKIPS ALL THE DETAIL LOOKUPS FOR THE COMPONENTS                   
* OF A ROTATION AND GOES STRAIGHT TO THE ROTATION SUMMARY LOOKUP                
*                                                                               
         B     DMLK06                           JUST DO ENTIRE ROTATION         
*                                                                               
*                                                                               
         XC    RADIOFLAG,RADIOFLAG                                              
         CLI   0(R3),X'FF'                       INDIVIDUAL COMPONENT           
         JNE   DMLK10                            UNTIL END OF DYTIME            
                                                                                
DMLK06   LA    R3,TMPDYTIM                       DO ENTIRE ROTATION             
                                                                                
DMLK6B   XC    DUB,DUB                                                          
         LA    R0,MYDBXTRA                                                      
         ST    R0,DUB+4                                                         
         MVI   SDBXF,C'S'                                                       
         ST    R5,DUB1                                                          
         GOTOR =A(STDBXLNK),RR=SRVRRELO                                         
         ICM   RF,15,FULL1                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXTLD,RF                                                        
         LA    RE,DBXTLIST                                                      
         MVC   DBXTLID,=C'DYTM'                                                 
         MVI   DBXTLIDX,0                                                       
         LA    R0,0                                                             
                                                                                
*                                               SET UP ROTATION LINK            
DMLK08   MVC   0(5,RE),0(R3)                    DAY,START END TIME              
         CLC   =C'RUA',INPFIL                  COUNTY COVERAGE                  
         BNE   *+16                            GETRUA NEEDS DBSELDAY            
         MVC   SPLKDAY,0(R3)                   AND DBSELTIM  NOT DYTM           
         MVC   SPLKTIM,1(R3)                   EXTENSION                        
         AHI   RE,5                                                             
         AHI   R3,5                                                             
*                                                                               
         CLI   0(R3),X'FF'                                                      
         BNE   DMLK08                                                           
         MVI   0(RE),0                                                          
         MVI   ROTFLAG,C'Y'                     SET DID ROTATION FLAG           
         J     DMLK12                                                           
         DROP  RF                                                               
*                                                                               
* LOOK UP A DAYTIME AT A TIME                                                   
DMLK10   CLI   SPLKMED,C'R'                                                     
         BNE   DMLK11                                                           
DMLK11   MVC   SPLKDAY,0(R3)                                                    
         MVC   SPLKTIM,1(R3)                                                    
         AHI   R3,5                             BUMP NEXT DAYTIME               
         ST    R3,DYTMPTR                       COMPONENT                       
         MVI   ROTFLAG,C'N'                     UNSET ROTATION FLAG             
                                                                                
DMLK12   MVI   SPLKSVI,X'FF'                                                    
         CLC   ALFMKTS,SPACES                                                   
         BNE   *+10                                                             
         XC    ALFMKTS,ALFMKTS                                                  
         OC    ALFMKTS,ALFMKTS                                                  
         BZ    DMLK14                                                           
         MVC   SPLKALF,ALFMKTS                                                  
*                                                                               
                                                                                
DMLK14   MVC   SPLKUID,USERID                                                   
*  SYSCODE                                                                      
         LA    RE,SYSCEXT                                                       
         USING SPLKXTD,RE                                                       
         XC    SPLKXTND,SPLKXTND   DEFAULT NO EXTENSION                         
         XC    SPXTSYSC,SPXTSYSC                                                
         MVC   SPXTSYSC,TMPSYSC                                                 
                                                                                
         MVI   SPXTFLG2,0                                                       
* THIS FLAG IS NECESSARY SO SPGETDEMF EVENTUALLY SETS                           
* SPLKOPT=SPLKOWTP- SO WE DONT DO THE WTP POSTING LOGIC                         
*                                                                               
         OI    SPXTFLAG,SPXTRAPP  SET CALLING RESEARCH APPL FLAG                
* SET NEW FLAG TO INDICATE CALLER IS SPOT DESKTOP DEMO ENGINE                   
****     MVI   SPXTFLG2,SPXTSDEM                                                
         OI    SPXTFLG2,SPXTSDEM                                                
                                                                                
         CLI   OPTIPRE,C'2'        2 DECIMAL IMPRESSION PRECISION?              
         JNE   *+8                                                              
         OI    SPXTFLG2,SPXTOP2I                                                
                                                                                
         CLC   VERSNUM,=AL4(SPVER33)           IF SPOT DESKTOP VERSION          
         BL    DMLK14A                         < 3.3.00                         
         CLC   RHOMEMKT,SPACES                 THEN  ALWAYS PROCESS             
         BE    DMLK14B                         MKTS OLD WAY - BEST MBK          
         OC    RHOMEMKT,RHOMEMKT                                                
         BZ    DMLK14B                                                          
* VERSION 3.3.00 AND LATER                                                      
* IF HOME MKT PASSED IN THEN IF THE MARKET WE ARE PROCESSING IS                 
* IS DIFFERENT THEN SPECIFY WE WANT TO PROCESS USING RADIO SPILL RULES          
* RADIO SPILL RULES STATES THAT MBK IS DONE USING THE BEST AVAILABLE            
* BOOKS WHEN AVAILABLE AND USE LATEST BOOK WHEN NOT AVAILABLE                   
* IF HOME MKT NOT PASSED IN THEN WE DONT WANT BEST MBK SPILL MKTRULE            
* - ALL BOOKS HAVE TO EXIST IN A MBK AVG                                        
         CLC   RHOMEMKT,SPLKALF      IF SPILL MARKET THEN                       
         BE    DMLK14B           HOME MARKET ALL BOOKS HAVE TO EXIST?           
         OI    SPXTFLG2,SPXTLATB    LOOKUP LATEST WHEN NO BOOKS FOUND           
         B     DMLK14A                                                          
DMLK14A  OI    SPXTFLG2,SPXTBMBK    BEST AVAILABLE MBK AVG                      
DMLK14B  ST    RE,SPLKXTND                                                      
                                                                                
* SET SPXTHEAD SO SPGETDEMF CAN CALL STAPACK TO LINK SPOT CABLE                 
* CALL LETTERS TO NSI CALL LETTERS                                              
         XC    SPXTHEAD,SPXTHEAD                                                
         OC    SPXTSYSC,SPXTSYSC                                                
         JZ    DMLK16                                                           
         ZICM  R0,SPXTSYSC,(3)                                                  
         CVD   R0,DUB                                                           
         UNPK  SPXTHEAD(4),DUB                                                  
         OI    SPXTHEAD+3,X'F0'                                                 
DMLK15   ST    RE,SPLKXTND                                                      
         DROP  RE                                                               
                                                                                
                                                                                
*                                                                               
DMLK16   OC    TMPMBKS,TMPMBKS           RADIO MULTIBOOK AVERAGE                
         BZ    DEMLK18                   GETS PROCESSED BY CALLING              
         CLI   DBSRC,C'C'                SPGETDEMF                              
         BE    *+8                       NCM ALSO PROCESS MBK BY                
         CLI   DBMED,C'R'                CALLING GETDEMF                        
         BNE   DEMLK18                   WHERE AS USTV MULTIBOOK AVG            
         XC    DUB,DUB                   IS PROCESSED IN SPDEMUP                
         LA    R0,WORK2                                                         
         ST    R0,DUB+4                                                         
         MVC   DUB+0(4),=C'MBKS'                                                
         MVI   SDBXF,C'S'                                                       
         ST    R5,DUB1                                                          
         GOTOR =A(STDBXLNK),RR=SRVRRELO                                         
                                                                                
         ICM   R1,15,FULL1                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXMBD,R1                                                        
         MVC   DBXMBKS(2),TMPBOOK+1                                             
         MVC   DBXMBKS+2(L'SPUPFBKL),TMPMBKS                                    
* FOR NIELSEN CONTINOUS AUDIO WE CAN NOT AVERAGE CERTAIN BOOKS                  
* TOGETHER BEFORE THE LIVE BOOK BECAUSE THE BOOKS CONTAIN OVERLAPPING           
* SURVEY DATA.  WE WILL ONLY RETURN THE 1ST BOOK'S DATA IN THE MBK              
* IF MBK CONSISTS OF MULTIPLE BOOKS IN THIS RESTRICTED LIST OF BOOKS            
*  version after 4.6.0.208 supports choosing universe book                      
         CLI   SPLKBTYP,0   ETHNIC DO NOT HAVE CDM                              
         BNE   DEMLK17                                                          
         CLI   DBMED,C'R'                ONLY FOR NIELSEN AUDIO NOT             
         BNE   DEMLK17                   NCM                                    
*                                                                               
         BRAS  RE,CONRADBK                                                      
         BE    DEMLK17                                                          
* CC = NOT EQ.  WE HAVE MORE THAN 1 BOOK IN MBK THAT IS RESTRICTED FOR          
* CONTINOUS AUDIO                                                               
         XC    DBXMBKS+2(L'SPUPFBKL),DBXMBKS+2                                  
         DROP  R1                                                               
*                                                                               
*                                                                               
DEMLK17  CLC   VERSNUM,=AL4(SPVER208)                                           
         BNH   *+8                                                              
         OI    SPLKOPT2,SPLKOPT2_RMBKU                                          
                                                                                
*                                                                               
                                                                                
                                                                                
DEMLK18  CLI   OPTDEC,C'2'                                                      
         BNE   DEMLK20                                                          
         XC    DUB,DUB                                                          
***      LA    R0,WORK                                                          
         LA    R0,DBXSPOTX                                                      
         ST    R0,DUB+4                                                         
         MVC   DUB+0(4),=C'SPOT'                                                
         MVI   SDBXF,C'S'                                                       
         ST    R5,DUB1                                                          
         GOTOR =A(STDBXLNK),RR=SRVRRELO                                         
                                                                                
         ICM   R1,15,FULL1                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXTTID,R1                                                       
         MVI   DBXTSCTL,C'2'                                                    
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'               1 DEC RTG/PUT                        
         MVI   DBXTTSP,X'01'               1 DEC SHARS                          
         MVI   DBXTTIP,X'02'               IMP'S TO 100'S                       
         MVI   TMPWHOLE,WHOLERTG                                                
         OI    TMPWHOLE,WHOLESHR                                                
         OI    TMPWHOLE,WHOLEPUT                                                
         TM    TMPWHOLE,WHOLERTG           ROUND RATINGS TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTRC2T,C'Y'                YEP                                 
         TM    TMPWHOLE,WHOLESHR           ROUND SHARES  TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTSC2T,C'Y'                YEP                                 
         TM    TMPWHOLE,WHOLEPUT           ROUND PUTS    TO WHOLE #S?           
         BZ    *+8                                                              
         MVI   DBXTPC2T,C'Y'                YEP                                 
         DROP  R1                                                               
                                                                                
*                                                                               
DEMLK20  DS    0C                                                               
* COUNTY COVERAGE SET STATE CODE IN BOOKTYPE FIELD                              
* SET COUNTY CODES IN DBEXTEND                                                  
         CLC   =C'RUA',INPFIL                  COUNTY COVERAGE                  
         BNE   DEMLK21                                                          
         MVC   SPLKBTYP,TMPSTATE                                                
         XC    DUB,DUB                                                          
         XC    DBXTCNTY,DBXTCNTY                                                
         XC    DBXTCTYC(DBXTCTYL),DBXTCTYC                                      
         LA    R0,DBXTCNTY                                                      
         ST    R0,DUB+4                                                         
         MVC   DUB+0(4),=C'MCTY'                                                
         MVI   SDBXF,C'S'                                                       
         ST    R5,DUB1                                                          
         GOTOR =A(STDBXLNK),RR=SRVRRELO                                         
                                                                                
         ICM   R1,15,FULL1                                                      
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBCNTYD,R1                                                       
         LA    RE,TMPSTATE                                                      
         LA    R3,TMPCNTY                                                       
         LA    RF,DBCNTSTL                                                      
DEMLK20A CLI   0(RE),X'FF'                   NO MORE STATE/COUNTY ?             
         JNE   DEMLK20B                                                         
         MVI   0(RF),X'FF'                                                      
         J     DEMLK21                                                          
DEMLK20B MVC   0(L'DBCNTYST,RF),0(RE)                                           
         MVC   1(L'DBCNTY,RF),0(R3)                                             
         AHI   RE,1                           NEXT STATE IN LIST                
         AHI   R3,2                           NEXT COUNTY IN LIST               
         AHI   RF,3                                                             
         J     DEMLK20A                                                         
         DROP  R1                                                               
                                                                                
DEMLK21  DS    0C                                                               
         MVC   SVSELDAY,SPLKDAY                                                 
         MVC   SVSETIM,SPLKTIM                                                  
* VIDEOLOGY DO NOT SET SPILL OR ALPHA MKT                                       
         CLI   DBSRC,C'C'           NCM                                         
         BE    *+8                                                              
         CLI   DBSRC,C'V'                                                       
         BE    *+8                                                              
* NIELSEN DO NOT SET SPILL OR ALPHA MKT FOR 0000 STATION                        
         CLI   DBSRC,C'N'                                                       
         BNE   DEMLK21A                                                         
         CLI   ALLSTATF,C'Y'                                                    
         BNE   DEMLK21A                                                         
*                                                                               
*                                                                               
         XC    SPLKALF,SPLKALF                                                  
         XC    SPLKSPL,SPLKSPL                                                  
*                                                                               
DEMLK21A OC    AUNIVDEM,AUNIVDEM              UNIVERSE ONLY LOOKUP              
         BZ    DEMLK21B                       ONE QHR                           
         MVI   SPLKDAY,X'40'                                                    
         MVC   SPLKTIM,=X'07D007DF'                                             
*                                             TEMP!!!                           
******   MVI   SPLKFIL,C'R'    ALWAYS LOOKUP DAYPART FOR TRITON                 
*                                                                               
DEMLK21B GOTO1 VSPDEMLK,DMCB,(X'FF',SPDEMLKD)                                   
         MVC   SPLKTIM,SVSETIM                                                  
         MVC   SPLKDAY,SVSELDAY                                                 
         MVC   THISPROG,SPLKPRG                                                 
                                                                                
         CLI   0(R1),X'80'                                                      
         BE    *+8                                                              
         CLI   0(R1),X'45'                     ANY ERRORS ?                     
         BNE   DEMLK21C                                                         
         MVI   ANYTPFLG,C'N'                                                    
* IF THERE IS AN ERROR READING UNIVERSE LEVEL FOR RADIO                         
* MAKE SURE WE STILL PASS DOWN THE 09 UNIV RECORDS                              
         CLI   DBMED,C'R'                                                       
         BNE   DMLK33                                                           
         OC    AUNIVDEM,AUNIVDEM                                                
         BZ    DMLK33                                                           
DEMLK21C DS    0C                                                               
         LA    RF,DROPSVIS                                                      
         BASR  RE,RF                                                            
*                                                                               
* DOWNLOAD UNIVERSE RECORD FOR RADIO                                            
         CLI   DBMED,C'R'                                                       
         BNE   DEMLK25                                                          
*                                                                               
         OC    AUNIVDEM,AUNIVDEM                 JUST PROCESSED UNIVRS?         
         JZ    DEMLK22                                                          
         XC    AUNIVDEM,AUNIVDEM                 YES, CLEAR IT                  
*                                                                               
         L     R1,AO#DDEMO                                                      
         MVI   1(R1),O#UDEMO                                                    
         LA    R3,UDEMVALS                                                      
         USING UDEMVALS,R3                                                      
         LA    R2,THISDEMS                                                      
         ZICM  R0,NUMDEMO,(3)                                                   
DEMLK21D MVI   UDEMSEND,C'Y'                                                    
         MVC   UDEMOS,0(R2)                                                     
         AHI   R3,UDEMVALL                                                      
         AHI   R2,4                                                             
         BCT   R0,DEMLK21D                                                      
         MVC   UDEMNUM,NUMDEMO                                                  
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         MVC   DMCB(24),SAVEDMCB                                                
                                                                                
*                                                                               
         L     R0,=A(SVUNVDEM)                                                  
         A     R0,SRVRRELO                                                      
         LHI   R1,THISDEML                       INTO SVUNVDEM                  
         LA    RE,THISDEMS                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R0,THISDEMS                       CLEAR THISDEMS                 
         LHI   R1,THISDEML                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
                                                                                
* IF THERE IS AN ERROR READING UNIVERSE LEVEL FOR RADIO                         
* MAKE SURE WE STILL PASS DOWN THE 09 UNIV RECORDS                              
         CLI   ANYTPFLG,C'N'                                                    
         BE    DMLK33                                                           
                                                                                
                                                                                
*                                                                               
* CHECK WHAT LEVEL OF RADIO READS DID WE PROCESS ALREADY                        
*                                                                               
DEMLK22  CLI   RADIOFLAG,RADIO_READIMPS          DID WE JUST READ IMPS          
         BE    DEMLK22B                          FOR RADIO                      
         CLI   RADIOFLAG,RADIO_READ_RTP          DID WE JUST READ RTP           
         BE    DEMLK24G                          FOR RADIO                      
         CLI   RADIOFLAG,RADIO_READ_RDP          DID WE JUST READ RDP           
         BE    DEMLK23E                          FOR RADIO                      
         CLI   RADIOFLAG,RADIO_READCUMS                                         
         BE    DEMLK23C                                                         
*                                                                               
*                                                                               
         GOTOR =A(CHKDAYPT),DMCB,RR=SRVRRELO                                    
         OC    TMPSELPG,TMPSELPG                 IF DAYPART NOT DEFINED         
         BZ    DEMLK23E                          THEN LOOK UP TP HOURLY         
         CLI   RDAYPFLAG,DAYPART_STANDARD                                       
         BE    DEMLK22B                                                         
*                                                                               
* CHECK IF WE SHOULD READ IMPRESSIONS                                           
* IF USER ASKED FOR MODIFER X OR F DEMO - WE NEED IMPRESSIONS                   
* AS PART OF THE CALCULATION                                                    
* ALSO ONLY GRAB IMPRESSION FOR DAYPART CUMES ONLY DAYPARTS                     
* BECAUSE STANDARD DAYPART RECORDS HAVE IMPRESSIONS ON THE RECORD               
         TM    RDEMFLAG,RDEM_X_MODIFIER         ASKS FOR TIME SPEND?            
         BO    *+8                                                              
         TM    RDEMFLAG,RDEM_O_MODIFIER         ASKS FOR TURN OVER?             
         BZ    DEMLK22B                                                         
         L     RF,=A(IMPSDEMS)                                                  
         A     RF,SRVRRELO                                                      
DEMLK22A MVI   1(RF),C'I'                        AND FORCE IMPRESSIONS          
         AHI   RF,3                                                             
         CLI   0(RF),X'FF'                                                      
         BNE   DEMLK22A                                                         
         L     R0,AIMPSDEM                                                      
         ST    R0,SPLKALST                       GO READ IMPS                   
         MVI   RADIOFLAG,RADIO_READIMPS                                         
         B     DMLK06                            NEED DYTM LINKS                
DEMLK22B DS    0C                                                               
         L     R0,=A(SVIMPDEM)                                                  
         A     R0,SRVRRELO                                                      
         LHI   R1,THISDEML                       INTO SVIMPDEM                  
         LA    RE,THISDEMS                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R0,THISDEMS                       CLEAR THISDEMS                 
         LHI   R1,THISDEML                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*                                                                               
DEMLK22C LA    R0,DEMODEMS                       NOW LETS PROCESS               
         ST    R0,SPLKALST                       ACTUAL REQUESTED DEMOS         
                                                                                
         MVC   SPLKSELP+1(1),TMPSELPG                                           
         MVI   SPLKFIL,C'R'                      RADIO DAYPART                  
*                                                                               
* IF USER ASKED FOR MODIFIER X, O OR F DEMOS THEN WE NEED TO                    
* READ CUMES DEMOS                                                              
*                                                                               
         OC    RDEMFLAG,RDEMFLAG                                                
         BZ    DEMLK23D                                                         
         CLI   RDAYPFLAG,DAYPART_STANDARD                                       
         BE    DEMLK23D                                                         
         L     RF,=A(CUMSDEMS)                                                  
         A     RF,SRVRRELO                                                      
DEMLK23  MVI   1(RF),C'C'                        AND FORCE CUMES                
         AHI   RF,3                                                             
         CLI   0(RF),X'FF'                                                      
         BNE   DEMLK23                                                          
         L     R0,ACUMSDEM                                                      
         ST    R0,SPLKALST                       GO READ CUMES                  
         MVI   RADIOFLAG,RADIO_READCUMS                                         
         B     DMLK06  NEED DAYTM LINKS                                         
                                                                                
* CUMES HAS BEEN READ.                                                          
                                                                                
DEMLK23C DS    0C                                                               
         L     R0,=A(SVCUMDEM)                                                  
         A     R0,SRVRRELO                                                      
         LHI   R1,THISDEML                       INTO SVCUMDEM                  
         LA    RE,THISDEMS                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R0,THISDEMS                       CLEAR THISDEMS                 
         LHI   R1,THISDEML                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
*                                                                               
* READ DAYPART RADIO FILE FOR DEMOLIST                                          
DEMLK23D DS    0C                                                               
         LA    R0,DEMODEMS                       NOW LETS PROCESS               
         ST    R0,SPLKALST                       ACTUAL REQUESTED DEMOS         
         MVI   RADIOFLAG,RADIO_READ_RDP          SET TO READ RDP MODE           
         B     DEMLK24                                                          
* READ HOURLY RADIO FILE FOR DEMOLIST                                           
DEMLK23E MVI   RADIOFLAG,RADIO_READ_RTP          SET TO READ RTP MODE           
         XC    SPLKSELP,SPLKSELP                                                
         MVI   SPLKFIL,C'T'                      RADIO FILE                     
         LA    R0,DEMODEMS                       NOW LETS PROCESS               
         ST    R0,SPLKALST                       ACTUAL REQUESTED DEMOS         
                                                                                
         L     RE,=A(THISDMS2-WORKD)                                            
         LA    RE,WORKD(RE)                                                     
         LR    R0,RE                                                            
         LHI   R1,THISDM2L                       THISDMS2                       
         LA    RE,THISDEMS                                                      
         LR    RF,R1                 LOOKS LIKE THIS CLEARS DBMED               
         MVCL  R0,RE                                                            
*                                                                               
* FOR THE LAST 4 DAYPARTS  OF THE R&F MODEL ONLY PASS CUMES BACK                
         CLI   ALLDAYPT,C'Y'                                                    
         BNE   DEMLK23F                                                         
         L     RE,ADAYPART                                                      
         OR    RE,RE                                                            
         BZ    DEMLK23F                                                         
         USING STAND,RE                                                         
         CLI   STANDFLG,ST_RF_DPT_RETURNZEROS                                   
         BE    DEMLK24G                                                         
*====================================================================           
* IF STANDARD DAYPARTS                                                          
* DONT GO BACK AND READ RTP RECORD. THE DAYPART RECORD SHOULD HAVE              
* ALL DEMOS                                                                     
*                                                                               
* COMMENTING THIS STATMENT OUT WILL REDIRECT THRE READ BACK FOR HOURLY          
* RTP RECORD                                                                    
DEMLK23F CLI   RDAYPFLAG,DAYPART_STANDARD                                       
         BE    DEMLK24G                                                         
*===================================================================            
DEMLK24  CLI   ROTFLAG,C'Y'                                                     
         JE    DMLK06                                                           
         J     DMLK16                            GO BACK TO READ DEMOS          
         DROP  R3                                                               
*                                                                               
* COMBINE THE TWO DEMO LIST                                                     
*                                                                               
DEMLK24G DS    0C                                                               
         GOTOR =A(COMBDEMO),DMCB,RR=SRVRRELO                                    
*                                                                               
* IF DAYPART RECORD WAS READ FOR RADIO THEN GO CALCULATE X,O,F                  
* DEMOS                                                                         
         CLI   RDAYPFLAG,DAYPART_CUME_ONLY                                      
         BNE   DEMLK25                                                          
         TM    RDEMFLAG,RDEM_X_MODIFIER         ASKS FOR TIME SPEND?            
         BZ    DEMLK24H                                                         
         GOTOR =A(CALCTSL),DMCB,RR=SRVRRELO                                     
DEMLK24H TM    RDEMFLAG,RDEM_O_MODIFIER         ASKS FOR TURN OVER?             
         BZ    DEMLK24K                                                         
         GOTOR =A(CALCTOR),DMCB,RR=SRVRRELO                                     
DEMLK24K TM    RDEMFLAG,RDEM_F_MODIFIER         ASKS FOR CUME RATINGS           
         BZ    DEMLK25                                                          
         GOTOR =A(CALCCRTG),DMCB,RR=SRVRRELO                                    
*                                                                               
                                                                                
DEMLK25  MVC   OUTPCID,TMPPCID                   DONT PASS PCID                 
         CLI   ROTFLAG,C'Y'                                                     
         JE    DMLK35                                                           
*                                                                               
* PROCESS THE DETAIL                                                            
                                                                                
         CLI   OUTPUTF,OUTPUTOF                  DO WE WANT OUTPUT              
         BE    DMLK25A                                                          
         CLI   ANYTPFLG,C'N'                     NO DETAILS                     
         BE    DMLK33                            THEN DO NEXT                   
         CLI   ALLTPFLG,C'Y'                     IF WE ARE DOING SECOND         
         JE    DEMLK26                           TP READ, T4-TP, TP-T4          
         MVC   OUTPCID,TMPPCID                   DONT PASS PCID                 
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
DMLK25A  GOTOR =A(CLRAREA),RR=SRVRRELO                                          
*                                                                               
DEMLK26  LA    R3,PGDTVALS                                                      
         USING PGDTVALS,R3                                                      
         MVI   PGDTSEND,C'Y'                                                    
         MVC   PFILE,INPFIL                                                     
         CLI   TMPDBFIL,C'R'    IF RDP REQUESTED                                
         BNE   DEMLK26A                                                         
         CLI   SPLKFIL,C'T'     BUT RTP WAS LOOKED UP INSTEAD                   
         BNE   DEMLK26A                                                         
         MVC   PFILE,=C'RTP'                                                    
         CLC   INPFIL,=C'TRD'   RDP FOR TRITON                                  
         BNE   DEMLK26A                                                         
         MVC   PFILE,=C'TRT'    RTP FOR TRITON                                  
DEMLK26A DS    0X                                                               
*                                                                               
         MVI   PACTF,C'N'                                                       
         CLI   ALLTPFLG,C'Y'                                                    
         BE    *+8                                                              
         MVI   PACTF,C'Y'                                                       
         MVC   PSTAT(L'DUMSTAT),DUMSTAT                                         
                                                                                
         CLI   PSTAT+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   PSTAT+4,0                                                        
*                                                                               
         CLC   INPFIL,=C'TVD'                                                   
         BNE   *+8                                                              
         MVI   PSTAT+4,C'D'                                                     
*                                                                               
                                                                                
         MVC   PPROG(L'SPLKPRG),SPLKPRG                                         
         MVC   PWEEKS(1),TMPBKWK                                                
         OI    PWEEKS,X'F0'                                                     
         MVC   PDAYS,SPLKDAY                                                    
         MVC   PSTIME,SPLKTIM                                                   
         MVC   PETIME,SPLKTIM+2                                                 
         OC    PSTIME,PSTIME                                                    
         BNZ   *+10                                                             
         MVC   PSTIME,=H'2400'                                                  
         OC    PETIME,PETIME                                                    
         BNZ   *+10                                                             
         MVC   PETIME,=H'2400'                                                  
                                                                                
         XC    DMCB,DMCB                                                        
         MVI   DMCB,BK_MONTH                                                    
         CLC   =C'OTP',PFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_OVERN                                                    
         CLC   =C'WTP',PFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_WKLY                                                     
         MVC   DMCB+1(3),TMPBOOK                                                
         LA    RE,PBOOK                                                         
         ST    RE,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         MVC   DMCB+9(1),INPWEEK                                                
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
*                                                                               
         L     R2,DMCB                                                          
         CLI   DBSRC,C'C'         NCM                                           
         BE    *+8                                                              
         CLI   DBMED,C'R'            RADIO MULTIBOOK AVERAGE CALLS              
         BNE   DMLK28                                                           
         OC    TMPMBKS,TMPMBKS                                                  
         BZ    DMLK28                                                           
         LA    R4,TMPMBKS                                                       
         LA    R0,3                                                             
*                                                                               
DMLK27   MVI   0(R2),C'+'                                                       
         AHI   R2,1                                                             
         XC    WORK,WORK                                                        
         MVC   WORK+1(5),0(R4)                                                  
         MVI   DMCB,BK_MONTH                                                    
         MVC   DMCB+1(3),WORK                                                   
         ST    R2,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
         L     R2,DMCB         ADDRESS OF END OF BOOK STRING                    
*                                                                               
         AHI   R4,2                                                             
         OC    0(2,R4),0(R4)                                                    
         BZ    DMLK28                                                           
         BCT   R0,DMLK27                                                        
*                                                                               
DMLK28   MVC   PGDTNUM,=H'1'                                                    
         LA    R4,PMFID                                                         
         USING MFIDD,R4                                                         
         EDIT  (B1,PDAYS),(3,MFDAY),ALIGN=RIGHT,ZERO=NOBLANK,FILL=0             
         EDIT  (B2,PSTIME),(4,MFSTIME),ALIGN=RIGHT,ZERO=NOBLANK,FILL=0          
         EDIT  (B2,PETIME),(4,MFETIME),ALIGN=RIGHT,ZERO=NOBLANK,FILL=0          
         DROP  R3,R4                                                            
         LA    R3,DDEMVALS                                                      
         USING DDEMVALS,R3                                                      
         LA    R2,THISDEMS                                                      
         ZICM  R0,NUMDEMO,(3)                                                   
DMLK30   MVI   DDEMSEND,C'Y'                                                    
         MVC   DDEMOS,0(R2)                                                     
         AHI   R3,DDEMVALL                                                      
         AHI   R2,4                                                             
         BCT   R0,DMLK30                                                        
         MVC   DDEMNUM,NUMDEMO                                                  
         L     R1,AO#DDEMO                                                      
         MVI   1(R1),O#DDEMO                                                    
         CLI   OUTPUTF,OUTPUTOF                  DO WE WANT OUTPUT              
         BE    DMLK31                                                           
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
DMLK31   GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         MVC   DMCB(24),SAVEDMCB                                                
DMLK33   CLI   ROTFLAG,C'Y'                                                     
         JE    DMLK35                                                           
         L     R3,DYTMPTR                                                       
         MVI   SPLKFIL,C'T'                      RESET TO RTP                   
         J     DMLK05                                                           
         DROP  R3                                                               
*                                                                               
* PROCESS SUMMARY RECORD                                                        
                                                                                
DMLK35   CLI   DBMED,C'D'         TV DAYPARTS LOOKUP PASS                       
         BE    DMLK35A            PCID SINCE WE ONLY PASS SUMMARY RECS          
*                                                                               
         CLI   ALLTPFLG,C'Y'                     IF WE ARE DOING SECOND         
         JE    DEMLKX                            TP READ, T4-TP, TP-T4          
DMLK35A  LA    R3,SUMMVALS                       WE DONT NEED 2ND SUMM          
         USING SUMMVALS,R3                                                      
*                                                                               
         CLI   DBMED,C'D'         TV DAYPARTS LOOKUP PASS PCID ONLY             
         BNE   *+16               ONCE- DONT PASS DOWN AGAIN IF                 
         CLI   TIMEZONE,C'E'      TRYING CENTRAL/MOUNTAIN DAYPART               
         BE    DMLK35B                                                          
         BNE   DMLK35D                                                          
*                                                                               
         CLI   ANYTPFLG,C'N'                                                    
         BNE   DMLK35D                                                          
* COMMENT OUT PRISMA DOES WANT NO DOMINAT PROGRAM RETURNED                      
         CLI   TMPLATBN,0       IF NOTHING FOUND ANY WE ARE PROCESSING          
         BNE   DEMLKX           SPOT DESKTOP'S LATEST BOOKS REQUEST             
DMLK35B  MVC   OUTPCID,TMPPCID  THEN EXIT AND DONT PASS DOWN PCID               
         CLI   OUTPUTF,OUTPUTOF DO WE WANT OUTPUT                               
         BE    DMLK35C                                                          
         L     R1,ALP           ELSE PROCEED BELOW -PASS DOWN                   
         L     RF,LP_APUTO-LP_D(R1)          NO DOMINANT PGM                    
         GOTOR (RF)                                                             
DMLK35C  GOTOR =A(CLRAREA),RR=SRVRRELO                                          
                                                                                
DMLK35D  DS    0C                                                               
         MVI   SUMMSEND,C'Y'                                                    
         MVC   SFILE,INPFIL                                                     
         CLI   TMPDBFIL,C'R'    IF RDP REQUESTED                                
         BNE   DMLK35DX                                                         
         CLI   SPLKFIL,C'T'     BUT RTP WAS LOOKED UP INSTEAD                   
         BNE   DMLK35DX                                                         
         MVC   SFILE,=C'RTP'                                                    
         CLC   INPFIL,=C'TRD'   RDP FOR TRITON                                  
         BNE   DMLK35DX                                                         
         MVC   SFILE,=C'TRT'    RTP FOR TRITON                                  
DMLK35DX DS    0X                                                               
*                                                                               
         MVC   SSTAT(L'DUMSTAT),DUMSTAT                                         
         CLI   SSTAT+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   SSTAT+4,0                                                        
         CLC   INPFIL,=C'TVD'                                                   
         BNE   *+8                                                              
         MVI   SSTAT+4,C'D'                                                     
         MVC   SPROG(L'SPLKPRG),SPLKPRG                                         
*                                                                               
         XC    SCMKTFLG,SCMKTFLG                                                
         CLC   =C'RTP',SFILE                                                    
         BE    *+10                                                             
         CLC   =C'TRT',SFILE    RTP FOR TRITON                                  
         BNE   *+12                                                             
         MVI   SCMKTFLG,C'N'                                                    
         CLI   CONDMKTF,C'C'                                                    
         BNE   *+8                                                              
         MVI   SCMKTFLG,C'Y'                                                    
*                                                                               
         L     RE,ADAYPART                                                      
         OR    RE,RE                                                            
         BZ    DMLK38                                                           
         CLI   DBMED,C'D'                                                       
         BE    DMLK38                                                           
         USING STAND,RE                                                         
         MVC   SPROG,STANDESC                                                   
         MVC   SARBID,STAARBID                                                  
         DROP  RE                                                               
*                                                                               
DMLK38   OC    TMPSYSC,TMPSYSC                                                  
         BZ    DMLK40                                                           
         EDIT  (B2,TMPSYSC),(5,SSYSC),ALIGN=LEFT,ZERO=BLANK                     
                                                                                
DMLK40   CLI   ANYTPFLG,C'N'                                                    
         BNE   DMLK42                                                           
* COMMENT OUT PRISMA DOES WANT NO DOMINAT PROGRAM RETURNED                      
         CLI   TMPLATBN,0       IF NOTHING FOUND ANY WE ARE PROCESSING          
         BNE   DEMLKX           SPOT DESKTOP'S LATEST BOOKS REQUEST             
*                                                                               
         XC    SPROG,SPROG      THEN JUST EXIT- DONT PASS DOWN                  
         CLI   DBMED,C'D'                                                       
         BNE   DMLK41                                                           
         CLI   TIMEZONE,C'E'                                                    
         BNE   DMLK41                                                           
         MVI   TIMEZONE,C'C'                                                    
         ZIC   RE,DPTINDEX      REREAD SAME DPTINDEX BUT FOR                    
         SHI   RE,1             CENTRAL TIME ZONE                               
         STC   RE,DPTINDEX                                                      
         MVI   ANYTPFLG,C'N'                                                    
         L     RE,SPLKAREC                CLEAR OUT                             
         LA    RF,2000                                                          
         XCEF                                                                   
         B     DMLK02                                                           
DMLK41   MVC   SPROG(15),=C'NO DOMINANT PGM'                                    
                                                                                
DMLK42   XC    DMCB,DMCB                                                        
         MVI   DMCB,BK_MONTH                                                    
         CLC   =C'OTP',SFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_OVERN                                                    
         CLC   =C'WTP',SFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_WKLY                                                     
         MVC   DMCB+1(3),TMPBOOK                                                
         LA    RE,SBOOK                                                         
         ST    RE,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         MVC   DMCB+9(1),INPWEEK                                                
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
*                                                                               
         L     R2,DMCB                                                          
         CLI   DBMED,C'R'            RADIO MULTIBOOK AVERAGE CALLS              
         BNE   DMLK50                                                           
* IF LATEST BOOK FLG TURNED ON EFFECTIVE VERSION 3.3.0.0                        
* THEN DISPLAY ONLY THE REQUESTED BOOKS WHICH WAS READ                          
         LA    RE,SYSCEXT                                                       
         USING SPLKXTD,RE                                                       
         TM    SPXTFLG2,SPXTLATB    LOOKUP LATEST WHEN NO BOOKS FOUND           
         BZ    DEMLK43                                                          
*                                                                               
         LA    R0,4                                                             
         L     R4,=A(BKSREAD-WORKD)                                             
         LA    R4,WORKD(R4)                                                     
         OC    0(2,R4),0(R4)                                                    
         BZ    *+10                                                             
         XC    0(2,R4),EFFS         FLIP THE BOOKS IN BKSREAD                   
         AHI   R4,2                                                             
         BCT   R0,*-20                                                          
*                                                                               
         DROP  RE                                                               
         LA    R2,SBOOK                                                         
         L     R4,=A(BKSREAD-WORKD)                                             
         LA    R4,WORKD(R4)                                                     
         LA    R0,4                                                             
         B     DEMLK44A                                                         
                                                                                
* REGULAR OLD CODE TO FORMAT BOOKS REQUESTED                                    
DEMLK43  OC    TMPMBKS,TMPMBKS                                                  
         BZ    DMLK50                                                           
         LA    R4,TMPMBKS                                                       
         LA    R0,3                                                             
DEMLK44  MVI   0(R2),C'+'                                                       
         AHI   R2,1                                                             
DEMLK44A XC    WORK,WORK                                                        
         MVC   WORK+1(5),0(R4)                                                  
         MVI   DMCB,BK_MONTH                                                    
         MVC   DMCB+1(3),WORK                                                   
         ST    R2,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
         L     R2,DMCB         ADDRESS OF END OF BOOK STRING                    
*                                                                               
         AHI   R4,2                                                             
         OC    0(2,R4),0(R4)                                                    
         BZ    DMLK50                                                           
         BCT   R0,DEMLK44                                                       
*                                                                               
DMLK50   MVC   SUMMNUM,=H'1'                                                    
         DROP  R3                                                               
         LA    R3,SDEMVALS                                                      
         USING SDEMVALS,R3                                                      
         LA    R2,THISDEMS                                                      
         ZICM  R0,NUMDEMO,(3)                                                   
DMLK56   MVI   SDEMSEND,C'Y'                                                    
         MVC   SDEMOS,0(R2)                                                     
         CLI   ANYTPFLG,C'N'                                                    
         BNE   *+10                                                             
         XC    SDEMOS,SDEMOS                                                    
         AHI   R3,SDEMVALL                                                      
         AHI   R2,4                                                             
         BCT   R0,DMLK56                                                        
         MVC   SDEMNUM,NUMDEMO                                                  
         L     R1,AO#DDEMO                                                      
         MVI   1(R1),O#SDEMO                                                    
         CLI   OUTPUTF,OUTPUTOF                                                 
         BE    DMLK58                                                           
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
DMLK58   GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         DROP  R3                                                               
* FIGURE OUT IF RADIO DEMO IS A CONDENSE MARKET DEMO                            
*                                                                               
*                                                                               
*                                                                               
DEMLK60  CLI   ALLDAYPT,C'N'                                                    
         JE    DEMLKX                                                           
         MVI   ANYTPFLG,C'N'                                                    
         MVI   TIMEZONE,C'E'                                                    
         L     RE,SPLKAREC                CLEAR OUT                             
         LA    RF,2000                                                          
         XCEF                                                                   
         XC    RADIOFLAG,RADIOFLAG                                              
*                                                                               
         CLI   DPTINDEX,0                                                       
         JNE   DMLK02                                                           
*                                                                               
                                                                                
DEMLKX   J     EXITY                                                            
***********************************************************************         
* ROUTINE TO PROCESS VALUES RETURNED FROM SPDEMLK                               
* BE CAREFUL NOT TO CLOBBER DMCB, SINCE SPDEMLK'S CALLER DEPENDS ON IT          
***********************************************************************         
DEMLKHK  NTR1                                                                   
         MVC   SAVEDMCB(24),DMCB                                                
         L     R4,SPLKDBLK                                                      
         USING DBLOCKD,R4                                                       
         L     RF,DBAREC                                                        
         MVC   READBKTP,(DRBTYP-DRKEY)(RF)                                      
                                                                                
         MVI   CONDMKTF,0                                                       
         CLI   DBSELMED,C'R'                                                    
         BNE   DMLKHK08                                                         
         LA    RE,25(RF)                                                        
         MVC   CONDMKTF,2(RE)                                                   
*                                                                               
         L     RE,=A(BKSREAD-WORKD)                                             
         LA    RE,WORKD(RE)                                                     
DMLKHK04 OC    0(2,RE),0(RE)      NEW BOOK                                      
         BZ    DMLKHK07                                                         
         CLC   0(2,RE),(DRBOOK-DRKEY)(RF)  OLD BOOK DONT SAVE                   
         BE    DMLKHK08                                                         
DMLKHK05 AHI   RE,2                                                             
         CLI   0(RE),X'FF'                                                      
         BE    DMLKHK08                                                         
         B     DMLKHK04                                                         
DMLKHK07 MVC   0(2,RE),(DRBOOK-DRKEY)(RF)                                       
*                                                                               
DMLKHK08 L     RE,=A(DFILTAB)                                                   
         A     RE,SRVRRELO                                                      
         USING DFILTABD,RE                                                      
DMLKHK12 CLI   0(RE),0                                                          
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DFILFMS,0(RF)                                                    
         JE    *+12                                                             
         AHI   RE,DFILTABL                                                      
         J     DMLKHK12                                                         
         MVC   TMPFILRD,DINFILE                                                 
         DROP  RE                                                               
         MVC   SAVEDIV,DBDIVSOR                                                 
                                                                                
         TM    SPLKDAY,X'90'                       CANT HAVE VAR,AGN            
         JO    EXITY                               FOR TT,T4                    
                                                                                
         MVI   ANYTPFLG,C'Y'                                                    
         LA    RF,DROPSVIS                                                      
         BASR  RE,RF                                                            
         CLC   DBFACTOR,=H'1'                                                   
         BE    DMLKHK30                                                         
         ZICM  R0,NUMDEMO,(3)                                                   
         LH    R1,DBFACTOR                                                      
         LA    R2,THISDEMS                                                      
DMLKHK20 L     RF,0(R2)                                                         
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,DMLKHK20                                                      
                                                                                
DMLKHK30 GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         MVC   DMCB(24),SAVEDMCB                                                
         J     EXITY                                                            
                                                                                
DROPSVIS LA    R1,THISDEMS                                                      
         LA    RF,8(R1)                                                         
         ZICM  R0,NUMDEMO,(3)                                                   
         MVC   4(4,R1),0(RF)                                                    
         LA    R1,4(R1)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,*-14                                                          
         BR    RE                                                               
*----------------------------------------------                                 
* COMBINE RADIO DEMOS FOR DAYPART AND HOURLY FILE                               
*----------------------------------------------                                 
COMBDEMO NTR1                                                                   
         LA    RE,THISDEMS                                                      
         L     RF,=A(THISDMS2-WORKD)                                            
         LA    RF,WORKD(RF)                                                     
         ZICM  R0,NUMDEMO,(3)                                                   
COMBDEM4 OC    0(4,RE),0(RE)                                                    
         BNZ   COMBDEM6                                                         
         MVC   0(4,RE),0(RF)                                                    
COMBDEM6 AHI   RE,4                                                             
         AHI   RF,4                                                             
         BCT   R0,COMBDEM4                                                      
COMDEMX  XIT1                                                                   
*                                                                               
*----------------------------------------------                                 
* CALCULATE TIME SPEND LISTENING                                                
*----------------------------------------------                                 
CALCTSL  NTR1                                                                   
         LA    R3,DEMODEMS                                                      
         L     RF,=A(SVIMPDEM)                                                  
         A     RF,SRVRRELO                                                      
         L     RE,=A(SVCUMDEM)                                                  
         A     RE,SRVRRELO                                                      
         LA    R2,THISDEMS                                                      
CALCTS03 CLI   0(R3),X'FF'                                                      
         BE    CALCTSLX                                                         
         CLI   1(R3),C'X'          TSL?                                         
         BNE   CALCTS05                                                         
         SR    R4,R4                                                            
         L     R5,0(RF)            GET IMPRESSIONS                              
         M     R4,=F'100'                                                       
         MH    R5,SAVEDIV          MULT BY # OF QTR HOURS                       
         OC    0(4,RE),0(RE)       CAN'T DIVIDE BY ZERO                         
         BZ    CALCTS05            KEEP IT AS ZERO                              
         D     R4,0(RE)            DIVIDE BY CUME                               
         A     R5,=F'5'                                                         
         SR    R4,R4                                                            
         D     R4,=F'10'                                                        
                                                                                
         SR    R4,R4                                                            
         M     R4,=F'15'                                                        
         D     R4,=F'600'                                                       
         LR    R1,R4                R1=REMAINDER                                
         SR    R4,R4                                                            
         M     R4,=F'100'           R5=HOURS                                    
                                                                                
         AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         AR    R5,R1                R1= TOTAL TIME HOUR:MINUTES                 
                                                                                
                                                                                
         ST    R5,0(R2)            REPLACE IT WITH TSL                          
CALCTS05 LA    R3,3(R3)            NEXT DEMO                                    
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         LA    R2,4(R2)                                                         
         B     CALCTS03                                                         
CALCTSLX DS    0C                                                               
         XIT1                                                                   
*                                                                               
*----------------------------------------------                                 
* CALCULATE TURN OVER RATIO                                                     
*----------------------------------------------                                 
CALCTOR  NTR1                                                                   
         LA    R3,DEMODEMS                                                      
         L     RF,=A(SVIMPDEM)                                                  
         A     RF,SRVRRELO                                                      
         L     RE,=A(SVCUMDEM)                                                  
         A     RE,SRVRRELO                                                      
         LA    R2,THISDEMS                                                      
CALCTOR3 CLI   0(R3),X'FF'                                                      
         BE    CALCTORX                                                         
         CLI   1(R3),C'O'          TURN OVER RATIO?                             
         BNE   CALCTOR5                                                         
         SR    R4,R4                                                            
         L     R5,0(RE)            GET CUMES                                    
         M     R4,=F'100'                                                       
         OC    0(4,RE),0(RE)       CAN'T DIVIDE BY ZERO                         
         BZ    CALCTS05            KEEP IT AS ZERO                              
         OC    0(4,RF),0(RF)       CAN'T DIVIDE BY ZERO                         
         BZ    CALCTS05            KEEP IT AS ZERO                              
         D     R4,0(RF)            DIVIDE BY IMP                                
         A     R5,=F'5'                                                         
         SR    R4,R4                                                            
         D     R4,=F'10'                                                        
                                                                                
         ST    R5,0(R2)            REPLACE IT WITH TURN OVER RATIO              
CALCTOR5 LA    R3,3(R3)            NEXT DEMO                                    
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         LA    R2,4(R2)                                                         
         B     CALCTOR3                                                         
                                                                                
CALCTORX DS    0C                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
*----------------------------------------------                                 
* CALCULATE CUME RATINGS                                                        
*----------------------------------------------                                 
CALCCRTG NTR1                                                                   
         LA    R3,DEMODEMS                                                      
         L     RF,=A(SVUNVDEM)                                                  
         A     RF,SRVRRELO                                                      
         L     RE,=A(SVCUMDEM)                                                  
         A     RE,SRVRRELO                                                      
         LA    R2,THISDEMS                                                      
CALCCRT3 CLI   0(R3),X'FF'                                                      
         BE    CALCCRTX                                                         
         CLI   1(R3),C'F'          TURN OVER RATIO?                             
         BNE   CALCCRT5                                                         
         SR    R4,R4                                                            
         L     R5,0(RE)            GET CUMES                                    
         M     R4,=F'10000'                                                     
         OC    0(4,RE),0(RE)       CAN'T DIVIDE BY ZERO                         
         BZ    CALCCRT5            KEEP IT AS ZERO                              
         D     R4,0(RF)            DIVIDE BY UNIV                               
         A     R5,=F'5'                                                         
         SR    R4,R4                                                            
         D     R4,=F'10'                                                        
                                                                                
         ST    R5,0(R2)            REPLACE IT WITH CUME RATING                  
CALCCRT5 LA    R3,3(R3)            NEXT DEMO                                    
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         LA    R2,4(R2)                                                         
         B     CALCCRT3                                                         
                                                                                
CALCCRTX DS    0C                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
*----------------------------------------------                                 
* ROUTINE CHECKS DAY TIME AGAINST DAYPART TABLE FOR RADIO                       
* OUTPUT- TMPSELPG WILL BE SET TO PROGRAM NUMBER IN TABLE                       
*----------------------------------------------                                 
CHKDAYPT NTR1                                                                   
         MVI   TMPSELPG,0                                                       
         LA    RF,TMPDYTIM                                                      
         LA    RE,1                                                             
CHKDYP10 CLI   0(RF),X'FF'                                                      
         BE    CHKDYP16                                                         
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         B     CHKDYP10                                                         
CHKDYP16 SHI   RE,1         RE=NUM OF BYTE IN TMPDYTIM -1 FOR EX INSTRU         
*                                                                               
         LA    RF,TMPDYTIM                                                      
***      LA    R1,STANDPTS                                                      
         L     R1,=A(STANDPTS)                                                  
         A     R1,SRVRRELO                                                      
         USING STAND,R1                                                         
CHKDYP20 CLI   0(R1),X'FF'                                                      
         BE    CHKDAYPX                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   STANDDTS(0),0(RF)                                                
         BE    CHKDYP30                                                         
         AHI   R1,L'STANDENT                                                    
         B     CHKDYP20                                                         
CHKDYP30 ST    R1,ADAYPART                                                      
         MVC   TMPSELPG,STANDPRG                                                
         MVI   RDAYPFLAG,DAYPART_STANDARD                                       
****     CLI   STANDESC+17,C'C'                                                 
         CLI   STANDESC+19,C'C'                                                 
         BNE   *+8                                                              
         MVI   RDAYPFLAG,DAYPART_CUME_ONLY                                      
         CLI   STANDFLG,ST_RF_DPT_RETURNZEROS                                   
         BNE   *+16                                                             
         MVC   TMPDYTIM(L'STANDDTS),STANDDTS                                    
         MVC   SPLKDAY(L'SPLKDAY+L'SPLKTIM),TMPDYTIM                            
CHKDAYPX XIT1                                                                   
         DROP  R1                                                               
*--------------------------------------------------------------------           
* ROUTINE GETS THE DAYPART VIA INDEX AND FILLS IN TMPDYTIM                      
* OUTPUT- TMPDYTIM IS FILLED WILL DAYTIMES PERTAINING TO DAYPART                
*--------------------------------------------------------------------           
*                                                                               
GETDAYPT NTR1                                                                   
         XC    TMPSELDP,TMPSELDP                                                
         MVI   TMPSELPG,0                                                       
         ZIC   RE,DPTINDEX                                                      
*                                                                               
         CLI   DBMED,C'R'                                                       
         BNE   GETDAYP7                                                         
         L     R1,=A(STANDPTS)                                                  
         A     R1,SRVRRELO                                                      
         USING STAND,R1                                                         
         MHI   RE,L'STANDENT                                                    
         AR    R1,RE                    R1= A(TABLE ENTRY)                      
         CLI   0(R1),X'FF'                                                      
         BNE   GETDAYP6                                                         
         XC    ADAYPART,ADAYPART                                                
         MVI   DPTINDEX,0                                                       
         B     GETDAYPX                                                         
GETDAYP6 MVC   TMPDYTIM(L'STANDDTS),STANDDTS                                    
         MVC   TMPSELPG,STANDPRG                                                
         MVI   RDAYPFLAG,DAYPART_STANDARD                                       
         CLI   STANDESC+19,C'C'                                                 
         BNE   *+8                                                              
         MVI   RDAYPFLAG,DAYPART_CUME_ONLY                                      
         ST    R1,ADAYPART                                                      
         ZIC   RE,DPTINDEX                                                      
         AHI   RE,1                                                             
         STC   RE,DPTINDEX                                                      
         B     GETDAYPX                                                         
*  TV DAYPART LOOKUP                                                            
GETDAYP7 DS    0C                                                               
         L     R1,=A(TVDAYPT1)                                                  
*                                                                               
GETDAYP8 CLI   SPLKSTA,X'F0'           MARKET LEVEL LOOKUP?                     
         BL    *+8                     MKT LEVEL ONLY LOOKUP                    
         L     R1,=A(TVD1SOFF)         SIGN ON SIGN OFF                         
*                                                                               
         A     R1,SRVRRELO                                                      
         USING TVDPTD,R1                                                        
         MHI   RE,L'TVDPTENT                                                    
         AR    R1,RE                    R1= A(TABLE ENTRY)                      
         CLI   0(R1),X'FF'                                                      
         BNE   GETDAYP9                                                         
         XC    ADAYPART,ADAYPART                                                
         MVI   DPTINDEX,0                                                       
         B     GETDAYPX                                                         
GETDAYP9 MVC   TMPSELDP,TVESTDPT        EASTERN/PACIFIC TIMEZONE                
         CLI   TIMEZONE,C'C'                                                    
         BNE   *+10                                                             
         MVC   TMPSELDP,TVCENDPT                                                
         ST    R1,ADAYPART                                                      
         ZIC   RE,DPTINDEX                                                      
         AHI   RE,1                                                             
         STC   RE,DPTINDEX                                                      
GETDAYPX XIT1                                                                   
         LTORG                                                                  
         DROP  R1                                                               
*                                                                               
* RADIO STANDARD DAYPARTS TABLE                                                 
                                                                                
         SPACE 3                                                                
STANDPTS DS    0C                                                               
*                                                                               
         DC    X'7C',AL2(0600),AL2(1000),X'FF'                                  
         DC    20X'00',AL1(01),C'MON-FRI 6AM-10AM    '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(64)                                                          
*                                                                               
         DC    X'7C',AL2(1000),AL2(1500),X'FF'                                  
         DC    20X'00',AL1(02),C'MON-FRI 10AM-3PM    '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(65)                                                          
         DC    X'7C',AL2(1500),AL2(1900),X'FF'                                  
         DC    20X'00',AL1(03),C'MON-FRI 3PM-7PM     '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(66)                                                          
         DC    X'7C',AL2(1900),AL2(2400),X'FF'                                  
         DC    20X'00',AL1(04),C'MON-FRI 7PM-MID     '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(67)                                                          
*                                                                               
         DC    X'02',AL2(0600),AL2(1000),X'FF'                                  
         DC    20X'00',AL1(01),C'SATURDAY 6AM-10AM   '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(71)                                                          
         DC    X'02',AL2(1000),AL2(1500),X'FF'                                  
         DC    20X'00',AL1(02),C'SATURDAY 10AM-3PM   '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(72)                                                          
         DC    X'02',AL2(1500),AL2(1900),X'FF'                                  
         DC    20X'00',AL1(03),C'SATURDAY 3PM-7PM    '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(73)                                                          
         DC    X'02',AL2(1900),AL2(2400),X'FF'                                  
         DC    20X'00',AL1(04),C'SATURDAY 7PM-MID    '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(74)                                                          
         DC    X'01',AL2(0600),AL2(1000),X'FF'                                  
         DC    20X'00',AL1(01),C'SUNDAY 6AM-10AM     '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(75)                                                          
         DC    X'01',AL2(1000),AL2(1500),X'FF'                                  
         DC    20X'00',AL1(02),C'SUNDAY 10AM-3PM     '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(76)                                                          
         DC    X'01',AL2(1500),AL2(1900),X'FF'                                  
         DC    20X'00',AL1(03),C'SUNDAY 3PM-7PM      '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(77)                                                          
         DC    X'01',AL2(1900),AL2(2400),X'FF'                                  
         DC    20X'00',AL1(04),C'SUNDAY 7PM-MID      '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(78)                                                          
*                                                                               
         DC    X'7C',AL2(0600),AL2(1000)                                        
         DC    X'7C',AL2(1500),AL2(1900),X'FF'                                  
         DC    15X'00',AL1(07),C'MON-FRI DRIVE      C'                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(70)                                                          
*                                                                               
         DC    X'7C',AL2(0600),AL2(1900),X'FF'                                  
         DC    20X'00',AL1(05),C'MON-FRI 6AM-7PM     '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(68)                                                          
         DC    X'7C',AL2(0600),AL2(2400),X'FF'                                  
         DC    20X'00',AL1(06),C'MON-FRI 6AM-MID     '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(69)                                                          
         DC    X'03',AL2(0600),AL2(2400),X'FF'                                  
         DC    20X'00',AL1(06),C'SAT-SUN 6AM-MID     '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(79)                                                          
                                                                                
         DC    X'7F',AL2(0600),AL2(2400),X'FF'                                  
         DC    20X'00',AL1(06),C'MON-SUN 6AM-MID     '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(84)                                                          
                                                                                
         DC    X'7E',AL2(0600),AL2(1000),X'FF'                                  
         DC    20X'00',AL1(01),C'MON-SAT 6AM-10AM    '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(80)                                                          
         DC    X'7E',AL2(1000),AL2(1500),X'FF'                                  
         DC    20X'00',AL1(02),C'MON-SAT 10AM-3PM    '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(81)                                                          
         DC    X'7E',AL2(1500),AL2(1900),X'FF'                                  
         DC    20X'00',AL1(03),C'MON-SAT 3PM-7PM     '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(82)                                                          
         DC    X'7E',AL2(1900),AL2(2400),X'FF'                                  
         DC    20X'00',AL1(04),C'MON-SAT 7PM-MID     '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(83)                                                          
         DC    X'7E',AL2(0600),AL2(1000)                                        
         DC    X'7E',AL2(1500),AL2(1900),X'FF'                                  
         DC    15X'00',AL1(07),C'M-SA 6-10A 3-7P    C'                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(110)                                                         
*                                                                               
         DC    X'7F',AL2(1000),AL2(1500),X'FF'                                  
         DC    20X'00',AL1(02),C'MON-SUN 10AM-3PM   C'                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(114)                                                         
*                                                                               
         DC    X'7F',AL2(1000),AL2(1500)                                        
         DC    X'7F',AL2(1900),AL2(2400),X'FF'                                  
         DC    15X'00',AL1(17),C'MON-SUN 10-3 7-MID C'                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(115)                                                         
         DC    X'7F',AL2(1900),AL2(2400),X'FF'                                  
         DC    20X'00',AL1(04),C'MON-SUN 7PM-MID    C'                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(113)                                                         
         DC    X'7C',AL2(0600),AL2(1500),X'FF'                                  
         DC    20X'00',AL1(18),C'MON-FRI 6AM-3PM    C'                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(96)                                                          
         DC    X'7C',AL2(1000),AL2(1500)                                        
         DC    X'7C',AL2(1900),AL2(2400),X'FF'                                  
         DC    15X'00',AL1(17),C'MON-FRI 10-3 7-MID C'                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(104)                                                         
         DC    X'7C',AL2(1000),AL2(1900),X'FF'                                  
         DC    20X'00',AL1(09),C'MON-FRI 10AM-7PM   C'                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(97)                                                          
         DC    X'7C',AL2(0600),AL2(1000)                                        
         DC    X'7C',AL2(1900),AL2(2400),X'FF'                                  
         DC    15X'00',AL1(14),C'MON-FRI 6-10 7-MID C'                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(103)                                                         
         DC    X'7C',AL2(1500),AL2(2400),X'FF'                                  
         DC    20X'00',AL1(11),C'MON-FRI 3PM-MID    C'                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(99)                                                          
         DC    X'7C',AL2(0600),AL2(1500)                                        
         DC    X'7C',AL2(1900),AL2(2400),X'FF'                                  
         DC    15X'00',AL1(15),C'MON-FRI 6-3P 7-MID C'                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(101)                                                         
         DC    X'7C',AL2(0600),AL2(1000)                                        
         DC    X'7C',AL2(1500),AL2(2400),X'FF'                                  
         DC    15X'00',AL1(13),C'MON-FRI 6-10 3-MID C'                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(102)                                                         
         DC    X'7C',AL2(1000),AL2(2400),X'FF'                                  
         DC    20X'00',AL1(10),C'MON-FRI 10AM-MID   C'                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(98)                                                          
         DC    X'7F',AL2(1500),AL2(2400),X'FF'                                  
         DC    20X'00',AL1(11),C'MON-SUN 3PM-MID    C'                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(112)                                                         
         DC    X'7C',AL2(0600),AL2(1000)                                        
         DC    X'7C',AL2(1500),AL2(1900)                                        
         DC    X'03',AL2(0600),AL2(2400),X'FF'                                  
         DC    10X'00',AL1(16),C'M-F 6-10 3-7P +W/E C'                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(105)                                                         
         DC    X'03',AL2(1000),AL2(1900),X'FF'                                  
         DC    20X'00',AL1(09),C'SAT-SUN 10AM-7PM    '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(85)                                                          
*                                                                               
*                                                                               
* THESE NEXT 4 DAYPARTS ONLY WILL PASS CUMES  AND NO IMPRESSIONS                
* INTO REACH AND FREQUENCY MODEL                                                
* FLAG SHOULD BE SET TO RETURN ZERO VALUES  FOR ALL DEMOS                       
* NOTE THESE DAYTIMES ARE NOT REALLY RELEVANT SINCE DEGETTP WILL READ           
* FOR A DAY/DAYPART NUMBER IN DBMINKEY                                          
* WE DO HOWEVER NEED THE DAYTIMES TO BE SET IN A WAY TO ALLOW THE               
* DBDQTAB LOGIC TO FALL THROUGH IN DEGETTP                                      
         DC    X'7F',AL2(0600),AL2(0600),X'FF'                                  
         DC    20X'00',AL1(20),C'MON-SUN 6AM-6AM     '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(129)                                                         
*                                                                               
         DC    X'7F',AL2(2400),AL2(0600),X'FF'                                  
         DC    20X'00',AL1(19),C'MON-SUN MID-6AM     '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(86)                                                          
*                                                                               
*&&DO                                                                           
         DC    X'7F',AL2(0500),AL2(0900),X'FF'                                  
         DC    20X'00',AL1(FF),C'MON-SUN 6AM-6AM     '                          
         DC    AL1(ST_RF_DPT_RETURNZEROS)                                       
         DC    AL1(129)                                                         
         DC    X'7F',AL2(0500),AL2(0400),X'FF'                                  
         DC    20X'00',AL1(FF),C'MON-SUN MID-6AM     '    INVALID               
         DC    AL1(ST_RF_DPT_RETURNZEROS)                                       
         DC    AL1(86)                                                          
*&&                                                                             
*                                                                               
         DC    X'7F',AL2(0100),AL2(0500),X'FF'                                  
         DC    20X'00',AL1(21),C'MON-SUN 1AM-5AM     '                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(88)                                                          
                                                                                
         DC    X'7C',AL2(0500),AL2(1000),X'FF'                                  
         DC    20X'00',AL1(12),C'MON-FRI 5AM-10AM   C'                          
         DC    AL1(ST_RF_DPT_RETURNDEMOS)                                       
         DC    AL1(111)                                                         
* NEXT ENTRIES NOT FOR R&F MODEL                                                
*                                                                               
         SPACE 1                                                                
         DC    X'7C',AL2(0100),AL2(0500),X'FF'                                  
         DC    20X'00',AL1(21),C'MON-FRI 1AM-5AM     '                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    X'02',AL2(0100),AL2(0500),X'FF'                                  
         DC    20X'00',AL1(21),C'SATURDAY 1A-5A      '                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         SPACE 1                                                                
         DC    X'01',AL2(0100),AL2(0500),X'FF'                                  
         DC    20X'00',AL1(21),C'SUNDAY 1AM-5AM      '                          
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         SPACE 1                                                                
*                                                                               
*                                                                               
*                                                                               
         DC    X'FF'                                                            
                                                                                
*   TV DAYPARTS TABLE                                                           
TVDAYPT1 DS    0C        EASTERN PACIFIC TIMEZONE                               
*                                                                               
         DC    X'15000C',C'M-F 6A-9A        '                                   
         DC    X'15000C',C'M-F 6A-9A        '                                   
*                                                                               
         DC    X'150C18',C'M-F 9A-12N       '                                   
         DC    X'150C18',C'M-F 9A-12N       '                                   
*                                                                               
         DC    X'151828',C'M-F 12N-4P       '                                   
         DC    X'151824',C'M-F 12N-3P       '                                   
*                                                                               
         DC    X'152830',C'M-F 4P-6P        '                                   
         DC    X'15242C',C'M-F 3P-5P        '                                   
*                                                                               
         DC    X'153038',C'M-F 6P-8P        '                                   
         DC    X'152C34',C'M-F 5P-7P        '                                   
*                                                                               
         DC    X'173845',C'MF 8-11P SU/7-11P'                                   
         DC    X'173441',C'MF 7-10P SU/6-10P'                                   
*                                                                               
         DC    X'174446',C'SU-SA 11P-1130P  '                                   
         DC    X'174042',C'SU-SA 10P-1030P  '                                   
*                                                                               
         DC    X'15464C',C'M-F 1130P-1A     '                                   
         DC    X'154248',C'M-F 1030P-12M    '                                   
* DAYPART 9 HARRIS DAYPART M-SU/1-6A SHOULD RETURN ZEROS                        
* FOR NOW                                                                       
*                                                                               
***      DC    X'EEEEEE',C'NOT AVAILABLE    '                                   
***      DC    X'EEEEEE',C'NOT AVAILABLE    '                                   
         DC    X'15505C',C'M-F 2A-5A        '                                   
         DC    X'15505C',C'M-F 2A-5A        '                                   
*                                                                               
         DC    X'600418',C'SAT 7A-12N       '                                   
         DC    X'600418',C'SAT 7A-12N       '                                   
*                                                                               
         DC    X'60182C',C'SAT 12N-5P       '                                   
         DC    X'60182C',C'SAT 12N-5P       '                                   
*                                                                               
         DC    X'600418',C'SAT 7A-12N       '                                   
         DC    X'600418',C'SAT 7A-12N       '                                   
*                                                                               
         DC    X'701C30',C'SUN 1-6P         '                                   
         DC    X'701C30',C'SUN 1-6P         '                                   
*                                                                               
*****    DC    X'17044C',C'S-SA 7A-1A       ' PROPOSER HAS THIS ENTRY           
*****    DC    X'17044C',C'S-SA 7A-1A       ' PROPOSER HAS THIS ENTRY           
                                                                                
*                                                                               
TVD1SOFF DS    0C                                                               
         DC    X'170050',C'S-SA 6A-2A       '                                   
         DC    X'170050',C'S-SA 6A-2A       '                                   
         DC    X'FF'                                                            
         SPACE 3                                                                
SVUNVDEM DS    (MAXDEMS*3)XL4          UNIVS                                    
SVIMPDEM DS    (MAXDEMS*3)XL4          IMPS                                     
SVCUMDEM DS    (MAXDEMS*3)XL4          CUMES                                    
*                                                                               
                                                                                
         DROP  RB,R4,R5                                                         
*----------------------------------------------                                 
* CHECK FOR CONTINOUS AUDIO RESTRICTED BOOKS                                    
* NO BOOKS IN MBKLIST CAN AVERAGE WITH BOOKS                                    
* THAT IS OVERLAPPING DATA 2 MONTHS BEFORE AND 2 MONTHS AFTER                   
* NO PRECDM BOOKS CAN BE AVG WITH CDM BOOKS                                     
*                                                                               
*----------------------------------------------                                 
CONRADBK NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MBKLIST(2),TMPBOOK+1                                             
         MVC   MBKLIST+2(L'SPUPFBKL),TMPMBKS                                    
         L     R5,=A(DBLOCK1-WORKD)                                             
         LA    R5,WORKD(R5)                                                     
         USING SPDEMLKD,R5                                                      
*                                                                               
         OC    TMPMBKS,TMPMBKS                                                  
         JZ    CONRADY                                                          
*                                                                               
         GOTO1 VDEMTABS,DMCB,CDMTAB                                             
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         JZ    *+2                 BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING CDMTABD,RE                                                       
CONRAD02 CLC   =X'FFFF',0(RE)                                                   
         BE    CONRADY                                                          
         OC    SPLKALF,SPLKALF                                                  
         BNZ   CONRAD04                                                         
         CLC   SPLKSPL,CDMNMKT                                                  
         BNE   CONRAD05                                                         
         B     CONRAD06                                                         
CONRAD04 CLC   SPLKALF,CDMAMKT    PPM MARKET IN TABLE?                          
         BE    CONRAD06                                                         
CONRAD05 AR    RE,R0                                                            
         B     CONRAD02                                                         
CONRAD06 DS    0H                                                               
*                                                                               
         ZIC   R1,CDMNBKS          R1= # OF OVERLAPPING SURVEYS                 
         LA    R0,0                COUNT OF BKS MATCHING RESTRICTED BKS         
* MAX NUMBER OF BOOKS IS 3 BY WE ONLY NEED TO ITERATE BY 2                      
* IN THE BCT LOOP BECAUSE WE START COMPARING THE FIRST BOOK TO                  
* THE 2ND BOOK IN LIST SO THERES ONLY TWO LOOP ITERATIONS                       
*                                                                               
         MVI   CDMFLAG,0                                                        
         LA    R2,MBKLIST                                                       
* FIGURE START BOOK WHICH CURRENT BOOK CAN STARTING AVERAGING WITH              
CONRAD07 DS    0H                                                               
         CLC   0(2,R2),=X'FFFF'    END OF MBKLIST ?                             
         BE    CONRAD12            FINIHED COMPARING ALL BOOKS                  
         OC    0(2,R2),0(R2)       END OF MBKLIST ?                             
         BZ    CONRAD12            FINIHED COMPARING ALL BOOKS                  
         CLC   0(2,R2),CDMLIVBK                                                 
         BNL   CONRAD7A                                                         
         OI    CDMFLAG,CDMPRE      SET WE HAVE BOOKS BEFORE EFFECTIVE           
***      B     CONRAD7B            START BOOK FOR CDM MKT                       
         B     CONRAD10            DONT NEED TO CHECK OVERLAPS FOR PRE          
*                                  CDM BOOK.                                    
CONRAD7A DS    0H                                                               
         OI    CDMFLAG,CDMPOST     WE HAVE BOOK AFTER EFFECTIVE BOOK            
CONRAD7B DS    0H                                                               
*                                                                               
         MVC   STARTOVB,0(R2)                                                   
         ZIC   RF,STARTOVB+1       MONTH NUMBER START                           
         SR    RF,R1               SUBTRACT BY NUMBER OF BOOKS OVERLAP          
         CHI   RF,1                                                             
         BL    CONRAD7C                                                         
         STC   RF,STARTOVB+1       SET START MONTH NUMBER                       
         B     CONRAD7H                                                         
* CROSSING BACK PASS BEGINNING OF YEAR MONTH NUMBER - ADJUST NEGATIVE           
* BY ADDING 12 TO GET THE CORRECT MONTH NUMBER                                  
* AND SUBTRACT YEAR BY ONE                                                      
CONRAD7C AHI   RF,12                                                            
         STC   RF,STARTOVB+1       ADJUST MONTH NUMBER                          
         ZIC   RF,STARTOVB                                                      
         SHI   RF,1                                                             
         STC   RF,STARTOVB         ADJUST YEAR NUMBER BACK ONE YEAR             
*                                                                               
* FIGURE END BOOK WHICH CURRENT BOOK CAN START AVERAGING WITH                   
CONRAD7H DS    0H                                                               
         MVC   ENDOVBK,0(R2)                                                    
         ZIC   RF,ENDOVBK+1        MONTH NUMBER                                 
         AR    RF,R1               ADD BY NUMBER OF BOOKS DATA OVERLAP          
         CHI   RF,12                                                            
         BH    CONRAD7I                                                         
         STC   RF,ENDOVBK+1        SET END MONTH NUMBER                         
         B    CONRAD7L                                                          
* CROSSING OVER PASS BEGINNING OF YEAR MONTH NUMBER - ADJUST                    
* BY SUBTRACTING 12 TO GET THE CORRECT MONTH NUMBER                             
* AND SUBTRACT YEAR BY ONE                                                      
CONRAD7I SHI   RF,12                                                            
         STC   RF,ENDOVBK+1                                                     
         ZIC   RF,ENDOVBK                                                       
         AHI   RF,1                                                             
         STC   RF,ENDOVBK          ADJUST YEAR NUMBER FORWARD ONE YEAR          
CONRAD7L DS    0H                                                               
* R6 IS THE ADDRESS OF THE BOOKS AFTER CURRENT BOOK POINTED TO BY R2            
* FOR EACH BOOK AT 0(R2), CHECK ALL BOOKS AFTER STARTING AT 0(R6)               
* TO MAKE SURE IT DOES NOT OVERLAP WITH THE BOOK AT 0(R2)                       
         LA    R6,2(R2)            R6 IS CURRENT BOOK IN LIST POINTER           
*                                                                               
CONRAD08 CLC   0(2,R6),=X'FFFF'    END OF MBKLIST ?                             
         BE    CONRAD10                                                         
         OC    0(2,R6),0(R6)  '    END OF MBKLIST ?                             
         BZ    CONRAD10                                                         
         CLC   0(2,R6),STARTOVB    IS BOOK BEFORE THE ALLOWED START             
         BNH   CONRAD09            BOOK WHICH DOESNT OVERLAP                    
* BOOK IS AFTER ALLOWED START BOOK - HAVE TO SEE IF ITS AFTER                   
* ALLOWED END BOOK                                                              
         CLC   0(2,R6),ENDOVBK                                                  
         BNL   CONRAD09                                                         
* FOUND BOOK INSIDE THE START END END BOOK OF OVERLLAPING SURVEYS               
         AHI   R0,1                UPDATE COUNT OF BOOK WITHIN RANGE            
CONRAD09 AHI   R6,2                NEXT BOOK IN MBKLIST                         
         B     CONRAD08                                                         
*                                                                               
CONRAD10 AHI   R2,2                                                             
         B     CONRAD07                                                         
CONRAD12 DS    0H                                                               
* IF ALL WE HAVE ARE BOOKS BEFORE CDM - SET CC TO YES                           
         CLI   CDMFLAG,CDMPRE                                                   
         BE    CONRADY                                                          
* IF WE HAVE MIX OF PRECDM AND POST CDM BOOKS SET CC TO N                       
* WE DO NOT WANT THESE TYPES OF AVERAGES TOO                                    
         TM    CDMFLAG,CDMPRE+CDMPOST                                           
         BO    CONRADN                                                          
* IF WE ALL POST CDM BOOKS THEN CHECK R0 TO SEE IF WE                           
* HAVE ANY BOOKS THAT IS AVERAGE OVERALAPPING DATA                              
         CLI   CDMFLAG,CDMPOST                                                  
         BE    *+6                                                              
         DC    H'0'        DIE- SOMETHING IS WRONG                              
*                                                                               
*  FOR MBKLIST WITH ALL POSTCDM AND MIX OF CDM                                  
         CHI   R0,0        ANY BOOKS AVERAGED RESTRICTED RANGE                  
         BH    CONRADN     OF BOOKS WHICH CAN NOT BE AVERAGED                   
         B     CONRADY                                                          
                                                                                
CONRADY  J     EXITY                                                            
CONRADN  J     EXITN                                                            
         LTORG                                                                  
MBKLIST  DS    4XL2                                                             
         DC    X'FFFF'                                                          
STARTOVB DS    XL2                                                              
ENDOVBK  DS    XL2                                                              
CDMFLAG  DS    X                                                                
CDMPRE   EQU   X'01'    MKT LIST HAS BOOKS BEFORE CDM                           
CDMPOST  EQU   X'02'    MKT LIST HAS BOOKS AFTER CDM         K                  
         DROP  R5                                                               
*                                                                               
*&&DO                                                                           
*----------------------------------------------                                 
* FILL IN CMKDEMOFS FOR RADIO CONDENSE MARKETS                                  
*----------------------------------------------                                 
RADCMKT  NTR1  BASE=*,LABEL=*                                                   
         CLI   CONDMKTF,C'C'                                                    
         JNE   RADCMKTX                                                         
         LARL  RE,ACRDEMS3         TABLE OF NON CONDENSE DEMOS                  
*                                                                               
         L     R2,SPLKALST                                                      
         LA    R3,CMKDEMOFS                                                     
         LA    RF,0                INDEX INTO SPLKALST DEMO LIST                
SPCLR16  CLI   0(RE),0             INVALID DEMO                                 
         JE    SPCLR18                                                          
         CLC   2(1,R2),0(RE)                                                    
         JE    SPCLR17             VALID DEMO SO EXIT                           
*                                                                               
         LA    RE,1(RE)            ??? SO GET THE NEXT ONE                      
         J     SPCLR16                                                          
* VALID DEMO IN THE TABLE SO SET FLAG FOR THIS DEMO TO C'N'                     
* TO INDICATE NOT A CONDENSE MARKET DEMO                                        
SPCLR17  LA    R2,0(RF,R2)                                                      
         LA    R3,0(RF,R3)                                                      
         MVI   0(R3),C'N'                                                       
         B     SPCLR20                                                          
*                                                                               
*  NOT IN TABLE SO MUST BE A CONDENSE MARKET - SET FLAG TO C'Y'                 
* TO INDICATE DEMO IS CONDENSE MARKET DEMO                                      
SPCLR18  LA    R2,0(RF,R2)                                                      
         LA    R3,0(RF,R3)                                                      
         MVI   0(R3),C'Y'                                                       
         B     SPCLR20                                                          
                                                                                
SPCLR20  LARL  RE,ACRDEMS3         TABLE OF NON CONDENSE DEMOS                  
                                                                                
                                                                                
                                                                                
         DROP  X                                                                
*                                                                               
SPCLR20  L     RE,4(R1)            INVALID DEMO                                 
RADCMKTX XIT1                                                                   
***    ++INCLUDE REARBCMKTS        ** COMMENTED OUT BY DEIS JAN/2014 **         
         LTORG                                                                  
         DROP  RB                                                               
*&&                                                                             
***********************************************************************         
* TRANSLATE MARKET CODE                                                         
* INPUT- ALFMKTS HAS TO BE SET TO ALPHA MARKET                                  
*        DBMED,DBSRC,TMPBKTYP,ACOMFACS                                          
*                                                                               
* OUTPUT- SPILLMKT WILL BE SET TO BINARY MKT NUMBER                             
***********************************************************************         
TRAMKT   NTR1  BASE=*,LABEL=*                                                   
         L     R5,=A(DBLOCK1-WORKD)                GET STATION                  
         LA    R5,WORKD(R5)                                                     
         USING DBLOCK,R5                                                        
         OC    ALFMKTS,ALFMKTS                                                  
         BZ    TRAMK50                                                          
         MVI   DBFUNCT,DBCNVA2N                  TRANSLATE ALPHA TO             
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         CLI   DBSELSRC,C'F'                     FUSION USE NSI TABLE           
         BNE   *+8                                                              
         MVI   DBSELSRC,C'N'                                                    
         CLI   DBSELMED,C'D'                     DAYPART FILE ALSO USE          
         BE    *+8                               NSI MONTHLY                    
         CLI   DBSELMED,C'O'                     OVERNIGHT USE NSI              
         BNE   *+8                               MONTHLY TABLE                  
         MVI   DBSELMED,C'T'                                                    
         MVC   DBBTYPE,TMPBKTYP                                                 
         OC    ALFMKTS,=X'404040'                NUMERIC MARKET                 
         MVC   DBSELALF,ALFMKTS                                                 
         MVC   DBCOMFCS,ACOMFACS                 A(COMFACS)                     
         GOTOR (#SETUSID,ASETUSID)                                              
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
         CLI   DBERROR,0                         ANY ERRORS?                    
         BNE   *+10                              NO, SET NUMERIC MKT            
         MVC   SPILLMKT,DBSELRMK                 AS SPILL                       
TRAMK50  XC    DBSELRMK,DBSELRMK                 CLEAR FIELD IN CASE            
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB,R5                                                            
*                                                                               
***********************************************************************         
*   FORMAT BOOK STRING ROUTINE                                        *         
*   ENTRY PARAMATER LIST AS DEFINED BY INPARAM                        *         
*   EXIT  - DMCB(4)= A(END OF FORMATTED BOOK STRING)                            
***********************************************************************         
FORMATBK NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R6,DMCB                                                          
         MVC   SAVEDMCB(24),DMCB                                                
         USING IN_PARAM,R6                                                      
*                                                                               
FORMAT10 CLI   INBKTYPE,0            ANY INTERNAL BOOKTYPE  ?                   
         BE    FORMAT20                                                         
         LA    R4,TWOCHRBT                                                      
         LA    R2,INBKTYPE                                                      
         GOTOR (#TRNSBKT,ATRNSBKT),MYDMCB,(R2),1,(R4),12                        
         MVC   DMCB(24),SAVEDMCB                                                
         CLI   0(R4),X'FF'           IF BOOKTYPE IS NOT IN TABLE                
         BNE   *+10                  THEN SOMETHING IS REALLY AMISS             
         MVC   0(2,R4),=C'??'        JUST PASS ??                               
*                                                                               
FORMAT20 XC    MYDMCB,MYDMCB                                                    
         MVI   BYTE1,1           BYTE1 = MONDAY FOR OVERNIGHTS                  
         CLI   INFORMAT,BK_OVERN                                                
         BE    FORMAT25                                                         
         CLI   INFORMAT,BK_WKLY                                                 
         BNE   FORMAT38                                                         
                                                                                
FORMAT22 MVI   BYTE1,0           BYTE1 = DEFAULT = SAT                          
                                                                                
FORMAT25 OC    INBOOK+1(2),INBOOK+1  EXIT IF NO BOOK PASSED IN                  
         BNZ   FORMAT27              FOR LATEST BOOK - NONE FOUND               
         L     R2,INOUTPUT           R2 POINTS TO OUTPUT AREA                   
         MVC   0(6,R2),=C'LATEST'                                               
         AHI   R2,6                                                             
         B     FORMATX                                                          
FORMAT27 GOTO1 =V(NSIWEEK),MYDMCB,(C'D',INBOOK+1),(BYTE1,VGETDAY),     +        
               VADDAY,VDATCON,RR=SRVRRELO                                       
         ZICM  R1,MYDMCB+1,(7)                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(6),0(R1)                                                    
                                                                                
         L     R2,INOUTPUT           R2 POINTS TO OUTPUT AREA                   
                                                                                
         GOTO1 VDATCON,MYDMCB,(X'80',WORK),(5,0(R2))                            
         CLI   INBKTYPE,0            ANY BOOKTYPE?                              
         BE    FORMATX                                                          
         LA    R2,8(R2)                                                         
         MVI   0(R2),C'('                                                       
         CLI   TWOCHRBT+1,X'40'                                                 
         BH    FORMAT30                                                         
         MVC   1(1,R2),TWOCHRBT                                                 
         MVI   2(R2),C')'            1 CHARACTER BOOKTYPE DISPLAY               
         AHI   R2,3                                                             
         B     FORMATX                                                          
FORMAT30 MVC   1(2,R2),TWOCHRBT                                                 
         MVI   3(R2),C')'            2 CHARACTER BOOKTYPE DISPLAY               
         AHI   R2,4                                                             
         B     FORMATX                                                          
*=====================================================================          
FORMAT38 MVI   MYBKH,18              CREATE FAKE FIELD                          
         GOTOR =V(UNBOOK),MYDMCB,(1,INBOOK),MYBKH,0,(C'+',=CL6' '),    +        
               RR=SRVRRELO                                                      
         L     R2,INOUTPUT           R2 POINTS TO OUTPUT AREA                   
         CLI   LATFLAG,C'Y'                                                     
         BNE   *+18                                                             
         MVC   0(6,R2),=C'LATEST'                                               
         AHI   R2,6                                                             
         B     FORMATX                                                          
         CLI   DMCB+2,X'FF'                                                     
         BNE   FORMAT42                                                         
         MVC   0(3,R2),=C'LAT'                                                  
         MVC   3(1,R2),INBOOK+2                                                 
         CLI   INBOOK+2,X'F0'       BACKWARD COMPATIBILTY TO OLD FORMAT         
         BH    *+12                                                             
         CLI   INBOOK+2,X'09'                                                   
         BH    FORMAT39                                                         
         OI    3(R2),X'F0'                                                      
         AHI   R2,4                                                             
         B     FORMAT40                                                         
FORMAT39 EDIT  (1,INBOOK+2),(2,3(R2))                                           
         AHI   R2,5                                                             
FORMAT40 B     FORMAT48                                                         
FORMAT42 MVC   0(L'MYBK,R2),MYBK                                                
         CLI   0(R2),C'P'                                                       
         BE    *+8                                                              
         CLI   0(R2),C'E'                                                       
         BNE   FORMAT46                                                         
         MVI   ANYTPFLG,C'N'                                                    
         B     FORMATX                                                          
FORMAT46 L     R2,INOUTPUT                                                      
         AHI   R2,5                                                             
FORMAT48 CLI   INBKTYPE,0                                                       
         BE    FORMAT60                                                         
         MVI   0(R2),C'('                                                       
         CLI   TWOCHRBT+1,X'40'                                                 
         BH    FORMAT52                                                         
         MVC   1(1,R2),TWOCHRBT                                                 
         MVI   2(R2),C')'            1 CHARACTER BOOKTYPE DISPLAY               
         AHI   R2,3                                                             
         B     FORMAT60                                                         
FORMAT52 MVC   1(2,R2),TWOCHRBT                                                 
         MVI   3(R2),C')'            2 CHARACTER BOOKTYPE DISPLAY               
         AHI   R2,4                                                             
FORMAT60 CLI   INWKNUM,0                                                        
         BE    FORMATX                                                          
         MVI   0(R2),C'-'                                                       
         MVC   1(1,R2),INWKNUM                                                  
FORMATX  ST    R2,DMCB                                                          
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
TWOCHRBT DS    CL2                                                              
*                                                                               
IN_PARAM DSECT                                                                  
INFORMAT DS    XL1                                                              
BK_OVERN EQU   C'O'                                                             
BK_WKLY  EQU   C'W'                                                             
BK_MONTH EQU   C'M'                                                             
INBOOK   DS    XL3                                                              
INOUTPUT DS    A                                                                
INBKTYPE DS    X                                                                
INWKNUM  DS    C                                                                
         EJECT                                                                  
SVRDEF   CSECT                                                                  
*--------------------------------------------------------------------           
* BUILD DEMO LIST -                                                             
* ENTRY- DEMOPTR    - ADDRESS OF DEMO LIST (NULL= GRAB FROM ADEMO)              
*        NUMDEMO2   - REMAINING NUMBER OF DEMOS TO PROCESS                      
*--------------------------------------------------------------------           
BLDDEMOL NTR1  BASE=*,LABEL=*                                                   
         CLI   DEMOHDRF,DMHDR_FROMSTART                                         
         BE    BLDDML03                                                         
         CLI   DEMOHDRF,DMHDR_CONTINUE                                          
         BE    BLDDML02                                                         
* BUILD NEXT SET OF DEMOS INTO DEMO HEADER RECORD                               
BLDDML02 L     R4,DEMOPTR                                                       
         ZICM  R2,NUMDEMO2,(3)               NUM OF DEMOS REMAINING             
         B     BLDDML05                                                         
*                                                                               
* BUILD DEMO HEADER RECORD FROM BEGINNING OF DEMO LIST                          
BLDDML03 SR    R4,R4                             GET DEMO                       
         ICM   R4,7,ADEMO                                                       
         LA    R2,1                                                             
         TM    DEMOIND,LQ_TSINQ                                                 
         BNO   BLDDML04                                                         
         LA    R4,LW_DATA1-LW_D(R4)                                             
         J     BLDDML05                                                         
BLDDML04 TM    DEMOIND,LQ_TLSTQ                                                 
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R2,3,LW_NUMN-LW_D(R4)                                            
         LA    R4,LW_DATA2-LW_D(R4)                                             
BLDDML05 LA    R6,DEMODEMS                                                      
         STCM  R2,3,NUMDEMO                                                     
         XC    NUMDEMO2,NUMDEMO2                                                
* TV -SPGETDEMF CAN SUPPORT 42 DEMOS VALUES                                     
* 14 RATINGS, 14 14 SHARES AND 14 PUTS                                          
* RADIO -SPGETDEMF CAN SUPPORT 42 DEMO VALUES                                   
* 8 RATINGS, 8 SHARES, 8 PUTS, 8 INTERNAL UNIV, 8 INTERNAL IMPRESSION           
*                                                                               
         LA    R0,14                                                            
         CLI   DBMED,C'R'                                                       
         BNE   *+8                                                              
         LA    R0,8                                                             
*                                                                               
****     CHI   R2,14                                                            
         CR    R2,R0                                                            
         BNH   BLDDML20                                                         
         CLI   OVSYS,8                         REP COMPARGRAPGH ONLY            
         BE    *+14                            SUPPORTS MAX 14 DEMOS            
         CLC   VERSNUM,=AL4(SPVER26)           IF SPOT DESKTOP VERSION          
         BNL   BLDDML12                        < 2.6 THEN ONLY PROCESS          
**       LA    R2,14                           UP TO 14 DEMOS                   
         LR    R2,R0                           up to 8 demos                    
                                                                                
BLDDML12 LR    RE,R2                                                            
***      SHI   RE,14                            REMAINING # OF DEMOS            
         SR    RE,R0                            REMAINING # OF DEMOS            
         STCM  RE,3,NUMDEMO2                                                    
***      LA    R2,14                                                            
         LR    R2,R0                                                            
         STCM  R2,3,NUMDEMO                                                     
BLDDML20 DS    0H                                                               
                                                                                
                                                                                
                                                                                
         LA    R5,DMHDVALS                                                      
         USING DMHDVALS,R5                                                      
BLDDML30 MVC   0(3,R6),0(R4)                     3 BYTE DEMO CODE               
         MVC   3(3,R6),0(R4)                                                    
         MVI   4(R6),C'S'                        TSA IMPRESSION                 
         CLI   DBMED,C'U'                                                       
         BE    BLDDML31                                                         
         CLI   DBMED,C'R'                                                       
         BE    BLDDML31                                                         
* WHEN PROGRAM SHARES FLAG PASSED, DONT USE Q,X                                 
         CLI   PROGSHRF,C'Y'                                                    
         BE    BLDDML31                                                         
         CLI   1(R4),C'I'                        IF REQUEST IMPRESSION          
         BNE   *+8                                                              
         MVI   4(R6),C'X'                                                       
BLDDML31 MVC   6(3,R6),0(R4)                                                    
         MVI   7(R6),C'P'                                                       
         CLI   PROGSHRF,C'Y'                                                    
         BE    *+8                                                              
         CLI   DBMED,C'U'                                                       
         BE    *+8                                                              
         CLI   DBMED,C'R'                                                       
         BE    *+16                                                             
         CLI   1(R4),C'I'                        TSA IMPRESSION                 
         BNE   *+8                                                              
         MVI   7(R6),C'Q'                                                       
*                                                                               
         CLI   DBMED,C'U'                                                       
         BE    *+8                                                              
         CLI   DBMED,C'R'                                                       
         BNE   BLDDML36                                                         
         CLI   1(R4),C'X'                        TIME SPEND LISTENING           
         BNE   *+8                               DISPLAY AS X                   
         MVI   4(R4),C'X'                                                       
*                                                                               
         CLI   1(R4),C'W'                        RADIO CUMES,IMP                
         BNE   *+8                               IN FULL PRECISION              
         MVI   4(R4),C'C'                        DISPLAY ORIGINAL C,I           
         CLI   1(R4),C'B'                        MODIFIER                       
         BNE   *+8                                                              
         MVI   4(R4),C'I'                                                       
*                                                                               
*                                                                               
BLDDML36 DS    0C                                                               
         MVC   DMHDDEM,4(R4)                                                    
         CLC   =C'HOMES',DMHDDEM+1                                              
         BNE   *+10                                                             
         MVC   DMHDDEM+1(5),=C'HH   '                                           
         CLC   =C'HOMES',DMHDDEM                                                
         BNE   *+10                                                             
         MVC   DMHDDEM(5),=C'HH   '                                             
                                                                                
         MVC   DMCB+0(1),1(R4)                   MOD FOR REQUESTED DEMO         
         GOTOR GETPREC,DMCB                                                     
         MVC   DMHDPRE(1),DMCB+4                                                
         AHI   R5,DMHDVALL                                                      
         XC    0(DMHDVALL,R5),0(R5)                                             
*                                                                               
* SPOT SYSTEM - IF VERSION 2.6 OR HIGHER THEN PASS BACK SHARE AND PUTS          
* BACK AUTOMATICALLY                                                            
         ST    R4,DEMOPTR                                                       
         CLI   OVSYS,8                                                          
         BE    BLDDML40                                                         
         CLC   VERSNUM,=AL4(SPVER26)                                            
         BNL   BLDDML40                                                         
         AHI   R6,3                                                             
         AHI   R4,24                                                            
         B     BLDDML50                                                         
                                                                                
*                                                                               
BLDDML40 MVC   DMHDDEM(5),=C'Share'                                             
         MVC   DMCB+0(1),4(R6)                   MOD FOR SHARE                  
         GOTOR GETPREC,DMCB                                                     
         MVC   DMHDPRE(1),DMCB+4                                                
         AHI   R5,DMHDVALL                                                      
         XC    0(DMHDVALL,R5),0(R5)                                             
         MVC   DMHDDEM(7),=C'HUT/PUT'                                           
         MVC   DMCB+0(1),7(R6)                   MOD FOR HPT                    
         GOTOR GETPREC,DMCB                                                     
         MVC   DMHDPRE(1),DMCB+4                                                
         AHI   R4,24                                                            
         ST    R4,DEMOPTR                                                       
         AHI   R6,9                                                             
         AHI   R5,DMHDVALL                                                      
         XC    0(DMHDVALL,R5),0(R5)                                             
         ZICM  R0,NUMDEMO,(3)                                                   
         AHI   R0,2                                                             
         STCM  R0,3,NUMDEMO                                                     
BLDDML50 BCT   R2,BLDDML30                                                      
         MVI   0(R6),X'FF'                                                      
         MVI   DMHDSEND,C'Y'                                                    
         MVC   DMHDNUM,NUMDEMO                                                  
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB,R5                                                            
         EJECT                                                                  
***********************************************************************         
*   -------------- CLEAR OUTPUT AREAS ------------------------------- *         
***********************************************************************         
CLRAREA  NTR1  BASE=*,LABEL=*                                                   
         LA    R0,OUTVALS                        CLEAR DOWN SAVE AREA           
         LHI   R1,OUTVALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*------------------------- SET DBEXTEND AREA -------------------------*         
                                                                                
* Sets up a link in the DBEXTEND area.                                          
* At entry,                                                                     
*   DUB(4)   = link identification if want to match to an existing link         
*            = x'00000000' if link must be added,                               
*   DUB+4(4) = A(link),                                                         
*   DUB1     = A(PARAMETER BLOCK) ACCORDING TO SDBXF:                           
*            = A(SPDEMLK block) when SDBXF=C'S'                                 
*            = A(DBLOCK)        when SDBXF=C'D'                                 
*            = A(SPDEMUP block) when SDBXF=C'U'                                 
* At exit,                                                                      
*   FULL1    = ADDRESS OF LINK, ZEROES IF NO LINK SET UP.                       
                                                                                
STDBXLNK NTR1  BASE=*,LABEL=*                                                   
         L     R6,DUB1                                                          
         XC    FULL1,FULL1                                                      
                                                                                
         CLI   SDBXF,C'S'                                                       
         BE    SDBX010                                                          
         CLI   SDBXF,C'D'                                                       
         BE    SDBX015                                                          
         CLI   SDBXF,C'U'                                                       
         BE    SDBX080                                                          
         DC    H'0'                                                             
                                                                                
                                                                                
SDBX010  DS    0H                  GET R2 TO START OF EXTENSION AREA            
         DS    0H                  GET R2 TO START OF EXTENSION AREA            
         USING SPDEMLKD,R6                                                      
         L     RF,SPLKAREC                                                      
         ICM   R0,15,8(RF)                                                      
         CLC   0(8,RF),=C'DBEXTEND'                                             
         BE    SDBX022                                                          
                                                                                
         MVC   0(8,RF),=C'DBEXTEND'                                             
         L     R2,DUB+4            USE A(CALLER'S LINK) IF DBEXTEND=0           
         STCM  R2,15,8(RF)                                                      
         MVC   0(4,R2),DUB+0        PUT IN USER'S LINK IDENTIFICATION           
         B     SDBX040              AND MAKE IT THE LAST LINK                   
         DROP  R6                                                               
                                                                                
SDBX015  DS    0H                  R6-->DBLOCK                                  
         USING DBLOCKD,R6                                                       
         ICM   R0,15,DBEXTEND                                                   
         BNZ   SDBX022                                                          
                                                                                
         L     R2,DUB+4            USE A(CALLER'S LINK) IF DBEXTEND=0           
         STCM  R2,15,DBEXTEND                                                   
         MVC   0(4,R2),DUB          PUT IN USER'S LINK IDENTIFICATION           
         B     SDBX040              AND MAKE IT THE LAST LINK                   
         DROP  R6                                                               
                                                                                
SDBX020  DS    0H                  BUMP TO APPROPRIATE LINK                     
         ICM   R0,15,4(R2)          GET ADDRESS OF NEXT LINK                    
         BZ    SDBX030               IF ZERO, ADD CALLER'S LINK                 
                                                                                
SDBX022  DS    0H                                                               
         LR    R2,R0                "BUMPED" TO NEXT LINK                       
         OC    DUB(4),DUB           IF LOOKING FOR MATCH,                       
         BZ    SDBX020                                                          
         CLC   DUB(4),0(R2)          AND LINKS' IDS MATCH,                      
         BNE   SDBX020                                                          
         B     SDBX050               PASS BACK ADDR OF CURRENT LINK             
                                                                                
SDBX030  DS    0H                  ADD CALLER'S LINK TO END                     
         MVC   4(4,R2),DUB+4        SET THE NEXT ADDR INTO LAST LINK            
         ICM   R2,15,4(R2)          BUMP TO THE NEW LAST LINK                   
         MVC   0(4,R2),DUB          PUT IN USER'S LINK IDENTIFICATION           
         B     SDBX040                                                          
                                                                                
SDBX040  DS    0H                  R2-->LAST LINK                               
         XC    4(4,R2),4(R2)        ZERO OUT THE NEXT ADDRESS                   
         B     SDBX050               NOPE                                       
                                                                                
SDBX050  DS    0H                                                               
         ST    R2,FULL1            RETURN ADDRESS OF MATCHED/ADDED LINK         
         B     SDBXX                                                            
                                                                                
                                                                                
SDBX080  DS    0H                  R6-->SPDEMUPD                                
         USING SPDEMUPD,R6                                                      
         ICM   R0,15,SPUPEXTN                                                   
         BNZ   SDBX022                                                          
                                                                                
         L     R2,DUB+4            USE A(CALLER'S LINK) IF SPUPEXTN=0           
         STCM  R2,15,SPUPEXTN                                                   
         MVC   0(4,R2),DUB          PUT IN USER'S LINK IDENTIFICATION           
         B     SDBX040              AND MAKE IT THE LAST LINK                   
         DROP  R6                                                               
                                                                                
                                                                                
SDBXX    DS    0H                                                               
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
                                                                                
                                                                                
**********************************************************************          
DEMUPGD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   ALLTPFLG,C'Y'                  IF WE ARE DOING SECOND            
         JE    DEMUP10                        TP READ, T4-TP, TP-T4             
         MVC   OUTPCID,TMPPCID                DONT PASS PCID                    
         L     R1,ALP                         ELSE PCID IS ALWAYS               
         L     RF,LP_APUTO-LP_D(R1)           SEND FOR UPGRADES                 
         GOTOR (RF)                           BECAUSE SUMMARY RECORD            
         GOTOR =A(CLRAREA),RR=SRVRRELO        IS ALWAYS SEND                    
                                                                                
*                                                                               
DEMUP10  XC    TMPUPNAM,TMPUPNAM                                                
         OC    TMPMBKS,TMPMBKS              MULTIBOOK AVERAGE?                  
         JNZ   DEMUP95                                                          
*  LOOK FOR CORRECT UPGRADE FORMULA                                             
         SR    RE,RE                                                            
         ICM   RE,7,AUPGRD                                                      
         LA    R1,1                                                             
         TM    UPGRIND,LQ_TSINQ                                                 
         BNO   DEMUP90                                                          
         LA    RE,LW_DATA1-LW_D(RE)                                             
         J     DEMUP92                                                          
DEMUP90  TM    UPGRIND,LQ_TLSTQ                                                 
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R1,3,LW_NUMN-LW_D(RE)                                            
         LA    RE,LW_DATA2-LW_D(RE)                                             
*  NOW FIND THE CORRECT PROJECTION FORMULA BASE ON THE PROJECTION INDEX         
*  CURRENTLY SEEKING  INDEX START AT 1                                          
         USING UPGRADD,RE                                                       
DEMUP92  CLC   UPINDEX,TMPUINDX                                                 
         BE    DEMUP94                                                          
         AHI   RE,UPGRADX                                                       
         BCT   R1,DEMUP92                                                       
         DC    H'0'                         MUST FIND UPGRADE INDEX             
*  FOUND THE CORRECT PROJECTION                                                 
DEMUP94  MVC   TMPUPNAM,UPNAME                                                  
                                                                                
                                                                                
DEMUP95  L     R6,=A(SPDEMUP1-WORKD)                                            
         LA    R6,WORKD(R6)                                                     
         USING SPDEMUPD,R6                                                      
         XC    0(SPDEMUP2,R6),0(R6)                                             
* MULTIBOOK AVERAGE                                                             
         XC    SPDEMUPD,SPDEMUPD                                                
         MVC   SPUPAREC,AIO1                                                    
         MVC   SPUPAFAC,ACOMFACS                                                
         MVC   SPUPAGY,AGYALPH                                                  
         MVC   SPUPMED,DBMED                                                    
         MVC   SPUPSTA,TMPSTA                                                   
****** SINCE COMPARAGRAPH IS ONLY USED BY THE REP GROUP ALWAYS SET              
******  SPUPSYS TO "R"                                                          
DEMUP97  MVI   SPUPSYS,C'R'                                                     
                                                                                
         OC    TMPMBKS,TMPMBKS              MULTIBOOK AVERAGE?                  
         JZ    DEMUP98                                                          
         MVC   SPUPFBK,TMPBOOK+1                                                
         MVC   SPUPBTYP,TMPBKTYP                                                
         MVC   SPUPFBKL,TMPMBKS                                                 
         MVI   SPUPFIL,C'T'                                                     
         MVC   SPUPTYPE(8),=XL8'0400006400000000'                               
         J     DEMUP99                                                          
                                                                                
DEMUP98  MVC   SPUPFBK,UPSHRBK                                                  
                                                                                
         MVC   SPUP2YRP,UP2YRP                                                  
         MVC   SPUP2YRR,UP2YRR                                                  
         MVC   SPUPUDAY,UPDYTIM                                                 
         MVC   SPUPUTIM,UPDYTIM+1                                               
         MVC   SPUPTYPE(L'UPBKVAL),UPBKVAL                                      
         MVC   SPUPBTYP,UPBKTYP                                                 
*                                                                               
         CLI   SPUPBTYP,X'40'                  ANY BOOKTYPE SPECIFIED?          
         BH    *+18                                                             
         CLI   FORCEBT,X'40'                   FORCE BOOKTYPE                   
         BNH   *+10                                                             
         MVC   SPUPBTYP,FORCEBT                                                 
*                                                                               
         MVI   SPUPFIL,C'T'                                                     
         MVC   TMPBKWK,UPWEEK                                                   
         XC    TMPBOOK,TMPBOOK                                                  
         MVC   TMPBOOK+1(2),SPUPFBK                                             
         MVC   TMPBKTYP,UPBKTYP                                                 
                                                                                
         CLI   TMPBKTYP,X'40'                   ANY BOOKTYPE SPECIFIED?         
         BH    *+18                                                             
         CLI   FORCEBT,X'40'                   FORCE BOOKTYPE                   
         BNH   *+10                                                             
         MVC   TMPBKTYP,FORCEBT                                                 
                                                                                
         MVC   BYTE2,UPWEEK                                                     
         NI    BYTE2,X'0F'                                                      
         PACK  BYTE1,BYTE2                                                      
         OC    SPUPFBK+1(1),BYTE1                                               
         DROP  RE                                                               
                                                                                
                                                                                
*  IF ROTATION THEN PROCESS ROTATION                                            
*  ROTATION HAS MORE THAN ONE DAYTIME (5 BYTES) IN TMPDYTIM                     
DEMUP99  LA    R3,TMPDYTIM                                                      
         MVI   ROTFLAG,C'N'                                                     
         CLI   TMPDYTIM+5,X'FF'                                                 
         BNE   DEMUP100                                                         
         MVC   SPUPDAY,TMPDYTIM                                                 
         MVC   SPUPTIM,TMPDYTIM+1                                               
         MVC   TMPDAY,SPUPDAY                                                   
         MVC   TEMPSTIM,SPUPTIM                                                 
         MVC   TEMPETIM,SPUPTIM+2                                               
         B     DEMUP124                                                         
* PROCESS ROTATION COMPONENTS FIRST THEN DO ENTIRE ROTATION                     
* UNLESS RECALC JUST DO ENTIRE ROTATION AND GET SUMMARY                         
                                                                                
DEMUP100 MVI   ROTFLAG,C'Y'                                                     
*                                                                               
DEMUP108 CLI   0(R3),X'FF'         IF DONE COMPONENTS THEN                      
         BE    DEMUP110            SET UP ENTIRE ROTATION                       
         MVC   SPUPDAY,0(R3)                                                    
         MVC   SPUPTIM,1(R3)                                                    
         MVC   TMPDAY,SPUPDAY                                                   
         MVC   TEMPSTIM,SPUPTIM                                                 
         MVC   TEMPETIM,SPUPTIM+2                                               
         AHI   R3,5                                                             
         ST    R3,DYTMPTR                                                       
         B     DEMUP124                                                         
                                                                                
DEMUP110 LA    R3,TMPDYTIM                                                      
                                                                                
DEMUP112 MVI   ROTFLAG,C'D'         SET TO DONE                                 
         XC    SPUPEXTN,SPUPEXTN                                                
         XC    DUB,DUB                                                          
         LA    R0,DBXTROTN                                                      
         ST    R0,DUB+4                                                         
         MVI   SDBXF,C'U'                                                       
         ST    R6,DUB1                                                          
         GOTOR STDBXLNK                                                         
         ICM   RF,15,FULL1                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DBXTLD,RF                                                        
         LA    RE,DBXTLIST                                                      
         MVC   DBXTLID,=C'DYTM'                                                 
         MVI   DBXTLIDX,0                                                       
                                                                                
DEMUP123 MVC   0(5,RE),0(R3)                                                    
         AHI   RE,5                                                             
         AHI   R3,5                                                             
         CLI   0(R3),X'FF'                                                      
         BNE   DEMUP123                                                         
         MVI   0(RE),0                                                          
DEMUP124 OI    SPUPOPTS,SPOPEXT                   EXTENDED BLK PRESENT          
         MVC   SPUPSPL,SPILLMKT                                                 
         MVI   SPUPFIL,C'T'                                                     
         MVC   SPUPSRC,DBSRC                                                    
         CLC   =C'T4',INPFIL                                                    
         BNE   *+8                                                              
         MVI   SPUPTPTT,C'P'                                                    
         CLC   =C'TF',INPFIL                     FUSION                         
         BNE   *+8                                                              
         MVI   SPUPSRC,C'F'                                                     
         MVC   SPUPSYSC,TMPSYSC                  SYSCODE                        
                                                                                
         OI    SPUPOPTS,SPOPDMAI                 IMPRESSIONS                    
         CLI   PROF1W+7,C'Y'                                                    
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM                 IF REP SYSTEM                  
                                                                                
         CLI   OVSYS,8                           ALWAYS NORMALIZE               
         JNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM                                                
*                                                                               
         CLI   OPTDEC,C'2'                                                      
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOP2DEC   2 DEC RATING PRECISION                       
         CLI   OPTIPRE,C'2'                                                     
         BNE   *+8                                                              
         OI    SPUPOPT2,SPOP2IPR   2 DEC IMPRESSION PRECISION                   
                                                                                
*   CALL STAPACK FOR CABLE LOOKUPS                                              
                                                                                
         OC    SPUPSYSC,SPUPSYSC   SYSCODE                                      
         BZ    DEMUP127                                                         
         XC    WORK(40),WORK                                                    
         LA    R1,WORK                                                          
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'X'        TRANSLATE 3CHAR NET TO 4CHAR                 
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQNET,SPUPSTA                                                 
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0           IF CAN'T TRANSLATE JUST KEEP IT              
         BNE   DEMUP127                                                         
                                                                                
         CLI   STAPQSTA,C' '       FOUND 4 CHAR NET?                            
         BE    DEMUP127            NO                                           
         MVC   SPUPSTA(4),STAPQSTA MOVE 4 CHAR NETWORK CODE                     
         MVI   SPUPSTA+4,C'T'                                                   
         DROP  R1                                                               
                                                                                
*                                                                               
DEMUP127 MVC   SPUPUID,USERID                                                   
* FOR NCM REDIRECT PROJECTIONS TO SPGETDEMF TO PERFORM STRAIGHT BOOK            
* LOOKUP BASED ON THE SHARE BOOK IN THE PROJECTION FORMULA                      
         CLI   DBSRC,C'C'                                                       
         BNE   DEMU127P                                                         
* SAVE OFF DYTMPR AND ROTFLAG                                                   
* DEMLK ALSO SETS DYTMPTR AND THIS WILL MESS UP THE DOWNSTREAM                  
* LOOP AFTER THE SPDEMUP CALL                                                   
         MVC   SVDYTPTR,DYTMPTR                                                 
         MVC   SVROTFLG,ROTFLAG                                                 
         MVI   OUTPUTF,OUTPUTOF                                                 
         GOTOR DEMLK,DMCB,(RC),RR=SRVRRELO                                      
         MVC   ROTFLAG,SVROTFLG                                                 
         MVC   DYTMPTR,SVDYTPTR                                                 
         MVC   SPUPPRG,THISPROG                                                 
         B     DEMU127X                                                         
*                                                                               
DEMU127P GOTO1 VSPDEMUP,DMCB,SPDEMUPD,DEMODEMS,THISDEMS                         
DEMU127X MVI   ANYTPFLG,C'Y'                                                    
         CLI   0(R1),0                                                          
         JE    DEMUP128                                                         
         MVI   ANYTPFLG,C'N'                                                    
         J     DEMUP165                                                         
* NOW SEND RECORDS TO PC                                                        
* PROCESS THE DETAIL                                                            
DEMUP128 CLI   ROTFLAG,C'D'                       NO DETAILS                    
         BE    DEMUP165                                                         
                                                                                
         LA    R3,PGDTVALS                                                      
         USING PGDTVALS,R3                                                      
         MVI   PGDTSEND,C'Y'                                                    
         MVC   PFILE,INPFIL                                                     
         MVI   PACTF,C'N'                                                       
         CLI   ALLTPFLG,C'Y'                                                    
         BE    *+8                                                              
         MVI   PACTF,C'Y'                                                       
         MVC   PSTAT(L'DUMSTAT),DUMSTAT                                         
                                                                                
         CLI   PSTAT+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   PSTAT+4,0                                                        
                                                                                
         MVC   PPROG(L'SPUPPRG),SPUPPRG                                         
         MVC   PWEEKS(1),TMPBKWK                                                
         OI    PWEEKS,X'F0'                                                     
         MVC   PDAYS(1),TMPDAY                                                  
         MVC   PSTIME,TEMPSTIM                                                  
         MVC   PETIME,TEMPETIM                                                  
         OC    PSTIME,PSTIME                                                    
         BNZ   *+10                                                             
         MVC   PSTIME,=H'2400'                                                  
         OC    PETIME,PETIME                                                    
         BNZ   *+10                                                             
         MVC   PETIME,=H'2400'                                                  
                                                                                
         MVI   DMCB,BK_MONTH                                                    
         CLC   =C'OTP',SFILE                                                    
         BE    *+14                                                             
         CLC   =C'OPA',SFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_OVERN                                                    
         MVC   DMCB+1(3),TMPBOOK                                                
         LA    RE,PBOOK                                                         
         ST    RE,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         MVC   DMCB+9(1),TMPBKWK                                                
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
         L     R2,DMCB         ADDRESS OF END OF BOOK STRING                    
                                                                                
* multibook code                                                                
DEMUP157 OC    TMPMBKS,TMPMBKS                                                  
         BZ    DEMUP162                                                         
         LA    R4,TMPMBKS                                                       
         LA    R0,3                                                             
                                                                                
DEMUP158 MVI   0(R2),C'+'                                                       
         AHI   R2,1                                                             
         XC    WORK,WORK                                                        
         MVC   WORK+1(5),0(R4)                                                  
         MVI   DMCB,BK_MONTH                                                    
         CLC   =C'OTP',SFILE                                                    
         BE    *+14                                                             
         CLC   =C'OPA',SFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_OVERN                                                    
         MVC   DMCB+1(3),WORK                                                   
         ST    R2,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
         L     R2,DMCB         ADDRESS OF END OF BOOK STRING                    
*                                                                               
DEMUP159 AHI   R4,2                                                             
         OC    0(2,R4),0(R4)                                                    
         BZ    DEMUP162                                                         
         BCT   R0,DEMUP158                                                      
                                                                                
DEMUP162 MVC   PGDTNUM,=H'1'                                                    
         XC    WORK2,WORK2                                                      
         MVC   WORK2(L'PBOOK),PBOOK                                             
         LA    R4,PMFID                                                         
         USING MFIDD,R4                                                         
         EDIT  (B1,PDAYS),(3,MFDAY),ALIGN=RIGHT,ZERO=NOBLANK,FILL=0             
         EDIT  (B2,PSTIME),(4,MFSTIME),ALIGN=RIGHT,ZERO=NOBLANK,FILL=0          
         EDIT  (B2,PETIME),(4,MFETIME),ALIGN=RIGHT,ZERO=NOBLANK,FILL=0          
         DROP  R3                                                               
         DROP  R4                                                               
         LA    R3,DDEMVALS                                                      
         USING DDEMVALS,R3                                                      
         LA    R2,THISDEMS                                                      
         ZICM  R0,NUMDEMO,(3)                                                   
DEMUP164 MVI   DDEMSEND,C'Y'                                                    
         CLI   OPTDEC,C'2'              TURN OF THE X'40' INDICATOR             
         BNE   *+8                      FOR EXTENDED PRECISION SET              
         NI    0(R2),X'FF'-X'40'        BY SPDEMUP                              
         MVC   DDEMOS,0(R2)                                                     
         AHI   R3,DDEMVALL                                                      
         AHI   R2,4                                                             
         BCT   R0,DEMUP164                                                      
         MVC   DDEMNUM,NUMDEMO                                                  
         L     R1,AO#DDEMO                                                      
         MVI   1(R1),O#DDEMO                                                    
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         MVC   DMCB(24),SAVEDMCB                                                
         CLI   ROTFLAG,C'Y'                                                     
         JNE   DEMUP165                                                         
         L     R3,DYTMPTR                                                       
         J     DEMUP100                                                         
         DROP  R3                                                               
                                                                                
* PROCESS SUMMARY RECORD                                                        
                                                                                
DEMUP165 CLI   ALLTPFLG,C'Y'                     IF WE ARE DOING SECOND         
         JE    DEMUPX                            TP READ, T4-TP, TP-T4          
DEMUP170 LA    R3,SUMMVALS                       WE DONT NEED 2ND SUMM          
         USING SUMMVALS,R3                                                      
         MVI   SUMMSEND,C'Y'                                                    
         MVC   SFILE,INPFIL                                                     
         MVC   SSTAT(L'DUMSTAT),DUMSTAT                                         
         CLI   SSTAT+4,C'T'                                                     
         BNE   *+8                                                              
         MVI   SSTAT+4,0                                                        
         MVC   SPROG(L'SPUPPRG),SPUPPRG                                         
                                                                                
         CLI   ANYTPFLG,C'N'                                                    
         BNE   *+16                                                             
         XC    SPROG,SPROG                                                      
         MVC   SPROG(17),=C'NO DATA AVAILABLE'                                  
         XC    SBOOK,SBOOK                                                      
         MVC   SBOOK(L'TMPUPNAM),TMPUPNAM                                       
*  ONLY MULTIBOOK AVERAGE GETS REAL BOOKS                                       
         OC    TMPMBKS,TMPMBKS                                                  
         BZ    DEMUP260                                                         
         MVI   DMCB,BK_MONTH                                                    
         CLC   =C'OTP',SFILE                                                    
         BE    *+14                                                             
         CLC   =C'OPA',SFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_OVERN                                                    
         MVC   DMCB+1(3),TMPBOOK                                                
         LA    RE,SBOOK                                                         
         ST    RE,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         MVC   DMCB+9(1),TMPBKWK                                                
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
         L     R2,DMCB         ADDRESS OF END OF BOOK STRING                    
                                                                                
                                                                                
DEMUP190 LA    R4,TMPMBKS                                                       
         LA    R0,3                                                             
* REPLACE ABOVE CODE WITH FORMATBK ROUTINE CALL                                 
DEMUP198 MVI   0(R2),C'+'                                                       
         AHI   R2,1                                                             
         XC    WORK,WORK                                                        
         MVC   WORK+1(5),0(R4)                                                  
         MVI   DMCB,BK_MONTH                                                    
         CLC   =C'OTP',SFILE                                                    
         BE    *+14                                                             
         CLC   =C'OPA',SFILE                                                    
         BNE   *+8                                                              
         MVI   DMCB,BK_OVERN                                                    
         MVC   DMCB+1(3),WORK                                                   
         ST    R2,DMCB+4                                                        
         MVC   DMCB+8(1),TMPBKTYP                                               
         MVC   DMCB+9(1),TMPBKWK                                                
         GOTOR =A(FORMATBK),DMCB,RR=SRVRRELO                                    
         L     R2,DMCB         ADDRESS OF END OF BOOK STRING                    
DEMUP199 AHI   R4,2                                                             
         OC    0(2,R4),0(R4)                                                    
         BZ    DEMUP260                                                         
         BCT   R0,DEMUP198                                                      
                                                                                
                                                                                
DEMUP260 MVC   SUMMNUM,=H'1'                                                    
         DROP  R3                                                               
         LA    R3,SDEMVALS                                                      
         USING SDEMVALS,R3                                                      
         LA    R2,THISDEMS                                                      
         ZICM  R0,NUMDEMO,(3)                                                   
DEMUP280 MVI   SDEMSEND,C'Y'                                                    
         MVC   SDEMOS,0(R2)                                                     
         CLI   ANYTPFLG,C'N'                                                    
         BNE   *+10                                                             
         XC    SDEMOS,SDEMOS                                                    
         AHI   R3,SDEMVALL                                                      
         AHI   R2,4                                                             
         BCT   R0,DEMUP280                                                      
         MVC   SDEMNUM,NUMDEMO                                                  
         L     R1,AO#DDEMO                                                      
         MVI   1(R1),O#SDEMO                                                    
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         J     EXITY                                                            
                                                                                
DEMUPX   J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
***********************************************************************         
*-------------------------- TRANSLATE WEEKS --------------------------*         
                                                                                
* Translates active week bits into printable format                             
* At entry,                                                                     
*   TMPWKS   = active week bits                                                 
* At exit,                                                                      
*   WORK     = formatted active weeks                                           
*   HALF1    = length of formatted active weeks                                 
                                                                                
TRSLTWKS NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK,SPACES                                                      
         LA    R4,WORK                                                          
                                                                                
         DS    0H                  FORMAT BOOK                                  
         LA    RE,B'00001000'      TRANSLATE BITS (STARTING W/ X'08')           
         LA    RF,C'1'              INTO EBCDIC DIGITS                          
TWK012   EX    RE,*+8              JUST PASS THOSE WEEK NOS. OF                 
         B     *+8                  THOSE WEEKS THAT WERE ACTIVE                
         TM    TMPWKS,0             (eg. X'0B' ==> C'134' )                     
         BNZ   TWK014                                                           
         MVI   0(R4),C'.'           MOVE IN A "." FOR NON ACTIVE WK             
         B     TWK016                                                           
TWK014   STC   RF,0(R4)                                                         
TWK016   LA    R4,1(R4)                                                         
         LA    RF,1(RF)                                                         
         SRL   RE,1                                                             
         OR    RE,RE                                                            
         BNZ   TWK012                                                           
                                                                                
         TM    TMPWKS,X'80'         IF DIFFERING ACTIVE WEEKS,                  
         BZ    *+12                                                             
         MVI   0(R4),C'*'           INDICATE IT SO WITH AN "*"                  
         LA    R4,1(R4)                                                         
                                                                                
TWKX     DS    0H                                                               
         LA    R0,WORK                                                          
         SR    R4,R0                                                            
         STH   R4,HALF1            SET LENGTH INTO HALF                         
                                                                                
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
***********************************************************************         
* ROUTINE TO DETERMINE THE NEXT LASTEST BOOK                                    
* ENTRY DMCB=TMPBOOK+1 THE BOOK TO ADJUST                                       
* EXIT  DMCB+4(2) - NEXT BOOK                                                   
***********************************************************************         
GETNLATB NTR1  BASE=*,LABEL=*                                                   
         MVC   DMCB+4(2),DMCB                                                   
         ZIC   RE,DMCB+1                                                        
         SHI   RE,1                                                             
         STC   RE,DMCB+5                                                        
                                                                                
         CLI   DMCB+5,1                                                         
         BNL   GETNLATX                                                         
                                                                                
         MVI   DMCB+5,12             RESET FOR PREVIOUS YEAR                    
         ZIC   RE,DMCB                                                          
         SHI   RE,1                                                             
         STC   RE,DMCB+4                                                        
                                                                                
                                                                                
GETNLATX J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
*                                                                               
***********************************************************************         
* GET A TRITON STATION FOR THE CURRENT MARKET.                                  
* RETURN STATION IN TMPSTA.                                                     
*                                                                               
* THIS IS NEEDED IN ORDER TO GET TO UNIVERSES. A REQUEST FOR UNIVERSES,         
* IDENTIFIED BY STATION 'AAAAA' WOULD READ THE MARKET TOTALS FOR                
* ARBITRON. HOWEVER, TRITON DOESN'T HAVE MARKET TOTALS. FOR THAT REASON         
* WE WILL SIMPLY READ A STATION (ANY STATION) TO GET THE UNIVERSES.             
* THIS ROUTINE WILL GIVE US A STATION IN THE MAKRKET/BOOK.                      
***********************************************************************         
TRITONST NTR1  BASE=*,LABEL=*                                                   
         CLC   TMPSTA,=C'AAAAA'                                                 
         BNE   TRITSN                                                           
         CLC   INPFIL,=C'TRT'         NO MARKET TOTALS FOR TRITON               
         BE    TRITS10                                                          
         CLC   INPFIL,=C'TRD'                                                   
         BNE   TRITSN                                                           
*                                                                               
TRITS10  L     R5,=A(DBLOCK1-WORKD)                                             
         LA    R5,WORKD(R5)                                                     
         USING DBLOCKD,R5                                                       
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,AIO1                                                      
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
         MVI   DBSELSRC,C'T'                                                    
         MVC   DBSELAGY,AGYALPH                                                 
         MVC   DBSELRMK,SPILLMKT                                                
         MVC   DBBTYPE,TMPBKTYP                                                 
         MVC   DBSELBK,TMPBOOK+1                                                
         MVI   DBFUNCT,DBGETMS                                                  
         MVC   DBCOMFCS,ACOMFACS                 A(COMFACS)                     
*                                                                               
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
         CLI   DBERROR,0                                                        
         BNE   TRITSY                        NO STATIONS FOUND                  
         ICM   RE,15,DBAREC                                                     
         USING MLKEY,RE                                                         
         MVC   TMPSTA,MLSTAT         RETURN A STATION IN THE MARKET/BK          
         DROP  RE                                                               
*                                                                               
         DROP  R5                                                               
TRITSY   XC    0(SPDEMLKL,R5),0(R5)       CLEAR BLOCK                           
         J     EXITY                                                            
TRITSN   J     EXITN                                                            
         LTORG                                                                  
         DROP  RB                                                               
***********************************************************************         
* READ MARKET RECORD                                                  *         
***********************************************************************         
                                                                                
GETMKT   NTR1  LABEL=*,BASE=*                                                   
                                                                                
         LA    RF,KEY                                                           
         USING ANMRECD,RF                    READ A-N MARKET PASSIVE            
         XC    ANMKEYD,ANMKEYD                                                  
         MVI   ANMKTYPE,ANMKTYPQ             C'L'-PASSIVE                       
         MVC   ANMKAGCY,AGYALPH                                                 
         MVC   ANMKMED,DBMED                                                    
         MVC   ANMKAMRK,ALFMKTS                                                 
         GOTO1 VDATAMGR,DMCB,(0,=CL8'DMRDHI'),=C'STATION',KEY,AIO2              
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         L     RF,AIO2                                                          
         CLC   ANMKNMRK,=CL7'0000000'        HAVE MARKET?                       
         JNH   *+2                                                              
                                                                                
         LA    R1,KEY                                                           
         USING MKTREC,R1           READ MARKET RECORD                           
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,DBMED                                                    
         MVC   MKTKMKT,ANMKNMRK                                                 
         MVC   MKTKAGY,AGYALPH                                                  
         MVC   MKTKFILL,=CL7'0000000'                                           
         GOTO1 VDATAMGR,DMCB,(0,=CL8'DMREAD'),=C'STATION',KEY,AIO2              
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         L     R1,AIO2                                                          
         MVC   FORCEFT,MKTCDEM                                                  
         J     EXITY                                                            
         DROP  R1,RF                                                            
***********************************************************************         
* REQUEST DEFINITIONS                                                 *         
***********************************************************************         
LVALUES  DS    0D                                                               
         DC    A(DEMBUFF)                                                       
         DC    A(O#DDEMON)                                                      
*                                                                               
         DC    10X'FF'                                                          
LVALUESX DS    0X                                                               
DEMBUFF  BUFFD TYPE=D,KEYLEN=5,COMLEN=2,BUFFERS=6                               
                                                                                
**REQUEST  DS    0X                                                             
***********************************************************************         
* REQUEST FOR CANADIAN SPOT DESKTOP LOOKUP                            *         
* THE SPEC WAS FOR MULTIPLE STATION EACH WITH MULTIPLE SPILL          *         
* MARKETS FOR ONE DAYTIME, ONE BOOK                                   *         
***********************************************************************         
M#CANLK  EQU   24                                                               
*                                                                               
                                                                                
REQCANL  LKREQ H,I#DECAND,OUTCANLK                                              
                                                                                
*                                                                               
RMED     LKREQ F,1,(D,B#SAVED,CANMED),CHAR,OLEN=1,TEXT=SP#MED,COL=*             
                                                                                
RRSMKT   LKREQ F,2,(I,B#SAVED,RRSMIND),CHAR,OLEN=CANRSRVL,SORT=N,      *        
               TEXT=(*,RSATEXT),LIST=F,COL=*                                    
                                                                                
*&&DO                                                                           
DAY      LKREQ F,5,(D,B#SAVED,CANDAY),LBIN,OLEN=1,TEXT=SP#DAY,COL=*             
                                                                                
                                                                                
STIME    LKREQ F,6,(D,B#SAVED,CANSTIM),LBIN,                           *        
               OLEN=L'CANSTIM,TEXT=(*,STIMTXT),COL=*                            
ETIME    LKREQ F,7,(D,B#SAVED,CANETIM),LBIN,                           *        
               OLEN=L'CANETIM,TEXT=(*,ETIMTXT),COL=*                            
                                                                                
BOOKS    LKREQ F,8,(D,B#SAVED,CANBOOK),BMON,                           *        
               OLEN=L'CANBOOK,TEXT=SP#1BBOK,COL=*                               
*&&                                                                             
                                                                                
DPREC    LKREQ F,9,(D,B#SAVED,CAN2DEC),CHAR,OLEN=1,TEXT=SP#DPREC,COL=*          
                                                                                
PROF1W   LKREQ F,10,(D,B#SAVED,CANPROF),CHAR,OLEN=L'CANPROF,           *        
               TEXT=(*,PROFLIT),COL=* ,                                         
DEMOS    LKREQ F,11,(I,B#SAVED,DEMOIND),CHAR,LIST=F,                   *        
               OLEN=3,SORT=N,TEXT=SP#DEMO,COL=*                                 
*                                                                               
         LKREQ E                                                                
***********************************************************************         
*  REQUEST FOR PROGRAM DETAILS                                        *         
***********************************************************************         
M#PGDET  EQU   25                                                               
REQPDET  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQPDETX+1-*)                                                
         DC    AL2(M#PGDET)                                                     
         DC    AL1(0)                                                           
         DC    AL2(OUTPGDET-SVRDEF)                                             
         DC    XL4'00'                                                          
                                                                                
CC#DEMO  EQU   1                                                                
CQ#DEMO  EQU   1                                                                
         DC    AL2(CQ#DEMO)                                                     
         DC    CL5'DEMO '                                                       
         DC    AL1(CC#DEMO)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALDEMO,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(DEMOIND-SAVED)                                               
         DC    AL1(24)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(60,20)                                                       
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
         DC    AL1(RESYSQ),AL2(RE#DEMOS)                                        
         DC    XL4'00'                                                          
CC#DEM2  EQU   19                                                               
CQ#DEM2  EQU   19                                                               
         DC    AL2(CQ#DEM2)                                                     
         DC    CL5'DEMO '                                                       
         DC    AL1(CC#DEM2)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALRCAT,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(DCATIND-SAVED)                                               
         DC    AL1(1)                                                           
         DC    AL1(LD_USERQ+LD_SCOLQ)                                           
         DC    AL1(60,20)                                                       
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
         DC    AL1(RESYSQ),AL2(RE#DEMOS)                                        
         DC    XL4'00'                                                          
CC#DEM3  EQU   20                                                               
CQ#DEM3  EQU   20                                                               
         DC    AL2(CQ#DEM3)                                                     
         DC    CL5'DEMO '                                                       
         DC    AL1(CC#DEM3)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALRDEM,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(DEMOIND-SAVED)                                               
         DC    AL1(23)                                                          
         DC    AL1(LD_USERQ+LD_ECOLQ)                                           
         DC    AL1(60,20)                                                       
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
         DC    AL1(RESYSQ),AL2(RE#DEMOS)                                        
         DC    XL4'00'                                                          
                                                                                
CC#STAT  EQU   2                                                                
CQ#STAT  EQU   2                                                                
         DC    AL2(CQ#STAT)                                                     
         DC    CL5'STA  '                                                       
         DC    AL1(CC#STAT)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALSTA,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(STAIND-SAVED)                                                
         DC    AL1(14)                                                          
         DC    AL1(LD_USERQ+LD_SCOLQ)                                           
         DC    XL2'00'                                                          
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
         DC    AL1(RESYSQ),AL2(RE#STA)                                          
         DC    XL4'00'                                                          
* STATE *                                                                       
CC#STATE EQU   21                                                               
CQ#STATE EQU   21                                                               
         DC    AL2(CQ#STATE)                                                    
         DC    CL5'STATE'                                                       
         DC    AL1(CC#STATE)                                                    
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#MULSTAT,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(STCDIND-SAVED)                                               
****     DC    AL1(11)                                                          
         DC    AL1(L'STASTATE)                                                  
         DC    AL1(LD_USERQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(RESYSQ),AL2(274)                                             
         DC    XL4'00'                                                          
* COUNTIES *                                                                    
CC#CNTY  EQU   22                                                               
CQ#CNTY  EQU   22                                                               
         DC    AL2(CQ#CNTY)                                                     
         DC    CL5'CNTY '                                                       
         DC    AL1(CC#CNTY)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#MULCNTY,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(CNTYIND-SAVED)                                               
******   DC    AL1(21)        10 TWO BYTE COUNTY CODES                          
         DC    AL1(L'STACNTY)                                                   
         DC    AL1(LD_USERQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(RESYSQ),AL2(273)                                             
         DC    XL4'00'                                                          
                                                                                
* DAYTIME *                                                                     
                                                                                
CC#DYTM  EQU   3                                                                
CQ#DYTM  EQU   3                                                                
         DC    AL2(CQ#DYTM)                                                     
         DC    CL5'DYTM '                                                       
         DC    AL1(CC#DYTM)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#MVTBUFF,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(DYTMIND-SAVED)                                               
         DC    AL1(61)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(RESYSQ),AL2(RE#DAYTM)                                        
         DC    XL4'00'                                                          
                                                                                
* BUY START DATE                                                                
                                                                                
CC#BSTRT EQU   8                                                                
CQ#BSTRT EQU   8                                                                
         DC    AL2(CQ#BSTRT)                                                    
         DC    CL5'BSTRT'                                                       
         DC    AL1(CC#BSTRT)                                                    
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VAL1WK,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(BUYDTIND-SAVED)                                              
         DC    AL1(4)                                                           
         DC    AL1(LD_USERQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#STDT)                                         
         DC    XL4'00'                                                          
* LPM START DATE                                                                
*                                                                               
                                                                                
IC#LPMSD EQU   9                                                                
IQ#LPMSD EQU   9                                                                
         DC    AL2(IQ#LPMSD)                                                    
         DC    CL5'LPMSD'                                                       
         DC    AL1(IC#LPMSD)                                                    
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(LPMSTIND-SAVED)                                              
         DC    AL1(4)                                                           
         DC    AL1(LD_CHARQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(RESYSQ),AL2(RE#INVNM)                                        
         DC    XL4'00'                                                          
*  SYSCODE                                                                      
CC#SYSC  EQU   15                                                               
CQ#SYSC  EQU   15                                                               
         DC    AL2(CQ#SYSC)                                                     
         DC    CL5'SYSC '                                                       
         DC    AL1(CC#SYSC)                                                     
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(SYSCIND-SAVED)                                               
         DC    AL1(2)        MAKE IT 4 JUST TO AGREE WITH SPLKN17               
         DC    AL1(LD_UBINQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
***      DC    AL1(RESYSQ),AL2(RE#SYSC)                                         
         DC    AL1(RESYSQ),AL2(272)                                             
         DC    XL4'00'                                                          
*                                                                               
* NUM OF ROWS FROM PC COMPARAGRAH REPORT                                        
                                                                                
CC#NROW  EQU   4                                                                
CQ#NROW  EQU   4                                                                
         DC    AL2(CQ#NROW)                                                     
         DC    CL5'NROW '                                                       
         DC    AL1(CC#NROW)                                                     
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(NROWIND-SAVED)                                               
         DC    AL1(4)                                                           
         DC    AL1(LD_UBINQ+LD_ECOLQ)                                           
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#NUM)                                          
         DC    AL1(RESYSQ),AL2(RE#ROWS)                                         
         DC    XL4'00'                                                          
                                                                                
* FILE *                                                                        
                                                                                
CC#FILE  EQU   5                                                                
CQ#FILE  EQU   5                                                                
         DC    AL2(CQ#FILE)                                                     
         DC    CL5'FILE '                                                       
         DC    AL1(CC#FILE)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALFILE,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(FILEIND-SAVED)                                               
         DC    AL1(3)                                                           
         DC    AL1(LD_USERQ+LD_SCOLQ)                                           
         DC    XL2'00'                                                          
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
****     DC    AL1(SPSYSQ),AL2(SP#1BFIL)                                        
         DC    AL1(RESYSQ),AL2(RE#FILE)                                         
         DC    XL4'00'                                                          
                                                                                
* BOOK *                                                                        
                                                                                
CC#BOOK  EQU   6                                                                
CQ#BOOK  EQU   6                                                                
         DC    AL2(CQ#BOOK)                                                     
         DC    CL5'BOOK '                                                       
         DC    AL1(CC#BOOK)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
*******  DC    AL1(#VALBOOK,0)                                                  
         DC    AL1(#MULTBKS,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(BOOKIND-SAVED)                                               
******   DC    AL1(6)                                                           
         DC    AL1(12)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
*****    DC    AL1(SPSYSQ),AL2(SP#1BBOK)                                        
         DC    AL1(RESYSQ),AL2(RE#BOOKS)                                        
         DC    XL4'00'                                                          
                                                                                
* PC ID ASSOCIATED WITH EACH FILE BOOK                                          
                                                                                
CC#PCID  EQU   7                                                                
CQ#PCID  EQU   7                                                                
         DC    AL2(CQ#PCID)                                                     
         DC    CL5'PCID '                                                       
         DC    AL1(CC#PCID)                                                     
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(PCIDIND-SAVED)                                               
         DC    AL1(5)                                                           
**       DC    AL1(LD_CHARQ+LD_ECOLQ)                                           
         DC    AL1(LD_CHARQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
*****    DC    AL1(SPSYSQ),AL2(SP#NUM)                                          
         DC    AL1(RESYSQ),AL2(RE#PCID)                                         
         DC    XL4'00'                                                          
                                                                                
* UPGRADE INDEX NUMBER                                                          
                                                                                
CC#UINDX EQU   10                                                               
CQ#UINDX EQU   10                                                               
         DC    AL2(CQ#UINDX)                                                    
         DC    CL5'UINDX'                                                       
         DC    AL1(CC#UINDX)                                                    
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(UINDXIND-SAVED)                                              
         DC    AL1(1)                                                           
         DC    AL1(LD_UBINQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(RESYSQ),AL2(RE#UPIDX)                                        
         DC    XL4'00'                                                          
*                                                                               
* LATEST BOOK NUMBER- NUMBER OF LATEST BOOKS TO PROCESS                         
* SPOT DESKTOP FEATURE MAPCODE                                                  
CC#LATBN EQU   16                                                               
CQ#LATBN EQU   16                                                               
         DC    AL2(CQ#LATBN)                                                    
         DC    CL5'LATBN'                                                       
         DC    AL1(CC#LATBN)                                                    
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(LATBNIND-SAVED)                                              
         DC    AL1(1)                                                           
         DC    AL1(LD_UBINQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#LATES)                                        
         DC    XL4'00'                                                          
*                                                                               
* LATEST BOOK TYPE                                                              
*                                                                               
CC#LATBT EQU   17                                                               
CQ#LATBT EQU   17                                                               
         DC    AL2(CQ#LATBT)                                                    
         DC    CL5'LATBT'                                                       
         DC    AL1(CC#LATBT)                                                    
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#TRNSBKT,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(LATBTIND-SAVED)                                              
         DC    AL1(1)                                                           
         DC    AL1(LD_USERQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#BKTY)                                         
         DC    XL4'00'                                                          
                                                                                
CC#LATWN EQU   26                                                               
CQ#LATWN EQU   26                                                               
         DC    AL2(CQ#LATWN)                                                    
         DC    CL5'LATWN'                                                       
         DC    AL1(CC#LATWN)                                                    
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(LATWKIND-SAVED)                                              
         DC    AL1(1)                                                           
         DC    AL1(LD_CHARQ+LD_ECOLQ)                                           
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#WEEK)                                         
         DC    XL4'00'                                                          
                                                                                
* UPGRADE                                                                       
                                                                                
CC#UPGR  EQU   11                                                               
CQ#UPGR  EQU   11                                                               
         DC    AL2(CQ#UPGR)                                                     
         DC    CL5'UPGR '                                                       
         DC    AL1(CC#UPGR)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#PARSEUP,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(UPGRIND-SAVED)                                               
         DC    AL1(19)                                                          
         DC    AL1(LD_USERQ+LD_SCOLQ)                                           
         DC    XL2'00'                                                          
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
         DC    AL1(RESYSQ),AL2(RE#UPGDE)                                        
         DC    XL4'00'                                                          
                                                                                
CC#UPNAM EQU   12                              UPGRADE NAME                     
CQ#UPNAM EQU   12                              CREATED BY PC                    
         DC    AL2(CQ#UPNAM)                                                    
         DC    CL5'UPNAM'                                                       
         DC    AL1(CC#UPNAM)                                                    
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(UPNAMIND-SAVED)                                              
         DC    AL1(15)                                                          
         DC    AL1(LD_CHARQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(RESYSQ),AL2(RE#UPNAM)                                        
         DC    XL4'00'                                                          
                                                                                
CC#UPIND EQU   13                              UPGRADE KEY NUMBER               
CQ#UPIND EQU   13                                                               
         DC    AL2(CQ#UPIND)                                                    
         DC    CL5'UPIND'                                                       
         DC    AL1(CC#UPIND)                                                    
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(UPININD-SAVED)                                               
         DC    AL1(1)                                                           
         DC    AL1(LD_UBINQ+LD_ECOLQ)                                           
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(RESYSQ),AL2(RE#UPNUM)                                        
         DC    XL4'00'                                                          
CC#DEC   EQU   14                                                               
CQ#DEC   EQU   14                                                               
         DC    AL2(CQ#DEC)                                                      
         DC    CL5'DEC  '                                                       
         DC    AL1(CC#DEC)                                                      
         DC    AL1(LD_IAWPQ)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(DECIND-SAVED)                                                
         DC    AL1(1)                                                           
         DC    AL1(LD_CHARQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#DPREC)                                        
         DC    XL4'00'                                                          
* DEFAULT BOOKTYPE TO USE UNIVERSALLY                                           
CC#DEFBT EQU   18                                                               
CQ#DEFBT EQU   18                                                               
         DC    AL2(CQ#DEFBT)                                                    
         DC    CL5'DEFBT'                                                       
         DC    AL1(CC#DEFBT)                                                    
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(DEFBTIND-SAVED)                                              
         DC    AL1(1)                                                           
         DC    AL1(LD_UBINQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#BKTY)                                         
         DC    XL4'00'                                                          
                                                                                
* RADIO HOME MARKET                                                             
CC#RHMKT EQU   23                                                               
CQ#RHMKT EQU   23                                                               
         DC    AL2(CQ#RHMKT)                                                    
         DC    CL5'RHMKT'                                                       
         DC    AL1(CC#RHMKT)                                                    
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RHMKBIND-SAVED)                                              
         DC    AL1(3)                                                           
         DC    AL1(LD_CHARQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#MKT)                                          
         DC    XL4'00'                                                          
                                                                                
CC#IPREC EQU   24                                                               
CQ#IPREC EQU   24                                                               
         DC    AL2(CQ#IPREC)                                                    
         DC    CL5'IPREC'                                                       
         DC    AL1(CC#IPREC)                                                    
         DC    AL1(LD_IAWPQ)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(IPREIND-SAVED)                                               
         DC    AL1(1)                                                           
         DC    AL1(LD_CHARQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#DPREC)                                        
         DC    XL4'00'                                                          
                                                                                
CC#PAGY  EQU   25                                                               
CQ#PAGY  EQU   25                                                               
         DC    AL2(CQ#PAGY)                                                     
         DC    CL5'PAGY '                                                       
         DC    AL1(CC#PAGY)                                                     
         DC    AL1(LD_IAWPQ)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(PAGYIND-SAVED)                                               
         DC    AL1(1)                                                           
         DC    AL1(LD_CHARQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#AGY)                                          
         DC    XL4'00'                                                          
                                                                                
CC#PSHF  EQU   27                                                               
CQ#PSHF  EQU   27                                                               
         DC    AL2(CQ#PSHF)                                                     
         DC    CL5'PSHF '                                                       
         DC    AL1(CC#PSHF)                                                     
         DC    AL1(LD_IAWPQ)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(PSHRIND-SAVED)                                               
         DC    AL1(1)                                                           
         DC    AL1(LD_CHARQ)                                                    
         DC    XL2'00'                                                          
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#AGY)                                          
         DC    XL4'00'                                                          
                                                                                
REQPDETX DC    AL1(LD_EOTQ)                                                     
                                                                                
REQUESTX DC    AL1(0)                                                           
                                                                                
***********************************************************************         
*======================================================================         
* OUTPUT MAP FOR PROGRAM DETAILS                                      |         
*======================================================================         
                                                                                
OUTPGDET DS    0X                                                               
                                                                                
O#PGDET  EQU   20                                                               
         DC    AL2(OUTPDETX-*)                                                  
         DC    AL2(O#PGDET)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
O#DEMHD  EQU   1                                 DEMO HEADER                    
         DC    AL2(O#DEMHDX-*)                                                  
         DC    AL2(O#DEMHD),C'ARRAY'                                            
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(O#DEMHD)                                                     
         DC    CL5'DEMHD'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYDEMHD-SVRDEF),AL1(0,0)                                    
         DC    XL4'00'                                                          
O#DEMHDX DS    0X                                                               
                                                                                
O#PC_ID  EQU   2                                 PC_ID                          
         DC    AL2(O#PC_IDX-*)                                                  
         DC    AL2(O#PC_ID),C'PC_ID'                                            
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(O#PC_ID)                                                     
         DC    CL5'PC_ID'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(OUTPCID-SAVED),AL1(LD_CHARQ,5)                               
         DC    XL4'00'                                                          
O#PC_IDX DS    0X                                                               
                                                                                
O#PGHDR  EQU   3                                 PROG DETAIL HEADER             
         DC    AL2(O#PGHDRX-*)                                                  
         DC    AL2(O#PGHDR),C'ARRAY'                                            
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(O#PGHDR)                                                     
         DC    CL5'PGHDR'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYPGDET-SVRDEF),AL1(0,0)                                    
         DC    XL4'00'                                                          
O#PGHDRX DS    0X                                                               
                                                                                
O#SUMHDR EQU   4                                 SUMMARY HEADER INFO            
         DC    AL2(O#SUMHDX-*)                                                  
         DC    AL2(O#SUMHDR),C'ARRAY'                                           
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(O#SUMHDR)                                                    
         DC    CL5'SUMHD'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYSUMM-SVRDEF),AL1(0,0)                                     
         DC    XL4'00'                                                          
O#SUMHDX DS    0X                                                               
                                                                                
O#SDEMO  EQU   5                                 PROG SUMMARY DEMOS             
O#DDEMO  EQU   6                                 PROGRAM DETAIL DEMOS           
O#UDEMO  EQU   9                                 UNIVERSE DETAILS               
O#RFLAG  EQU   10                                RADIO FLAG                     
         DC    AL2(O#DDEMOX-*)                                                  
O#DDEMON DC    AL2(O#DDEMO),C'ARRAY'                                            
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(O#DDEMO)                                                     
         DC    CL5'DDEMO'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYDDEM-SVRDEF),AL1(0,0)                                     
         DC    XL4'00'                                                          
O#DDEMOX DS    0X                                                               
                                                                                
                                                                                
OUTPDETX DS    0X                                                               
                                                                                
                                                                                
                                                                                
*======================================================================         
* OUTPUT MAP FOR CANADIAN SPOT DESKTOP LOOKUP                         |         
*======================================================================         
OUTCANLK LKOUT H                                                                
OUTCAN   LKOUT R,1                                                              
OUTPROG  LKOUT C,1,(D,B#SAVED,CANPROG),CHAR                                     
ARRAY    LKOUT C,2,(A,ARYCDEM)                                                  
OUTLKBK  LKOUT C,3,(D,B#SAVED,CANLKBK),CHAR                                     
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
*======================================================================         
* ARRAY DEFINITION DEMO VALUES FOR CANADIAN LOOKUPS                   |         
*======================================================================         
ARYCDEM  LKOUT A,(D,B#SAVED,SDEMVALS),ROWWIDTH=SDEMVALL,               *        
               NROWS=(B#SAVED,SDEMNUM)                                          
         LKOUT C,2,(D,,SDEMOS),UBIN,LEN=L'SDEMOS                                
         LKOUT E                                                                
                                                                                
*======================================================================         
* ARRAY DEFINITION PROGRAM DETAILS INFO                               |         
*======================================================================         
                                                                                
ARYPGDET DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(PGDTVALS-SAVED)                                              
         DC    AL2(PGDTNUM-SAVED)                  # OF ROWS                    
         DC    AL2(PGDTVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(PGDTCOLN)                                                    
         DC    XL4'00'                                                          
PGDTCOL  DC    0X                                                               
                                                                                
DD#PSTAT EQU   1                                   STATION                      
         DC    AL2(DD#PSTAT),C'STAT '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PSTAT-PGDTVALS),AL1(L'PSTAT)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#PPROG EQU   2                                   PROGRAM                      
         DC    AL2(DD#PPROG),C'PROG '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PPROG-PGDTVALS),AL1(L'PPROG)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#PFILE EQU   3                                   FILE                         
         DC    AL2(DD#PFILE),C'FILE '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PFILE-PGDTVALS),AL1(L'PFILE)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#PBOOK EQU   4                                   BOOK                         
         DC    AL2(DD#PBOOK),C'BOOK '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PBOOK-PGDTVALS),AL1(L'PBOOK)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#PSTIM EQU   5                                   START TIME                   
         DC    AL2(DD#PSTIM),C'STIM '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PSTIME-PGDTVALS),AL1(L'PSTIME)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#PETIM EQU   6                                   END TIME                     
         DC    AL2(DD#PETIM),C'ETIM '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PETIME-PGDTVALS),AL1(L'PETIME)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
DD#PWEEK EQU   7                                   WEEK                         
         DC    AL2(DD#PWEEK),C'WEEK '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PWEEKS-PGDTVALS),AL1(L'PWEEKS)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#DYTIM EQU   10                                  DAYTIME STRING               
         DC    AL2(DD#DYTIM),C'DYTIM'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PDYTIM-PGDTVALS),AL1(L'PDYTIM)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#PDAYS EQU   8                                   DAYS                         
         DC    AL2(DD#PDAYS),C'DAYS '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PDAYS-PGDTVALS),AL1(L'PDAYS)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
DD#PACTF EQU   9                                   ACTIVE FLAG                  
         DC    AL2(DD#PACTF),C'ACTF '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(PACTF-PGDTVALS),AL1(L'PACTF)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#MFID  EQU   11                                  MAINFRAME ID                 
         DC    AL2(DD#MFID),C'MFID '                                            
         DC    AL1(0,0,0)                                                       
         DC    AL2(PMFID-PGDTVALS),AL1(L'PMFID)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#FACT  EQU   12                                  FACTOR                       
         DC    AL2(DD#FACT),C'FACT '                                            
         DC    AL1(0,0,0)                                                       
         DC    AL2(PFACT-PGDTVALS),AL1(L'PFACT)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
PGDTCOLN EQU   (*-PGDTCOL)/LX_COLSL                                             
                                                                                
*======================================================================         
* ARRAY DEFINITION SUMMARY INFO                                       |         
*======================================================================         
                                                                                
ARYSUMM  DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(SUMMVALS-SAVED)                                              
         DC    AL2(SUMMNUM-SAVED)                  # OF ROWS                    
         DC    AL2(SUMMVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(SUMMCOLN)                                                    
         DC    XL4'00'                                                          
SUMMCOL  DC    0X                                                               
                                                                                
DD#SSTAT EQU   1                                   STATION                      
         DC    AL2(DD#PSTAT),C'STAT '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(SSTAT-SUMMVALS),AL1(L'SSTAT)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#SPROG EQU   2                                   PROGRAM/HIST PROGRAM         
         DC    AL2(DD#PPROG),C'PROG '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(SPROG-SUMMVALS),AL1(L'SPROG)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#SFILE EQU   3                                   FILE                         
         DC    AL2(DD#PFILE),C'FILE '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(SFILE-SUMMVALS),AL1(L'SFILE)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#SBOOK EQU   4                                   BOOK                         
         DC    AL2(DD#PBOOK),C'BOOK '                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(SBOOK-SUMMVALS),AL1(L'SBOOK)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#SSYSC EQU   5                                   SYSCODE                      
         DC    AL2(DD#SSYSC),C'SYSCD'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(SSYSC-SUMMVALS),AL1(L'SSYSC)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#ARBID EQU   6                                   ARBITRON ID                  
         DC    AL2(DD#ARBID),C'ARBID'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(SARBID-SUMMVALS),AL1(L'SARBID)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
DD#CMKTF EQU   7                               RADIO CONDENSE MKT FLAG          
         DC    AL2(DD#CMKTF),C'CMKTF'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(SCMKTFLG-SUMMVALS),AL1(L'SCMKTFLG)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
                                                                                
SUMMCOLN EQU   (*-SUMMCOL)/LX_COLSL                                             
*======================================================================         
* ARRAY DEFINITION FOR DEMOS PROGRAM SUMMARY                          |         
*======================================================================         
                                                                                
ARYSDEM  DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(SDEMVALS-SAVED)                                              
         DC    AL2(SDEMNUM-SAVED)                  # OF ROWS                    
         DC    AL2(SDEMVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(SDEMCOLN)                                                    
         DC    XL4'00'                                                          
SDEMCOL  DC    0X                                                               
                                                                                
DD#SDEMS EQU   1                                   DEMOS                        
         DC    AL2(DD#SDEMS),C'SDEMO'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(SDEMOS-SDEMVALS),AL1(L'SDEMOS)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
SDEMCOLN EQU   (*-SDEMCOL)/LX_COLSL                                             
                                                                                
*======================================================================         
* ARRAY DEFINITION FOR DEMOS     PROGRAM DETAIL                       |         
*======================================================================         
                                                                                
ARYDDEM  DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(DDEMVALS-SAVED)                                              
         DC    AL2(DDEMNUM-SAVED)                  # OF ROWS                    
         DC    AL2(DDEMVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(DDEMCOLN)                                                    
         DC    XL4'00'                                                          
DDEMCOL  DC    0X                                                               
                                                                                
DD#DDEMS EQU   1                                   DEMOS                        
         DC    AL2(DD#DDEMS),C'DDEMO'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(DDEMOS-DDEMVALS),AL1(L'DDEMOS)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
DDEMCOLN EQU   (*-DDEMCOL)/LX_COLSL                                             
                                                                                
*======================================================================         
* ARRAY DEFINITION FOR DEMOS HEADER RECORD                            |         
*======================================================================         
                                                                                
ARYDEMHD DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(DMHDVALS-SAVED)                                              
         DC    AL2(DMHDNUM-SAVED)                  # OF ROWS                    
         DC    AL2(DMHDVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(DMHDCOLN)                                                    
         DC    XL4'00'                                                          
DMHDCOL  DC    0X                                                               
                                                                                
DD#DHPRE EQU   1                                   PRECISION                    
         DC    AL2(DD#DHPRE),C'DMPRE'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(DMHDPRE-DMHDVALS),AL1(L'DMHDPRE)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
DD#DHDEM EQU   2                                   DEMO NAME                    
         DC    AL2(DD#DHDEM),C'DMNAM'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(DMHDDEM-DMHDVALS),AL1(L'DMHDDEM)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DMHDCOLN EQU   (*-DMHDCOL)/LX_COLSL                                             
                                                                                
PROFLIT  DC    C'1W PROFILE'                                                    
RSATEXT  DC    C'RATING SERVICE, STATION, ALPHA MKTS'                           
STIMTXT  DC    C'START TIME'                                                    
ETIMTXT  DC    C'END TIME'                                                      
LATBNTXT DC    C'NUMBER OF LATEST BOOKS'                                        
LATBTTXT DC    C'BOOKTYPE FOR LATEST BOOK'                                      
*======================================================================         
SAVED    DSECT                                                                  
SVVALUES DS    0D                                                               
RELOLST2 DS    0A                                                               
ADEMBUFF DS    A                                                                
AO#DDEMO DS    A                                                                
RLOLSTN2 EQU   (*-RELOLST2)/L'RELOLST2                                          
EFFS     DS    XL10                XL10'FFFFFFFFFFFFFFFFFFFF'                   
SVVALUESL EQU   *-SVVALUES                                                      
                                                                                
REQVALS  DS    0A                                                               
                                                                                
                                                                                
BYTE     DS    X                                                                
BOOKIND  DS    XL1                                                              
ABOOK    DS    AL3                                                              
DAYIND   DS    XL1                                                              
ADAY     DS    AL3                                                              
DEMOIND  DS    XL1                                                              
ADEMO    DS    AL3                                                              
         ORG   DEMOIND                                                          
DCATIND  DS    XL1                                                              
ADEMOCAT DS    AL3                                                              
TIMEIND  DS    XL1                                                              
ATIME    DS    AL3                                                              
BKTYIND  DS    XL1                                                              
ABKTY    DS    AL3                                                              
DATEIND  DS    XL1                                                              
ADATE    DS    AL3                                                              
FILEIND  DS    XL1                                                              
AFILE    DS    AL3                                                              
STAIND   DS    XL1                                                              
ASTAT    DS    AL3                                                              
OSTATIND DS    XL1                                                              
AOSTAT   DS    AL3                                                              
AMKTIND  DS    XL1                                                              
AAMKT    DS    AL3                                                              
DYTMIND  DS    XL1                                                              
ADAYTIME DS    AL3                                                              
NROWIND  DS    XL1                                                              
ANROW    DS    AL3                                                              
PCIDIND  DS    XL1                                                              
APCID    DS    AL3                                                              
DYPTIND  DS    XL1                                                              
ADYPT    DS    AL3                                                              
BUYDTIND DS    XL1                                                              
ABUYDATE DS    AL3                                                              
LPMSTIND DS    XL1                                                              
ALPMSTRT DS    AL3                                                              
MFIDIND  DS    XL1                                                              
AMFID    DS    AL3                                                              
UPGRIND  DS    XL1                                                              
AUPGRD   DS    AL3                                                              
UINDXIND DS    XL1                                                              
AUINDX   DS    AL3                                                              
UPININD  DS    XL1                                                              
AUPIND   DS    AL3                                                              
UPNAMIND DS    XL1                                                              
AUPNAME  DS    AL3                                                              
DECIND   DS    XL1                 RATING PRECISION                             
ADECIMAL DS    AL3                                                              
IPREIND  DS    XL1                 IMPRESSION PRECISION                         
AIPRE    DS    AL3                                                              
SYSCIND  DS    XL1                                                              
ASYSCODE DS    AL3                                                              
RRSMIND  DS    XL1                                                              
ARSTAMKT DS    AL3                                                              
LATBNIND DS    XL1                                                              
ALATBKNM DS    AL3                                                              
LATBTIND DS    XL1                                                              
ALATBKTP DS    AL3                                                              
DEFBTIND DS    XL1                                                              
ADEFBKTP DS    AL3                                                              
STCDIND  DS    XL1                                                              
ASTATECD DS    AL3                                                              
CNTYIND  DS    XL1                                                              
ACOUNTY  DS    AL3                                                              
RHMKBIND DS    XL1                                                              
ARHMKIND DS    AL3                                                              
PAGYIND  DS    XL1                                                              
APAGY    DS    AL3                 PRISMA AGENCY                                
LATWKIND DS    XL1                 WEEKNUM USED FOR NUMBER OF LATEST BK         
ALATWKNM DS    AL3                                                              
PSHRIND  DS    XL1                 PROGAM SHARE AND PUT                         
APSHR    DS    AL3                                                              
                                                                                
CANMED   DS    C                                                                
CANPROF  DS    CL16                                                             
CAN2DEC  DS    C                                                                
REQVALSL EQU   *-REQVALS                                                        
                                                                                
VERS76   EQU   X'0100004D'                                                      
SPVER26  EQU   X'02060000'         SPOT DESKTOP 2.6                             
SPVER33  EQU   X'03030000'         SPOT DESKTOP 2.3                             
SPVER208 EQU   X'040600D0'         SPOT DESKTOP 4.6.0.208                       
VERSNUM  DS    XL4                                                              
ELCODE   DS    C                                                                
MYSTAPTR DS    A                                                                
AMKTPTR  DS    A                                                                
DYTMPTR  DS    A                                                                
SVDYTPTR DS    A                                                                
OUTPUTF  DS    C                                                                
OUTPUTON EQU   X'08'                                                            
OUTPUTOF EQU   X'04'                                                            
NUMDEMO  DS    AL2                                                              
NUMDEMO2 DS    AL2                                                              
NUMSTAT  DS    AL2                                                              
DEMOPTR  DS    A                                                                
NMAMKTS  DS    X                                                                
INPSTA   DS    CL(L'STATION)                                                    
INPWEEK  DS    CL1                                                              
TMPSELPG DS    XL1                                                              
TMPSELDP DS    XL3                                                              
TMPFILE  DS    CL3                                                              
TMPBOOK  DS    XL3                                                              
TMPBKTYP DS    C                                                                
TMPBKWK  DS    CL1                     WEEK                                     
TMPDAY   DS    CL1                                                              
TMPSETIM DS    XL4                                                              
TMPPCID  DS    XL(L'OUTPCID)                                                    
TMPUINDX DS    XL1                                                              
TMPLATBN DS    XL1                                                              
TMPLATBT DS    C                                                                
TMPSTA   DS    CL(L'DBSELSTA)                                                   
TMPDYTIM DS    200CL5                                                           
TMPSYSC  DS    XL2                                                              
TMPFILRD DS    CL3                                                              
TMPDBFIL DS    X                                                                
TMPCNTY  DS    XL(L'STACNTY)                                                    
TMPSTATE DS    XL(L'STASTATE)                                                   
CNTYINDX DS    X                                                                
FLAG2DEC DS    C                                                                
MYDMCB   DS    6F                                                               
SAVEDMCB DS    6F                                                               
DEMOHDRF DS    C                    DEMO HEADER OUTPUT RECORD  FLAG             
DMHDR_RELEASED EQU 01               DEMO RECORD RELEASED/DOWNLOADED             
DMHDR_FROMSTART EQU 10              DOWNLOAD FROM START OF DEMO LIST            
DMHDR_CONTINUE EQU 20               CONTINUE DOWNLOAD REST OF DEMO LIST         
TEMPSTIM DS    XL2                                                              
TEMPETIM DS    XL2                                                              
TEMPIDAY DS    XL1                                                              
DAYTIME  DS    CL13                                                             
TMPWKS   DS    XL1                                                              
MAXDAYS  EQU   20                                                               
DAYS     DS    (MAXDAYS)XL5                                                     
FILBKPTR DS    A                                                                
ROWCOUNT DS    F                                 CURRENT ROW COUNT              
FBKCOUNT DS    F                                 CURRENT FILE BK COUNT          
TOTALROW DS    F                                 TOTAL NUMBER OF ROWS           
TOTALFBK DS    F                                 TOTAL NUMBER OF FIL/BK         
ROTFLAG  DS    C                                                                
SVROTFLG DS    C                                                                
ALLTPFLG DS    C                                 ALL TP PROCESSED?              
ALLSTATF DS    C                                                                
TIMEZONE DS    C                                 ALL STATION LOOKUP             
THISPROG DS    CL17                                                             
ANYDETLS DS    C                                 DETAILS FLAG                   
ANYTPFLG DS    C                                                                
SAVGFLAG DS    C                                                                
         DS     0H                                                              
SAVEDIV  DS     XL(L'DBDIVSOR)                                                  
RDAYPFLAG DS    X                                                               
DAYPART_STANDARD  EQU X'01'                                                     
DAYPART_CUME_ONLY EQU X'10'                                                     
RDEMFLAG DS    X                                                                
RDEM_X_MODIFIER EQU X'01'                                                       
RDEM_O_MODIFIER EQU X'02'                                                       
RDEM_F_MODIFIER EQU X'04'                                                       
RADIOFLAG DS   X                                                                
RADIO_READ_RDP EQU X'01'                                                        
RADIO_READ_RTP EQU X'10'                                                        
RADIO_READIMPS EQU X'40'                                                        
RADIO_READCUMS EQU X'80'                                                        
SVSELDAY DS    X                                                                
SVSETIM  DS    XL(L'TMPSETIM)                                                   
SVLATBK  DS    XL2                                                              
SVLATBT  DS    C                                                                
DPTINDEX DS    X                                                                
ADAYPART DS    A                                                                
ALLDAYPT DS    X                                                                
NUMLATBK DS    X                                                                
LATBKTYP DS    C                                                                
OVRFLAG  DS    C                     OVERRIDE STATION FLAG                      
LATFLAG  DS    C                                                                
FROMTYPE DS    CL1                                                              
TMPUPNAM DS    CL15                                                             
TMPMBKS  DS    XL6                                                              
OFFLFLAG DS    CL1                                                              
TMPWHOLE DS    XL(L'WHOLEFLG)      WHOLE # FLAG FOR EXTRACTING DEMOS            
WHOLEFLG DS     XL1                 MODIFIERS TO DISPLAY AS WHOLE #S            
WHOLERTG EQU    X'80'               RTGS DISPLAYED AS XX.0                      
WHOLESHR EQU    X'40'               SHRS DISPLAYED AS XX.0                      
WHOLEPUT EQU    X'20'               PUTS DISPLAYED AS XX.0                      
BUYENDT  DS    XL3                                                              
LPMSTRT  DS    XL3                                                              
OPTDEC   DS    C                   RATING PRECISION                             
OPTIPRE  DS    C                   IMPRESSION PRECISION                         
OPTPAGY  DS    CL2                 PRISMA AGENCY                                
PROGSHRF DS    C                                                                
RHOMEMKT DS    CL3                                                              
FORCEBT  DS    X                                                                
FORCEFT  DS    C                                                                
READBKTP DS    X                    INTERNAL BOOKTYPE                           
**NSTATS   DS    X                   NUMBER OF STATIONS REQUESTED               
NSTATS   DS    CL2                 NUMBER OF STATIONS REQUESTED                 
PREVSTAT DS    CL5                                                              
ATHISTAT DS    A                   ADDRESS OF STATION BEING PROCESSED           
SYSCEXT  DS    0X                                                               
         DS    XL(L'SPXTAREA)                                                   
                                                                                
CONDMKTF DS    C                       CONDENSE MARKET FLAG                     
MYBKH    DS    CL8                                                              
MYBK     DS    CL10                                                             
DUMSTATH DS    CL8                                                              
DUMSTAT  DS    CL20                                                             
ALFMKTS  DS    CL3                                                              
SPILLMKT DS    XL2                                                              
PREVMKT  DS    XL2                                                              
                                                                                
MAXDEMS  EQU   60                                                               
THISDEMS DS    (MAXDEMS*3)XL4          CURRENT DEMOUT DEMOVALUES                
THISDEMX DS    0X                                                               
THISDEML EQU   THISDEMX-THISDEMS                                                
*                                                                               
DEMODEMS DS    (MAXDEMS*3)XL3,X        DEMO VALUES (+LIST TERMINATOR)           
DEMODEML EQU   *-DEMODEMS                                                       
                                                                                
                                                                                
                                                                                
MYPROF1W DS    CL(L'PROF1W)                                                     
AUNIVDEM DS    A                                                                
AIMPSDEM DS    A                                                                
ACUMSDEM DS    A                                                                
VGETPROF DS    A                                                                
MYDATDSP DS    H                                                                
SDBXF    DS    C                       SET DBLOCK EXTENSION FLAG                
DBXSPOTX DS    XL128                   DBBLOCK EXTENSION "SPOT"                 
MYDBXTRA DS    XL128                   DBLOCK EXTENSION                         
         DS    0F                      (FORCE FULL-WORD ALIGNMENT)              
DBXTROTN DS    0X                      DBLOCK EXTENSION FOR ROTATIONS           
         DS    CL4                     LINK ID                                  
         DS    A                       A(NEXT LINK)                             
DBXTDYTM DS    (MAXDTCMP)XL5,XL1       DAY/TIME LINK                            
MAXDTCMP EQU   (255)/(19+1)            L(MTWTFSS/1130A-1230P) = 19              
                                                                                
DBXTCNTY DS    0X                      DBLOCK EXTENSION FOR ROTATIONS           
         DS    CL4                     LINK ID                                  
         DS    A                       A(NEXT LINK)                             
DBXTCTYC DS    (NUMCNTY)XL3,XL1        COUNTY CODES  X'FF' =EOL                 
DBXTCTYL EQU   *-DBXTCTYC                                                       
NUMCNTY  EQU   10                                                               
*                                                                               
         DS    0F                                                               
                                                                                
*                                      DIFFERING ACTIVE WEEKS                   
AIUNWRK  DS    A                       A(WORK AREA FOR IUN STUFF)               
IUNWRKL  EQU   IUNRECL+500                                                      
*                                                                               
OUTVALS  DS    0X                                                               
CANPROG  DS     CL(L'THISPROG)                                                  
OUTPCID  DS    CL5                               PC_ID                          
OUTMFID  DS    CL14                              OUT MAINFRAME ID               
CANLKBK  DS    CL12                                                             
                                                                                
DDEMNUM  DS    AL2                                                              
DDEMVALS DS    0X                                                               
DDEMSEND DS    CL1                                                              
DDEMOS   DS    CL4                                                              
DDEMVALL EQU   *-DDEMVALS                                                       
         DS    60XL(DDEMVALL)                                                   
         ORG   DDEMNUM                                                          
SDEMNUM  DS    AL2                                                              
SDEMVALS DS    0X                                                               
SDEMSEND DS    CL1                                                              
SDEMOS   DS    CL4                                                              
SDEMVALL EQU   *-SDEMVALS                                                       
         DS    60XL(SDEMVALL)                                                   
         ORG   DDEMNUM                                                          
UDEMNUM  DS    AL2                                                              
UDEMVALS DS    0X                                                               
UDEMSEND DS    CL1                                                              
UDEMOS   DS    CL4                                                              
UDEMVALL EQU   *-SDEMVALS                                                       
         DS    60XL(SDEMVALL)                                                   
         ORG   DDEMNUM                                                          
CMKFNUM  DS    AL2                                                              
CMKFVALS DS    0X                                                               
CMKFSEND DS    CL1                                                              
CMKDEMOF DS    CL4                   CONDENSE MKT DEMO FLAG (Y)                 
CMKFVALL EQU   *-CMKFVALS                                                       
         DS    60XL(CMKFVALL)                                                   
                                                                                
DMHDNUM  DS    AL2                                                              
DMHDVALS DS    0X                                                               
DMHDSEND DS    CL1                                                              
DMHDPRE  DS    XL1                                                              
DMHDDEM  DS    CL20                                                             
DMHDVALL EQU   *-DMHDVALS                                                       
         DS    60XL(DMHDVALL)                                                   
DMHDBUFFL EQU  *-DMHDNUM                                                        
                                                                                
PGDTNUM  DS    AL2                                                              
PGDTVALS DS    0X                                                               
PGDTSEND DS    CL1                                                              
PSTAT    DS    CL10                                                             
PPROG    DS    CL(L'THISPROG)                                                   
PBOOK    DS    CL50                                                             
PSTIME   DS    XL2                                                              
PETIME   DS    XL2                                                              
PWEEKS   DS    CL4                                                              
PDAYS    DS    XL1                                                              
PFILE    DS    CL3                                                              
PACTF    DS    CL1                                                              
PDYTIM   DS    CL20                                                             
PMFID    DS    CL15                                                             
PFACT    DS    XL2                                                              
PGDTVALL EQU   *-PGDTVALS                                                       
                                                                                
SUMMNUM  DS    AL2                                                              
SUMMVALS DS    0X                                                               
SUMMSEND DS    CL1                                                              
SSTAT    DS    CL10                                                             
SPROG    DS    CL(L'THISPROG)                                                   
SBOOK    DS    CL50                                                             
SFILE    DS    CL3                                                              
SSYSC    DS    CL5                                                              
SARBID   DS    XL1                                                              
SCMKTFLG DS    CL1                                                              
SUMMVALL EQU   *-SUMMVALS                                                       
                                                                                
                                                                                
                                                                                
OUTVALSL EQU   *-OUTVALS                                                        
                                                                                
                                                                                
         PRINT OFF                                                              
                                                                                
       ++INCLUDE DELNKWRK                                                       
WORKD    DSECT                                                                  
         ORG   OVERWORK                                                         
THISDMS2 DS    (MAXDEMS*3)XL4          CURRENT DEMOUT DEMOVALUES                
THISDM2X DS    0X                                                               
THISDM2L EQU   THISDM2X-THISDMS2                                                
                                                                                
BKSREAD  DS    XL8                                                              
BKSREADX DS    X                                                                
                                                                                
                                                                                
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENANMK                                                      
       ++INCLUDE SPDDEQUS                                                       
       ++INCLUDE REDDEQUS                                                       
       ++INCLUDE DEDEMEQUS                                                      
                                                                                
       ++INCLUDE REGENSET                                                       
       ++INCLUDE REGENSTA                                                       
                                                                                
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DEDEMTABD                                                      
                                                                                
SPDEMLKD DSECT                                                                  
       ++INCLUDE SPDEMLK                                                        
                                                                                
RRDPRECD DSECT                                                                  
       ++INCLUDE REGENRDP                                                       
       ++INCLUDE FAFACTS                                                        
                                                                                
       ++INCLUDE DEDEMFILE                                                      
                                                                                
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE DDBUFFD                                                        
SPDEMLKD DSECT                                                                  
       ++INCLUDE SPDEMLKXTD                                                     
       ++INCLUDE SPSTAPACKD                                                     
                                                                                
*******************************************************************             
*========================== IUN RECORD DSECT =========================*         
                                                                                
IUNRECD  DSECT                                                                  
                                                                                
IUNIVS   DS    (IUNNVALS)F        UNIVERSES                                     
                                                                                
IUNOLD   DS    0F                 ORIGINAL (OLD) BOOK VALUES                    
IOLDRTG  DS    (IUNNVALS)F         RATINGS                                      
         ORG   IOLDRTG+IUNHMDSP                                                 
IUORHOME DS    F                                                                
         ORG                                                                    
IOLDIMP  DS    (IUNNVALS)F         IMPRESSIONS                                  
IOLDHPT  DS    (IUNNVALS)F         HUTS/PUTS                                    
         ORG   IOLDHPT+IUNHMDSP                                                 
IUOPHOME DS    F                                                                
         ORG                                                                    
IOLDTOT  DS    (IUNNVALS)F         TSA TOTALS                                   
         ORG   IOLDTOT+IUNHMDSP                                                 
IUOQHOME DS    F                                                                
         ORG                                                                    
IUNOLDX  EQU   *                                                                
                                                                                
IUNNEW   DS    0F                 NEW VALUES                                    
INEWRTG  DS    (IUNNVALS)F         RATINGS                                      
         ORG   INEWRTG+IUNHMDSP                                                 
IUNRHOME DS    F                                                                
         ORG                                                                    
INEWIMP  DS    (IUNNVALS)F         IMPRESSIONS                                  
INEWHPT  DS    (IUNNVALS)F         HUTS/PUTS                                    
         ORG   INEWHPT+IUNHMDSP                                                 
IUNPHOME DS    F                                                                
         ORG                                                                    
INEWTOT  DS    (IUNNVALS)F         TSA TOTALS                                   
         ORG   INEWTOT+IUNHMDSP                                                 
IUNQHOME DS    F                                                                
         ORG                                                                    
IUNNEWX  EQU   *                                                                
                                                                                
         DS    0CL((IUNOLDX-IUNOLD)-(IUNNEWX-IUNNEW)+1)                         
         DS    0CL((IUNNEWX-IUNNEW)-(IUNOLDX-IUNOLD)+1)                         
                                                                                
IUNOTH   DS    0F                 OTHER VALUES                                  
ISHOMES  DS    F                                                                
ISMETA   DS    F                                                                
ISMETB   DS    F                                                                
                                                                                
ILUNVS   DS    (IUNNVALS)F         LOONEYVERSES                                 
ILUNVX   EQU   *                                                                
                                                                                
IUNRECL  EQU   *-IUNRECD                                                        
                                                                                
                                                                                
IUNNVALS EQU   32                  # OF IUN VALUES                              
IUNLVALS EQU   IUNNVALS*4          LENGTH OF IUN VALUES                         
IUNHMNDX EQU   20                  INDEX TO HOMES RTGS IN IUN RTGS AREA         
IUNHMDSP EQU   IUNHMNDX*4          DISPL TO HOMES RTGS IN IUN RTGS AREA         
***********************************************************************         
*******************************************************************             
*  2 DSECTS TO COVER TABLES BUILT BY DDLINK                       *             
*  DSECT TO COVER UPGRADES AND INDEX NUMBER                       *             
*******************************************************************             
UPGRADD  DSECT                                                                  
UPBKTYP  DS    CL1                                                              
UPBKVAL  DS    XL8                                                              
UPSHRBK  DS    XL2                                                              
UPDYTIM  DS    XL5                                                              
UP2YRP   DS    CL1                                                              
UP2YRR   DS    CL1                                                              
UPWEEK   DS    XL1                                                              
UPNAME   DS    XL15                              UPGRADE NAME                   
UPINDEX  DS    XL1                                                              
UPGRADX  EQU   *-UPGRADD                                                        
*                                                                               
FILBKD   DSECT                                                                  
*                                          DSECT TO COVER FILE  BOOK            
FBKFILE  DS    CL3                               FILE CODE                      
FBKBKTY  DS    CL1                               BOOKTYPE                       
FBKWEEK  DS    CL1                               WEEK                           
FBKVBFLG DS    XL1                               VALID BOOK FLAG                
FBKBOOK  DS    XL3                               3 BYTE BOOK VALUE              
*  NEWEST                                                                       
FBKMULBK DS    XL6                               3( 2 BYTES BOOKS)              
*                                                                               
FBKPCID  DS    CL5                               PC ID                          
*                                                                               
FBKUPIND DS    XL1                               UPGRADE INDEX                  
*                                                                               
FBKLATBN DS    XL1                               NUM OF LATEST BKS              
FBKLATBT DS    XL1                               BOOKTYPE FOR LATEST BK         
FBKLATWN DS    XL1                               WEEKNUM FOR LATBN              
                                                                                
                                                                                
FILBKL   EQU   *-FILBKD                                                         
                                                                                
STATTABD DSECT                                                                  
*                               DSECT TO COVER STATION HEADER                   
STATION  DS    CL11                 INPUT STATION                               
STAFLG   DS    CL1                  VALID STATION FLAG                          
STALEN   DS    XL1                  LENGTH OF STATION INPUT                     
STASPLLF DS    XL1                  SPILL STAT FLAG                             
STASTATE DS    XL((NUMCNTY*1)+1)    STATE                                       
STACNTY  DS    XL((NUMCNTY*2)+1)    COUNTIES                                    
STADYTM  DS    CL61                 LIST OF DAY/TIME CODE                       
                                                                                
STABSTRT DS    XL2                  BUY START DATE                              
STABEND  DS    XL2                  BUY END DATE                                
STABLPMD DS    XL4                  BUY LPM START DATE                          
STASYSC  DS    CL2                  SYSCODE                                     
                                                                                
STATNROW DS    XL4                  NUMBER OF ROWS                              
STATTABL EQU   *-STATTABD                                                       
                                                                                
                                                                                
VDOUTD   DSECT                     ** DEMO OUTPUT ROW **                        
VDODEMCD DS    0XL3                ** DEMO CODE **                              
         DS    X                                                                
VDODEMTY DS    C                   DEMO TYPE                                    
VDODEMNO DS    X                   DEMO NUMBER                                  
VDODEMYN DS    C                   ** DEMO IS VALID INDICATOR **                
VDODEMYQ EQU   C'Y'                DEMO IS VALID                                
VDODEMNQ EQU   C'N'                DEMO IS INVALID                              
VDODEMNM DS    CL20                DEMO NAME/INPUT STRING                       
VDOUTL   EQU   *-VDOUTD            WIDTH OF DEMO ROW                            
VDTOTLN  EQU   MAXDEMN*VDOUTL+1    LENGTH OF INPUT VALIDATED DEMO BLOCK         
MAXDEMN  EQU   42                                                               
                                                                                
CANRSRVD DSECT                     CANADIAN LOOKUP SOURCE/STA/AMKT              
CANRSRV  DS    CL1                 RATING SERVICE                               
CANSTAT  DS    CL5                 STATION                                      
CANNMKTS DS    XL1                 NUMBER OF ALPHA MARKETS                      
CANAMKTS DS    17CL3               INPUT ALPHA MKT (17 MAX)                     
CANDAY   DS    X                                                                
CANSTIM  DS    XL2                                                              
CANETIM  DS    XL2                                                              
CANBOOK  DS    CL2                                                              
CANRSRVL EQU   *-CANRSRVD          WIDTH                                        
                                                                                
MFIDD    DSECT                     MAINFRAME ID                                 
MFDAY    DS    CL3                 DAYCODE                                      
MFSTIME  DS    CL4                 START TIME                                   
MFETIME  DS    CL4                 END TIME                                     
MFPURE   DS    CL4                 PURE NUMBER                                  
*                                                                               
DFILTABD DSECT                                                                  
DINFILE  DS    CL3                 INPUT FILE STRING FROM PC                    
DFILMED  DS    CL1                 MEDIA CODE                                   
DFILSRC  DS    CL1                 SOURCE                                       
DFILFMS  DS    CL3                 FMS FROM RECORD                              
DTMPFILE DS    CL3                 MOVED INTO TMPFILE                           
DFIL2DEC DS    CL1                 2 DECIMAL ALLOWED?                           
DFILDBF  DS    X                   DBFILE                                       
DFILTABL EQU   *-DFILTABD                                                       
                                                                                
*                                                                               
STAND    DSECT                                                                  
STANDENT DS    0CL49                                                            
STANDDTS DS    XL26           UP 5 DAY/HOURS  PLUS X'FF' EOL INDICATOR          
STANDPRG DS    XL1            'PROGRAM CODE'                                    
STANDESC DS    CL20           DESCRIPTION                                       
STANDFLG DS    CL1                                                              
STAARBID DS    X              ARBITRON ID                                       
ST_RF_DPT_RETURNDEMOS   EQU   X'01'                                             
ST_RF_DPT_RETURNZEROS   EQU   X'11'                                             
*                                                                               
TVDPTD   DSECT                                                                  
TVDPTENT DS    0XL40                                                            
TVESTDPT DS    XL3            EASTERN/PACIFIC DAYPART                           
TVESTPRG DS    XL17                                                             
TVCENDPT DS    XL3            CENTRAL/PACIFIC DAYPART                           
TVCENPRG DS    XL17                                                             
*                                                                               
PRECTABD DSECT                                                                  
PTABMOD  DS    CL1                 MODIFIER                                     
PTABPRE  DS    XL1                 DEFAULT PRECISION                            
PTABMED  DS    CL1                 MEDIA                                        
PTABOVR1 DS    XL1                 2 DECIMAL PRE OVERRIDE                       
PTABLNQ  EQU   *-PTABMOD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068DELNK13   02/26/21'                                      
         END                                                                    
