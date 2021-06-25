*          DATA SET GEFIL05    AT LEVEL 038 AS OF 08/10/11                      
*          DATA SET GEFIL05    AT LEVEL 036 AS OF 09/19/02                      
*PHASE T00AB5C                                                                  
*&&      SET   NOP=N                                                            
GEFIL05  TITLE 'GLOBAL LEVEL DOWNLOAD OBJECT'                                   
*                                                                               
* 034 TSMY 06JUL98 SET CORRECT START LSLINE# FOR SESSION AT DISLST              
*                                                                               
GEFIL05  CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
***********************************************************************         
* DOWNLOAD OBJECT                                                     *         
*                                                                     *         
* NTRY: P1 = OBJECT CODE                                              *         
*       P2 = EQUATED VERB                                             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
LIST     NMOD1 LCWORKL,**GF05**,R6,RR=RE,CLEAR=YES                              
         USING LCWORKD,RC                                                       
         USING TWAD,RA                                                          
         ST    RE,LCRELO                                                        
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
GSFRR    USING FRRELD,GSFRREL                                                   
PSFRR    USING FRRELD,PSFRREL                                                   
GSFRA    USING FRAELD,GSFRAEL                                                   
GSFRP    USING FRPELD,GSFRPEL                                                   
PSFRA    USING FRAELD,PSFRAEL                                                   
P        USING SSAVD,PSSAV                                                      
*                                                                               
         MVC   LCAREC,AIOREC                                                    
         MVC   LCEQUREC,=AL4(XIO11)                                             
         TM    LSSTAT1,LSSMAIN     TEST MAINTENANCE LIST                        
         BZ    *+16                                                             
         MVC   LCAREC,AIO4                                                      
         MVC   LCEQUREC,=AL4(XIO4)                                              
         L     RF,ACOM                                                          
         L     RF,CDLFLD-COMFACSD(RF)                                           
         ST    RF,VDLFLD                                                        
*                                                                               
LISTNTR  ST    R1,LCAR1                                                         
         MVC   LCPARMS,0(R1)                                                    
*                                                                               
         LA    R1,OBJTAB                                                        
         USING OBJTABD,R1                                                       
OBJ02    CLI   OBJTABD,EOT                                                      
         BE    EXITOK                                                           
         CLC   OBJVERB,LCPARMS1+3                                               
         BE    *+12                                                             
         LA    R1,OBJTABL(R1)                                                   
         B     OBJ02                                                            
         ICM   R7,15,OBJADR                                                     
         A     R7,LCRELO                                                        
         BR    R7                                                               
         DROP  R1                                                               
*                                                                               
OBJTAB   DS    0X                                                               
         DC    AL1(ODLOAD,0,0,0),AL4(DWNLD)                                     
         DC    AL1(OREP,0,0,0),AL4(REPORT)                                      
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* DOWNLOAD OBJECT                                                     *         
***********************************************************************         
         SPACE 1                                                                
DWNLD    LA    R1,DWNLTAB                                                       
         USING OBJTABD,R1                                                       
*                                                                               
DWNL02   CLI   OBJVERB,EOT                                                      
         BE    DWNL08                                                           
         CLC   OBJVERB,LCVERB                                                   
         BNE   *+14                MATCHED                                      
         CLC   OBJSVB,LCSVB                                                     
         BE    *+12                                                             
         LA    R1,OBJTABL(R1)                                                   
         B     DWNL02              BUMP & LOOP                                  
*                                                                               
DWNL04   TM    OBJIND1,OBJPRIV     PRIVATE?                                     
         BZ    DWNL06                                                           
         L     RE,4(RD)            MAKE SURE INVOKED AT THIS LEVEL              
         L     RF,4(RE)                                                         
         CLC   16(4,RE),16(RF)                                                  
         BE    DWNL06                                                           
         DC    H'0'                                                             
*                                                                               
DWNL06   ICM   RF,15,OBJADR                                                     
         A     RF,LCRELO                                                        
         BR    RF                                                               
         DROP  R1                                                               
*                                                                               
DWNL08   DC    H'0'                NOT KNOWN AT THIS LEVEL                      
*                                                                               
DWNLTAB  DS    0A                                                               
         DC    AL1(DLDO,0,0,0),AL4(DOIT)                                        
         DC    AL1(DPQINIT,0,0,0),AL4(PQINIT)                                   
         DC    AL1(DSPOOK,0,0,0),AL4(DSPK)    BUILD OFFLINE REQUEST             
         DC    AL1(EOT)                                                         
*                                                                               
ROUTS    NTR1                                                                   
         LA    RF,DWNROUTS                                                      
ROUTS02  CLI   0(RF),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLM   R1,1,0(RF)                                                       
         BE    *+12                                                             
         LA    RF,L'DWNROUTS(RF)                                                
         B     ROUTS02                                                          
*                                                                               
         ICM   R7,15,1(RF)                                                      
         A     R7,LCRELO                                                        
         BR    R7                                                               
*                                                                               
DWNROUTS DS    0XL5                                                             
         DC    AL1(DPQOPEN),AL4(PQOPEN)                                         
         DC    AL1(DBLDREP),AL4(BLDDWN)                                         
         DC    AL1(DPQCLSE),AL4(PQCLSE)                                         
         DC    AL1(DINTDLCB),AL4(INITDLCB)                                      
         DC    AL1(DCLSDLCB),AL4(CLSEDLCB)                                      
         DC    AL1(DGETFRST),AL4(GETFRST)                                       
         DC    AL1(DGETNEXT),AL4(GETNEXT)                                       
         DC    AL1(DTSARDIR),AL4(TSARDIR)                                       
         DC    AL1(DTSARFIL),AL4(TSARFIL)                                       
         DC    AL1(DTSARTSA),AL4(TSARTSA)                                       
         DC    AL1(DDISLINE),AL4(DISLINE)                                       
         DC    AL1(DSETCOLS),AL4(COLUMNS)                                       
         DC    AL1(DSCREEN),AL4(SCREEN)                                         
         DC    AL1(DDLINIT),AL4(INITDL)                                         
         DC    AL1(DDISLST),AL4(DISLST)                                         
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD DOWNLOAD REPORT                                               *         
***********************************************************************         
         SPACE 1                                                                
DOIT     GOTOX ROUTS,DPQOPEN       OPEN THE PRINT QUEUE                         
         GOTOX ROUTS,DINTDLCB      INITIALISE THE DLCB                          
         GOTOX ROUTS,DBLDREP       DO THE DOWNLOAD                              
         BL    DOITX               ERROR BUILDING DOWNLOAD                      
*                                                                               
         GOTOX ROUTS,DCLSDLCB      CLOSE THE DLCB                               
         GOTOX ROUTS,DPQCLSE       CLOSE THE PRINT QUEUE                        
         CR    RB,RB               ENSURE CC EQUAL                              
*                                                                               
DOITX    B     EXIT                DONE.                                        
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE PRINT QUEUE FOR DOWNLOADING                              *         
*                                                                     *         
* IF PRINTING IS TO BE SOON OR OVERNIGHT, THE APPLICATION MUST SET    *         
* UP THE FOLLOWING FIELDS WITH THE CORRECT VALUES:                    *         
*                                                                     *         
* INSYSID, INPRGID, INJCLID, INPRTY1, INPRTY2                         *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
PQINIT   L     R5,AREP                                                          
         USING REPD,R5                                                          
*                                                                               
         MVC   INDEST,CUUSER       SET DEFAULT DESTINATION                      
         MVI   INWIDTH,REPWREGQ    SET REGULAR REPORT WIDTH AS DEFAULT          
*                                                                               
         MVI   REPACTN,REPAINI     INITIALIZE REPBLK                            
         LA    R0,REPHS            SET A(REPORT BUFFER)                         
         ST    R0,REPABUF                                                       
         MVC   REPAPQB,ATIA        SET A(TIA)                                   
         MVC   REPACOM,ACOM        SET A(COMFACS)                               
         MVC   REPDATE,ASBDAT      SET TODAY'S DATE                             
         MVC   REPCTRY,CUCTRY      SET CONNECTED COUNTRY                        
         MVC   REPLANG,CULANG      SET CONNECTED LANGUAGE                       
         MVC   REPSYSID,INSYSID    SET SYSTEM ID                                
         MVC   REPPRGID,INPRGID    SET PROGRAM ID                               
*                                                                               
         CLI   ASONOFF,ASOFF       OFFLINE - SET UP MASTC/BOX                   
         BNE   DIN02                                                            
         MVC   REPAMST,TWMASTC                                                  
         MVC   REPABOX,TWBOX                                                    
*                                                                               
DIN02    MVC   REPWIDTH,INWIDTH    SET REGULAR REPORT WIDTH AS DEFAULT          
         MVI   REPHEADN,REPHN      REGULAR HEADLINES                            
         MVI   REPMIDSN,REPMN      REGULAR MIDLINES                             
         MVI   REPPRNTN,REPPN      REGULAR PRINTLINES                           
         MVI   REPFOOTN,REPFN      REGULAR FOOTLINES                            
*                                                                               
         GOTOX AOLY,LCPARM,ODLOAD,DPQINIT                                       
*                                                                               
         MVC   REPWIDTH,INWIDTH    SEE IF WIDTH OVERRIDDEN                      
         CLI   REPWIDTH,REPWIDEQ   TEST WIDE LINE OVERRIDE                      
         BNE   *+20                                                             
         MVI   REPHEADN,REPHWN     WIDE HEADLINES                               
         MVI   REPMIDSN,REPMWN     WIDE MIDLINES                                
         MVI   REPPRNTN,REPPWN     WIDE PRINTLINES                              
         MVI   REPFOOTN,REPFWN     WIDE FOOTLINES                               
*                                                                               
         CLI   REPWIDTH,REPWNARQ   TEST NARROW LINE OVERRIDE                    
         BNE   *+20                                                             
         MVI   REPHEADN,REPHNN     NARROW HEADLINES                             
         MVI   REPMIDSN,REPMNN     NARROW MIDLINES                              
         MVI   REPPRNTN,REPPNN     NARROW PRINTLINES                            
         MVI   REPFOOTN,REPFNN     NARROW FOOTLINES                             
*                                                                               
         B     EXITOK                                                           
         DROP  R5                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* OPEN PRINT QUEUE FOR DOWNLOADING                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
PQOPEN   L     R5,AREP                                                          
         USING REPD,R5                                                          
         MVI   REPACTN,REPAINI     INITIALIZE REPBLK                            
         GOTOX VREPORT,REPBLK                                                   
*                                                                               
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         GOTOX VREPORT,REPBLK                                                   
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   ASONOFF,ASOFF       OFFLINE?                                     
         BNE   DOPN06                                                           
         OC    REPAPRT,REPAPRT                                                  
         BZ    DOPN06                                                           
*                                                                               
         L     R1,TWMASTC          LOCATE REMOTEC                               
         L     R1,MCVREMOT-MASTD(R1)                                            
         USING REMOTED,R1                                                       
         OC    REMOTKEY,REMOTKEY   TEST REMOTE PRINTING                         
         BZ    DOPN06                                                           
         CLI   INOTYP,C'@'         TEST REFORM ID IN DESCRIPTION                
         BE    DOPN04                                                           
         CLI   REMOTFLG,X'FA'      TEST NEW STYLE LAYOUT                        
         BL    DOPN02               NO                                          
         OC    REPDESC,REPDESC     OVERIDE REPORT SHORT DESCRIPTION ?           
         BZ    *+10                                                             
         MVC   REMOTDSC,REPDESC                                                 
         OC    REPMAXL,REPMAXL     OVERIDE MAX LINES PER PAGE?                  
         BZ    *+10                                                             
         MVC   REMOTLPP,REPMAXL                                                 
         OC    REPFORM,REPFORM     OVERIDE FORMS CODE?                          
         BZ    *+10                                                             
         MVC   REMOTFNO,REPFORM                                                 
         OC    REPCOPY,REPCOPY     OVERIDE NUMBER OF COPIES?                    
         BZ    *+10                                                             
         MVC   REMOTCPY,REPCOPY                                                 
         B     *+10                                                             
*                                                                               
DOPN02   MVC    REMOTKEY(L'REPDESC),REPDESC                                     
*                                                                               
DOPN04   MVC    REMOTPAS,REPPSWD    OVERIDE REPORT PQ PASSWORD                  
         DROP   R1                                                              
*                                                                               
DOPN06   MVI   REPACTN,REPAPUT     SET THIS                                     
         GOTOX ROUTS,DSCREEN       PRINT SCREEN ON PAGE 1                       
         OI    REPHEADI,REPHFRCE                                                
         GOTOX VREPORT,REPBLK      DO DOWNLOAD ON PAGE 2                        
         B     EXITOK                                                           
         DROP  R5                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO PRINT THE SCREEN ON PAGE 1                               *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
SCREEN   GOTOX AOLY,LCPARM,ODLOAD,DSCREEN                                       
         BNH   EXITOK                                                           
*                                                                               
         L     R5,AREP                                                          
         USING REPD,R5                                                          
*                                                                               
         CLI   ASONOFF,ASOFF       OFFLINE?                                     
         BNE   SCRN04              YES                                          
         ICM   R2,15,REPAPRT       SEE IF PRINT ROUTINE RESOLVED                
         BZ    EXITOK                                                           
*                                                                               
         XR    R0,R0               PRINT REQUEST DETAILS                        
         ICM   RF,15,REPABOX                                                    
         BZ    *+8                                                              
         LA    R0,C'B'                                                          
         GOTOX VREQTWA,LCPARM,(3,TWAD),(X'FF',ACOM),(R2),((R0),(RF))            
         B     EXITOK                                                           
*                                                                               
SCRN04   LA    R2,TWAPOLY                                                       
         USING FHD,R2                                                           
         MVI   REPACTN,REPAPUT   PUT A LINE OF ***`S AT THE TOP                 
         MVI   REPP1,SURROUND                                                   
         MVC   REPP1+1(SCRWIDTH),REPP1                                          
         GOTOX VREPORT,REPBLK                                                   
         MVC   REPP1,BCSPACES    CLEAR ROW                                      
         XR    RF,RF                                                            
         XR    RE,RE                                                            
         ICM   RF,3,FHAD         ADDRESS OF FIRST FIELD                         
         D     RE,=F'80'                                                        
         LR    R4,RF             THIS HOLDS THE CURRENT ROW NUMBER              
         XR    R3,R3                                                            
*                                                                               
SCRB02   ICM   R3,1,FHLN         LAST FIELD ON SCREEN REACHED YET?              
         BZ    SCRB10            YES                                            
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,3,FHAD         ADDRESS OF FIELD ON SCREEN                     
         D     RE,=F'80'                                                        
         CR    R4,RF             THIS FIELD ON CURRENT ROW?                     
         BE    SCRB06            YES - CONTINUE                                 
         LR    R0,RF             SAVE THIS ROW NUMBER                           
*                                                                               
SCRB04   MVI   REPACTN,REPAPUT   PUT CURRENT ROW TO PRINT                       
         MVI   REPP1,SURROUND    PUT *..* AROUND LINE                           
         MVI   REPP1+SCRWIDTH,SURROUND                                          
         GOTOX VREPORT,REPBLK                                                   
         MVC   REPP1,BCSPACES    CLEAR ROW                                      
         LA    R4,1(R4)          INCREMENT COUNTER                              
         CR    R4,R0             REACHED THE ROW WE REQUIRE NEXT                
         BL    SCRB04            NOT YET                                        
         B     SCRB02                                                           
*                                                                               
SCRB06   LA    RE,REPP1+1(RE)    DISP. INTO FIELD EQUATED TO PRINT LINE         
         LR    RF,R3             R5 HOLDS THE LENGTH REMEMBER                   
         LA    R1,FHDAD+1                                                       
         TM    FHAT,FHATXH       EXTENDED FIELD?                                
         BZ    *+8                                                              
         LA    R1,FHDAD+FHDAD+1                                                 
         SR    RF,R1             LENGTH TO MOVE ONTO PRINT LINE                 
         BM    SCRB08                                                           
         MVC   0(0,RE),FHDA                                                     
         EX    RF,*-6                                                           
*                                                                               
SCRB08   LA    R2,0(R3,R2)       NEXT FIELD ON SCREEN                           
         B     SCRB02                                                           
*                                                                               
SCRB10   MVI   REPACTN,REPAPUT   PUT LAST ROW TO PRINT                          
         MVI   REPP1,SURROUND    PUT *..* AROUND LINE                           
         MVI   REPP1+SCRWIDTH,SURROUND                                          
         GOTOX VREPORT,REPBLK                                                   
         MVI   REPACTN,REPAPUT   PUT A LINE OF ***`S AT THE END                 
         MVI   REPP1,SURROUND                                                   
         MVC   REPP1+1(SCRWIDTH),REPP1                                          
         GOTOX VREPORT,REPBLK                                                   
         MVC   REPP1,BCSPACES    CLEAR ROW                                      
         B     EXITOK                                                           
         DROP  R5,R2                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE DLFLD FOR DOWNLOADING                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
INITDLCB LA    R1,DLCB             DOWNLOAD CONTROL BLOCK IN P3                 
         USING DLCBD,R1                                                         
         L     R5,AREP             BUILD DOWNLOAD CONTROL BLOCK                 
         USING REPD,R5                                                          
         XC    DLCBD(DLCBL),DLCBD                                               
         LA    RF,DLLINE                                                        
         ST    RF,DLCBAPR          USER SUPPLIED PRINT ROUTINE                  
         LA    RF,REPP1                                                         
         ST    RF,DLCBAPL          USER SUPPLIED PRINT LINE                     
         MVI   DLCBACT,DLCBSOR     START OF REPORT                              
         OI    DLCBFLG1,DLCBFXFL+DLCBFXTN  EXTENDED TEXT FIELD USED             
         GOTOX VDLFLD              FIRST FOR REPORT                             
         B     EXITOK                                                           
         DROP  R1,R5                                                            
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* CLOSE DLFLD FOR DOWNLOADING                                         *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
CLSEDLCB LA    R1,DLCB             DOWNLOAD CONTROL BLOCK IN P3                 
         USING DLCBD,R1                                                         
         MVI   DLCBACT,DLCBEOR                                                  
         GOTOX VDLFLD              LAST FOR REPORT                              
         B     EXITOK                                                           
         DROP  R1                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* CLOSE OR PURGE PRINT QUEUE NORMALLY                                 *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
PQCLSE   L     R5,AREP                                                          
         USING REPD,R5             R5=A(REPORT BLOCK)                           
         TM    REPIND1,REPIPUT     TEST ANY LINES PUT                           
         BNZ   DCLS02                                                           
         MVI   REPACTN,REPACLO     NO - CLOSE REPORT                            
         GOTOX VREPORT,REPBLK                                                   
*                                                                               
         CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BE    EXITOK                                                           
         MVI   FVOMTYP,GTMWRN                                                   
         MVC   FVMSGNO,=AL2(WRNMSG1)                                            
         B     EXITL                                                            
*                                                                               
DCLS02   MVI   REPACTN,REPACLO     CLOSE THE REPORT                             
         GOTOX VREPORT,REPBLK                                                   
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                CLOSE ERROR                                  
         CLI   ASONOFF,ASOFF                                                    
         BE    EXITOK                                                           
*                                                                               
         LA    RF,BCWORK           SET UP XXX,99999                             
         MVC   0(3,RF),REPSUBID                                                 
         MVI   3(RF),C','                                                       
         XR    R0,R0                                                            
         ICM   R0,3,REPREPNO                                                    
         CVD   R0,BCDUB                                                         
         OI    BCDUB+7,X'0F'                                                    
         UNPK  BCWORK+4(5),BCDUB                                                
         LA    R1,4                                                             
         LA    RE,BCWORK+4                                                      
         CLI   0(RE),C'0'                                                       
         BNE   *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,RF),0(RE)                                                    
         LA    R1,4(R1)            L'TEXT - 1                                   
*                                                                               
         CLM   R1,1,=AL1(L'FVXTRA-1)                                            
         BNH   *+8                                                              
         LA    R1,L'FVXTRA-1                                                    
         MVC   FVXTRA(0),0(RF)     SET EXTRA INFORMATION XXX,99999              
         EX    R1,*-6                                                           
         MVI   FVOMTYP,GTMINF      SET INFORMATION MESSAGE                      
         MVC   FVMSGNO,=AL2(INFREP3)                                            
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* USER SUPPLIED PRINT LINE ROUTINE                                    *         
***********************************************************************         
         SPACE 1                                                                
DLLINE   NTR1                                                                   
         L     R5,AREP                                                          
         USING REPD,R5                                                          
         MVC   REPPAGE,=AL2(2)     DOWNLOADED DATA ALWAYS ON P2                 
         MVI   REPLINE,1           ??                                           
*                                                                               
         MVI   REPACTN,REPAPUT     OPEN THE REPORT                              
         GOTOX VREPORT,REPBLK                                                   
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WRITE DOWNLOAD REPORT IN FILE SEQUENCE ORDER             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
BLDDWN   GOTOX ROUTS,DSETCOLS                                                   
         MVC   LSINIKEY,GSRECKEY   SET INITIAL DOWNLOAD KEY                     
*                                                                               
         TM    DLINDS,DLBALL       BUILD ALL LIST BEFORE DOWNLOAD?              
         BZ    BXX02               NO                                           
         GOTOX ROUTS,DDLINIT       INITIALISE DOWNLOAD LIST                     
         BL    EXITL               ERROR ON INIT                                
*                                                                               
         ICM   R1,15,BLDLST        BUILD ENTIRE DOWNLOAD LIST                   
         GOTO1 AGENLST                                                          
         BL    EXITL               ERROR ON BUILD                               
*                                                                               
         GOTOX ROUTS,DDISLST       DISPLAY ENTIRE DOWNLOAD LIST                 
         B     EXIT                                                             
*                                                                               
BLDLST   DC    AL1(AGLBLDLS),XL3'FFFFFF'                                        
*                                                                               
BXX02    XC    LSLASKEY,LSLASKEY   CLEAR LAST AND INITIAL KEYS                  
         MVC   LCRECKEY,GSRECKEY                                                
         GOTOX AGENLST,LCPARM,OLIST,LLSTFRST,LCRECKEY                           
         BL    BLDL                                                             
         MVC   LSLASKEY,LCRECKEY                                                
*                                                                               
         GOTOX ROUTS,DGETFRST                                                   
         BNE   BDWN04                                                           
         B     BDWN03                                                           
*                                                                               
BDWN02   GOTOX ROUTS,DGETNEXT                                                   
         BNE   BDWN04                                                           
*                                                                               
BDWN03   GOTOX ROUTS,DDISLINE                                                   
         B     BDWN02                                                           
*                                                                               
BDWN04   GOTOX AGENLST,LCPARM,OLIST,LLSTLAST                                    
         BL    BLDL                                                             
         OI    LSLTIND1,LSLTIEOL   END-OF-LIST FOUND                            
         B     BLDOK                                                            
*                                                                               
BLDL     B     EXITL               ERROR EXIT                                   
*                                                                               
BLDOK    B     EXITOK              GOOD EXIT                                    
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE BUILD ALL TYPE DOWNLOAD FOR LIST OBJECT USE              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
INITDL   MVI   LSSTAT1,LSSINIT+LSSBALL                                          
         XC    LSSTAT2,LSSTAT2                                                  
         XC    LSSTAT3,LSSTAT3                                                  
         XC    LSLTIND1,LSLTIND1                                                
         XC    GCFULL2,GCFULL2                                                  
         GOTOX AOLY,LCPARM,('GCBOVER',ODLOAD),DDLINIT                           
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD LINE OF DOWNLOAD REPORT                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DISLST   MVC   LSLINE#,LSLST#1     RESET CURRENT LINE NUMBER                    
         B     DLST04                                                           
*                                                                               
DLST02   LH    RF,LSLINE#          INCREMENT CURRENT LINE NUMBER                
         LA    RF,1(RF)                                                         
         CH    RF,LSLST#X          SCOPE WITHIN RANGE OF LIST                   
         BH    EXITOK              FINISHED THIS LIST                           
         STH   RF,LSLINE#                                                       
*                                                                               
DLST04   ICM   R1,15,LGETTSAR      GET NEXT TSAR RECORD                         
         GOTO1 AGENLST                                                          
         ICM   R1,15,LGETREC       GET ASSOCIATED FILE RECORD                   
         GOTO1 (RF)                                                             
*                                                                               
         GOTOX ROUTS,DDISLINE      DISPLAY LINE AS DOWNLOAD                     
         B     DLST02                                                           
*                                                                               
LGETTSAR DC    AL1(AGLGTITM),XL3'FFFFFF'                                        
LGETREC  DC    AL1(AGLGREC),XL3'FFFFFF'                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD LINE OF DOWNLOAD REPORT                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
DISLINE  L     RF,LCAREC           A(RECORD)                                    
         TM    LSSTAT1,LSSTSAR     LINE IS ON TSAR RECORD ONLY?                 
         BZ    *+8                                                              
         L     RF,ATLST            A(TSAR RECORD)                               
         GOTOX AGEN,LCPARM,('GCBOVER',ODATA),0,('DFIRST',DDIS),(RF),0           
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,LSVARNUM       NUMBER OF COLUMNS TO DOWNLOAD                
         BNZ   *+6                 RETURNED FROM GEFILDWN IN HERE               
         DC    H'0'                                                             
*                                                                               
         LA    R3,LSVARCLM         COLUMNS IN LINE RETURNED FROM                
         USING DCTABD,R3           GEFILDWN IN HERE                             
         LA    R4,DLCB                                                          
         USING DLCBD,R4                                                         
*                                                                               
DLINE02  GOTOX AGEN,LCPARM,ODATA,DLDIS,DCTFLD#,LCAREC                           
         BL    EXITL                                                            
*                                                                               
         LA    RF,FVIFLD+L'FVIFLD-1                                             
         LA    R1,L'FVIFLD         REPLACE ANY `"` WITH `'` IN DATA             
         CLI   0(RF),C'"'          ELSE DOWNLOAD NOT HAPPY...                   
         BNE   *+8                                                              
         MVI   0(RF),C''''         ALL THAT FOR ONE LOUSY DINK...               
         BCTR  RF,0                                                             
         BCT   R1,*-14                                                          
*---------------------------------------------------------JRD                   
         TM    DCTINDS1,X'08'      NUMERIC DOWNLOAD?                            
         BZ    DLINE10             NO                                           
*                                                                               
         MVI   DLCBFLX,C' '        FILL FIELD WITH SPACES                       
         MVC   DLCBFLX+1(L'DLCBFLX-1),DLCBFLX                                   
*                                                                               
         LA    RE,FVIFLD+L'FVIFLD-1                                             
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RF,FVIFLD                                                        
         SR    RE,RF                                                            
         CH    RE,=Y(L'DLCBFLD-1)                                               
         BNH   *+8                                                              
         LA    RE,L'DLCBFLD-1                                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),FVIFLD                                                
*                                                                               
         MVI   DLCBACT,DLCBPUT     PUTTING TEXT (FOR NOW)                       
         MVI   DLCBTYP,DLCBNUM                                                  
         OI    DLCBFLG1,DLCBFXFL+DLCBFXTN                                       
*                                                                               
         B     DLINE20                                                          
*---------------------------------------------------------JRD                   
DLINE10  DS    0H                                                               
         MVI   DLCBFLX,C' '        FILL FIELD WITH SPACES                       
         MVC   DLCBFLX+1(L'DLCBFLX-1),DLCBFLX                                   
*                                                                               
         MVC   DLCBFLX(L'FVIFLD),FVIFLD                                         
         MVI   DLCBACT,DLCBPUT     PUTTING TEXT (FOR NOW)                       
         MVI   DLCBTYP,DLCBTXT                                                  
         OI    DLCBFLG1,DLCBFXFL+DLCBFXTN                                       
*                                                                               
DLINE20  DS    0H                                                               
         GOTOX VDLFLD,DLCBD                                                     
*                                                                               
         LA    R3,DCTABL(R3)       NEXT COLUMN FOR THIS LINE                    
         BCT   R0,DLINE02          DO FOR ALL COLUMNS                           
*                                                                               
         L     RF,LCAREC                                                        
         TM    LSSTAT1,LSSTSAR                                                  
         BZ    *+8                                                              
         L     RF,ATLST                                                         
         GOTOX AGEN,LCPARM,('GCBOVER',ODATA),0,('DLAST',DDIS),(RF),0            
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF HEADING TEXT LINE                     
         GOTOX VDLFLD,DLCBD                                                     
         B     EXITOK                                                           
         DROP  R3,R4                                                            
         LTORG                                                                  
         SPACE 1                                                                
***********************************************************************         
* GET FIRST/NEXT DIRECTORY RECORD FOR DOWNLOAD & FORMAT TSAR BLOCK    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
GETFRST  LA    R3,LGETFRST                                                      
         L     RF,=A(GET)                                                       
         A     RF,LCRELO                                                        
         BR    RF                                                               
         LTORG                                                                  
*                                                                               
         DS    0H                                                               
         USING *,R7                                                             
GETNEXT  GOTOX AOLY,LCPARM,OLIST,LGETFRST,LCRECKEY,LSLASKEY,LCEQUREC            
         LA    R3,LGETNEXT         RE-ESTABLISH SEQUENCE                        
         L     RF,=A(GET)                                                       
         A     RF,LCRELO                                                        
         BR    RF                                                               
         LTORG                                                                  
*                                                                               
         DS    0H                                                               
         USING *,R7                                                             
GET      LR    R7,RF                                                            
         L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         GOTOX AOLY,LCPARM,OLIST,(R3),LCRECKEY,LSLASKEY,LCEQUREC                
         BNE   EXITL                                                            
*                                                                               
         TM    LSSTAT1,LSSTSAR     TEST TSAR RECORDS ONLY                       
         BZ    GET02                                                            
         GOTOX ROUTS,DTSARDIR                                                   
         GOTOX ROUTS,DTSARFIL                                                   
         B     GETX                                                             
*                                                                               
GET02    TM    LSSTAT1,LSSMAIN     MAINTENANCE TYPE LIST?                       
         BO    GET04               YES                                          
         CLI   CSREC,O#FLTR                                                     
         BE    GET04                                                            
         GOTOX AGEN,LCPARM,OFILT,FDOD,LCRECKEY                                  
         BE    GET04                                                            
         L     RF,=A(GETNEXT)      GOTO GETNEXT                                 
         A     RF,LCRELO                                                        
         LR    R7,RF                                                            
         BR    R7                                                               
*                                                                               
GET04    GOTOX ROUTS,DTSARDIR                                                   
*                                                                               
         TM    LSSTAT1,LSSTSAR     IS IS A FILE RECORD?                         
         BO    GET06               NO                                           
         GOTOX AGEN,LCPARM,OIO,IRECRD,0,TLRDA,LCEQUREC                          
*                                                                               
GET06    TM    LSSTAT1,LSSMAIN     LISTING OTHER THAN RECORDS?                  
         BO    GET08               YES                                          
         GOTOX AGEN,LCPARM,OFILT,FDOR,LCAREC                                    
         BE    GET08                                                            
         L     RF,=A(GETNEXT)      GOTO GETNEXT                                 
         A     RF,LCRELO                                                        
         LR    R7,RF                                                            
         BR    R7                                                               
*                                                                               
GET08    GOTOX ROUTS,DTSARFIL                                                   
         GOTOX ROUTS,DTSARTSA      ALLOW USER FILTERING                         
         BE    GETX                                                             
         L     RF,=A(GETNEXT)      GOTO GETNEXT                                 
         A     RF,LCRELO                                                        
         LR    R7,RF                                                            
         BR    R7                                                               
*                                                                               
GETX     MVC   LSLASKEY,LCRECKEY   SAVE LAST KEY                                
         B     EXITOK                                                           
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET TSAR RECORD FROM DIRECTORY RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
TSARDIR  L     R2,ATLST                                                         
         USING TLSTD,R2                                                         
         MVC   TLRLEN,=AL2(TLMINLNQ)                                            
         XC    TLNUM,TLNUM         INITIALIZE TSAR RECORD                       
         XC    TLREC(256),TLREC                                                 
         TM    LSSTAT1,LSSTSAR                                                  
         BO    TDIR02                                                           
         L     RF,LCAREC                                                        
         SH    RF,=Y(L'IODA+L'IOWORK)                                           
         MVC   TLRDA,0(RF)         SET DISK ADDRESS OF RECORD                   
*                                                                               
TDIR02   GOTOX AOLY,LCPARM,OLIST,LTSARDIR,LCRECKEY                              
         B     EXIT                                                             
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* SET TSAR RECORD FROM FILE RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
TSARFIL  GOTOX AOLY,LCPARM,OLIST,LTSARFIL,AIOREC                                
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* SET TSAR RECORD FROM TSAR RECORD (ALLOW USER TO FILTER HERE)        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
TSARTSA  GOTOX AOLY,LCPARM,OLIST,LTSARTSA,0 ALLOW USER FILTERING                
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SET LIST HEADERS FOR COLUMNS                                        *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
COLUMNS  GOTOX ('SWCHFC',AGROUTS),=AL1(GCSYSGEN)                                
         LH    R0,LSVARNUM                                                      
         LA    R2,LSVARCLM                                                      
         USING DCTABD,R2                                                        
*                                                                               
COL      USING FDRRECD,IOKEY                                                    
COL01    XC    COL.FDRKEY,COL.FDRKEY                                            
         MVI   COL.FDRKMIN,FDRKMINQ  BUILD FIELD RECORD KEY FOR COLUMN          
         MVI   COL.FDRKTYP,FDRKTYPQ                                             
         MVC   COL.FDRKSYS,GCOVSYS                                              
         MVC   COL.FDRKPRG,GCPRGNO                                              
         MVC   COL.FDRKREC,CSREC                                                
         MVC   COL.FDRKNUM,DCTFLD#                                              
         MVC   COL.FDRKCTRY,CUCTRY                                              
         XI    COL.FDRKCTRY,FF                                                  
         MVI   COL.FDRKSUB,FF                                                   
         MVC   COL.FDRKTEST,ASTEST                                              
         GOTOX ('GETFLD',AGROUTS),BOPARM,GFREAD,AIO1                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FVIFLD,BCSPACES     RESET FVIFLD                                 
*                                                                               
         L     R3,AIO1                                                          
         LA    R3,FDRFIRST(R3)                                                  
         USING FDRELD,R3                                                        
         LA    R4,DLCB                                                          
         USING DLCBD,R4                                                         
         XR    RF,RF                                                            
COL02    CLI   FDREL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   FDREL,FDRELQ                                                     
         BE    COL04                                                            
         IC    RF,FDRLN                                                         
         LA    R3,0(RF,R3)                                                      
         B     COL02                                                            
*                                                                               
COL04    OC    FDRLHED1,FDRLHED1   FIRST HEADLINE?                              
         BZ    TSAF106                                                          
*                                                                               
         MVC   FVIFLD(L'FDRLHED1),FDRLHED1                                      
         MVI   FVIFLD,DD#ESCL      SET HEADLINE 1 LEFT ALIGNED                  
*                                                                               
         LA    RF,L'FVIFLD         LENGTH OF DISPLAY FIELD                      
         CLI   FDRLHED2,DD#ESUL2   SECOND HEADLINE IS UNDERLINE?                
         BNL   *+8                 NO                                           
         SRL   RF,1                LENGTH OF DISPLAY FIELD/2                    
         CLM   RF,1,FVIFLD+3       SHORTER THAN SPACE?                          
         BNL   *+8                                                              
         STCM  RF,1,FVIFLD+3       MAKE IT FIT                                  
*                                                                               
         ICM   RF,15,=C'SL  '      RESOLVE FIRST HEADLINE                       
         ICM   RF,2,COL.FDRKSYS                                                 
         GOTOX VDICTAT,BOPARM,(RF),FVIFLD,0,0                                   
*                                                                               
TSAF106  CLI   FDRLHED2,DD#ESUL2   SECOND HEADLINE IS UNDERLINE?                
         BNL   TSAF108             YES                                          
         OC    FDRLHED2,FDRLHED2   SECOND HEADLINE?                             
         BZ    TSAF108             NO                                           
*                                                                               
         LA    RF,FVIFLD+L'FVIFLD-1                                             
         CLI   0(RF),C' '          PUT HEADING 2 AFTER HEADING 1                
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'/'          PUT SLASH BETWEEN THEM                       
         LA    RF,2(RF)                                                         
         MVC   0(L'FDRLHED2,RF),FDRLHED2                                        
         MVI   0(RF),DD#ESCL       SET LEFT ALIGN                               
         LA    R5,FVIFLD+L'FVIFLD-1                                             
         SR    R5,RF                                                            
         CLM   R5,1,3(RF)          WHAT'S WANTED LESS THAN WHAT'S LEFT?         
         BNL   *+8                 YES                                          
         STC   R5,3(RF)                                                         
         ICM   R5,15,=C'SL  '      RESOLVE SECOND HEADLINE                      
         ICM   R5,2,COL.FDRKSYS                                                 
         GOTOX VDICTAT,BOPARM,(R5),(RF)                                         
*                                                                               
TSAF108  LA    RF,FVIFLD+L'FVIFLD-1                                             
         LA    R1,L'FVIFLD         REPLACE ANY `"` WITH `'` IN DATA             
         CLI   0(RF),C'"'          ELSE DOWNLOAD NOT HAPPY...                   
         BNE   *+8                                                              
         MVI   0(RF),C''''                                                      
         BCTR  RF,0                                                             
         BCT   R1,*-14                                                          
*                                                                               
         GOTOX AOLY,LCPARM,ODLOAD,DSETCOLS,FVIFLD,FDRELD                        
*                                                                               
         MVI   DLCBFLX,C' '        FILL FIELD WITH SPACES                       
         MVC   DLCBFLX+1(L'DLCBFLX-1),DLCBFLX                                   
*                                                                               
         MVC   DLCBFLX(L'FVIFLD),FVIFLD                                         
         MVI   DLCBACT,DLCBPUT     PUTTING TEXT (FOR NOW)                       
         MVI   DLCBTYP,DLCBTXT                                                  
         OI    DLCBFLG1,DLCBFXFL+DLCBFXTN                                       
         GOTOX VDLFLD,DLCBD                                                     
*                                                                               
         LA    R2,DCTABL(R2)       NEXT COLUMN FOR THIS LINE                    
         BCT   R0,COL01            DO FOR ALL COLUMNS                           
*                                                                               
         MVI   DLCBACT,DLCBEOL     END OF HEADING TEXT LINE                     
         GOTOX VDLFLD,DLCBD                                                     
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     EXITOK                                                           
         DROP  R2,R3,R4,COL                                                     
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD OFFLINE REQUEST USING SPOOK                                   *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         PUSH  USING                                                            
         USING *,R7                                                             
DSPK     LR    R7,RF                                                            
         L     R5,AREP                                                          
         USING REPD,R5                                                          
         USING SPOOK,BOELEM                                                     
         XC    SPOOK(SPOOKXL),SPOOK                                             
         MVC   SPOOKUID,CUUSER     SET CONNECTED USER                           
         MVC   SPOOKDES,INDEST     SET DESTINATION                              
         MVC   SPOOKTID,CUTERM     SET TERMINAL                                 
         MVC   SPOOKAGY,CUAALF     SET COMPANY ALPHA                            
         MVC   SPOOKAGX,CUABIN     SET COMPANY BINARY                           
         MVC   SPOOKDID,INUSER     SET USER ID                                  
         MVC   SPOOKSYS,INSYSID    SET SYSTEM ID                                
         MVC   SPOOKEOD,INPRGID    SET PROGRAM ID                               
         MVC   SPOOKJCL,INJCLID    SET JCL BOOK ID                              
         MVC   SPOOKPR1,INPRTY1    SET PRIORITY 1                               
         MVC   SPOOKPR2,INPRTY2    SET PRIORITY 2                               
         MVC   SPOOKWEN,INWHEN     SET WHEN REPORT TO RUN                       
*                                                                               
         MVC   SPOOKXT,=C'XT='     SPOOK EXTENSION                              
*                                                                               
         CLC   INOTYP,=CL6'DOWN'   TEST FOR DOWNLOAD TYPE                       
*??      BNE   *+8                                                              
         OI    SPOOKTY,X'10'                                                    
*                                                                               
         CLI   INOTYP,C'@'                                                      
         BNE   DSPK02                                                           
         MVC   SPOOKSQL,INOTYP+1                                                
         OI    SPOOKTY,X'08'                                                    
         B     DSPK04                                                           
*                                                                               
DSPK02   CLI   INOTYP,C'/'                                                      
         BNE   DSPK04                                                           
         CLC   INOTYP+3(3),=CL3'   '                                            
         BNE   *+14                                                             
         MVC   SPOOKSUB,INOTYP+1   /XX INPUT                                    
         B     DSPK04                                                           
         MVC   SPOOKSUB,INOTYP+4   /SPPXX INPUT                                 
         B     DSPK04                                                           
*                                                                               
DSPK04   L     R3,AIO1             A(IO AREA)                                   
         USING REQHDR,R3                                                        
         XC    REQHDR(REQEOH-REQHDR),REQHDR                                     
         MVC   REQOUT,INOTYP                                                    
         MVC   REQDEST,INDEST                                                   
         MVI   REQUEST,C' '                                                     
         MVC   REQUEST+1(L'REQUEST-1),REQUEST                                   
         MVC   REQJCLID,INJCLID                                                 
         MVC   REQAGYID,CUAALF                                                  
         ICM   R0,15,ASSIN                                                      
         CVD   R0,GCDUB1                                                        
         OI    GCDUB1+L'GCDUB1-1,X'0F'                                          
         UNPK  REQSIN,GCDUB1                                                    
*                                                                               
         GOTOX VREQTWA,LCPARM,TWAD,REQHDR,VDMGR,ACOM,SPOOK                      
*                                                                               
         LH    RF,GSDSPACT         SET CURSOR TO ACTION FIELD                   
         A     RF,ATWA                                                          
         STCM  RF,15,FVADDR                                                     
*                                                                               
         CLI   INWHEN,INWNOVNT     OVERNIGHT ?                                  
         BNE   DSPK06                                                           
         MVC   FVMSGNO,=AL2(INFREP1)                                            
         MVI   FVOMTYP,GTMINF                                                   
         B     EXITOK                                                           
*                                                                               
DSPK06   CLI   INWHEN,INWNSOON     SOON ?                                       
         BE    *+14                                                             
         CLI   INWHEN,INWNUSN      UPDATIVE SOON ?                              
         BE    *+6                                                              
         DC    H'0'                WHAT ARE YOU DOING HERE THEN???              
*                                                                               
         CLI   8(R1),X'FE'         TERMINAL QUEUE FULL ?                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFTFUL)                                            
         B     EXITL                                                            
*                                                                               
         CLI   8(R1),X'FF'         PRINT QUEUE FULL ?                           
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFQFUL)                                            
         B     EXITL                                                            
*                                                                               
         L     RE,8(R1)            JCL ERROR - NO KEY RETURNED ?                
         OC    0(7,RE),0(RE)                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFEJCL)                                            
         B     EXITL                                                            
*                                                                               
         LA    RF,BCWORK           SET UP XXX,99999                             
         MVC   0(3,RF),REPSUBID                                                 
         MVI   3(RF),C','                                                       
         XR    R0,R0                                                            
         ICM   R0,3,6(RE)                                                       
         CVD   R0,BCDUB                                                         
         OI    BCDUB+7,X'0F'                                                    
         UNPK  BCWORK+4(5),BCDUB                                                
         LA    R1,4                                                             
         LA    RE,BCWORK+4                                                      
         CLI   0(RE),C'0'                                                       
         BNE   *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,RF),0(RE)                                                    
         LA    R1,4(R1)            L'TEXT - 1                                   
*                                                                               
         CLM   R1,1,=AL1(L'FVXTRA-1)                                            
         BNH   *+8                                                              
         LA    R1,L'FVXTRA-1                                                    
         MVC   FVXTRA(0),0(RF)     SET EXTRA INFORMATION XXX,99999              
         EX    R1,*-6                                                           
         MVC   FVMSGNO,=AL2(INFREP2)                                            
         MVI   FVOMTYP,GTMINF      SET INFORMATION MESSAGE                      
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         POP   USING                                                            
         TITLE 'GLOBAL LEVEL REPORT OBJECT'                                     
         EJECT                                                                  
***********************************************************************         
* REPORT OBJECT                                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
REPORT   LA    R1,RPOTAB                                                        
         USING OBJTABD,R1                                                       
*                                                                               
RPO02    CLI   OBJVERB,EOT                                                      
         BE    RPO08                                                            
         CLC   OBJVERB,LCVERB                                                   
         BNE   *+14                MATCHED                                      
         CLC   OBJSVB,LCSVB                                                     
         BE    *+12                                                             
         LA    R1,OBJTABL(R1)                                                   
         B     RPO02               BUMP & LOOP                                  
*                                                                               
RPO04    TM    OBJIND1,OBJPRIV     PRIVATE?                                     
         BZ    RPO06                                                            
         L     RE,4(RD)            MAKE SURE INVOKED AT THIS LEVEL              
         L     RF,4(RE)                                                         
         CLC   16(4,RE),16(RF)                                                  
         BE    RPO06                                                            
         DC    H'0'                                                             
*                                                                               
RPO06    ICM   RF,15,OBJADR                                                     
         A     RF,LCRELO                                                        
         BR    RF                                                               
         DROP  R1                                                               
*                                                                               
RPO08    DC    H'0'                NOT KNOWN AT THIS LEVEL                      
*                                                                               
RPOTAB   DS    0F                                                               
         DC    AL1(RDO,0,0,0),AL4(RDOIT)      BUILD THE REPORT                  
         DC    AL1(RPQINIT,0,0,0),AL4(RINITPQ) INITIALISE REPORT BLOCK          
         DC    AL1(RPSVAL,0,0,0),AL4(RSCRVAL) VALIDATE SCREEN                   
         DC    AL1(RSPOOK,0,0,0),AL4(RSPK)    BUILD OFFLINE REQUEST             
         DC    AL1(EOT)                                                         
*                                                                               
RROUTS   NTR1                       INTERNAL REPORT ROUTINES                    
         LA    RF,REPROUTS                                                      
RROUTS02 CLI   0(RF),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLM   R1,1,0(RF)                                                       
         BE    *+12                                                             
         LA    RF,L'REPROUTS(RF)                                                
         B     RROUTS02                                                         
*                                                                               
         ICM   R7,15,1(RF)                                                      
         A     R7,LCRELO                                                        
         BR    R7                                                               
*                                                                               
REPROUTS DS    0XL5                                                             
         DC    AL1(RPQOPEN),AL4(ROPENPQ)                                        
         DC    AL1(RBLDREP),AL4(BLDREP)                                         
         DC    AL1(RPQCLSE),AL4(RCLSEPQ)                                        
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD NORMAL REPORT                                                 *         
***********************************************************************         
         SPACE 1                                                                
RDOIT    GOTOX RROUTS,RPQOPEN      OPEN THE PRINT QUEUE                         
         GOTOX RROUTS,RBLDREP      DO THE REPORT                                
         BL    EXITL               ERROR BUILDING REPORT                        
         GOTOX RROUTS,RPQCLSE      CLOSE THE PRINT QUEUE                        
         B     EXITOK              DONE.                                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE PRINT QUEUE FOR REPORTING                                *         
*                                                                     *         
* IF PRINTING IS TO BE SOON OR OVERNIGHT, THE APPLICATION MUST SET    *         
* UP THE FOLLOWING FIELDS WITH THE CORRECT VALUES:                    *         
*                                                                     *         
* INSYSID, INPRGID, INJCLID, INPRTY1, INPRTY2                         *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
RINITPQ  LR    R7,RF                                                            
         L     R5,AREP                                                          
         USING REPD,R5                                                          
*                                                                               
         MVC   INDEST,CUUSER       SET DEFAULT DESTINATION                      
         MVI   INWIDTH,REPWREGQ    SET REGULAR REPORT WIDTH AS DEFAULT          
*                                                                               
         MVI   REPACTN,REPAINI     INITIALIZE REPBLK                            
         LA    R0,REPHS            SET A(REPORT BUFFER)                         
         ST    R0,REPABUF                                                       
         MVC   REPAPQB,ATIA        SET A(TIA)                                   
         MVC   REPACOM,ACOM        SET A(COMFACS)                               
         MVC   REPDATE,ASBDAT      SET TODAY'S DATE                             
         MVC   REPCTRY,CUCTRY      SET CONNECTED COUNTRY                        
         MVC   REPLANG,CULANG      SET CONNECTED LANGUAGE                       
         MVC   REPSYSID,INSYSID    SET SYSTEM ID                                
         MVC   REPPRGID,INPRGID    SET PROGRAM ID                               
*                                                                               
         CLI   ASONOFF,ASOFF       OFFLINE - SET UP MASTC/BOX                   
         BNE   RIN02                                                            
         MVC   REPAMST,TWMASTC                                                  
         MVC   REPABOX,TWBOX                                                    
*                                                                               
RIN02    MVC   REPWIDTH,INWIDTH    SET REGULAR REPORT WIDTH AS DEFAULT          
         MVI   REPHEADN,REPHN      REGULAR HEADLINES                            
         MVI   REPMIDSN,REPMN      REGULAR MIDLINES                             
         MVI   REPPRNTN,REPPN      REGULAR PRINTLINES                           
         MVI   REPFOOTN,REPFN      REGULAR FOOTLINES                            
*                                                                               
         GOTOX AOLY,LCPARM,OREP,RPQINIT  ALLOW USER TO SET OVERRIDES            
*                                                                               
         MVC   REPWIDTH,INWIDTH    SEE IF WIDTH OVERRIDDEN                      
         CLI   REPWIDTH,REPWIDEQ   TEST WIDE LINE OVERRIDE                      
         BNE   *+20                                                             
         MVI   REPHEADN,REPHWN     WIDE HEADLINES                               
         MVI   REPMIDSN,REPMWN     WIDE MIDLINES                                
         MVI   REPPRNTN,REPPWN     WIDE PRINTLINES                              
         MVI   REPFOOTN,REPFWN     WIDE FOOTLINES                               
*                                                                               
         CLI   REPWIDTH,REPWNARQ   TEST NARROW LINE OVERRIDE                    
         BNE   *+20                                                             
         MVI   REPHEADN,REPHNN     NARROW HEADLINES                             
         MVI   REPMIDSN,REPMNN     NARROW MIDLINES                              
         MVI   REPPRNTN,REPPNN     NARROW PRINTLINES                            
         MVI   REPFOOTN,REPFNN     NARROW FOOTLINES                             
*                                                                               
         B     EXITOK                                                           
         DROP  R5                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* OPEN PRINT QUEUE FOR REPORTING                                      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
ROPENPQ  L     R5,AREP                                                          
         USING REPD,R5                                                          
         GOTOX VREPORT,REPBLK      SHOULD INITIALISE REPORT                     
*                                                                               
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         GOTOX VREPORT,REPBLK                                                   
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   ASONOFF,ASOFF                                                    
         BNE   ROPN06                                                           
         OC    REPAPRT,REPAPRT                                                  
         BZ    ROPN06                                                           
*                                                                               
         L     R1,TWMASTC          LOCATE REMOTEC                               
         L     R1,MCVREMOT-MASTD(R1)                                            
         USING REMOTED,R1                                                       
         OC    REMOTKEY,REMOTKEY   TEST REMOTE PRINTING                         
         BZ    ROPN06                                                           
         CLI   INOTYP,C'@'         TEST REFORM ID IN DESCRIPTION                
         BE    ROPN04                                                           
         CLI   REMOTFLG,X'FA'      TEST NEW STYLE LAYOUT                        
         BL    ROPN02               NO                                          
         OC    REPDESC,REPDESC     OVERIDE REPORT SHORT DESCRIPTION ?           
         BZ    *+10                                                             
         MVC   REMOTDSC,REPDESC                                                 
         OC    REPMAXL,REPMAXL     OVERIDE MAX LINES PER PAGE?                  
         BZ    *+10                                                             
         MVC   REMOTLPP,REPMAXL                                                 
         OC    REPFORM,REPFORM     OVERIDE FORMS CODE?                          
         BZ    *+10                                                             
         MVC   REMOTFNO,REPFORM                                                 
         OC    REPCOPY,REPCOPY     OVERIDE NUMBER OF COPIES?                    
         BZ    *+10                                                             
         MVC   REMOTCPY,REPCOPY                                                 
         B     *+10                                                             
*                                                                               
ROPN02   MVC    REMOTKEY(L'REPDESC),REPDESC                                     
*                                                                               
ROPN04   MVC    REMOTPAS,REPPSWD    OVERIDE REPORT PQ PASSWORD                  
         DROP   R1                                                              
*                                                                               
ROPN06   B     EXITOK                                                           
         DROP  R5                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* BUILD OFFLINE REQUEST USING SPOOK                                   *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         PUSH  USING                                                            
         USING *,R7                                                             
RSPK     LR    R7,RF                                                            
         L     R5,AREP                                                          
         USING REPD,R5                                                          
         USING SPOOK,BOELEM                                                     
         XC    SPOOK(SPOOKXL),SPOOK                                             
         MVC   SPOOKUID,CUUSER     SET CONNECTED USER                           
         MVC   SPOOKDES,INDEST     SET DESTINATION                              
         MVC   SPOOKTID,CUTERM     SET TERMINAL                                 
         MVC   SPOOKAGY,CUAALF     SET COMPANY ALPHA                            
         MVC   SPOOKAGX,CUABIN     SET COMPANY BINARY                           
         MVC   SPOOKDID,INUSER     SET USER ID                                  
         MVC   SPOOKSYS,INSYSID    SET SYSTEM ID                                
         MVC   SPOOKEOD,INPRGID    SET PROGRAM ID                               
         MVC   SPOOKJCL,INJCLID    SET JCL BOOK ID                              
         MVC   SPOOKPR1,INPRTY1    SET PRIORITY 1                               
         MVC   SPOOKPR2,INPRTY2    SET PRIORITY 2                               
         MVC   SPOOKWEN,INWHEN     SET WHEN REPORT TO RUN                       
*                                                                               
         MVC   SPOOKXT,=C'XT='     SPOOK EXTENSION                              
*                                                                               
         CLC   INOTYP,=CL6'DOWN'   TEST FOR DOWNLOAD TYPE                       
         BNE   *+8                                                              
         OI    SPOOKTY,X'10'                                                    
*                                                                               
         CLI   INOTYP,C'@'                                                      
         BNE   RSPK02                                                           
         MVC   SPOOKSQL,INOTYP+1                                                
         OI    SPOOKTY,X'08'                                                    
         B     RSPK04                                                           
*                                                                               
RSPK02   CLI   INOTYP,C'/'                                                      
         BNE   RSPK04                                                           
         CLC   INOTYP+3(3),=CL3'   '                                            
         BNE   *+14                                                             
         MVC   SPOOKSUB,INOTYP+1   /XX INPUT                                    
         B     RSPK04                                                           
         MVC   SPOOKSUB,INOTYP+4   /SPPXX INPUT                                 
         B     RSPK04                                                           
*                                                                               
RSPK04   L     R3,AIO1             A(IO AREA)                                   
         USING REQHDR,R3                                                        
         XC    REQHDR(REQEOH-REQHDR),REQHDR                                     
         MVC   REQOUT,INOTYP                                                    
         MVC   REQDEST,INDEST                                                   
         MVI   REQUEST,C' '                                                     
         MVC   REQUEST+1(L'REQUEST-1),REQUEST                                   
         MVC   REQJCLID,INJCLID                                                 
         MVC   REQAGYID,CUAALF                                                  
         ICM   R0,15,ASSIN                                                      
         CVD   R0,GCDUB1                                                        
         OI    GCDUB1+L'GCDUB1-1,X'0F'                                          
         UNPK  REQSIN,GCDUB1                                                    
*                                                                               
         GOTOX VREQTWA,LCPARM,TWAD,REQHDR,VDMGR,ACOM,SPOOK                      
*                                                                               
         LH    RF,GSDSPACT         SET CURSOR TO ACTION FIELD                   
         A     RF,ATWA                                                          
         STCM  RF,15,FVADDR                                                     
*                                                                               
         CLI   INWHEN,INWNOVNT     OVERNIGHT ?                                  
         BNE   RSPK06                                                           
         MVC   FVMSGNO,=AL2(INFREP1)                                            
         MVI   FVOMTYP,GTMINF                                                   
         B     EXITOK                                                           
*                                                                               
RSPK06   CLI   INWHEN,INWNSOON     SOON ?                                       
         BE    *+14                                                             
         CLI   INWHEN,INWNUSN      UPDATIVE SOON ?                              
         BE    *+6                                                              
         DC    H'0'                WHAT ARE YOU DOING HERE THEN???              
*                                                                               
         CLI   8(R1),X'FE'         TERMINAL QUEUE FULL ?                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFTFUL)                                            
         B     EXITL                                                            
*                                                                               
         CLI   8(R1),X'FF'         PRINT QUEUE FULL ?                           
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFQFUL)                                            
         B     EXITL                                                            
*                                                                               
         L     RE,8(R1)            JCL ERROR - NO KEY RETURNED ?                
         OC    0(7,RE),0(RE)                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFEJCL)                                            
         B     EXITL                                                            
*                                                                               
         LA    RF,BCWORK           SET UP XXX,99999                             
         MVC   0(3,RF),REPSUBID                                                 
         MVI   3(RF),C','                                                       
         XR    R0,R0                                                            
         ICM   R0,3,6(RE)                                                       
         CVD   R0,BCDUB                                                         
         OI    BCDUB+7,X'0F'                                                    
         UNPK  BCWORK+4(5),BCDUB                                                
         LA    R1,4                                                             
         LA    RE,BCWORK+4                                                      
         CLI   0(RE),C'0'                                                       
         BNE   *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,RF),0(RE)                                                    
         LA    R1,4(R1)            L'TEXT - 1                                   
*                                                                               
         CLM   R1,1,=AL1(L'FVXTRA-1)                                            
         BNH   *+8                                                              
         LA    R1,L'FVXTRA-1                                                    
         MVC   FVXTRA(0),0(RF)     SET EXTRA INFORMATION XXX,99999              
         EX    R1,*-6                                                           
         MVC   FVMSGNO,=AL2(INFREP2)                                            
         MVI   FVOMTYP,GTMINF      SET INFORMATION MESSAGE                      
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* CLOSE OR PURGE PRINT QUEUE NORMALLY                                 *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
RCLSEPQ  L     R5,AREP                                                          
         USING REPD,R5             R5=A(REPORT BLOCK)                           
         TM    REPIND1,REPIPUT     TEST ANY LINES PUT                           
         BNZ   RCLS02                                                           
         MVI   REPACTN,REPACLO     NO - CLOSE REPORT                            
         GOTOX VREPORT,REPBLK                                                   
*                                                                               
         CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BE    EXITOK                                                           
         MVI   FVOMTYP,GTMWRN                                                   
         MVC   FVMSGNO,=AL2(WRNMSG1)                                            
         B     EXITL                                                            
*                                                                               
RCLS02   MVI   REPACTN,REPACLO     CLOSE THE REPORT                             
         GOTOX VREPORT,REPBLK                                                   
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                CLOSE ERROR                                  
         CLI   ASONOFF,ASOFF                                                    
         BE    EXITOK                                                           
*                                                                               
         LA    RF,BCWORK           SET UP XXX,99999                             
         MVC   0(3,RF),REPSUBID                                                 
         MVI   3(RF),C','                                                       
         XR    R0,R0                                                            
         ICM   R0,3,REPREPNO                                                    
         CVD   R0,BCDUB                                                         
         OI    BCDUB+7,X'0F'                                                    
         UNPK  BCWORK+4(5),BCDUB                                                
         LA    R1,4                                                             
         LA    RE,BCWORK+4                                                      
         CLI   0(RE),C'0'                                                       
         BNE   *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,RF),0(RE)                                                    
         LA    R1,4(R1)            L'TEXT - 1                                   
*                                                                               
         CLM   R1,1,=AL1(L'FVXTRA-1)                                            
         BNH   *+8                                                              
         LA    R1,L'FVXTRA-1                                                    
         MVC   FVXTRA(0),0(RF)     SET EXTRA INFORMATION XXX,99999              
         EX    R1,*-6                                                           
         MVC   FVMSGNO,=AL2(INFREP3)                                            
         MVI   FVOMTYP,GTMINF      SET INFORMATION MESSAGE                      
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SCREEN INFORMATION FOR REPORT                              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
RSCRVAL  LR    R7,RF                                                            
         XC    GSRECKEY,GSRECKEY   CLEAR CURRENT VALUES                         
         XC    GSRECSTA,GSRECSTA                                                
         XC    GSRECDA,GSRECDA                                                  
*                                                                               
         GOTOX AOLY,LCPARM,OREP,RPFIRST,GSRECKEY,RPSVAL                         
         BL    EXITL               ERROR                                        
*                                                                               
         LH    R3,GSDSPOVR                                                      
         A     R3,ATWA             START OF KEY FIELDS                          
         USING FHD,R3                                                           
         XR    R4,R4                                                            
RVAL02   ICM   R4,1,FHLN                                                        
         BZ    RVAL10              END OF SCREEN                                
         TM    FHAT,FHATXH                                                      
         BO    *+12                                                             
         ST    R3,RTTAG                                                         
         B     RVAL08              IGNORE IF NO EXTENDED HEADER                 
*                                                                               
         LA    RF,FHD(R4)                                                       
         SH    RF,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RF)       EXTENDED HEADER HERE                         
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,FVIXUS         FIELD NUMBER (INTERNAL REPN.)                
         BZ    RVAL08              IGNORE IF 0                                  
         BCTR  RF,0                MAKE ZERO-BASED                              
         SLL   RF,2                                                             
         A     RF,AFDRADDR         START OF FORMATTED RECORD INFO.              
         L     RF,0(RF)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                FIELD NOT IN LIST OF FIELDS                  
         USING FDRELD,RF                                                        
*                                                                               
         TM    FDRINDS1,FDR1XTAG                                                
         BO    RVAL04                                                           
         DROP  RF                                                               
         GOTOX AGEN,LCPARM,ODATA,DMHED,(RF),RTTAG                               
*                                                                               
RVAL04   TM    FHAT,FHATPR                                                      
         BZ    RVAL06              FIELD IS PROTECTED                           
*                                                                               
         GOTOX AGEN,LCPARM,ODATA,DRDIS,FHD,0                                    
         BL    EXIT                                                             
         B     RVAL08              DISPLAY THIS FIELD                           
*                                                                               
RVAL06   GOTOX AGEN,LCPARM,ODATA,DRVAL,FHD,0                                    
         BL    EXIT                                                             
*                                                                               
RVAL08   LA    R3,0(R3,R4)                                                      
         B     RVAL02                                                           
         DROP  R3                                                               
*                                                                               
RVAL10   GOTOX AOLY,LCPARM,OREP,RPLAST,GSRECKEY,RPSVAL                          
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO WRITE REPORT                                             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         USING *,R7                                                             
BLDREP   L     R5,AREP                                                          
         USING REPD,R5                                                          
         OC    REPAPRT,REPAPRT      SEE IF PRINT ROUTINE RESOLVED               
         BZ    BREP02                                                           
         XR    R0,R0                PRINT REQUEST DETAILS                       
         ICM   RF,15,REPABOX                                                    
         BZ    *+8                                                              
         LA    R0,C'B'                                                          
         GOTOX VREQTWA,LCPARM,(3,TWAD),,REPAPRT,((R0),(RF))                     
*                                                                               
BREP02   MVI   REPACTN,REPAPUT                                                  
         GOTOX AOLY,LCPARM,OREP,RDO                                             
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
SCRWIDTH EQU   80                                                               
SURROUND EQU   C'*'                                                             
WRNMSG1  EQU   X'FF00'+0002        WARNING NO LINES GENERATED                   
INFREP1  EQU   X'FF00'+0020        REPORT WILL BE PROCESSED OVERNIGHT           
INFREP2  EQU   X'FF00'+0021        REPORT &T WILL BE PROCESSED SOON             
INFREP3  EQU   X'FF00'+0027        REPORT &T HAS BEEN SPOOLED                   
INFREP4  EQU   X'FF00'+0028        ENTER DATA                                   
INFREP5  EQU   X'FF00'+0031        REPORT DISPLAYED                             
         SPACE 1                                                                
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
LCWORKD  DSECT                                                                  
LCRELO   DS    A                                                                
LCAR1    DS    A                   A(INCOMING PARAMETER LIST)                   
LCPARMS  DS    0XL24               SAVED PARAMETERS                             
LCPARMS1 DS    A                                                                
         ORG   *-1                                                              
LCOBJECT DS    XL1                 OBJECT                                       
LCPARMS2 DS    A                                                                
         ORG   LCPARMS2                                                         
LCSVB    DS    XL1                 SUB-VERB                                     
         DS    XL2                                                              
LCVERB   DS    XL1                 VERB                                         
LCPARMS3 DS    A                                                                
LCPARMS4 DS    A                                                                
LCPARMS5 DS    A                                                                
LCPARMS6 DS    A                                                                
*                                                                               
LCPARM   DS    XL24                                                             
LCAREC   DS    A                   A(IO AREA FOR CURRENT LIST RECORD)           
LCEQUREC DS    A                   EQUATE FOR IO AREA                           
VDLFLD   DS    A                   V(DLFLD) FOR DOWNLOADING                     
RTTAG    DS    A                   A(TAG FIELD PRIOR TO INPUT FIELD)            
*                                                                               
LCRECKEY DS    XL(L'GSRECKEY)                                                   
*                                                                               
DLCB     DS    XL(DLCBXLX)                                                      
LCWORKL  EQU   *-LCWORKD                                                        
         EJECT                                                                  
* GEFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE GEFILWORK                                                      
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDDEQUS                                                       
         PRINT ON                                                               
* DDSPOOK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOK                                                        
         PRINT ON                                                               
* DDDLCB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
* DDCOMFACSD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACSD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
         PRINT OFF                                                              
REQHDR   DSECT                                                                  
       ++INCLUDE DMREQHDR                                                       
REQUEST  DS    0CL80               REQUEST CARD LAYOUT                          
REQJCLID DS    CL2                 JCL ID                                       
REQAGYID DS    CL2                 AGENCY ID                                    
         DS    CL1                 N/D                                          
REQSIN   DS    CL6                 SYSTEM INPUT NUMBER                          
         ORG   REQUEST+L'REQUEST                                                
         PRINT ON                                                               
                                                                                
GEFIL05  CSECT                                                                  
         ORG   GEFIL05+(((*-GEFIL05)/2048)+1)*2048                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038GEFIL05   08/10/11'                                      
         END                                                                    
