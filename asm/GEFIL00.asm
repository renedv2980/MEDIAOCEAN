*          DATA SET GEFIL00    AT LEVEL 038 AS OF 03/07/12                      
*          DATA SET GEFIL00    AT LEVEL 034 AS OF 04/19/06                      
*PHASE T00AB0A                                                                  
GEFIL00  TITLE 'NEW FILE PROGRAM ROUTINES'                                      
GEFIL00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL RTWORKL,GEFIL00*,R5,R6,R7,CLEAR=YES,RR=RE                        
         USING RTWORKD,RC                                                       
         ST    RE,RTRELO                                                        
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         ST    R1,RTPARMA          SAVE A(CALLERS R1)                           
         SRL   RF,32-8                                                          
         CLM   RF,1,=AL1(ROUTSN)                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BNM   *+6                                                              
         DC    H'0'                SHOULDN`T BE HERE                            
         SLL   RF,2                                                             
         L     RF,ROUTS(RF)                                                     
         A     RF,RTRELO                                                        
         BR    RF                                                               
*                                                                               
ROUTS    DS    0A                                                               
         DC    A(SWCH)             SWITCH SYSTEMS                               
         DC    A(STMSG)            SET MESSAGE                                  
         DC    A(OVRLY)            OVERLAY A PHASE                              
         DC    A(SVVAL)            SAVE SESSION VALUES                          
         DC    A(RSVAL)            RESTORE SESSION VALUES                       
         DC    A(SVFDR)            SAVE FDREL BLOCK                             
         DC    A(TSARINF)          TSAR INTERFACE                               
         DC    A(BDDIR)            BUILD DIRECTORY FIELDS FROM FILE             
         DC    A(GCEXIT)           RETURN TO EXIT IN ROUT                       
         DC    A(PUTRC)            PUT RECORD ACTIVITY                          
         DC    A(GETRC)            GET RECORD ACTIVITY                          
         DC    A(XIOR)             NEW I/O ROUTINE                              
         DC    A(GTPID)            GET PERSONAL ID                              
         DC    A(FVL)              VALIDATE FIELD                               
         DC    A(TSTAC)            TEST AUTHORISATION                           
         DC    A(GUID)             GET USER ID                                  
         DC    A(VLUID)            VALIDATE USER ID                             
         DC    A(VPID)             VALIDATE PERSONAL ID                         
         DC    A(FSEC)             FIELD SECURITY                               
         DC    A(GTRM)             GET TERMINAL #                               
         DC    A(DDIC)             DISPLAY DICTIONARY ENTRY                     
         DC    A(VDIC)             VALIDATE DICTIONARY ENTRY                    
         DC    A(VWHEN)            VALIDATE WHEN TO RUN REPORT                  
         DC    A(VDEST)            VALIDATE REPORT DESTINATION                  
         DC    A(FLDGET)           GET FIELD RECORD                             
         DC    A(PRTSCR)           PROTECT NESTED SCREEN                        
         DC    A(UPRTSCR)          UNPROTECT NESTED SCREEN                      
         DC    A(VOUT)             VALIDATE REPORT OUTPUT TYPE                  
         DC    A(GTREC)            GET CORRECT RECORD RECORD                    
         DC    A(GTACT)            GET CORRECT ACTION RECORD                    
         DC    A(XIOCN)            NEW I/O ROUTINE FOR CONTROLLER               
ROUTSN   EQU   (*-ROUTS)/4                                                      
*                                                                               
EXITL    CLI   *,255               SET CC LOW                                   
         J     EXIT                                                             
EXITH    CLI   *,0                 SET CC HIGH                                  
         J     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
EXITND   MVC   FVMSGNO,=AL2(GE$FNDAL)                                           
         B     EXITL                                                            
*                                                                               
GSFRR    USING FRRELD,GSFRREL                                                   
PSFRR    USING FRRELD,PSFRREL                                                   
GSFRA    USING FRAELD,GSFRAEL                                                   
PSFRA    USING FRAELD,PSFRAEL                                                   
GSFRP    USING FRPELD,GSFRPEL                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SWITCH TO REQUESTED SYSTEM                               *         
*                                                                     *         
* R1 = A(SYSTEM TO BE SWITCHED IN TO)                                 *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
SWCH     MVC   RTBYTE1,0(R1)       SAVE REQUESTED SYSTEM                        
         OC    RTBYTE1,RTBYTE1     IF (0) THEN WANT NATIVE SYSTEM               
         BNZ   *+10                                                             
         OC    RTBYTE1,GSSYS       SET NATIVE SYSTEM                            
         BNZ   SWC02               GSSYS WAS CORRUPTED SOMEHOW IF =0            
         OC    RTBYTE1,GSFRR.FRRSYS                                             
         OC    GSSYS,GSFRR.FRRSYS                                               
         BNZ   SWC02                                                            
         DC    H'0'                WHAT SYSTEM WAS REQUESTED?                   
*                                                                               
SWC02    MVC   RTBYTE2,RTBYTE1     COPY OF REQUESTED SYSTEM                     
         LA    R1,RTBYTE2                                                       
         CLC   GCSWSYSC,0(R1)      TEST ALREADY IN SYSTEM                       
         BE    SWC18               MAKE SURE DIRECTORY NAME OK                  
         CLI   0(R1),QSCON         TEST SWITCH TO CONTROL SYSTEM                
         BE    SWC12               YES, SWITCH UNCONDITIONALLY                  
*&&DO                                                                           
         CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BE    SWC03X                                                           
         MVC   RTPARM(1),0(R1)     DO OK TO SWITCH CALL                         
         MVC   RTPARM+1(3),BCEFFS                                               
         MVC   RTPARM+4(4),BCEFFS                                               
         GOTOX VSWITCH,RTPARM                                                   
         CLI   4(R1),0             ABLE TO SWITCH?                              
         BNE   SWC05               NO                                           
         B     SWC14                                                            
*&&                                                                             
SWC03X   L     RE,ASWSTAB                                                       
         USING SYSSWTAB,RE         RE=A(SYSTEM SWITCH TABLE)                    
         LA    RF,SYSSWMAX                                                      
*                                                                               
SWC04    CLC   SYSSWSOV,0(R1)      MATCH ON LOGICAL SYSTEM NUMBER               
         BNE   *+12                                                             
         LA    R1,SYSSWSYS         FOUND - POINT R1 TO ACTUAL SE NUMBER         
         B     SWC12                                                            
*                                                                               
         LA    RE,SYSSWLEN(RE)                                                  
         BCT   RF,SWC04                                                         
*                                                                               
SWC05    L     RE,ASYSLST          SYSTEM NOT AUTHORISED                        
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE          RE=A(SYSTEM LIST)                            
*                                                                               
SWC06    CLI   0(RE),EOT                                                        
         BE    SWC08                                                            
         CLC   SYSLNUM,RTBYTE1     MATCH ON SYSTEM NUMBER                       
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     SWC06                                                            
*                                                                               
         MVC   FVXTRA(L'SYSLNAME),SYSLNAME                                      
         B     SWC10                                                            
*                                                                               
SWC08    CURED (B1,0(R1)),(3,FVXTRA),0,DMCB=BOPARM,ALIGN=LEFT                   
*                                                                               
SWC10    MVC   FVMSGNO,=AL2(CE#SYSNA)                                           
         MVI   FVOSYS,QSCON                                                     
         B     EXITL                                                            
         DROP  RE                                                               
*                                                                               
SWC12    CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BNE   SWC14                                                            
         ICM   RF,15,BCAUTL        YES - MOVE SE NUMBER TO UTL                  
         MVC   TSYS-UTLD(L'TSYS,RF),0(R1)                                       
         B     SWC16                                                            
*                                                                               
SWC14    MVC   RTPARM(1),0(R1)     SWITCH TO SYSTEM                             
         MVC   RTPARM+1(3),BCEFFS                                               
         XC    RTPARM+4(4),RTPARM+4                                             
         GOTOX VSWITCH,RTPARM                                                   
         CLI   4(R1),0             TEST SWITCH SUCCESSFUL                       
         BNE   EXITH                                                            
*                                                                               
SWC16    MVC   GCSWSYSP,GCSWSYSC   SAVE PREVIOUS SYSTEM NUMBER                  
         MVC   GCSWSYSC,RTBYTE1    SAVE CURRENT SYSTEM NUMBER                   
*                                                                               
SWC18    L     RF,AXFILTAB         SET GCFILNAM                                 
         USING NFITABD,RF                                                       
         XR    RE,RE                                                            
         MVC   RTBYTE1,GSFRR.FRRDIR                                             
*                                                                               
SWC20    CLI   0(RF),EOT           TEST EOT - SYSTEM NOT SUPPORTED              
         BNE   SWC22                                                            
*                                                                               
         CLI   GCSWSYSC,QSCON      SWITCHING INTO CONTROL SYSTEM?               
         BE    *+6                                                              
         DC    H'0'                NO - WHAT ARE YOU DOING THEN?                
*                                                                               
         MVC   RTBYTE1,=AL1(XOGENDIR/256)                                       
         L     RF,AXFILTAB                                                      
         B     SWC20               FIDDLE FOR CONTROLLER RECORDS                
                                                                                
SWC22    CLC   NFIOSE,GCSWSYSC     MATCH ON SYSTEM NUMBER                       
         BNE   SWC24                                                            
         OC    RTBYTE1,RTBYTE1                                                  
         BZ    *+14                                                             
         CLC   NFINUM,RTBYTE1      MATCH ON DIRECTORY FOR THIS SYSTEM           
         BNE   SWC24                                                            
*                                                                               
         TM    NFIINDS2,NFIIID     I/S WITH D/A                                 
         BO    *+14                                                             
         MVC   GCFILNAM,NFINAME    MOVE IN NAME OF I/S FILE                     
         B     EXITOK                                                           
*                                                                               
         XR    R1,R1               GET NUMBER OF ASSOCIATED D/A                 
         IC    R1,NFINUM2                                                       
         B     SWC26                                                            
*                                                                               
SWC24    LA    RF,NFITABL(RF)                                                   
         B     SWC20                                                            
*                                                                               
SWC26    L     RF,AXFILTAB         LOOK FOR ASSOCIATED D/A FILE                 
         USING NFITABD,RF                                                       
         XR    RE,RE                                                            
*                                                                               
SWC28    CLI   0(RF),EOT           TEST EOT - FILE NOT SUPPORTED                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLM   R1,1,NFINUM         MATCH ON FILE FOR THIS SYSTEM                
         BNE   SWC30                                                            
         MVC   GCFILNAM,NFINAME    MOVE IN NAME OF D/A FILE                     
         B     EXITOK                                                           
*                                                                               
SWC30    LA    RF,NFITABL(RF)                                                   
         B     SWC28                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT ERROR MESSAGE, FIELD INDEX INFO & EXTRA MESSAGE   *         
*                                                                     *         
* NTRY - FVADDR=A(FIELD HEADER OF FIELD IN ERROR)                     *         
*        FVMSGNO=FIELD ERROR NUMBER                                   *         
*        FVFLAG=ZERO IF A STANDARD CONTROLLER ERROR MESSAGE REQUIRED  *         
*        FVOSYS=OVERRIDE SYSTEM FOR GETTXT CALL (ZERO=STANDARD)       *         
*        FVINDX=MULTIPLE FIELD INDEX NUMBER                           *         
*        FVSUBX=MULTIPLE FIELD SUB-INDEX NUMBER                       *         
*        FVXTRA=USER SUPPLIED MESSAGE TO TACK ONTO GENERAL MESSAGE    *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
STMSG    OC    FVMSGNO,FVMSGNO                                                  
         BNZ   SMSG02                                                           
         CLI   ASONOFF,ASOFF                                                    
         BNE   SMSG02                                                           
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(GI$REPDN)                                           
*                                                                               
SMSG02   CLC   FVMSGNO,=AL2(FVFSET) TEST USER HAS SUPPLIED MESSAGE              
         BE    SMSG12                                                           
         MVC   CSMSGNUM,FVMSGNO                                                 
         MVC   CSMSGTYP,FVOMTYP                                                 
         LA    R1,BCPARM           DEFINE GETTXT CONTROL BLOCK                  
         USING GETTXTD,R1                                                       
         CLC   FVMSGNO,=AL2(FVFGTSET)   TEST APPL SET GETTXT BLOCK              
         BNE   *+12                                                             
         LA    R1,BOPARM           APPLICATION HAS DEFINED BLOCK                
         B     SMSG10                                                           
*                                                                               
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTINDX,FVINDX                                                    
         MVC   GTSUBX,FVSUBX                                                    
         MVC   GTMSGNO,FVMSGNO                                                  
         MVC   GTMSYS,FVOSYS       OVERRIDE SYSTEM (IF SET)                     
         CLI   GTMSYS,0            TEST OVERRIDE SYSTEM SET                     
         BNE   *+10                                                             
         MVC   GTMSYS,ASSYSO       NO - SET NATIVE SYSTEM                       
*                                                                               
         MVC   GTMTYP,FVOMTYP      OVERRIDE MESSAGE TYPE (IF SET)               
         CLI   GTMSGNO,FF          STD CONTROLLER MSG                           
         BNE   *+12                                                             
         MVI   GTMSGNO,0                                                        
         MVI   GTMSYS,FF           GENERAL SYSTEM MESSAGE                       
*                                                                               
         CLI   ASONOFF,ASOFF       OFFLINE?                                     
         BNE   SMSG04                                                           
         OI    GT1INDS,GT1OHDR                                                  
         LA    R0,TWASCR                                                        
         STCM  R0,7,GTAOUT                                                      
*                                                                               
SMSG04   OC    GTMSGNO,GTMSGNO     MESSAGE 0 = DATAMGR ERR                      
         BNZ   SMSG06                                                           
         LA    R0,BCPARM                                                        
         STCM  R0,7,GTADMCB                                                     
         OI    GT1INDS,GT1DMGRE                                                 
*                                                                               
SMSG06   LA    RF,FVXTRA+L'FVXTRA-1                                             
         LA    R0,L'FVXTRA                                                      
         CLI   0(RF),C' '                                                       
         BH    *+14                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         B     SMSG08                                                           
         LA    RF,FVXTRA                                                        
         STCM  RF,7,GTATXT         SET LENGTH & ADDRESS OF EXTRA TEXT           
         STCM  R0,1,GTLTXT                                                      
*                                                                               
SMSG08   CLC   FVMSGNO,=AL2(FVFEKEY)   TEST 'ENTER KEY' MESSAGE                 
         BE    *+14                                                             
         CLC   FVMSGNO,=AL2(FVFEKEYD)  TEST 'ENTER KEY & DATA' MESSAGE          
         BNE   *+8                                                              
         OI    GT1INDS,GT1NOREF    SWITCH OFF REFERENCE                         
*                                                                               
         LA    R1,BCPARM           BCPARM DEFINED INTERNALLY                    
         CLI   GTMSGNO,FF          CHECK FOR GENERAL MESSAGES                   
         BNE   SMSG10                                                           
         MVI   GTMSYS,FF           FORCE SYSTEM ZERO LOOKUP                     
         MVI   GTMSGNO,0                                                        
*                                                                               
SMSG10   LA    R0,FVPARMS          SET A(SUBSTITUTION PARAMETERS)               
         STCM  R0,7,GTASUBST                                                    
         GOTOX VGETTXT,(R1)        RESOLVE MESSAGE                              
         DROP  R1                                                               
*                                                                               
SMSG12   OI    TWAMSGH+FHOID,FHOITR                                             
         L     R1,AINP             TEST IF OVERLAY SET CURSOR                   
         USING TIOBD,R1                                                         
         TM    TIOBINDS,TIOBSETC                                                
         BNZ   STMSGX                                                           
         TM    FVCURIND,FVCKEEP    TEST KEEP CURSOR WHERE IT IS                 
         BZ    SMSG14                                                           
         MVC   TIOBCURS,CSCURDSP                                                
         XC    TIOBCURD,TIOBCURD                                                
         OI    TIOBINDS,TIOBSETC                                                
         B     STMSGX                                                           
         DROP  R1                                                               
*                                                                               
SMSG14   ICM   R1,15,BOCURSOR      TEST CURSOR ADDRESS SET                      
         BNZ   *+12                                                             
         ICM   R1,15,FVADDR        TEST IF OVERLAY SET FIELD ADDRESS            
         BZ    STMSGX                                                           
*                                                                               
         CLI   FVERRNDX,0          TEST FIELD INDEX VALUE SET                   
         BE    SMSG16                                                           
         L     RE,AINP                                                          
         USING TIOBD,RE                                                         
         L     R0,ATWA                                                          
         SR    R1,R0               R1=DISPLACEMENT TO FIELD                     
         STCM  R1,3,TIOBCURD                                                    
         MVC   TIOBCURI,FVERRNDX                                                
         OI    TIOBINDS,TIOBSETC                                                
         A     R1,ATWA                                                          
         DROP  RE                                                               
*                                                                               
         USING FHD,R1                                                           
SMSG16   OI    FHOI,FHOICU                                                      
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)                                                       
         AR    R1,RE               POINT TO NEXT FIELD HEADER                   
         LH    RF,GSDSPMAX                                                      
         A     RF,ATWA                                                          
         BCTR  RF,0                RF=END OF LARGEST SCREEN                     
         ICM   RE,1,FHLN           TURN OFF CURSORS TO BOTTOM OF TWA            
         BZ    STMSGX                                                           
         NI    FHOI,FF-FHOICU                                                   
         BXLE  R1,RE,*-12                                                       
         DROP  R1                                                               
*                                                                               
STMSGX   B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO LOAD IN OVERLAY                                          *         
*                                                                     *         
* NTRY: R1 = A(OVERLAY NUMBER)                                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
OVRLY    CLI   0(R1),0             MUST HAVE A NUMBER TO OVERLAY.               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   CSOVER,0(R1)                                                     
*                                                                               
         TM    GCINDS2,GCILOAD     WANT TO LOAD OVERLAY?                        
         BO    *+14                                                             
         CLC   GCOVER,CSOVER       TEST ALREADY HAVE OVERLAY                    
         BE    OVR12                                                            
*                                                                               
         NI    GCINDS2,FF-GCILOAD                                               
         CLI   CSOVER,O#MAX        GLOBAL OVERLAYS                              
         BH    OVR08                                                            
*                                                                               
         CLI   PSFRR.FRRPHASE,0    ANY PREVIOUS SESSION OVERLAY?                
         BE    OVR06               NO                                           
         XR    R0,R0                                                            
         ICM   R0,8,PSFRR.FRRPHASE                                              
         OC    PSFRA.FRAOACT,PSFRA.FRAOACT                                      
         BZ    *+8                                                              
         ICM   R0,8,PSFRA.FRAOACT                                               
         CLM   R0,8,=AL1(O#MAX)                                                 
         BH    OVR02                                                            
         ICM   RF,8,=C'R'          PS OVERLAY ALSO IN T00AB4                    
         ICM   RF,7,=XL3'000AB4'                                                
         GOTOX VCOLY,RTPARM,0,(RF),0                                            
         CLI   4(R1),FF                                                         
         BNE   OVR04                                                            
         MVC   FVMSGNO,=AL2(FVFEOLY)                                            
         B     EXITL                                                            
*                                                                               
OVR02    GOTOX VCOLY,RTPARM,(R0),0,0                                            
*                                                                               
OVR04    MVC   APSOLY,0(R1)        LOAD PREVIOUS SESSION OVERLAY                
         MVI   APSOLY,0                                                         
*                                                                               
OVR06    ICM   RF,8,=C'R'          GLOBALS ALL INCLUDED IN T00AB4               
         ICM   RF,7,=XL3'000AB4'                                                
         GOTOX VCOLY,RTPARM,0,(RF),0                                            
         CLI   4(R1),FF                                                         
         BNE   OVR10                                                            
         MVC   FVMSGNO,=AL2(FVFEOLY)                                            
         B     EXITL                                                            
*                                                                               
OVR08    GOTOX VCOLY,RTPARM,(CSOVER,0),0,0                                      
         CLI   4(R1),FF                                                         
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFEOLY)                                            
         B     EXITL                                                            
         MVC   GCOVER,CSOVER                                                    
*                                                                               
OVR10    MVC   BONTRYA,0(R1)       SET OVERLAY ADDRESS                          
         MVI   BONTRYA,0           MAKE SURE THIS IS 0                          
         MVC   AOLY,0(R1)          OVERLAY OBJECT ADDRESS                       
         MVI   AOLY,0                                                           
         MVC   AOVERLAY,AOLY                                                    
*                                                                               
OVR12    L     R0,AOVERWRK         CLEAR OVERWRK FOR NEW OVERLAY                
         LH    R1,=Y(OVERWRKL)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   RTBYTE1,GSSYS       SAVE CURRENT SYSTEM                          
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)   SWITCH TO RECORD SYSTEM             
*                                                                               
         L     RF,AOLY             CALL OVERLAY TO INITIALIZE ITSELF            
         CLI   CSOVER,O#FLTR       FILTERING?                                   
         BNE   *+8                                                              
         L     RF,APSOLY           CALL PS OVERLAY TO INITIALIZE ITSELF         
*                                                                               
         ICM   RF,8,=AL1(1)                                                     
         LA    R1,RTPARM                                                        
         XC    RTPARM,RTPARM                                                    
         BASR  RE,RF                                                            
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),RTBYTE1  BACK TO CURRENT SYSTEM               
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SAVE VALUES AT END OF TRANSACTION                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
SVVAL    GOTOX ('TSARIO',AGROUTS),TSASAV SAVE TSAR BUFFER IF NECESSARY          
*                                                                               
         LH    RE,=Y(TWVALS-TWAD)                                               
         A     RE,ATWA             SAVE GLOBAL W/S VALUES IN TWA0               
         LA    R0,BCVALS                                                        
         LA    RF,BCVALSL                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,ATWA             SAVE CONTROLLER W/S VALUES IN TWA0           
         AH    RE,=Y(GSVALS-TWAD)                                               
         LA    R0,GSSAVE                                                        
         LA    RF,GSSAVEL                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLI   ASONOFF,ASOFF                                                    
         BE    EXITOK                                                           
*                                                                               
         ICM   R3,12,=C'L='        SAVE TEMPSTR PAGE FOR FDRELDS                
         ICM   R3,3,=Y(TWAMAX-FDRDISPQ)                                         
         GOTOX VDMGR,RTPARM,DMREAD,TEMPSTR,(4,0),ATIA,,(R3)                     
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R3,3,=Y(TWAMAX)                                                  
         GOTOX (RF),(R1),DMWRITE,TEMPSTR,(4,0),ATIA,,(R3)                       
         BE    EXITOK                                                           
         DC    H'0'                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RESTORE VALUES AT START OF TRANSACTION                   *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
RSVAL    L     RE,ATWA             RESTORE CONTROLLER W/S VALUES IN TWA         
         AH    RE,=Y(GSVALS-TWAD)                                               
         LA    R0,GSSAVE                                                        
         LA    RF,GSSAVEL                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   GCLASKEY,GSRECKEY   SET LAST KEY READ                            
         MVC   GCLASSTA,GSRECSTA                                                
         MVC   GCLASDA,GSRECDA                                                  
*                                                                               
         OC    CSOVER,CSOVER       LOAD OVERLAY                                 
         BZ    RSVL02                                                           
         GOTOX ('OVRLAY',AGROUTS),CSOVER                                        
*                                                                               
RSVL02   CLI   ASONOFF,ASOFF                                                    
         BE    RSVL03                                                           
*                                                                               
         ICM   R3,12,=C'L='        RESTORE TEMPSTR PAGE FOR FDRELDS             
         ICM   R3,3,=Y(TWAMAX)                                                  
         GOTOX VDMGR,RTPARM,DMREAD,TEMPSTR,(4,0),ATIA,,(R3)                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RSVL03   OC    LSLINE#,LSLINE#     TEST CURRENT LINE NUMBER EXISTS              
         BZ    RSVL04                                                           
         CLC   LSLINE#,LSLST#1                                                  
         BL    RSVL04                                                           
         CLC   LSLINE#,LSLST#X                                                  
         BH    RSVL04                                                           
         L     RF,ATLST                                                         
         USING TLSTD,RF                                                         
         MVC   TLNUM,LSLINE#                                                    
         DROP   RF                                                              
         GOTOX ('TSARIO',AGROUTS),TSAGET     YES - GET IT                       
*                                                                               
RSVL04   XR    R0,R0                                                            
         ICM   R0,1,GS#FDR                                                      
         BZ    RSVLX                                                            
         L     R1,ATIA             RESTORE FDRBLK                               
         AH    R1,=Y(FDRDISPQ)                                                  
         USING FDRELD,R1                                                        
         L     RE,AFDRADDR                                                      
         XR    RF,RF                                                            
*                                                                               
RSVL06   IC    RF,FDRLN                                                         
         CLI   FDREL,FDRELQ                                                     
         BE    *+12                                                             
         CLI   FDREL,FLTRLQ                                                     
         BNE   *+12                                                             
         ST    R1,0(RE)                                                         
         LA    RE,L'FDRADDR(RE)                                                 
         AR    R1,RF                                                            
         BCT   R0,RSVL06                                                        
*                                                                               
RSVLX    B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MAINTAIN FDRELD BLOCK                                    *         
*                                                                     *         
* NTRY: P1 BYTE 0 = C'C' TO CLEAR SAVED FDRELD BLOCK                  *         
*             1-3 = SEQUENCE NUMBER TO CLEAR FORM                     *         
*       P1 BYTE 0 = C'N' TO ADD FDRELD FOR GIVEN FIELD NUMBER         *         
*             1-3 = A(ELEMENT CODE, FIELD NUMBER)                     *         
*       P1 BYTE 0 = C'A' TO ADD FDRELD FOR GIVEN FIELD ELEMENT        *         
*             1-3 = A(FDRELD ELEMENT)                                 *         
* EXIT: P1        = A(SAVED FDRELD ELEMENT)                           *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
SVFDR    L     R1,RTPARMA                                                       
         MVC   RTPARMS,0(R1)                                                    
         XR    R2,R2                                                            
         ICM   R2,7,RTPARMS1+1     R2=PARAMETER 1                               
         CLI   RTPARMS1,C'C'       TEST CLEARING BLOCK                          
         BNE   SFDR06                                                           
         STC   R2,GS#FDR           SET NEW NUMBER OF ENTRIES                    
*                                                                               
         LR    RE,R2                                                            
         MH    RE,=Y(L'GSFDRLST)                                                
         LA    RE,GSFDRLST(RE)     RE=A(1ST SAVED FIELD NUMBER)                 
         LR    RF,R2                                                            
         SLL   RF,2                                                             
         A     RF,AFDRADDR         RF=A(1ST SAVED ADDRESS)                      
         LA    R0,GS#FDRMX                                                      
         SR    R0,R2               R0=NUMBER TO CLEAR                           
         BP    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SFDR02   XC    0(L'GSFDRLST,RE),0(RE)                                           
         XC    0(L'FDRADDR,RF),0(RF)                                            
         LA    RE,L'GSFDRLST(RE)                                                
         LA    RF,L'FDRADDR(RF)                                                 
         BCT   R0,SFDR02                                                        
*                                                                               
         LTR   R2,R2               CLEAR SAVE AREA AFTER LAST ELEMENT           
         BZ    SFDR04                                                           
         BCTR  R2,0                                                             
         SLL   R2,2                                                             
         A     R2,AFDRADDR                                                      
         L     R2,0(R2)                                                         
         XR    RF,RF                                                            
         IC    RF,1(R2)                                                         
         AR    R2,RF                                                            
         XC    0(32,R2),0(R2)                                                   
*                                                                               
SFDR04   B     EXITOK                                                           
*                                                                               
SFDR06   CLI   RTPARMS1,C'A'       TEST GIVEN ELEMENT                           
         BE    SFDR14                                                           
         CLI   RTPARMS1,C'N'       TEST GIVEN NUMBER                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   RTMSG,FVMSGNO                                                    
*                                                                               
         OC    ASTEST,ASTEST       USING TEST PHASE?                            
         BZ    SFDR08                                                           
*                                                                               
K        USING FDRRECD,IOKEY                                                    
         XC    K.FDRKEY,K.FDRKEY   READ FOR CONNECTED COUNTRY                   
         MVI   K.FDRKMIN,FDRKMINQ  AND TEST PHASE                               
         MVI   K.FDRKTYP,FDRKTYPQ                                               
         MVC   K.FDRKSYS,GCOVSYS                                                
         MVC   K.FDRKPRG,GSSHPRG                                                
         MVC   K.FDRKREC,GSSHREC                                                
         MVC   K.FDRKNUM,1(R2)                                                  
         MVC   K.FDRKCTRY,CUCTRY                                                
         XI    K.FDRKCTRY,X'FF'                                                 
         MVI   K.FDRKSUB,X'FF'                                                  
         MVC   K.FDRKTEST,ASTEST                                                
*                                                                               
         L     R1,=AL4(XOREAD+XOGENDIR+XIO1)                                    
         GOTOX ('XIOC',AGROUTS)                                                 
         BE    SFDR12                                                           
*                                                                               
SFDR08   XC    K.FDRKEY,K.FDRKEY   READ FOR CONNECTED COUNTRY                   
         MVI   K.FDRKMIN,FDRKMINQ  AND LIVE PHASE                               
         MVI   K.FDRKTYP,FDRKTYPQ                                               
         MVC   K.FDRKSYS,GCOVSYS                                                
         MVC   K.FDRKPRG,GSSHPRG                                                
         MVC   K.FDRKREC,GSSHREC                                                
         MVC   K.FDRKNUM,1(R2)                                                  
         MVC   K.FDRKCTRY,CUCTRY                                                
         XI    K.FDRKCTRY,X'FF'                                                 
         MVI   K.FDRKSUB,X'FF'                                                  
         XC    K.FDRKTEST,K.FDRKTEST                                            
*                                                                               
         L     R1,=AL4(XOREAD+XOGENDIR+XIO1)                                    
         GOTOX ('XIOC',AGROUTS)                                                 
         BE    SFDR12                                                           
*                                                                               
         OC    ASTEST,ASTEST                                                    
         BZ    SFDR10                                                           
*                                                                               
         XC    K.FDRKEY,K.FDRKEY   READ FOR HOST PROCESSOR                      
         MVI   K.FDRKMIN,FDRKMINQ  AND TEST PHASE                               
         MVI   K.FDRKTYP,FDRKTYPQ                                               
         MVC   K.FDRKSYS,GCOVSYS                                                
         MVC   K.FDRKPRG,GSSHPRG                                                
         MVC   K.FDRKREC,GSSHREC                                                
         MVC   K.FDRKNUM,1(R2)                                                  
         MVI   K.FDRKCTRY,X'FF'                                                 
         MVI   K.FDRKSUB,X'FF'                                                  
         MVC   K.FDRKTEST,ASTEST                                                
         L     R1,=AL4(XOREAD+XOGENDIR+XIO1)                                    
*                                                                               
         GOTOX ('XIOC',AGROUTS)                                                 
         BE    SFDR12                                                           
*                                                                               
SFDR10   XC    K.FDRKEY,K.FDRKEY   READ FOR HOST PROCESSOR                      
         MVI   K.FDRKMIN,FDRKMINQ  AND LIVE PHASE                               
         MVI   K.FDRKTYP,FDRKTYPQ                                               
         MVC   K.FDRKSYS,GCOVSYS                                                
         MVC   K.FDRKPRG,GSSHPRG                                                
         MVC   K.FDRKREC,GSSHREC                                                
         MVC   K.FDRKNUM,1(R2)                                                  
         MVI   K.FDRKCTRY,X'FF'                                                 
         MVI   K.FDRKSUB,X'FF'                                                  
         XC    K.FDRKTEST,K.FDRKTEST                                            
*                                                                               
         L     R1,=AL4(XOREAD+XOGENDIR+XIO1)                                    
         GOTOX ('XIOC',AGROUTS)                                                 
         BE    SFDR12                                                           
         DC    H'0'                ABSOLUTELY SOD ALL FOR THIS FIELD            
*                                                                               
SFDR12   L     R1,=AL4(XOGET+XOGENFIL+XIO1)                                     
         GOTOX ('XIOC',AGROUTS)                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,RTMSG                                                    
         L     R1,AIO1                                                          
*                                                                               
         LA    R1,FDRFIRST(R1)                                                  
         USING FDRELD,R1                                                        
         XR    RF,RF                                                            
         CLC   FDREL,0(R2)                                                      
         BE    *+12                                                             
         IC    RF,FDRLN                                                         
         BXH   R1,RF,*-14                                                       
         DROP  R1                                                               
         LR    R2,R1                                                            
         USING FDRELD,R2                                                        
*                                                                               
SFDR14   XR    R3,R3               TEST ADDING FIRST ELEMENT                    
         ICM   R3,1,GS#FDR                                                      
         BNZ   SFDR16                                                           
         L     R1,ATIA             R1=A(AREA FOR FIRST ELEMENT)                 
         AH    R1,=Y(FDRDISPQ)                                                  
         B     SFDR18                                                           
*                                                                               
SFDR16   LR    R1,R3               R1=A(COPY AREA FOR NEXT ELEMENT)             
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         A     R1,AFDRADDR                                                      
         L     R1,0(R1)                                                         
         XR    RF,RF                                                            
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
*                                                                               
SFDR18   XR    RF,RF                                                            
         IC    RF,FDRLN            COPY ELEMENT                                 
         MVC   0(0,R1),FDRELD                                                   
         EX    RF,*-6                                                           
         LR    RE,R3               SAVE A(ELEMENT IN BLOCK)                     
         SLL   RE,2                                                             
         A     RE,AFDRADDR                                                      
         ST    R1,0(RE)                                                         
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         MVI   0(RF),0                                                          
*                                                                               
         LA    R3,1(R3)            UPDATE NUMBER OF ELEMENTS                    
         STC   R3,GS#FDR           SAVE FIELD NUMBER                            
         MH    R3,=Y(L'GSFDRLST)                                                
         LA    R3,GSFDRLST-L'GSFDRLST(R3)                                       
         USING GSFDRLST,R3                                                      
         MVC   GSFDRLEL,FDREL                                                   
         MVC   GSFDRL#,FDRNUM                                                   
         DROP  R3                                                               
*                                                                               
SFDRX    L     RE,RTPARMA                                                       
         ST    R1,0(RE)            RETURN A(FDRELD)                             
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INTERFACE WITH TSAR                                      *         
*                                                                     *         
* NTRY - R1 = TSAR ACTION VALUE (TSAR INITIALISED IF NECESSARY)       *         
*     OR R1 = A(TSAR ACTION VALUE, A(RECORD))                         *         
* EXIT - CC = EQUAL IF OK                                             *         
*        CC = LOW IF END-OF-FILE REACHED                              *         
*        CC = HIGH IF RECORD NOT FOUND FOR READ HIGH                  *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
TSARINF  MVC   TITSACTN,RTPARMA+3  SAVE CALLER'S ACTION                         
         L     R2,ATLST                                                         
         OC    RTPARMA(3),RTPARMA  TEST ACTION,A(RECORD) PASSED                 
         BZ    *+14                                                             
         MVC   TITSACTN,0(R1)                                                   
         ICM   R2,7,1(R1)                                                       
         LA    R2,0(R2)            MAKE SURE THIS IS A 24-BIT ADDRESS           
TI       USING TLSTD,R2            R2=A(CURRENT LIST ENTRY)                     
*                                                                               
         L     R3,ATSABLK                                                       
         USING TSARD,R3            R3=A(TSAR BLOCK)                             
         LA    R0,TI.TLREC                                                      
         ST    R0,TSAREC           SET A(RECORD)                                
         CLI   ASONOFF,ASOFF                                                    
         BNE   *+10                                                             
         MVC   TSABUF,ATSOBUF                                                   
*                                                                               
         TM    BCTSINDS,BCTSIRES   TEST ALREADY RESTORED                        
         BNZ   TSIN04                                                           
*                                                                               
         MVC   TSACOM,ACOM         SET A(COMFACS)                               
         MVI   TSKEYL,L'TLKEY      SET KEY LENGTH                               
         MVI   TSRECI,TSRVAR       SET VARIABLE LENGTH                          
         MVC   TSRECL,=Y(TLMAXLNQ) SET MAXIMUM RECORD LENGTH                    
         MVI   TSPAGN,TSPEXPN      SET NUMBER OF TEMPEST PAGES                  
*        MVI   TSINDS,TSIALLOC     SET TO ALLOCATE FROM TEMPEST                 
         OI    TSINDS,TSIALLOC                                                  
*                                                                               
         LA    R1,TSACTN           DISCOVER WHERE TO PUT THE ACTION             
         CLI   ASONOFF,ASOFF                                                    
         BNE   *+8                                                              
         LA    R1,TSOFFACT                                                      
         MVI   0(R1),TSAINI        SET INITIALISE                               
*                                                                               
         CLI   TITSACTN,TSASAV     TEST SAVE                                    
         BE    TSINX                                                            
*                                                                               
         TM    BCTSINDS,BCTSIINI   TEST TEMPEST BUFFER INITIALISED              
         BZ    TSIN02                                                           
         MVI   TSACTN,TSARES       SET RESTORE                                  
         MVC   TSPAGL,BCTSLOWP     SET LOW PAGE NUMBER                          
         MVC   TSPAGN,BCTSNUMP     SET NUMBER OF PAGES ALLOCATED                
         B     TSIN03                                                           
*                                                                               
TSIN02   CLI   ASONOFF,ASOFF       OFFLINE INITIALISE?                          
         BNE   TSIN03                                                           
         MVC   TSAREC,LTSOBUF      SET BUFFER LENGTH                            
*                                                                               
TSIN03   GOTOX AGTSAR,TSARD        CALL TO INITIALISE/RESTORE                   
         BNE   TSINDIE             ABEND                                        
*                                                                               
         LA    R0,TI.TLREC         IN CASE OFFLINE INITIALISE                   
         ST    R0,TSAREC           SET A(RECORD)                                
*                                                                               
         MVC   BCTSLOWP,TSPAGL     SAVE LOW TSAR PAGE NUMBER                    
         MVC   BCTSNUMP,TSPAGN     SAVE NUMBER OF PAGES ALLOCATED               
         OI    BCTSINDS,BCTSIINI+BCTSIRES                                       
*                                                                               
TSIN04   LA    R1,TSACTN           DISCOVER WHERE TO PUT THE ACTION             
         CLI   ASONOFF,ASOFF       AND SET IT                                   
         BNE   *+8                                                              
         LA    R1,TSOFFACT                                                      
         MVC   0(L'TITSACTN,R1),TITSACTN                                        
*                                                                               
         CLI   TITSACTN,TSAINI     TEST EXPLICIT INITIALISE                     
         BE    TSINX                                                            
         CLI   TITSACTN,TSARES     TEST EXPLICIT RESTORE                        
         BE    TSINX                                                            
         CLI   TITSACTN,TSASAV     TEST SAVE                                    
         BNE   TSIN06                                                           
         CLI   ASONOFF,ASOFF                                                    
         BE    TSINX                                                            
*                                                                               
         NI    BCTSINDS,FF-BCTSIRES                                             
         GOTOX AGTSAR,TSARD                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
TSIN06   MVC   TSRNUM,TI.TLNUM                                                  
         GOTOX AGTSAR,TSARD                                                     
         MVC   TI.TLNUM,TSRNUM     SET RECORD LIST NUMBER                       
         BE    TSIN08                                                           
         CLI   TITSACTN,TSARDH     TEST READ-HIGH/NEXT                          
         BE    TSIN07                                                           
         CLI   TITSACTN,TSANXT                                                  
         BE    TSIN07                                                           
*                                                                               
         CLI   TITSACTN,TSAADD     ADDING TSAR RECORD?                          
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         OI    LSSTAT2,LSTSFULL    SET BUFFER FULL                              
         B     EXITL                                                            
*                                                                               
TSIN07   TM    TSERRS,TSEEOF       RETURN CC=LOW FOR END-OF-FILE ERROR          
         BO    EXITL                                                            
         TM    TSERRS,TSERNF       RETURN CC=HIGH IF RECORD NOT FOUND           
         BO    EXITH                                                            
         DC    H'0'                                                             
*                                                                               
TSIN08   CLI   TITSACTN,TSAADD     ADDING?                                      
         BNE   TSIN12                                                           
         TM    TI.TLKSES,X'F0'     TEST FOR NTRSES LEVEL                        
         BNZ   TSIN10                                                           
         ICM   RE,3,BCTSHIGH       YES - INCREMENT GLOBAL HIGH RECORD #         
         LA    RE,1(RE)                                                         
         STCM  RE,3,BCTSHIGH                                                    
         MVC   CSHIRECN,BCTSHIGH                                                
         B     TSINX                                                            
*                                                                               
TSIN10   CLI   TI.TLKSES,TLKSTEMP  TEST ADDING TEMPORARY RECORD                 
         BL    TSINX                                                            
         OI    GCINDS1,GCITEMP     YES - SET FLAG SO GETS DELETED LATER         
         B     TSINX                                                            
*                                                                               
TSIN12   CLI   TITSACTN,TSADEL     DELETING?                                    
         BNE   TSINX                                                            
         TM    TI.TLKSES,X'F0'     TEST FOR NTRSES LEVEL                        
         BNZ   TSINX                                                            
         ICM   RE,3,BCTSHIGH       YES - DECREMENT GLOBAL HIGH RECORD #         
         BCTR  RE,0                                                             
         STCM  RE,3,BCTSHIGH                                                    
         CLC   CSHIRECN,BCTSHIGH                                                
         BNH   TSINX                                                            
         MVC   CSHIRECN,BCTSHIGH                                                
*                                                                               
TSINX    B     EXITOK                                                           
*                                                                               
TSINDIE  LH    R1,GSDSPACT         ABEND IF INITIALISE/RESTORE FAILS            
         A     R1,ATWA                                                          
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(GE$ISUTS)                                           
         OI    CSINDSG1,CSINDUNW   SET TO UNWIND VIA $ABEND                     
         L     RD,BCSVRD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                  EXIT TO ROOT CALL POINT                      
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD GSRECKEY/STA/DA FROM A RECORD READ                 *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BDDIR    LH    RF,GSDIRDSP                                                      
         A     RF,AXFILTAB                                                      
         ST    RF,RTFILDIR         DIRECTORY ENTRY                              
         USING NFITABD,RF                                                       
         ICM   R2,15,0(R1)         PASSED RECORD AREA?                          
         BNZ   *+8                                                              
         L     R2,AIOREC           DEFAULT AREA                                 
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,NFIKEYL        LENGTH OF RECORD KEY                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         MVC   GSRECKEY(0),0(R2)   MOVE IN KEY                                  
         EX    RE,*-6                                                           
*                                                                               
         LA    R1,3(RE,R2)         2 IS FOR RECORD LENGTH                       
         ICM   RE,1,NFICTLL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         MVC   GSRECSTA(0),0(R1)   MOVE IN STATUS                               
         EX    RE,*-6                                                           
*                                                                               
         MVC   GSRECMSK,BCEFFS     SET MASK TO `FF`S                            
         GOTOX APRG,BOPARM,('GCBOVER',OKEY),KMASK,GSRECKEY,GSRECMSK             
*                                                                               
         L     RF,RTFILDIR                                                      
         MVC   GSRECDA,BCEFFS      SET SPECIAL CONTROL FILE D/A                 
         TM    NFIINDS2,NFIIID     LINKED D/A                                   
         BO    BDD02               YES                                          
         TM    NFIINDS,NFIIVL      V/L I/S THEN                                 
         BO    BDD04               YES                                          
         DC    H'0'                                                             
*                                                                               
BDD02    LR    R1,R2               A(IO AREA)                                   
         SH    R1,=Y(L'IODA+L'IOWORK)                                           
         MVC   GSRECDA,0(R1)       MOVE IN D/A                                  
         OC    GSRECDA,GSRECDA                                                  
         BNZ   BDD04                                                            
         DC    H'0'                SHIT                                         
*                                                                               
BDD04    GOTOX APRG,BOPARM,('GCBOVER',ORECH),RMASK,(R2),GSRECMSK                
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* PUT RECORD ACTIVITY ELEMENTS INTO RECORD                            *         
*                                                                     *         
* NTRY - P1 BYTE 0 = RACTADD TO SET ADD ELEMENT ONLY                  *         
*                  = RACTCHA TO SET CHANGE ELEMENT ONLY               *         
*                  = RACTADD+RACTCHA TO SET BOTH ELEMENTS             *         
*              1-3 = A(RECORD)                                                  
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING PRWORKD,RTWORK                                                   
PUTRC    TM    GCINDS3,GCINOACT    SUPPRESS ACTIVITY?                           
         BO    EXITOK              YES                                          
*                                                                               
         MVC   PRTYPE,0(R1)                                                     
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)                                                       
*                                                                               
         LA    R3,PRRACEL          SET UP ACTIVITY ELEMENT                      
         USING RACELD,R3                                                        
         MVI   RACEL,RACELQ                                                     
         MVI   RACLN,RACLNQ                                                     
         MVC   RACUSER,CUUSER                                                   
         MVC   RACPERS,CUPASS                                                   
         MVC   RACTERM,CUTERM                                                   
         MVC   RACDATE,ASPDAT                                                   
         MVC   RACTIME,ASTIME                                                   
*                                                                               
         GOTOX PRAC,RACTADD                                                     
         GOTOX PRAC,RACTCHA                                                     
PUTRACX  B     EXITOK                                                           
*                                                                               
PRAC     STC   R1,RACTYPE          TEST ELEMENT REQUIRED                        
         NC    RACTYPE,PRTYPE                                                   
         BZR   RE                                                               
         LR    R0,RE               SAVE RE                                      
*                                                                               
         GOTOX VHELLO,RTPARM,(C'G',GCFILNAM),('RACELQ',(R2)),          *        
               (L'RACTYPE,RACTYPE)                                              
         CLI   12(R1),0                                                         
         BNE   PRAC02              ADD ELEMENT                                  
         ICM   R1,15,12(R1)                                                     
         MVC   0(RACLNQ,R1),RACELD COPY NEW ACTIVITY ELEMENT                    
         B     PRACX                                                            
*                                                                               
PRAC02   GOTOX VHELLO,BCPARM,(C'P',GCFILNAM),(R2),RACELD                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PRACX    LA    RF,GCRACADD                                                      
         CLI   RACTYPE,RACTCHA                                                  
         BNE   *+8                                                              
         LA    RF,GCRACCHA                                                      
         MVC   0(RACLNQ,RF),RACELD UPDATE RACEL IN MEMORY                       
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 1                                                                
PRWORKD  DSECT                     ** PUTRAC S/R LOCAL W/S **                   
PRTYPE   DS    XL1                                                              
PRRACEL  DS    XL(RACLNQ)          ACTIVITY ELEMENT                             
*                                                                               
GEFIL00  CSECT                                                                  
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* GET RECORD ACTIVITY ELEMENTS FROM RECORD                            *         
*                                                                     *         
* NTRY - P1 = A(RECORD)                                               *         
*        P2 = A(AREA FOR ADD ELEMENT) OR 0                            *         
*        P3 = A(AREA FOR CHANGE ELEMENT) OR 0                         *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
GETRC    TM    GCINDS3,GCINOACT    SUPPRESS ACTIVITY?                           
         BO    EXITOK              YES                                          
*                                                                               
         LM    R2,R4,0(R1)                                                      
         LTR   R3,R3                                                            
         BZ    *+10                                                             
         XC    0(RACLNQ,R3),0(R3)  CLEAR AREA FOR ADD ELEMENT                   
         LTR   R4,R4                                                            
         BZ    *+10                                                             
         XC    0(RACLNQ,R4),0(R4)  CLEAR AREA FOR CHANGE ELEMENT                
*                                                                               
         LTR   R3,R3               WANT ADD ELEMENT?                            
         BZ    GRAC02                                                           
         GOTOX VHELLO,RTPARM,(C'G',GCFILNAM),('RACELQ',(R2)),          *        
               (1,=AL1(RACTADD))                                                
         CLI   12(R1),0                                                         
         BNE   GRAC02              NO ADD ELEMENT                               
*                                                                               
         ICM   R1,15,12(R1)                                                     
         USING RACELD,R1           R1=A(ACTIVITY ELEMENT)                       
         MVC   0(RACLNQ,R3),RACELD                                              
*                                                                               
GRAC02   LTR   R4,R4               WANT CHANGE ELEMENT?                         
         BZ    GRACX                                                            
         GOTOX VHELLO,RTPARM,(C'G',GCFILNAM),('RACELQ',(R2)),          *        
               (1,=AL1(RACTCHA))                                                
         CLI   12(R1),0                                                         
         BNE   GRACX               NO CHANGE ELEMENT                            
*                                                                               
         ICM   R1,15,12(R1)                                                     
         USING RACELD,R1           R1=A(ACTIVITY ELEMENT)                       
         MVC   0(RACLNQ,R4),RACELD                                              
*                                                                               
GRACX    B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ISSUE AN I/O TO ANY STANDARD SYSTEM FILE                 *         
*                                                                     *         
* NTRY - R1=I/O CONTROL BYTES SET FROM IO EQUATES                     *         
*           CONTAINS - FILE NUMBER       (ZERO=USE IOFILE)            *         
*                      COMMAND NUMBER    (ZERO=USE IOCMND)            *         
*                      COMMAND QUALIFIER (READ LOCK/READ DELETES)     *         
*                      I/O AREA NUMBER   (ZERO=USE IOADDR)            *         
*                                                                     *         
* EXIT - CC=LOW IF A HARD I/O ERROR OCCURED                           *         
*        CC=EQUAL IF I/O SUCCESSFUL (NO ERRORS)                       *         
*        CC=HIGH IF A SOFT ERROR (EOF/NOT FOUND/DELETED)              *         
*        IOADDR=A(I/O AREA USED)                                      *         
*        IOERR=DATAMGR ERROR BYTE                                     *         
*        IOKEYSV=SAVE IOKEY VALUE (BEFORE I/O IS EXECUTED)            *         
*        IODA=DISK ADDRESS EXTRACTED FOR I/S RECORD (I/S D/A PAIR)    *         
*                                                                     *         
* NOTE - FOR INDEX SEQUENTIAL I/O'S IOKEY IS ALWAYS SAVED IN IOKEYSV  *         
*        BEFORE I/O IS EXECUTED. FOR D/A FILE I/O'S IF IODA IS ZERO   *         
*        AND FILE HAS A DIRECTORY ATTACHED (I/S D/A PAIR) THE READ    *         
*        SPECIFIED TO THE FILE (HIGH/READ) IS EXECUTED TO THE         *         
*        DIRECTORY.                                                   *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING IORTND,RTWORK                                                    
XIOR     ST    R1,IOCTRL           SAVE I/O CONTROL BYTES IN W/S                
         MVI   IOFLAG,0            RESET I/O FLAG BYTE                          
         MVI   IOQ,0               ESTABLISH COMMAND QUALIFIER                  
         N     R1,=AL4(XOLOCK)     TEST READ-FOR-UPDATE                         
         LTR   R1,R1                                                            
         BZ    *+8                                                              
         OI    IOQ,X'80'                                                        
         L     R1,IOCTRL                                                        
         N     R1,=AL4(XORDEL)     TEST DELETED RECORDS WANTED                  
         BZ    *+8                                                              
         OI    IOQ,X'08'                                                        
*                                                                               
         L     R1,IOCTRL           ESTABLISH WHICH I/O AREA REQUIRED            
         N     R1,=AL4(XIOAREAS)                                                
         BZ    IOEX02              NO I/O AREA SET                              
*                                                                               
         SRL   R1,4                R1=I/O AREA NUMBER                           
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R1,AIO1(R1)         A(IO AREA)                                   
         L     R1,0(R1)            I/O AREA IN R1 NOW                           
         STCM  R1,15,IOADDR        SET REAL I/O ADDRESS                         
*                                                                               
         SH    R1,=Y(L'IODA+L'IOWORK)                                           
         STCM  R1,15,IOAREAD       SAVE I/O ADDRESS                             
*                                                                               
IOEX02   L     R1,IOCTRL           ESTABLISH WHICH FILE REQUIRED                
         N     R1,=AL4(XOFILES)                                                 
         BNZ   IOEX04                                                           
*                                                                               
         OC    IOFILE,IOFILE       CALLER MUST SUPPLY FILE NAME                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOFILNM,IOFILE      SET FILE NAME                                
         B     IOEX10              COMMAND NAME MUST ALSO BE SUPPLIED           
*                                                                               
IOEX04   SRL   R1,8                R1=FILE NUMBER                               
         L     RE,AXFILTAB         POINT TO SYSTEM FILES                        
         USING NFITABD,RE                                                       
*                                                                               
IOEX06   CLI   NFINUM,EOT          TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID FILE NUMBER                          
         CLM   R1,1,NFINUM         MATCH ON FILE NUMBER                         
         BE    *+12                                                             
         LA    RE,NFITABL(RE)                                                   
         B     IOEX06                                                           
*                                                                               
         MVC   IOFILV,NFINUM       EXTRACT FILE VALUES                          
         CLC   GCSWSYSC,IOFILSE    CHECK WE ARE IN THE CORRECT SYSTEM           
         BE    IOEX08                                                           
         GOTOX IOSWITCH,IOFILOSE   SWITCH TO REQUIRED SYSTEM                    
         BE    IOEX08                                                           
         GOTOX IOSWITCH,GCSWSYSN   CAN`T SWITCH - SWITCH BACK TO NATIVE         
         B     EXITL               SWITCH SETS ERROR MESSAGE                    
*                                                                               
IOEX08   L     RE,AXCMDTAB         RE=A(I/O COMMAND TABLE)                      
         USING CMDTABD,RE          RE=A(FILE/COMMAND TABLE)                     
         XR    RF,RF                                                            
         LA    R1,XOCMNDS          ESTABLISH COMMAND EQUATE                     
         N     R1,IOCTRL                                                        
         BNZ   IOEX12              EQUATE SET                                   
*                                                                               
IOEX10   OC    IOCMND,IOCMND       TEST COMMAND NAMED                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOCMDNM,IOCMND      SET COMMAND NAME                             
*                                                                               
         ICM   R0,15,IOADDR        SEE IF ADDRESS SET                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOX VDMGR,RTPARM,(IOQ,IOCMDNM),IOFILNM,(R0),(R0)                     
         MVC   IOERR,8(R1)                                                      
         B     IOEXX                                                            
*                                                                               
IOEX12   CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(1),0(RE)      TEST CORRECT SERIES OF COMMANDS              
         NC    IODUB(1),IOFILI                                                  
         CLC   IODUB(1),IOFILI                                                  
         BNE   *+12                                                             
         LA    RE,4(RE)                                                         
         B     IOEX14                                                           
*                                                                               
         ICM   RF,3,2(RE)                                                       
         LA    RE,3(RF,RE)                                                      
         B     IOEX12                                                           
*                                                                               
IOEX14   CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID COMMAND                              
         CLM   R1,1,CMDNUMB        MATCH ON COMMAND NUMBER                      
         BE    *+12                                                             
         LA    RE,CMDTABL(RE)                                                   
         B     IOEX14                                                           
*                                                                               
         MVC   IOCMDV,CMDNAME      EXTRACT COMMAND VALUES                       
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    IODA,IODA                                                        
         TM    IOCMDI,CMDIDADD     TEST DISK ADDRESS RETURNED                   
         BNZ   IOEX22                                                           
         TM    IOCMDI,CMDIDARQ     TEST D/A REQUIRED FOR I/O                    
         BZ    IOEX24                                                           
         OC    IODAOVER,IODAOVER   TEST OVERRIDE D/A SET                        
         BZ    IOEX16                                                           
         MVC   IODA,IODAOVER       YES - SET D/A AND CLEAR OVERRIDE             
         ICM   R1,15,IOAREAD       R1=A(I/O AREA)                               
         BZ    *+10                                                             
         MVC   0(L'IODA,R1),IODAOVER                                            
         XC    IODAOVER,IODAOVER                                                
         B     IOEX22                                                           
*                                                                               
IOEX16   ICM   R1,15,IOAREAD       R1=A(I/O AREA)                               
         BZ    IOEX20                                                           
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    0(L'IODA,R1),0(R1)                                               
*                                                                               
         OC    0(L'IODA,R1),0(R1)  D/A IN I/O AREA?                             
         BZ    *+10                                                             
         MVC   IODA,0(R1)          YES - SET D/A                                
         LA    R1,L'IODA(R1)                                                    
*                                                                               
IOEX18   OC    0(L'IOWORK,R1),0(R1) WORK IN I/O AREA?                           
         BZ    *+10                                                             
         MVC   IOWORK,0(R1)        YES - SET WORK                               
         LA    R1,L'IOWORK(R1)                                                  
*                                                                               
IOEX20   OC    IODA,IODA           TEST D/A PRESENT                             
         BNZ   IOEX22                                                           
*                                                                               
         TM    IOFILI,NFIIIS       TEST THIS IS A D/A FILE                      
         BNZ   *+14                                                             
         TM    IOFILI2,NFIIDI      AND THAT AN I/S FILE IS ATTACHED             
         BNZ   *+6                                                              
         DC    H'0'                WHAT IS IT THEN?                             
*                                                                               
         L     R1,IOCTRL                                                        
         ICM   R1,2,IOFILN2        GET LINKED DIRECTORY ENTRY                   
         GOTOX ('XIO',AGROUTS)     RECURSIVE DIRECTORY I/O FOR D/A              
         BE    IOEX22              SUCCESSFUL I/O                               
         BL    IOEXX               EXIT ON BAD I/S ERRORS                       
         TM    IOERR,IOERNF        TEST RECORD-NOT-FOUND                        
         BNZ   IOEXX                                                            
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    IOEXX                                                            
         OC    IODA,IODA           TEST DISK ADDRESS SET                        
         BNZ   *+6                                                              
         DC    H'0'                SOMETHING BAD HAPPENED                       
*                                                                               
IOEX22   ICM   R0,15,IOADDR        I/O AREA ADDRESS SET?                        
         BNZ   *+6                                                              
         DC    H'0'                NO I/O AREA ADDRESS                          
*                                                                               
*??      MVC   IOWFILE,IOFILNM     SET FILE NAME IN WORK AREA                   
*??      TM    IOINDS1,IOIDMP6     SET 6TH PARAMETER IF REQUIRED                
*??      BZ    *+10                                                             
*??      MVC   RTPARM+20(4),IODMP6                                              
*                                                                               
         GOTOX VDMGR,RTPARM,(IOQ,IOCMDNM),IOFILNM,IODA,(R0),IOWORK              
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         TM    IOERR,IOERRS-IOEDEL                                              
         BNZ   IOEXX                                                            
*                                                                               
         ICM   R1,15,IOAREAD       POINT TO I/O AREA                            
         BZ    IOEXX                                                            
         MVC   0(L'IODA,R1),IODA   MOVE IN D/A AND WORK                         
         LA    R1,L'IODA(R1)                                                    
         MVC   0(L'IOWORK,R1),IOWORK                                            
         B     IOEXX               EXIT TO CALLER                               
*                                                                               
IOEX24   TM    IOFILI,NFIIIS       TEST INDEX SEQUENTIAL FILE                   
         BZ    IOEX28                                                           
         MVC   IOKEYSAV,IOKEY      SAVE CURRENT I/O KEY                         
         LA    R0,IOKEY            FL I/S READS INTO IOKEY                      
         TM    IOFILI2,NFIIID      TEST I/S FILE HAS D/A ATTACHED               
         BNZ   IOEX26                                                           
         TM    IOFILI,NFIIVL       TEST I/S FILE IS V/L                         
         BZ    IOEX26                                                           
         ICM   R0,15,IOADDR        VL I/S MUST READ INTO IOAREA ALSO            
         BNZ   IOEX26                                                           
         DC    H'0'                NO IOAREA SET UP                             
*                                                                               
IOEX26   GOTOX VDMGR,RTPARM,(IOQ,IOCMDNM),IOFILNM,IOKEY,(R0)                    
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         TM    IOERR,IOERRS-IOEDEL                                              
         BNZ   IOEXX                                                            
*                                                                               
         ICM   R1,15,IOAREAD       POINT TO I/O AREA                            
         BZ    IOEXX                                                            
         XR    RF,RF                                                            
         IC    RF,IOFILKL          KEY LENGTH                                   
         XR    RE,RE                                                            
         IC    RE,IOFILCL          CONTROL LENGTH                               
         LA    RF,0(RE,RF)                                                      
         LA    RF,IOKEY(RF)        POINT TO DISK ADDRESS                        
         MVC   IODA,0(RF)                                                       
*                                                                               
         TM    IOFILI2,NFIIID      TEST D/A FILE ATTACHED TO THIS FILE          
         BZ    *+10                                                             
         MVC   0(L'IODA,R1),IODA                                                
         LA    R1,L'IODA(R1)       BUMP BY D/A LENGTH                           
         MVC   0(L'IOWORK,R1),IOWORK                                            
         B     IOEXX                                                            
*                                                                               
IOEX28   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOX VDMGR,BCPARM,(IOQ,IOCMDNM),IOFILNM,(R0),(R0)                     
         MVC   IOERR,8(R1)                                                      
         B     IOEXX                                                            
*                                                                               
IOEXX    TM    IOFLAG,IOFSWTCH     TEST SYSTEM SWITCH OCCURRED                  
         BZ    IOEXX2                                                           
         TM    IOINDS1,IO1XSWCH    TEST AUTO SWITCH BACK AFTER I/O              
         BO    IOEXX2                                                           
         GOTOX IOSWITCH,BCSWSYSP   SWITCH TO PREVIOUS SYSTEM                    
*                                                                               
IOEXX2   TM    IOERR,IOERRS                                                     
         BZ    EXITOK              NO ERRORS                                    
*                                                                               
         MVC   FVMSGNO,=AL2(FVFIOER)                                            
         TM    IOERR,IOEEOF+IOERNF+IOEDEL                                       
         BNZ   EXITH               SET IRRECOVERABLE ERROR                      
         B     EXITL               SET LOGICAL I/O ERROR                        
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SWITCH TO A SYSTEM                                       *         
*                                                                     *         
* NTRY - R1=A(LOGICAL SYSTEM NUMBER)                                  *         
* EXIT - CC=LOW   - USER NOT AUTHORISED FOR SYSTEM                    *         
*        CC=EQUAL - SWITCH SUCCESSFUL                                 *         
*        CC=HIGH  - SYSTEM NOT AVAILABLE (ONLINE ONLY)                *         
* NOTE - IF ERROR OCCURRS IOERR IS SET TO X'FF' WHICH WILL RETURN A   *         
*        CC OF HIGH FROM I/O ROUTINE WITH FVMSGNO SET TO FVFIOER. IT  *         
*        IS THE CALLER'S RESPONSIBILITY TO DEAL WITH THIS OTHERWISE   *         
*        A RANDOM DATAMGR ERROR WILL BE REPORTED.                     *         
***********************************************************************         
         SPACE 1                                                                
IOSWITCH CLC   BCSWSYSC,0(R1)      TEST SWITCHED TO CORRECT SYSTEM              
         BNE   IOSW02                                                           
         CLC   GCSWSE,IOFILSE      TEST CORRECT SE NUMBER                       
         BER   RE                                                               
*                                                                               
IOSW02   LR    R0,RE               SAVE RETURN ADDRESS                          
         MVC   IOBYTE,0(R1)        SAVE SYSTEM NUMBER REQUIRED                  
*                                                                               
         OC    IOFILSE,IOFILSE     OVERRIDE SYSTEM PASSED FOR SWITCH?           
         BZ    IOSW04                                                           
         CLC   BCSWSYSP,0(R1)      SWITCHING BACK TO PREV SYSTEM?               
         BNE   *+12                                                             
         MVI   IOFILSE,0           SET NO OVERRIDE SYSTEM THEN                  
         B     IOSW04                                                           
*                                                                               
         MVC   IOBYTE,IOFILSE                                                   
         LA    R1,IOFILSE                                                       
         B     IOSW10                                                           
*                                                                               
IOSW04   L     RE,ASWSTAB                                                       
         USING SYSSWTAB,RE         RE=A(SYSTEM SWITCH TABLE)                    
         LA    RF,SYSSWMAX                                                      
IOSW06   CLC   SYSSWSOV,IOBYTE     MATCH ON LOGICAL SYSTEM NUMBER               
         BNE   IOSW08                                                           
         LA    R1,SYSSWSYS         FOUND - POINT R1 TO ACTUAL SE NUMBER         
         CLI   IOFILSE,0           SPECIFIC USER SE NUMBER SUPPLIED?            
         BE    IOSW10                                                           
         CLC   SYSSWSYS,IOFILSE    MATCH ON USER SE NUMBER                      
         BE    IOSW10                                                           
*                                                                               
IOSW08   LA    RE,SYSSWLEN(RE)     BUMP TO NEXT SWITCH TABLE ENTRY              
         BCT   RF,IOSW06                                                        
         MVI   IOBYTE,0            SET CC=LOW FOR INVALID SYSTEM                
         B     IOSWX                                                            
*                                                                               
IOSW10   MVC   GCSWSE,IOFILSE      SAVE USER SUPPLIED SE NUMBER                 
         CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BNE   IOSW12                                                           
         ICM   RF,15,BCAUTL        YES - MOVE SE NUMBER TO UTL                  
         MVC   TSYS-UTLD(L'TSYS,RF),0(R1)                                       
         B     IOSW14                                                           
*                                                                               
IOSW12   MVC   RTPARM(1),0(R1)     SWITCH TO A SYSTEM                           
         MVC   RTPARM+1(3),=X'FFFFFF'                                           
         XC    RTPARM+4(4),RTPARM+4                                             
         GOTOX VSWITCH,RTPARM                                                   
         CLI   4(R1),0             TEST SWITCH SUCCESSFUL                       
         BE    IOSW14                                                           
         MVI   IOBYTE,2            SET CC=HIGH FOR CAN'T SWITCH                 
         B     IOSWX                                                            
*                                                                               
IOSW14   MVC   BCSWSYSP,BCSWSYSC   SAVE PREVIOUS SYSTEM NUMBER                  
         MVC   BCSWSYSC,IOBYTE     SAVE CURRENT SYSTEM NUMBER                   
         OI    IOFLAG,IOFSWTCH     SET SYSTEM SWITCH OCCURRED                   
         MVI   IOBYTE,1            SET CC=EQUAL FOR OK                          
*                                                                               
IOSWX    CLI   IOBYTE,1            SET CC FOR CALLER                            
         BE    *+8                                                              
         MVI   IOERR,X'FF'         SET ALL ERROR BITS ON                        
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  RE                                                               
         SPACE 1                                                                
IORTND   DSECT                 *** IO S/R LOCAL W/S ***                         
IODUB    DS    D                   GENERAL WORK AREA                            
IOAREAD  DS    A                   I/O AREA ADDRESS                             
IOCTRL   DS    F                   I/O COMMAND WORD                             
IOBYTE   DS    X                   I/O BYTE                                     
IOQ      DS    X                   I/O COMMAND QUALIFIER (RFU/DELETES)          
*                                                                               
IOFILV   DS    0XL28               EXTRACTED FILE VALUES (THIS I/O)             
IOFILNO  DS    XL1                 FILE NUMBER                                  
IOFILGLB EQU   14                  GLOBAL FILES ARE 14-15                       
IOFILOSE DS    XL1                 OVERLAY SYSTEM                               
IOFILSE  DS    XL1                 OVERRIDE SYSTEM                              
IOFILNM  DS    CL7                 FILE NAME                                    
IOFILI   DS    XL1                 FILE INDICATORS - 1                          
IOFILI2  DS    XL1                 FILE INDICATORS - 2                          
IOFILN2  DS    XL1                 FILE NUMBER 2 (I/S D/A PAIR)                 
IOFILKL  DS    XL1                 KEY LENGTH                                   
IOFILCL  DS    XL1                 CONTROL LENGTH                               
IOFILDE  EQU   IOFILCL             DISPLACEMENT TO FIRST ELEMENT                
IOFILML  DS    XL2                 MAXIMUM RECORD LENGTH                        
         DS    XL3                                                              
         DS    XL8                                                              
*                                                                               
IOCMDV   DS    0XL10               EXTRACTED COMMAND VALUES (THIS I/O)          
IOCMDNM  DS    CL7                 COMMAND NAME                                 
IOCMDNO  DS    X                   COMMAND NUMBER                               
IOCMDI   DS    X                   COMMAND INDICATORS - 1                       
IOCMDI2  DS    X                   COMMAND INDICATORS - 2                       
IOSWSYS  DS    XL1                 SWITCH SYSTEM NUMBER                         
IOSWFIL  DS    XL1                 SWITCH FILE NUMBER                           
IOSWSYSN DS    CL3                 SWITCH SYSTEM NAME                           
IOWORKX  EQU   *                                                                
*                                                                               
GEFIL00  CSECT                                                                  
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXIT POINT IN ROOT                                       *         
***********************************************************************         
         SPACE 1                                                                
GCEXIT   L     RD,BCSVRD           RETURN TO ROOT                               
         LM    RF,RC,16(RD)                                                     
         L     RE,GCAEXIT                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST AUTHORISATION FOR A RECORD OR RECORD/ACTION COMBO   *         
***********************************************************************         
         SPACE 1                                                                
TSTAC    LR    RF,R1                                                            
         OC    TWALEN,TWALEN       TEST NEW SECURITY IN USE                     
         BZ    EXITOK                                                           
         CLI   L'CSREC(RF),0       TEST ACTION SPECIFIED                        
         BE    TSTAC02                                                          
         GOTOX VSECRET,BCPARM,('SECPRACT',ASECBLK),(0(RF),1(RF))                
         B     TSTACX                                                           
*                                                                               
TSTAC02  GOTOX VSECRET,BCPARM,('SECPRCD',ASECBLK),(RF)                          
*                                                                               
TSTACX   BNE   EXITH                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET USER-ID CODE INTO BCWORK                             *         
*                                                                     *         
* NTRY - R1=A(USER-ID NUMBER)                                         *         
* EXIT - BCWORK=USER-ID CODE                                          *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING GUIDD,RTWORK                                                     
GUID     XC    BCWORK,BCWORK                                                    
         MVC   GUIDKEY,IOKEY       SAVE CURRENT KEY                             
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   RTHALF,0(R1)                                                     
         MVC   CTIKNUM,0(R1)                                                    
         L     R2,AIO1                                                          
         CLC   IOKEY(L'CTIKEY),0(R2)                                            
         BE    GUID02                                                           
*                                                                               
         L     R1,=AL4(XOREAD+XOCONFIL+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    GUID02                                                           
*                                                                               
         MVI   BCWORK,C'#'                                                      
         XR    R0,R0                                                            
         ICM   R0,3,RTHALF                                                      
         CURED (R0),(5,BCWORK+1),0,DMCB=BOPARM,ALIGN=LEFT,ZERO=NOBLANK          
         B     GUIDX                                                            
*                                                                               
GUID02   LA    R1,CTIDATA                                                       
         XR    RE,RE                                                            
         USING CTDSCD,R1                                                        
*                                                                               
GUID04   CLI   CTDSCEL,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+16                                                             
         IC    RE,CTDSCLEN                                                      
         LA    R1,0(RE,R1)                                                      
         B     GUID04                                                           
*                                                                               
         IC    RE,CTDSCLEN                                                      
         SH    RE,=Y(CTDSC-CTDSCD+1)                                            
         MVC   BCWORK(0),CTDSC                                                  
         EX    RE,*-6                                                           
GUIDX    MVC   IOKEY,GUIDKEY                                                    
         B     EXITOK                                                           
*                                                                               
GUIDD    DSECT                 *** GETUID S/R LOCAL W/S ***                     
GUIDKEY  DS    XL(L'IOKEY)         SAVE IOKEY AREA                              
*                                                                               
GEFIL00  CSECT                                                                  
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE USER-ID NAME                                    *         
*                                                                     *         
* NTRY: R1=A(NAME)                                                    *         
* EXIT: BCWORK=NUMBER                                                 *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
VLUID    LA    R2,IOKEY                                                         
         USING CTIREC,R2           R2=A(ID RECORD)                              
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,0(R1)                                                     
         L     R1,=AL4(XOCONFIL+XORD+XIO2)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXIT                                                             
*                                                                               
         L     R2,AIO2                                                          
         LA    RF,CTIDATA                                                       
         USING CTDSCD,RF           RF=A(DESCRIPTION ELEMENT)                    
         XR    RE,RE                                                            
VUID02   IC    RE,CTDSCLEN                                                      
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+8                                                              
         BXH   RF,RE,VUID02                                                     
         XC    BCWORK,BCWORK                                                    
         SH    RE,=Y(CTDSC+1-CTDSCD)                                            
         MVC   BCWORK(0),CTDSC                                                  
         EX    RE,*-6                                                           
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET PUBLIC-ID INTO BCWORK                                *         
*                                                                     *         
* NTRY - R1=A(PASSWORD NUMBER)                                        *         
* EXIT - BCWORK=PUBLIC-ID                                             *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING GPWORKD,RTWORK                                                   
GTPID    MVC   GPIOKEY,IOKEY                                                    
         MVI   BCWORK,C'?'                                                      
         MVC   BCWORK+1(L'SAPALPID-1),BCWORK                                    
*                                                                               
         LA    R2,IOKEY                                                         
         USING SA0REC,R2           BUILD KEY OF RECORD                          
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,CUASEC                                                   
         MVC   SA0KNUM,0(R1)                                                    
         L     R1,=AL4(XOREAD+XOCONFIL+XIO3)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   GETPIDX                                                          
*                                                                               
         L     R2,AIO3                                                          
         LA    R2,SA0DATA                                                       
         USING SAPALD,R2                                                        
         XR    R0,R0                                                            
*                                                                               
GPID02   CLI   SAPALEL,0           TEST EOR                                     
         BE    GETPIDX                                                          
         CLI   SAPALEL,SAPALELQ    TEST PERSON PERSONAL-ID ELEMENT              
         BE    *+14                                                             
         IC    R0,SAPALLN                                                       
         AR    R2,R0                                                            
         B     GPID02                                                           
         MVC   BCWORK(L'SAPALPID),SAPALPID                                      
*                                                                               
GETPIDX  MVC   IOKEY,GPIOKEY                                                    
         B     EXITOK                                                           
         SPACE 1                                                                
GPWORKD  DSECT                 *** GETPID S/R LOCAL W/S ***                     
GPIOKEY  DS    XL(L'IOKEY)         SAVE IOKEY AREA                              
GEFIL00  CSECT                                                                  
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A PERSONAL-ID                                   *         
*                                                                     *         
* NTRY - R1=A(PERSON-ID)                                              *         
* EXIT - BCWORK=PASSWORD NUMBER                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING VPWORKD,RTWORK                                                   
VPID     MVC   VPIOKEY,IOKEY                                                    
         LA    R2,IOKEY                                                         
         USING SAPEREC,R2          BUILD KEY OF RECORD                          
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,CUASEC                                                   
         MVC   SAPEPID,0(R1)                                                    
         L     R1,=AL4(XOHI+XOCONFIL+XIO3)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,VPIOKEY                                                    
         L     R2,AIO3                                                          
         CLC   SAPEREC(SAPEDEF-SAPEREC),IOKEYSAV                                
         BNE   VPIDL                                                            
         MVC   BCWORK,BCSPACES                                                  
*                                                                               
         LA    R2,SAPEDATA                                                      
         USING SAPWDD,R2                                                        
         SR    R0,R0                                                            
VPID02   CLI   SAPWDEL,0           TEST EOR                                     
         BE    VPID14                                                           
         CLI   SAPWDEL,SAPWDELQ    TEST PASSWORD ELEMENT                        
         BE    VPID06                                                           
         CLI   SAPWDEL,SANAMELQ    TEST PERSON NAME ELEMENT                     
         BE    VPID08                                                           
VPID04   IC    R0,SAPWDLN                                                       
         AR    R2,R0                                                            
         B     VPID02                                                           
*                                                                               
VPID06   MVC   BCWORK(L'SAPWDNUM),SAPWDNUM                                      
         B     VPID04                                                           
*                                                                               
         USING SANAMD,R2                                                        
VPID08   LR    R3,R2                                                            
NAME     USING SANAMD,R3                                                        
         TM    SANAMIND,SANAMIFN                                                
         BZ    VPID10                                                           
         XR    R1,R1                                                            
         IC    R1,SANAMELN                                                      
         SHI   R1,1                SUBTRACT ONE FOR EX                          
         MVC   BCWORK+2(0),SANAME                                               
         EX    R1,*-6                                                           
         AHI   R1,2                BUMP TO THE NEXT                             
         AR    R3,R1                                                            
VPID10   TM    SANAMIND,SANAMIMN                                                
         BZ    VPID12                                                           
         XR    R1,R1                                                            
         IC    R1,NAME.SANAMELN                                                 
         CHI   R1,16               CHECK NAME LONGER THAN 16 BYTES              
         BNH   *+8                 NO                                           
         LA    R1,16               YES - SET 16 AS MAX LENGTH                   
         SHI   R1,1                SUBTRACT ONE FOR EX                          
         MVC   BCWORK+42(0),NAME.SANAME                                         
         EX    R1,*-6                                                           
         AHI   R1,2                BUMP TO THE NEXT                             
         AR    R3,R1                                                            
VPID12   TM    SANAMIND,SANAMILN                                                
         BZ    VPID04                                                           
         XR    R1,R1                                                            
         IC    R1,NAME.SANAMELN                                                 
         SHI   R1,1                SUBTRACT ONE FOR EX                          
         MVC   BCWORK+22(0),NAME.SANAME                                         
         EX    R1,*-6                                                           
         B     VPID04                                                           
         DROP  NAME                                                             
*                                                                               
VPID14   CLC   BCWORK(L'SAPWDNUM),BCSPACES                                      
         BE    VPIDL                                                            
         B     EXITOK                                                           
*                                                                               
VPIDL    MVC   FVMSGNO,=AL2(CE#RECNF)                                           
         MVI   FVOSYS,QSCON                                                     
         B     EXITH                                                            
         DROP  R2                                                               
         SPACE 1                                                                
VPWORKD  DSECT                     ** VALPID S/R LOCAL W/S **                   
VPIOKEY  DS    XL(L'IOKEY)         SAVE IOKEY AREA                              
         SPACE 1                                                                
GEFIL00  CSECT                                                                  
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT AND PRE-VALIDATE A TWA INPUT FIELD               *         
*                                                                     *         
* NTRY - R1=A(TWA FIELD HEADER)                                       *         
*        FVMINL=MINIMUM INPUT FIELD LENGTH (ZERO=OPTIONAL FIELD)      *         
*        FVMAXL=MAXIMUM INPUT FIELD LENGTH (ZERO=MAXIMUM LENGTH)      *         
*        FVXTRA=NARRATIVE TO BE ATTACHED TO ERROR IF FIELD IS INVALID *         
*                                                                     *         
* EXIT - FVADDR=A(TWA FIELD HEADER)                                   *         
*        FVINDX=ZERO                                                  *         
*        FVSUBX=ZERO                                                  *         
*        FVMINL=ZERO                                                  *         
*        FVMAXL=ZERO                                                  *         
*        FVXTRA=SPACES                                                *         
*        FVIHDR=EXTRACTED INPUT FIELD HEADER (SEE FVIHDR IN WORKD)    *         
*        FVIFLD=EXTRACTED & SPACE FILLED INPUT FIELD                  *         
*        FVFIELD=SPECIAL FIELD TYPE (RECORD, ACTION ETC.)             *         
*        FVMSGNO=SET TO STANDARD ERROR NUMBER (SEE FVMSGNO EQUATES)   *         
*        CC=LOW IF FIELD IS NOT INPUT                                 *         
*        CC=EQUAL IF FIELD IS INPUT AND VALID                         *         
*        CC=HIGH IF INPUT TOO SHORT/LONG ETC.                         *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
FVL      MVI   FVINDS,0                                                         
         XC    BCFULL,BCFULL                                                    
         XC    BCDUB,BCDUB                                                      
         LTR   R1,R1               TEST A(TWA FIELD HEADER) PASSED              
         BNZ   *+10                                                             
         LR    RF,R0               SET FIELD LENGTH IN RF                       
         B     FVAL04                                                           
*                                                                               
         ST    R1,FVADDR           SET A(INPUT FIELD HEADER)                    
         MVI   FVINDX,0            RESET INDEX & SUB-INDEX VALUES               
         MVI   FVSUBX,0                                                         
         MVI   FVOMTYP,0           RESET MESSAGE TYPE                           
         MVI   FVIFLD,C' '                                                      
         MVC   FVIFLD+1(L'FVIFLD-1),FVIFLD                                      
*                                                                               
         MVC   FVIHDR,0(R1)        EXTRACT FIELD HEADER                         
         XC    FVIXHDR,FVIXHDR                                                  
         SR    RF,RF                                                            
         IC    RF,FVTLEN                                                        
         LA    R0,L'FVIHDR+1                                                    
         TM    FVATRB,FVAXTND                                                   
         BZ    FVAL02                                                           
         LA    RE,1(R1,RF)                                                      
         SR    RE,R0                                                            
         MVC   FVIXHDR,0(RE)       COPY EXTENDED HEADER                         
         LA    R0,L'FVIHDR+L'FVIHDR+1                                           
*                                                                               
FVAL02   SR    RF,R0               RF=MAXIMUM INPUT LENGTH-1                    
         BNM   *+6                                                              
         DC    H'0'                THIS IS A BAD TWA FIELD                      
         MVC   FVIFLD(0),L'FVIHDR(R1)                                           
         EX    RF,*-6              EXTRACT FIELD DATA                           
*                                                                               
FVAL04   LA    R1,FVIFLD(RF)       R1=A(END OF INPUT FIELD)                     
         LA    RF,1(RF)            RF=LOOP COUNT                                
FVAL06   CLI   0(R1),C' '          LOCATE LAST INPUT CHARACTER IN FIELD         
         BH    FVAL08                                                           
         MVI   0(R1),C' '          SET FUNNIES TO SPACES                        
         BCTR  R1,0                                                             
         BCT   RF,FVAL06                                                        
*                                                                               
FVAL08   STC   RF,FVILEN           SET ACTUAL INPUT LENGTH                      
         MVC   FVMSGNO,=AL2(FVFSHRT) ENSURE NOT TOO SHORT OR LONG               
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         CLM   RF,1,FVMINL                                                      
         BL    FVAL20                                                           
         CLI   FVMAXL,0            IF FVMAXL=ZERO DON'T TEST LONG               
         BE    *+18                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         CLM   RF,1,FVMAXL                                                      
         BH    FVAL20                                                           
         NI    FVIIND,FF-FVINUM-FVIALF-FVIHEX                                   
         LTR   RF,RF               EXIT IF NO INPUT IN FIELD                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     FVAL18                                                           
*                                  SET FIELD VALIDITY BITS                      
         MVC   FVMSGNO,=AL2(FVFOK) INDICATE FIELD IS OK                         
         OI    FVIIND,FVINUM+FVIALF+FVIHEX                                      
FVAL10   TM    FVIIND,FVINUM+FVIALF+FVIHEX                                      
         BZ    FVAL14                                                           
         CLI   0(R1),C'A'                                                       
         BNL   *+12                                                             
         NI    FVIIND,FF-FVINUM-FVIALF-FVIHEX                                   
         B     FVAL12                                                           
         CLI   0(R1),C'Z'                                                       
         BNH   *+12                                                             
         NI    FVIIND,FF-FVIALF                                                 
         B     FVAL12                                                           
         NI    FVIIND,FF-FVINUM                                                 
         CLI   0(R1),C'F'                                                       
         BNH   *+8                                                              
         NI    FVIIND,FF-FVIHEX                                                 
*                                                                               
FVAL12   BCTR  R1,0                                                             
         BCT   RF,FVAL10                                                        
*                                                                               
FVAL14   IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         STC   RF,FVXLEN           SET EXECUTE LENGTH (INPUT LENGTH-1)          
         TM    FVIIND,FVINUM                                                    
         BZ    FVAL16                                                           
         CLI   FVILEN,8            TEST INPUT NOT LONGER THAN 8 BYTES           
         BNH   *+12                                                             
         NI    FVIIND,FF-FVINUM                                                 
         B     FVAL16                                                           
         EX    RF,*+8              SET PACKED/BINARY NUMERIC VALUES             
         B     *+10                                                             
         PACK  BCDUB,FVIFLD(0)                                                  
         CVB   R0,BCDUB                                                         
         ST    R0,BCFULL                                                        
*                                                                               
FVAL16   MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
FVAL18   MVI   FVMINL,0                                                         
         MVI   FVMAXL,0                                                         
         MVI   FVXTRA,C' '                                                      
         MVC   FVXTRA+1(L'FVXTRA-1),FVXTRA                                      
*                                                                               
FVAL20   MVI   FVFIELD,0                                                        
         CLC   FVMSGNO,=AL2(FVFNONE)                                            
         BE    FVAL24                                                           
         MVI   FVFLAG,0                                                         
         CLI   FVILEN,0                                                         
         BE    FVALX                                                            
         MVI   FVFLAG,1                                                         
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    FVALX                                                            
*                                                                               
FVAL24   MVI   FVFLAG,2                                                         
*                                                                               
FVALX    CLI   FVFLAG,1            SET CONDITION CODE FOR CALLER                
         MVI   FVFLAG,0                                                         
         B     EXIT                RETURN TO CALLER                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* TEST SECURITY FOR A FIELD                                           *         
*                                                                     *         
* NTRY - R1=A(FIELD SECURITY NUMBER)                                  *         
* EXIT - CC=EQUAL IF FIELD VALID FOR READ AND WRITE                   *         
*        CC=HIGH IF FIELD VALID FOR READ ONLY                         *         
*        CC=LO  IF FIELD INVALID FOR READ AND WRITE                   *         
***********************************************************************         
         SPACE 1                                                                
FSEC     CLI   0(R1),0                                                          
         BE    EXITOK                                                           
         LR    RF,R1                                                            
         GOTOX VSECRET,BCPARM,('SECPFLDP',ASECBLK),(RF)                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET TERMINAL NAME FROM NUMBER                            *         
*         OR GET TERMINAL NUMBER FROM NAME                            *         
*                                                                     *         
* NTRY: P1=(0,A(TERMINAL FILE NUMBER))                                *         
*    OR P1=(1,A(TERMINAL NAME))                                       *         
* EXIT: BCWORK=TERMINAL NAME/NUMBER                                   *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
GTRM     XR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         XR    R3,R3                                                            
         ICM   R3,7,1(R1)                                                       
         LA    R2,IOKEY                                                         
         USING CTTREC,R2                                                        
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,CTTKTYPQ                                                 
         LTR   R0,R0                                                            
         BZ    *+14                                                             
         MVC   CTTKTID,0(R3)                                                    
         B     *+10                                                             
         MVC   CTTKPASS+8(2),0(R3)                                              
         L     R1,=AL4(XOCONFIL+XORD+XIO2)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL                                                            
*                                                                               
GTRM02   L     R2,AIO2                                                          
         LA    RF,CTTDATA                                                       
         USING CTPASD,RF           RF=A(PASSIVE POINTER ELEMENT)                
         XR    RE,RE                                                            
*                                                                               
GTRM04   IC    RE,CTPASLEN                                                      
         CLI   CTPASEL,CTPASELQ                                                 
         BE    *+8                                                              
         BXH   RF,RE,GTRM04                                                     
*                                                                               
         XC    BCWORK,BCWORK                                                    
         SH    RE,=Y(CTPASDTA+1-CTPASD)                                         
         MVC   BCWORK(0),CTPASDTA                                               
         EX    RE,*-6                                                           
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A DICTIONARY EQUATE                             *         
*                                                                     *         
* P1 = SYSTEM                                                         *         
* RETURNS DICTIONARY NUMBER IN BCHALF                                 *         
* RETURNS DICTIONARY REFERENCE IN BCFULL                              *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING DWORKD,RTWORK                                                    
VDIC     L     R2,0(R1)                                                         
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
         XC    BCFULL,BCFULL                                                    
         GOTOX VSCANNER,BOPARM,FVADDR,(3,MYSCAN)                                
         MVC   NOFLD,4(R1)                                                      
*                                                                               
         CLI   NOFLD,2                                                          
         BNL   *+18                NOT ENOUGH INPUT FIELDS                      
         MVC   FVMSGNO,=AL2(AE$TFPRM)                                           
         MVI   FVOSYS,QSACC        ACCOUNT SYSTEM                               
         B     VDICL                                                            
         CLI   NOFLD,4                                                          
         BNH   *+18                TOO MANY INPUT FIELDS                        
         MVC   FVMSGNO,=AL2(AE$TMPRM)                                           
         MVI   FVOSYS,QSACC        ACCOUNT SYSTEM                               
         B     VDICL                                                            
*                                                                               
         LA    R3,MYSCAN                                                        
         USING SCANBLKD,R3                                                      
         CLI   FVIFLD,C'&&'        FOR ADDING TAGS ONTO SCREENS                 
         BNE   VDIC02                                                           
         MVC   FVIFLD(L'FVIFLD+1),FVIFLD+1                                      
         MVC   SC1STFLD(L'SC1STFLD-1),SC1STFLD+1                                
         XR    RE,RE                                                            
         IC    RE,SC1STLEN                                                      
         BCTR  RE,0                                                             
         STC   RE,SC1STLEN                                                      
*                                                                               
VDIC02   CLC   =C'GE#',FVIFLD      OVERRIDING WITH GEN SYSTEM EQUATE?           
         BE    VDIC06              (MUST BE SET EXPLICITLY)                     
*                                                                               
         LR    RF,R2               R2 = SYSTEM                                  
         MH    RF,=Y(3)                                                         
         LA    RF,PFXTABLE(RF)     RF=PFXTABLE+(SYSTEM * 3)                     
         MVC   BCFULL(2),0(RF)                                                  
         MVI   BCFULL+2,C'#'       BCFULL(3)=DICTIONARY EQUATE PREFIX           
*                                                                               
         CLC   FVIFLD(3),BCFULL    TEST USER ENTERED PREFIX                     
         BE    VDIC06                                                           
*                                                                               
         CLI   SC1STLEN,5          NO - SEE IF CAN INSERT IT                    
         BNH   VDIC04                                                           
         MVC   FVMSGNO,=AL2(CE#EQPRE)                                           
         MVI   FVOSYS,QSCON        CONTROL SYSTEM                               
         MVC   FVXTRA(3),BCFULL                                                 
         B     VDICL                                                            
*                                                                               
VDIC04   MVC   SC2NDFLD,SC1STFLD   SAVE THE INPUT                               
         MVC   SC1STFLD(3),BCFULL  PREFIX INPUT WITH THE PREFIX                 
         MVC   SC1STFLD+3(5),SC2NDFLD                                           
         XR    RE,RE                                                            
         IC    RE,SC1STLEN         BUMP LENGTH                                  
         LA    RE,3(RE)                                                         
         STC   RE,SC1STLEN                                                      
*                                                                               
         PUSH  USING                                                            
         USING GMSGD,IOKEY                                                      
VDIC06   XC    GQKEY,GQKEY         READ EQUATE NAME PASSIVE                     
         MVI   GQKREC,GQKRECQ                                                   
         MVC   GQKQNAME,SC1STFLD                                                
         L     R1,=AL4(XOHI+XOGENDIR+XIO1)                                      
         GOTOX ('XIOC',AGROUTS)                                                 
         BNE   *+14                                                             
         CLC   GQKEY(GQKMNUM-GQKEY),IOKEYSAV                                    
         BE    VDIC08                                                           
         MVI   FVOSYS,QSCON        CONTROL SYSTEM                               
         MVC   FVMSGNO,=AL2(CE#EQNOF)                                           
         B     VDICL                                                            
*                                                                               
VDIC08   MVC   BCHALF,GQKMNUM      RETURN REFERENCE # IN BCHALF                 
         MVC   BCFULL+1(L'GQKMNUM),GQKMNUM AND SAVE IT FOR DD REFERENCE         
         POP   USING                                                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         LA    RF,1(RF)            FOR THE COMMA                                
         STC   RF,CURLEN           SET CURRENT DISPLACEMENT INTO FIELD          
         LA    R3,SCBLKLQ(R3)                                                   
*                                                                               
         TM    SC1STVAL,SCNUMQ     IS THIS FIELD NUMERIC?                       
         BO    *+20                                                             
         MVC   FVERRNDX,CURLEN                                                  
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VDICL                                                            
*                                                                               
         L     RF,SC1STNUM         LENGTH CONSTRAINED 2<LEN<80                  
         CH    RF,=H'80'                                                        
         BNH   *+24                                                             
         MVC   FVERRNDX,CURLEN     TOO LARGE                                    
         MVC   FVMSGNO,=AL2(AE$FLVTL)                                           
         MVI   FVOSYS,QSACC        ACCOUNT SYSTEM                               
         B     VDICL                                                            
*                                                                               
         CH    RF,=H'2'                                                         
         BNL   *+24                                                             
         MVC   FVERRNDX,CURLEN     TOO SMALL                                    
         MVC   FVMSGNO,=AL2(AE$FLVTS)                                           
         MVI   FVOSYS,QSACC        ACCOUNT SYSTEM                               
         B     VDICL                                                            
*                                                                               
         STC   RF,ESCLEN           SAVE THE LENGTH                              
         STC   RF,BCFULL+3         PUT IN THE DD REFERENCE LENGTH               
*                                                                               
         XR    RF,RF                                                            
         IC    RF,SC1STLEN         LENGTH OF THIS INPUT                         
         XR    RE,RE                                                            
         IC    RE,CURLEN           LENGTH OF VALIDATED INPUT                    
         LA    RF,1(RE,RF)         +1 FOR THE COMMA                             
         STC   RF,CURLEN           SET CURRENT DISPLACEMENT INTO FIELD          
*                                                                               
         CLI   NOFLD,3             SETTING ALIGNMENT?                           
         BNL   VDIC10              YES                                          
         CLI   ESCLEN,2            DEFAULT ALIGNMENT IS LEFT                    
         BNE   *+8                                                              
         MVI   BCFULL,DD#ESCL2     DD LENGTH=2                                  
         CLI   ESCLEN,3                                                         
         BNE   *+8                                                              
         MVI   BCFULL,DD#ESCL3     DD LENGTH=3                                  
         BNH   *+8                                                              
         MVI   BCFULL,DD#ESCL      DD LENGTH>3                                  
         B     VDICE                                                            
*                                                                               
VDIC10   LA    R3,SCBLKLQ(R3)                                                   
         CLC   =C'COLOUR',SC1STFLD DEFAULT ALIGNMENT REQUIRED                   
         BE    *+14                                                             
         CLC   =C'HIGH',SC1STFLD                                                
         BNE   VDIC12                                                           
*                                                                               
         MVI   BCFULL,DD#ESCL                                                   
         CLI   ESCLEN,3                                                         
         BNE   *+8                                                              
         MVI   BCFULL,DD#ESCL3                                                  
         CLI   ESCLEN,2                                                         
         BNE   *+8                                                              
         MVI   BCFULL,DD#ESCL2                                                  
         B     VDICE                                                            
*                                                                               
VDIC12   CLI   SC1STLEN,2          ONLY 2 CHARACTERS MAX ALLOWED                
         BNH   *+20                                                             
         MVC   FVERRNDX,CURLEN     NOT VALID IF >2 CHARS                        
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VDICL                                                            
*                                                                               
         CLI   ESCLEN,3            ORDINARY DD EQUATE                           
         BH    VDIC20              YES                                          
         BL    VDIC16              LENGTH 2?                                    
*                                                                               
         CLC   =C'LU',SC1STFLD     IS IT UNDERLINED?                            
         BNE   *+12                NO                                           
         MVI   BCFULL,DD#ESUL3                                                  
         B     VDICE                                                            
*                                                                               
         CLC   =C'L ',SC1STFLD     IS IT LEFT ALIGNED?                          
         BE    VDIC14                                                           
         CLC   =C'  ',SC1STFLD     DEFAULT IS LEFT ALIGNED                      
         BE    VDIC14                                                           
         MVC   FVERRNDX,CURLEN     NOT VALID                                    
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VDICL                                                            
*                                                                               
VDIC14   MVI   BCFULL,DD#ESCL3                                                  
         B     VDICE                                                            
*                                                                               
VDIC16   CLI   ESCLEN,2            LENGTH 2?                                    
         BE    *+6                                                              
         DC    H'0'                SHOULD HAVE TRAPPED THIS ABOVE               
*                                                                               
         CLC   =C'LU',SC1STFLD     IS IT UNDERLINED?                            
         BNE   *+12                NO                                           
         MVI   BCFULL,DD#ESUL2                                                  
         B     VDICE                                                            
*                                                                               
         CLC   =C'L ',SC1STFLD     IS IT LEFT ALIGNED?                          
         BE    VDIC18                                                           
         CLC   =C'  ',SC1STFLD     DEFAULT IS LEFT ALIGNED                      
         BE    VDIC18                                                           
         MVC   FVERRNDX,CURLEN     NOT VALID                                    
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VDICL                                                            
*                                                                               
VDIC18   MVI   BCFULL,DD#ESCL2                                                  
         B     VDICE                                                            
*                                                                               
VDIC20   LA    RF,DDTAB                                                         
         USING DDTABD,RF                                                        
         XR    RE,RE                                                            
         IC    RE,SC1STLEN                                                      
         BCTR  RE,0                                                             
*                                                                               
VDIC22   CLI   DEQU,EOT            NO MATCH ON CHARACTERS                       
         BNE   *+20                                                             
         MVC   FVERRNDX,CURLEN     NOT VALID                                    
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VDICL                                                            
*                                                                               
         EX    RE,*+8              TRY TO MATCH ON INPUT                        
         BE    VDIC24                                                           
         CLC   DCHAR(0),SC1STFLD                                                
         LA    RF,DDTABL(RF)                                                    
         B     VDIC22                                                           
*                                                                               
VDIC24   MVC   BCFULL(L'DEQU),DEQU SET EQUATE                                   
         B     VDICE                                                            
*                                                                               
VDICE    GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     EXITOK                                                           
*                                                                               
VDICL    GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     EXITL                                                            
*                                                                               
DWORKD   DSECT                                                                  
NOFLD    DS    XL1                 NUMBER OF SCANNED FIELDS                     
ESCLEN   DS    XL1                                                              
CURLEN   DS    XL1                                                              
MYSCAN   DS    3CL(SCBLKLQ)                                                     
*                                                                               
DDTABD   DSECT                                                                  
DEQU     DS    XL1                                                              
DCHAR    DS    XL2                                                              
DDTABL   EQU   *-DDTABD                                                         
*                                                                               
GEFIL00  CSECT                                                                  
DDTAB    DC    AL1(DD#ESCL),CL2'L '                                             
         DC    AL1(DD#ESCL),CL2'  '      DEFAULT IS LEFT ALIGNED                
         DC    AL1(DD#ESUL),CL2'LU'                                             
         DC    AL1(DD#ESCR),CL2'R '                                             
         DC    AL1(DD#ESUR),CL2'RU'                                             
         DC    AL1(DD#ESCC),CL2'C '                                             
         DC    AL1(DD#ESUC),CL2'CU'                                             
         DC    AL1(DD#ESCF),CL2'F '                                             
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY A DICTIONARY REFERENCE                           *         
*                                                                     *         
* P1 = SYSTEM                                                         *         
* BCFULL HOLDS DICTIONARY REFERENCE ON ENTRY                          *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING GMSGD,IOKEY                                                      
DDIC     L     R2,0(R1)            SYSTEM                                       
         XC    GMKEY,GMKEY                                                      
         MVI   GMKREC,GMKRECQ                                                   
         MVI   GMKTYP,GMKTGDIC                                                  
         STC   R2,GMKSYS           SET UP THE SYSTEM                            
         XR    RF,RF                                                            
         ICM   RF,3,BCFULL+1       GET DICTIONARY NUMBER                        
         TM    BCFULL+1,X'40'      IS IT GENERAL?                               
         BZ    *+12                                                             
         SH    RF,=Y(GE#GEN)                                                    
         MVI   GMKSYS,GENSYS       MOVE IN GENERAL SYSTEM NUMBER                
*                                                                               
         STCM  RF,3,GMKMSG         SAVE DICTIONARY NUMBER                       
         L     R1,=AL4(XOHI+XOGENDIR+XIO1)                                      
         GOTOX ('XIOC',AGROUTS)                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   GMKEY(GMKLANG-GMKEY),IOKEYSAV                                    
         BE    DDIC02                                                           
         XR    RF,RF                                                            
         ICM   RF,3,BCFULL+1                                                    
         CURED (RF),(6,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                         
         B     EXITOK                                                           
*                                                                               
DDIC02   L     R1,=AL4(XOGET+XOGENFIL+XIO1)                                     
         GOTOX ('XIOC',AGROUTS)                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO1                                                          
         LA    R3,GMFIRST(R1)                                                   
         USING GMQSYD,R3                                                        
         XR    RF,RF                                                            
         CLI   GMQSYEL,GMQSYELC                                                 
         BE    *+14                                                             
         IC    RF,GMQSYELL                                                      
         BXH   R3,RF,*-12                                                       
         DC    H'0'                                                             
*                                                                               
         MVC   FVIFLD(L'GMQSYSYM),GMQSYSYM                                      
         LA    R3,FVIFLD+L'GMQSYSYM                                             
         LA    R0,L'GMQSYSYM                                                    
         CLI   0(R3),C' '          FIND THE FIRST NON-SPACE                     
         BH    *+10                                                             
         BCTR  R3,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         LA    R3,1(R3)                                                         
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         XR    RF,RF                                                            
         IC    RF,BCFULL+3         GET THE LENGTH OF THE DDICT REF.             
         CURED (RF),(3,(R3)),0,DMCB=BOPARM,ALIGN=LEFT                           
         AR    R3,R0               LENGTH RETURNED IN R0                        
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         XR    RF,RF                                                            
         IC    RF,BCFULL+3         GET THE LENGTH OF THE DDICT REF.             
         CLM   RF,1,=AL1(3)        ORDINARY DD EQUATE                           
         BH    DDIC06              YES                                          
         BL    DDIC04              LENGTH 2?                                    
*                                                                               
         CLI   BCFULL,DD#ESUL3     LENGTH 3 & UNDERLINED?                       
         BNE   *+14                NO                                           
         MVC   0(2,R3),=C'LU'                                                   
         B     DDICE                                                            
         CLI   BCFULL,DD#ESCL3     LENGTH 3 & LEFT ALIGNED                      
         BNE   *+14                                                             
         MVC   0(2,R3),=C'L '                                                   
         B     DDICE                                                            
         DC    H'0'                                                             
*                                                                               
DDIC04   CLI   BCFULL,DD#ESUL2     LENGTH 2 & UNDERLINED?                       
         BNE   *+14                NO                                           
         MVC   0(2,R3),=C'LU'                                                   
         B     DDICE                                                            
         CLI   BCFULL,DD#ESCL2     LENGTH 2 & LEFT ALIGNED                      
         BNE   *+14                                                             
         MVC   0(2,R3),=C'L '                                                   
         B     DDICE                                                            
         DC    H'0'                                                             
*                                                                               
DDIC06   LA    RF,DDTAB                                                         
         USING DDTABD,RF                                                        
DDIC08   CLI   DEQU,EOT            NO MATCH ON CHARACTERS                       
         BE    DDICL                                                            
         CLC   DEQU,BCFULL                                                      
         BE    *+12                                                             
         LA    RF,DDTABL(RF)                                                    
         B     DDIC08                                                           
*                                                                               
         MVC   0(L'DCHAR,R3),DCHAR                                              
         B     DDICE                                                            
*                                                                               
DDICE    GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     EXITOK                                                           
*                                                                               
DDICL    GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     EXITL                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE WHEN FIELD                                                 *         
*                                                                     *         
* NTRY - R1=A(WHEN TWA FIELD HEADER)                                  *         
* EXIT - CC=NOT EQUAL ON ERROR ELSE INWHEN CONTAINS WHEN VALUE        *         
***********************************************************************         
         SPACE 1                                                                
VWHEN    XR    R2,R2                                                            
         IC    R2,FVXLEN           R2=L'INPUT-1                                 
         CLI   FVXLEN,WHENDDL                                                   
         BL    *+8                                                              
         LA    R2,WHENDDL                                                       
*                                                                               
         LA    R3,WHENTAB          R3=A(WHEN TABLE)                             
         USING WHENTABD,R3                                                      
VWHEN02  CLI   WHENCODE,EOT        TEST E-O-T                                   
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL                                                            
*                                                                               
         MVC   BCWORK(L'WHENDD),WHENDD                                          
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,GCOVSYS                                                     
         GOTOX VDICTAT,BCPARM,(RF),BCWORK,0                                     
         EX    R2,*+8                                                           
         BE    VWHEN05                                                          
         CLC   FVIFLD(0),BCWORK    MATCH INPUT TO TABLE                         
*                                                                               
         MVC   BCWORK(L'WHENDD),WHENDD                                          
         ICM   RF,15,=C'SU  '                                                   
         ICM   RF,2,GCOVSYS                                                     
         GOTOX VDICTAT,BCPARM,(RF),BCWORK,0                                     
         EX    R2,*+8                                                           
         BNE   VWHEN04                                                          
         CLC   FVIFLD(0),BCWORK    MATCH INPUT TO TABLE                         
*                                                                               
VWHEN05  XR    R1,R1                                                            
         IC    R1,0(R3)                                                         
         STC   R1,INWHEN           SET INTERNAL WHEN VALUE                      
         B     VWHEN06             @@@@@@ TEMP TEMP TEMP @@@@@@@@@              
*                                                                               
*@@      EX    R1,*+8              TEST VALID FOR THIS OVERLAY                  
*@@      BNZ   VWHEN06                                                          
*@@      TM    INMIX2,0            SET ON ACTION RECORD ??? AATK                
                                                                                
VWHEN04  LA    R3,WHENTABL(R3)     BUMP TO NEXT TABLE ENTRY                     
         B     VWHEN02                                                          
*                                                                               
VWHEN06  CLI   INWHEN,INWNSOON     IF INWHEN IS SOON                            
         BNE   VWHENX                                                           
         TM    GSFRA.FRAINDS1,FRA1UPD  AND ACTION IS UPDATIVE                   
         BZ    VWHENX                                                           
         MVI   INWHEN,INWNUSN      THEN INWHEN IS UPDATIVE SOON                 
         B     VWHENX                                                           
*                                                                               
VWHENX   B     EXITOK                                                           
         DROP  R3                                                               
*                                                                               
WHENTAB  DS    0C                  ** TABLE OF WHEN VALUES **                   
         DC    AL1(INWNNOW)        NOW                                          
         DCDDL GE#NOW,WHENDDL                                                   
         DC    AL1(INWNSOON)       SOON                                         
         DCDDL GE#SOON,WHENDDL                                                  
         DC    AL1(INWNSOON)       SOON                                         
         DCDDL GE#SOON2,WHENDDL                                                 
         DC    AL1(INWNSOON)       ASAP (SOON)                                  
         DCDDL GE#ASAP,WHENDDL                                                  
         DC    AL1(INWNOVNT)       OV (OVERNIGHT)                               
         DCDDL GE#OV,WHENDDL                                                    
         DC    AL1(INWNOVNT)       ON (OVERNIGHT)                               
         DCDDL GE#ON,WHENDDL                                                    
         DC    AL1(INWNOSN)        LATE (OVERNIGHT/SOON)                        
         DCDDL GE#LATE,WHENDDL                                                  
WHENTABX DC    AL1(EOT)                                                         
*                                                                               
WHENTABD DSECT                                                                  
WHENCODE DS    XL1                                                              
WHENDD   DS    XL4                                                              
WHENTABL EQU   *-WHENTABD                                                       
WHENDDL  EQU   9                                                                
*                                                                               
GEFIL00  CSECT                                                                  
         SPACE 1                                                                
***********************************************************************         
* VALIDATE DESTINATION FIELD                                          *         
*                                                                     *         
* NTRY - R1=A(DESTINATION TWA FIELD HEADER)                           *         
* EXIT - CC=NOT EQUAL ON ERROR ELSE REPUSRID CONTAINS VALUE           *         
*                                   CSDSTID CONTAINS VALUE            *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING CTIKEY,IOKEY                                                     
VDEST    L     R4,AREP                                                          
         USING REPD,R4                                                          
         MVC   INDEST,CUUSER       SET DEFAULTS                                 
*                                                                               
         XC    CTIKEY,CTIKEY       READ ID RECORD FOR THIS USER                 
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,CUUSER                                                   
         L     R1,=AL4(XORD+XOCONFIL+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                CAN'T READ USER ID RECORD                    
*                                                                               
         LTR   R4,R4               TEST REPORT WORK AVAILABLE                   
         BZ    VDEST04                                                          
         L     R1,AIO1             YES - EXTRACT ORIGIN DETAILS                 
         LA    R1,CTIDATA-CTIKEY(R1)                                            
         XR    R0,R0                                                            
         USING CTORGD,R1                                                        
*                                                                               
VDEST02  CLI   CTORGEL,0           TEST E-O-R                                   
         BE    VDEST04                                                          
         CLI   CTORGEL,CTORGELQ    TEST ORIGIN NAME                             
         BE    *+12                                                             
         IC    R0,CTORGLEN                                                      
         BXH   R1,R0,VDEST02                                                    
         MVC   REPUSRN,CTORGNAM    SET TO ORIGIN NAME & ADDRESS                 
         MVC   REPUSRA,CTORGADD                                                 
*                                                                               
VDEST04  CLI   FVILEN,0                                                         
         BE    VDEST12                                                          
         GOTOX VGETIDS,BCPARM,(C'D',AIO1),0,VDMGR                               
         XR    RE,RE                                                            
         ICM   RE,1,0(R1)          RE=NUMBER OF DESTINATION ID'S                
         BZ    VDEST08                                                          
         L     RF,4(R1)            RF=A(LIST OF VALID DESTINATIONS)             
*                                                                               
VDEST06  CLC   FVIFLD(10),0(RF)    MATCH INPUT TO DESTINATION LIST              
         BE    VDEST10                                                          
         LA    RF,12(RF)                                                        
         BCT   RE,VDEST06                                                       
*                                                                               
VDEST08  MVC   FVMSGNO,=AL2(FVFEDST)                                            
         B     EXITL                                                            
*                                                                               
VDEST10  MVC   INDEST,10(RF)       SET DESTINATION ID FROM LIST                 
*                                                                               
VDEST12  TM    GSINDSG1,GSG1ORIG   TEST USE ORIGIN NAME                         
         BO    VDESTX                                                           
         TM    GSINDSG1,GSG1DEST   TEST USE DESTINATION NAME                    
         BZ    VDESTX              (ORIGIN NAME IS DEFAULT)                     
*                                                                               
         LTR   R4,R4               TEST REPORT WORK AVAILABLE                   
         BZ    EXITOK                                                           
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,INDEST                                                   
         L     R1,=AL4(XORD+XOCONFIL+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VDEST14  L     R1,AIO1                                                          
         LA    R1,CTIDATA-CTIKEY(R1)                                            
         USING CTDSTD,R1           GET DESTINATION DETAILS                      
         XR    R0,R0                                                            
*                                                                               
VDEST16  CLI   CTDSTEL,0                                                        
         BE    VDESTX                                                           
         CLI   CTDSTEL,CTDSTELQ                                                 
         BE    *+12                                                             
         IC    R0,CTDSTLEN                                                      
         BXH   R1,R0,VDEST16                                                    
         MVC   REPUSRN,CTDSTNAM                                                 
         MVC   REPUSRA,CTDSTADD                                                 
*                                                                               
         CLI   ASONOFF,ASOFF       TEST OFF-LINE                                
         BNE   VDESTX                                                           
         L     RF,REPAMST          OVERRIDE OFF-LINE ORIGIN DETAILS             
         USING MASTD,RF                                                         
         MVC   MCORIGIN,REPUSRN                                                 
         MVC   MCORIGAD,REPUSRA                                                 
         DROP  RF                                                               
*                                                                               
VDESTX   MVC   REPUSRID,INDEST                                                  
         MVC   CSDSTID,INDEST                                                   
         B     EXITOK                                                           
         POP   USING                                                            
*                                                                               
GEFIL00  CSECT                                                                  
         SPACE 1                                                                
***********************************************************************         
* GET FIELD RECORD                                                    *         
*                                                                     *         
* NTRY - P1=COMMAND (1=READ,2=RDHI,3=RSEQ,4=DIRECT READ)              *         
*        P2=A(IO AREA TO USE) OR 0 IF DIRECTORY READ                  *         
*        IOKEY CONTAINS KEY TO PROCESS                                *         
*        P3=DISP INTO TABLE IF DIRECT READ, ELSE 0                    *         
* EXIT - P3 HOLDS DISP TO RECORD IN TABLE IF LIVE SYSTEM              *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING GETWORKD,RTWORK                                                  
FLDGET   L     R1,RTPARMA                                                       
         MVC   RTPARMS,0(R1)                                                    
         CLC   =AL1(GFGREC),RTPARMS1+3                                          
         BE    FGET08            NOT A DIRECT INDEX READ                        
*                                                                               
         MVC   GFLDKEY,IOKEY       SAVE OUR ORIGINAL KEY                        
*                                                                               
FG       USING FDRRECD,GFLDKEY                                                  
FGTHIS   USING FDRRECD,IOKEY                                                    
FGREAD   USING FDRRECD,IOKEYSAV                                                 
*                                                                               
FGET02   MVC   FGREAD.FDRKEY,FGTHIS.FDRKEY  SAVE KEY & RESET D/A                
         XC    FGTHIS.FDRKDA,FGTHIS.FDRKDA                                      
*                                                                               
         GOTOX AGETFLD,RTPARM,RTPARMS1,IOKEY,RTPARMS2                           
         BE    FGET10              FOUND MATCHING FIELD RECORD                  
*                                                                               
         OC    FGREAD.FDRKTEST,FGREAD.FDRKTEST                                  
         BZ    FGET04              TRIED TO READ LIVE VERSION                   
*                                                                               
         XC    FGREAD.FDRKTEST,FGREAD.FDRKTEST                                  
         MVC   FGTHIS.FDRKEY,FGREAD.FDRKEY                                      
         B     FGET02              NOW TRY TO READ LIVE VERSION                 
*                                                                               
FGET04   CLI   FGREAD.FDRKCTRY,FF  READ WAS FOR HOST PROCESSOR?                 
         BNE   FGET06              NO                                           
         CLI   FGREAD.FDRKSUB,FF                                                
         BNE   FGET06              NO                                           
         DC    H'0'                CANNOT FIND THIS FIELD RECORD                
*                                                                               
         XC    FGREAD.FDRKTEST,FGREAD.FDRKTEST                                  
         MVC   FGTHIS.FDRKEY,FGREAD.FDRKEY                                      
         B     FGET02            NOW TRY TO READ LIVE VERSION                   
*                                                                               
FGET06   MVC   FGTHIS.FDRKEY,FG.FDRKEY                                          
         MVI   FGTHIS.FDRKCTRY,FF                                               
         MVI   FGTHIS.FDRKSUB,FF SET INCOMING KEY TO HOST PROCESSOR             
         B     FGET02            AND TRY AGAIN                                  
*                                                                               
FGET08   L     RF,RTPARMS3                                                      
         GOTOX AGETFLD,RTPARM,RTPARMS1,RTPARMS2,(RF)                            
         B     FGET10                                                           
*                                                                               
FGET10   OC    FGTHIS.FDRKDA(2),FGTHIS.FDRKDA  D/A SET (FILE READ)              
         BNZ   *+10                                                             
         MVC   FGTHIS.FDRKDA,8(R1)   SET INDEX NUMBER FOR TABLES                
*                                                                               
         ICM   RF,15,RTPARMS2    DID WE WANT THE FILE?                          
         BZ    EXITOK            NO                                             
*                                                                               
         SH    RF,=Y(L'IODA+L'IOWORK)                                           
         MVC   0(L'IODA,RF),FGTHIS.FDRKDA                                       
         B     EXITOK                                                           
*                                                                               
GETWORKD DSECT                                                                  
GFLDKEY  DS    XL(L'IOKEY)                                                      
*                                                                               
GEFIL00  CSECT                                                                  
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* PROTECT KEY / LIST HEADER SCREEN IF NESTED                          *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
PRTSCR   L     R1,RTPARMA                                                       
         MVC   RTPARMS,0(R1)                                                    
         CLI   TWASESNL,1          NESTED?                                      
         BNH   EXITOK              NO                                           
         TM    GCINDS1,GCIPROT     RESET IF KEY CHANGES?                        
         BO    EXITOK              YES                                          
         TM    GSINDSL2,GSI2PLST   PREVIOUS NEST LEVEL HAS LIST?                
         BZ    EXITOK              NO - NO NEED TO PROTECT                      
*                                                                               
         LH    R3,GSDSPOVR                                                      
         A     R3,ATWA                                                          
         USING FHD,R3                                                           
*                                                                               
PSCR02   CLI   FHLN,0              END OF SCREEN                                
         BE    EXITOK                                                           
         TM    FHAT,FHATXH         EXTENDED FIELD HEADER                        
         BZ    PSCR04              NO - NOT ONE OF OURS                         
         TM    FHAT,FHATPR         USUALLY PROTECTED?                           
         BO    PSCR04              YES                                          
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         LA    RF,FHD(RF)          EXTRACT EXTENDED FIELD HEADER                
         SH    RF,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RF)                                                    
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2        FIELD# INTERNAL REP HERE                     
         BZ    PSCR04              NON-DATA FIELD                               
*                                                                               
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         A     RF,AFDRADDR                                                      
         ICM   RF,15,0(RF)                                                      
         BNZ   *+6                                                              
         DC    H'0'                WHAT IS THIS FIELD?                          
*                                                                               
         USING FDRELD,RF                                                        
         TM    FDRLVL,FDRIKEY      KEY LEVEL?                                   
         BZ    PSCR04              NO                                           
         XR    R0,R0                                                            
         ICM   R0,3,FDRNUM                                                      
         DROP  RF                                                               
*                                                                               
         GOTOX APRG,RTPARM,('GCBOVER',ODATA),(R0),DSET,TWASESRA                 
         CLI   0(R1),DFLTX                                                      
         BE    PSCR04              DEFINATELY WON'T AFFECT ANYTHING             
*                                                                               
         OI    FHAT,FHATPR         PROTECT FIELD                                
         OI    FHOI,FHOITR         TRANSMIT FIELD                               
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         LA    RF,FHD(RF)          SET NORMALLY UNPROTECTED FLAG                
         SH    RF,=Y(FHDAD)        IN EXTENDED HEADER                           
         OI    FHUS-FHNU(RF),FVX1PRO                                            
*                                                                               
PSCR04   XR    R1,R1                                                            
         IC    R1,FHLN                                                          
         LA    R3,0(R1,R3)                                                      
         B     PSCR02                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* UNPROTECT KEY / LIST HEADER SCREEN IF NESTED                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
UPRTSCR  L     R1,RTPARMA                                                       
         MVC   RTPARMS,0(R1)                                                    
         CLI   TWASESNL,1          NESTED?                                      
         BNH   EXITOK              NO                                           
         TM    GCINDS1,GCIPROT     RESET IF KEY CHANGES?                        
         BO    EXITOK              YES                                          
*                                                                               
         LH    R3,GSDSPOVR                                                      
         A     R3,ATWA                                                          
         USING FHD,R3                                                           
*                                                                               
USCR02   CLI   FHLN,0              END OF SCREEN                                
         BE    EXITOK                                                           
         TM    FHAT,FHATXH         EXTENDED FIELD HEADER                        
         BZ    USCR04              NO - NOT ONE OF OURS                         
         TM    FHAT,FHATPR         PROTECTED?                                   
         BZ    USCR04              NO                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FHLN                                                          
         LA    RF,FHD(RF)                                                       
         SH    RF,=Y(FHDAD)                                                     
         TM    FHUS-FHNU(RF),FVX1PRO                                            
         BZ    USCR04              NOT A TEMP PROTECT FIELD                     
*                                                                               
         NI    FHUS-FHNU(RF),FF-(FVX1PRO)                                       
         NI    FHAT,FF-(FHATPR)                                                 
         OI    FHOI,FHOITR                                                      
*                                                                               
USCR04   XR    R1,R1                                                            
         IC    R1,FHLN                                                          
         LA    R3,0(R1,R3)                                                      
         B     USCR02                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE OUTPUT TYPE FIELD                                          *         
*                                                                     *         
* NTRY - R1=A(OUTPUT TYPE TWA FIELD HEADER)                           *         
* EXIT - CC=NOT EQUAL ON ERROR ELSE INOTYP CONTAINS VALUE             *         
***********************************************************************         
         SPACE 1                                                                
VOUT     L     R4,AREP                                                          
         USING REPD,R4                                                          
         XC    INOTYP,INOTYP                                                    
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'0'         TEST STARTS WITH A-I,J-R,S-Z,0-9             
         BL    *+12                                                             
         CLI   FVIFLD,C'9'                                                      
         BNH   VOUT02                                                           
         CLI   FVIFLD,C'S'                                                      
         BL    *+12                                                             
         CLI   FVIFLD,C'Z'                                                      
         BNH   VOUT02                                                           
         CLI   FVIFLD,C'J'                                                      
         BL    *+12                                                             
         CLI   FVIFLD,C'R'                                                      
         BNH   VOUT02                                                           
         CLI   FVIFLD,C'A'                                                      
         BL    *+12                                                             
         CLI   FVIFLD,C'I'                                                      
         BNH   VOUT02                                                           
         CLI   FVIFLD,C'+'         TEST OUTPUT TYPE SPECIALS +,&,#              
         BE    VOUT02                                                           
         CLI   FVIFLD,C'&&'                                                     
         BE    VOUT02                                                           
         CLI   FVIFLD,C'#'                                                      
         BE    VOUT02                                                           
*                                                                               
         MVC   INOTYP,FVIFLD       SAVE SPECIAL CODE IN OUTPUT TYPE             
         CLI   FVIFLD,C'@'         TEST IF SQL TRANSFORM                        
         BE    VOUT06                                                           
         CLI   FVIFLD,C'/'         TEST IF REMOTE AFP NAME                      
         BE    VOUT08                                                           
         B     VOUTERR                                                          
*                                                                               
VOUT02   LA    R1,IOKEY            BUILD OUTPUT TYPE RECORD KEY                 
         USING CTOKEY,R1                                                        
         XC    CTOKEY,CTOKEY                                                    
         MVI   CTOKTYP,C'O'                                                     
         MVC   CTOKID,FVIFLD                                                    
         L     R1,=AL4(XIO1+XOREAD+XOCONFIL)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   VOUTERR             CAN'T READ RECORD                            
*                                                                               
         L     R1,AIO1                                                          
         LA    R1,CTODATA          POINT TO FIRST ELEMENT                       
         XR    R0,R0                                                            
         USING CTOUTD,R1                                                        
VOUT04   CLI   CTOUTEL,0           LOCATE OUTPUT TYPE ELEMENT                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CTOUTEL,CTOUTELQ                                                 
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VOUT04                                                           
*                                                                               
         TM    CTOUTSTA,X'80'      TEST VALID OUTPUT TYPE                       
         BZ    VOUTERR                                                          
         MVC   INOTYP,FVIFLD       SET OUTPUT TYPE IN W/S                       
         B     EXITOK                                                           
*                                                                               
VOUT06   LA    R2,IOKEY            SET KEY FOR SQL REFORM RECORD                
         USING GREFD,R2                                                         
         XC    GREFKEY,GREFKEY                                                  
         MVI   GREFKREC,GREFRECQ                                                
         MVC   GREFAGY,CUAALF                                                   
         MVC   GREFID,FVIFLD+1                                                  
         L     R1,=AL4(XIO1+XOGENDIR+XOREAD)                                    
         GOTOX ('XIOC',AGROUTS)                                                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$INSQL) INVALID SQL                               
         B     EXITL                                                            
*                                                                               
         MVC   INOTYP,FVIFLD       SAVE SQL FORMULA IN OUTPUT TYPE              
         B     EXITOK                                                           
*                                                                               
VOUT08   CLI   FVILEN,3            REMOTE AFP INPUT AS /XX                      
         BE    VOUT10                                                           
         CLI   FVILEN,6            REMOTE AFP INPUT AS /SPPXX                   
         BE    VOUT10                                                           
         B     VOUTERR                                                          
*                                                                               
VOUT10   MVC   INOTYP,FVIFLD       SAVE SQL FORMULA IN OUTPUT TYPE              
         B     EXITOK                                                           
*                                                                               
VOUTERR  MVC   FVMSGNO,=AL2(FVFEOUT) SET INVALID OUTPUT TYPE                    
         B     EXITL                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* GET CORRECT RECORD                                                  *         
*                                                                     *         
* NTRY - R1=SYS/PROG/REC/0                                            *         
* EXIT - CC=EQ AIO1 HOLDS RECORD                                      *         
*      - CC=NE RECORD NOT FOUND                                       *         
***********************************************************************         
         SPACE 1                                                                
GTREC    ST    R1,RTFULL                                                        
         MVI   RTBYTE1,C'N'                                                     
         TM    RTFULL,X'80'        READING ONLY FOR NAME?                       
         BZ    *+8                 NO                                           
         MVI   RTBYTE1,C'Y'                                                     
         NI    RTFULL,X'7F'                                                     
*                                                                               
X        USING FRRRECD,RTWORK      BUILD FULL KEY IN W/S                        
         XC    RTWORK,RTWORK                                                    
         MVI   X.FRRKMIN,FRRKMINQ                                               
         MVI   X.FRRKTYP,FRRKTYPQ                                               
         MVC   X.FRRKSYS,RTFULL    SYSTEM                                       
         MVC   X.FRRKPRG,RTFULL+1  PROGRAM                                      
         MVC   X.FRRKREC,RTFULL+2  RECORD NUMBER                                
         MVC   X.FRRKCTRY,CUCTRY                                                
         XI    X.FRRKCTRY,FF                                                    
         MVI   X.FRRKSUB,FF                                                     
         MVC   X.FRRKTEST,ASTEST                                                
         DROP  X                                                                
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
*                                                                               
         OC    ASTEST,ASTEST       CONNECTED TO TEST PHASE                      
         BZ    GTRC02              NO                                           
*                                                                               
X        USING FRRRECD,IOKEY                                                    
         MVC   X.FRRKEY,RTWORK     TRY CONNECTED COUNTRY + TEST PHASE           
         BRAS  R2,GTRD                                                          
         BE    GTRC08                                                           
*                                                                               
GTRC02   MVC   X.FRRKEY,RTWORK     TRY CONNECTED COUNTRY + LIVE PHASE           
         XC    X.FRRKTEST,X.FRRKTEST                                            
         BRAS  R2,GTRD                                                          
         BE    GTRC08                                                           
*                                                                               
         OC    ASTEST,ASTEST       CONNECTED TO TEST PHASE                      
         BZ    GTRC04              NO                                           
*                                                                               
         MVC   X.FRRKEY,RTWORK     TRY HOST PROCESSOR + TEST PHASE              
         MVI   X.FRRKCTRY,FF                                                    
         BRAS  R2,GTRD                                                          
         BE    GTRC08                                                           
*                                                                               
GTRC04   MVC   X.FRRKEY,RTWORK     TRY HOST PROCESSOR + LIVE PHASE              
         MVI   X.FRRKCTRY,FF                                                    
         XC    X.FRRKTEST,X.FRRKTEST                                            
         BRAS  R2,GTRD                                                          
         BE    GTRC08                                                           
*                                                                               
         CLI   RTBYTE1,C'Y'        ACCEPT ANY MATCHING RECORD?                  
         BNE   GTRC06                                                           
*                                                                               
         MVC   X.FRRKEY,RTWORK     READ HIGH FOR RECORD NUMBER                  
         XC    X.FRRKCTRY,X.FRRKCTRY                                            
         XC    X.FRRKSUB,X.FRRKSUB                                              
         XC    X.FRRKTEST,X.FRRKTEST                                            
         BRAS  R2,GTRDHI                                                        
         CLC   X.FRRKEY(FRRKCTRY-FRRRECD),RTWORK                                
         BE    GTRC08                                                           
*                                                                               
GTRC06   GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         MVC   FVMSGNO,=AL2(GE$INREC) SET INVALID RECORD NUMBER                 
         J     EXITL                                                            
*                                                                               
GTRC08   L     R1,=AL4(XOGET+XOGENFIL+XIO1)                                     
         GOTOX ('XIOC',AGROUTS)                                                 
         JE    *+6                                                              
         DC    H'0'                BAD RECORD ON GENDIR                         
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         J     EXITOK                                                           
         DROP  X                                                                
         EJECT                                                                  
***********************************************************************         
* GET CORRECT ACTION                                                  *         
*                                                                     *         
* NTRY - R1=SYS/PROG/REC/ACT                                          *         
* EXIT - CC=EQ AIO1 HOLDS RECORD                                      *         
*      - CC=NE RECORD NOT FOUND                                       *         
***********************************************************************         
         SPACE 1                                                                
GTACT    ST    R1,RTFULL                                                        
         MVI   RTBYTE1,C'N'                                                     
         TM    RTFULL,X'80'        READING ONLY FOR NAME?                       
         BZ    *+8                 NO                                           
         MVI   RTBYTE1,C'Y'                                                     
         NI    RTFULL,X'7F'                                                     
*                                                                               
X        USING FRARECD,RTWORK      BUILD FULL KEY IN W/S                        
         XC    RTWORK,RTWORK                                                    
         MVI   X.FRAKMIN,FRAKMINQ                                               
         MVI   X.FRAKTYP,FRAKTYPQ                                               
         MVC   X.FRAKSYS,RTFULL    SYSTEM                                       
         MVC   X.FRAKPRG,RTFULL+1  PROGRAM                                      
         MVC   X.FRAKREC,RTFULL+2  RECORD NUMBER                                
         MVC   X.FRAKACT,RTFULL+3  ACTION NUMBER                                
         MVC   X.FRAKCTRY,CUCTRY                                                
         XI    X.FRAKCTRY,FF                                                    
         MVI   X.FRAKSUB,FF                                                     
         MVC   X.FRAKTEST,ASTEST                                                
         DROP  X                                                                
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(QSCON)                                   
*                                                                               
         OC    ASTEST,ASTEST       CONNECTED TO TEST PHASE                      
         BZ    GTAC02              NO                                           
*                                                                               
X        USING FRARECD,IOKEY                                                    
         MVC   X.FRAKEY,RTWORK     TRY CONNECTED COUNTRY + TEST PHASE           
         BRAS  R2,GTRD                                                          
         BE    GTAC08                                                           
*                                                                               
GTAC02   MVC   X.FRAKEY,RTWORK     TRY CONNECTED COUNTRY + LIVE PHASE           
         XC    X.FRAKTEST,X.FRAKTEST                                            
         BRAS  R2,GTRD                                                          
         BE    GTAC08                                                           
*                                                                               
         OC    ASTEST,ASTEST       CONNECTED TO TEST PHASE                      
         BZ    GTAC04              NO                                           
*                                                                               
         MVC   X.FRAKEY,RTWORK     TRY HOST PROCESSOR + TEST PHASE              
         MVI   X.FRAKCTRY,FF                                                    
         BRAS  R2,GTRD                                                          
         BE    GTAC08                                                           
*                                                                               
GTAC04   MVC   X.FRAKEY,RTWORK     TRY HOST PROCESSOR + LIVE PHASE              
         MVI   X.FRAKCTRY,FF                                                    
         XC    X.FRAKTEST,X.FRAKTEST                                            
         BRAS  R2,GTRD                                                          
         BE    GTAC08                                                           
*                                                                               
         CLI   RTBYTE1,C'Y'        ACCEPT ANY MATCHING RECORD?                  
         BNE   GTAC06                                                           
*                                                                               
         MVC   X.FRAKEY,RTWORK     READ HIGH FOR RECORD NUMBER                  
         XC    X.FRAKCTRY,X.FRAKCTRY                                            
         XC    X.FRAKSUB,X.FRAKSUB                                              
         XC    X.FRAKTEST,X.FRAKTEST                                            
         BRAS  R2,GTRDHI                                                        
         CLC   X.FRAKEY(FRAKCTRY-FRARECD),IOKEYSAV                              
         BE    GTAC08                                                           
*                                                                               
GTAC06   GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         MVC   FVMSGNO,=AL2(GE$INREC) SET INVALID RECORD NUMBER                 
         B     EXITL                                                            
*                                                                               
GTAC08   L     R1,=AL4(XOGET+XOGENFIL+XIO1)                                     
         GOTOX ('XIOC',AGROUTS)                                                 
         BE    *+6                                                              
         DC    H'0'                BAD RECORD ON GENDIR                         
*                                                                               
         GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     EXITOK                                                           
         DROP  X                                                                
         EJECT                                                                  
***********************************************************************         
* READ GENDIR SET D/A IN IO1 - USED FOR GTREC AND GTACT ROUTINES      *         
* NOTE: BRANCH REGISTER !!!                                           *         
***********************************************************************         
         SPACE 1                                                                
GTRD     L     R1,=AL4(XOGENDIR+XOREAD+XIO1)                                    
         GOTOX ('XIOC',AGROUTS),(R1)                                            
         CLI   IOERR,0                                                          
         BR    R2                                                               
*                                                                               
GTRDHI   L     R1,=AL4(XOGENDIR+XOHIGH+XIO1)                                    
         GOTOX ('XIOC',AGROUTS),(R1)                                            
         CLI   IOERR,0                                                          
         BR    R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ISSUE AN I/O TO GENDIR/GENFILE FOR CONTROLLER            *         
*                                                                     *         
* NTRY - R1=I/O CONTROL BYTES SET FROM IO EQUATES                     *         
*           CONTAINS - FILE NUMBER       (ZERO=USE IOFILE)            *         
*                      COMMAND NUMBER    (ZERO=USE IOCMND)            *         
*                      COMMAND QUALIFIER (READ LOCK/READ DELETES)     *         
*                      I/O AREA NUMBER   (ZERO=USE IOADDR)            *         
*                                                                     *         
* EXIT - CC=LOW IF A HARD I/O ERROR OCCURED                           *         
*        CC=EQUAL IF I/O SUCCESSFUL (NO ERRORS)                       *         
*        CC=HIGH IF A SOFT ERROR (EOF/NOT FOUND/DELETED)              *         
*        IOADDR=A(I/O AREA USED)                                      *         
*        IOERR=DATAMGR ERROR BYTE                                     *         
*        IOKEYSV=SAVE IOKEY VALUE (BEFORE I/O IS EXECUTED)            *         
*        IODA=DISK ADDRESS EXTRACTED FOR I/S RECORD (I/S D/A PAIR)    *         
*                                                                     *         
* NOTE - FOR INDEX SEQUENTIAL I/O'S IOKEY IS ALWAYS SAVED IN IOKEYSV  *         
*        BEFORE I/O IS EXECUTED. FOR D/A FILE I/O'S IF IODA IS ZERO   *         
*        AND FILE HAS A DIRECTORY ATTACHED (I/S D/A PAIR) THE READ    *         
*        SPECIFIED TO THE FILE (HIGH/READ) IS EXECUTED TO THE         *         
*        DIRECTORY.                                                   *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING IORTND,RTWORK                                                    
XIOCN    ST    R1,IOCTRL           SAVE I/O CONTROL BYTES IN W/S                
         MVI   IOFLAG,0            RESET I/O FLAG BYTE                          
         MVI   IOQ,0               ESTABLISH COMMAND QUALIFIER                  
         L     R1,IOCTRL                                                        
         N     R1,=AL4(XORDEL)     TEST DELETED RECORDS WANTED                  
         BZ    *+8                                                              
         OI    IOQ,X'08'                                                        
*                                                                               
         L     R1,IOCTRL           ESTABLISH WHICH I/O AREA REQUIRED            
         N     R1,=AL4(XIOAREAS)                                                
         BZ    IOXC02              NO I/O AREA SET                              
         SRL   R1,4                R1=I/O AREA NUMBER                           
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R1,AIO1(R1)         A(IO AREA)                                   
         L     R1,0(R1)            I/O AREA IN R1 NOW                           
         STCM  R1,15,IOADDR        SET REAL I/O ADDRESS                         
         AHI   R1,-(L'IODA+L'IOWORK)                                            
         STCM  R1,15,IOAREAD       SAVE I/O ADDRESS                             
*                                                                               
IOXC02   L     R1,IOCTRL           ESTABLISH WHICH FILE REQUIRED                
         N     R1,=AL4(XOFILES)                                                 
         BNZ   IOXC04                                                           
         OC    IOFILE,IOFILE       CALLER MUST SUPPLY FILE NAME                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOFILNM,IOFILE      SET FILE NAME                                
         B     IOXC10              COMMAND NAME MUST ALSO BE SUPPLIED           
*                                                                               
IOXC04   SRL   R1,8                R1=FILE NUMBER                               
         L     RE,AXFILTAB         POINT TO SYSTEM FILES                        
         USING NFITABD,RE                                                       
*                                                                               
IOXC06   CLI   NFINUM,EOT          TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID FILE NUMBER                          
         CLM   R1,1,NFINUM         MATCH ON FILE NUMBER                         
         BE    *+12                                                             
         AHI   RE,NFITABL                                                       
         B     IOXC06                                                           
*                                                                               
         MVC   IOFILV,NFINUM       EXTRACT FILE VALUES                          
         L     RE,AXCMDTAB         RE=A(I/O COMMAND TABLE)                      
         USING CMDTABD,RE          RE=A(FILE/COMMAND TABLE)                     
         XR    RF,RF                                                            
         LA    R1,XOCMNDS          ESTABLISH COMMAND EQUATE                     
         N     R1,IOCTRL                                                        
         BNZ   IOXC12              EQUATE SET                                   
*                                                                               
IOXC10   OC    IOCMND,IOCMND       TEST COMMAND NAMED                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   IOCMDNM,IOCMND      SET COMMAND NAME                             
         ICM   R0,15,IOADDR        SEE IF ADDRESS SET                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOX VDMGR,RTPARM,(IOQ,IOCMDNM),IOFILNM,(R0),(R0)                     
         MVC   IOERR,8(R1)                                                      
         B     IOXCX                                                            
*                                                                               
IOXC12   CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IODUB(1),0(RE)      TEST CORRECT SERIES OF COMMANDS              
         NC    IODUB(1),IOFILI                                                  
         CLC   IODUB(1),IOFILI                                                  
         BNE   *+12                                                             
         LA    RE,4(RE)                                                         
         B     IOXC14                                                           
*                                                                               
         ICM   RF,3,2(RE)                                                       
         LA    RE,3(RF,RE)                                                      
         B     IOXC12                                                           
*                                                                               
IOXC14   CLI   0(RE),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID COMMAND                              
         CLM   R1,1,CMDNUMB        MATCH ON COMMAND NUMBER                      
         BE    *+12                                                             
         LA    RE,CMDTABL(RE)                                                   
         B     IOXC14                                                           
*                                                                               
         MVC   IOCMDV,CMDNAME      EXTRACT COMMAND VALUES                       
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    IODA,IODA                                                        
         TM    IOCMDI,CMDIDADD     TEST DISK ADDRESS RETURNED                   
         BNZ   IOXC22                                                           
         TM    IOCMDI,CMDIDARQ     TEST D/A REQUIRED FOR I/O                    
         BZ    IOXC24                                                           
         OC    IODAOVER,IODAOVER   TEST OVERRIDE D/A SET                        
         BZ    IOXC16                                                           
         MVC   IODA,IODAOVER       YES - SET D/A AND CLEAR OVERRIDE             
         ICM   R1,15,IOAREAD       R1=A(I/O AREA)                               
         BZ    *+10                                                             
         MVC   0(L'IODA,R1),IODAOVER                                            
         XC    IODAOVER,IODAOVER                                                
         B     IOXC22                                                           
*                                                                               
IOXC16   ICM   R1,15,IOAREAD       R1=A(I/O AREA)                               
         BZ    IOXC20                                                           
         TM    IOCMDI,CMDIDAXC     TEST CLEAR D/A NOW                           
         BZ    *+10                                                             
         XC    0(L'IODA,R1),0(R1)                                               
*                                                                               
         OC    0(L'IODA,R1),0(R1)  D/A IN I/O AREA?                             
         BZ    *+10                                                             
         MVC   IODA,0(R1)          YES - SET D/A                                
         LA    R1,L'IODA(R1)                                                    
*                                                                               
IOXC18   OC    0(L'IOWORK,R1),0(R1) WORK IN I/O AREA?                           
         BZ    *+10                                                             
         MVC   IOWORK,0(R1)        YES - SET WORK                               
         LA    R1,L'IOWORK(R1)                                                  
*                                                                               
IOXC20   OC    IODA,IODA           TEST D/A PRESENT                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
IOXC22   ICM   R0,15,IOADDR        I/O AREA ADDRESS SET?                        
         BNZ   *+6                                                              
         DC    H'0'                NO I/O AREA ADDRESS                          
*                                                                               
         GOTOX VDMGR,RTPARM,(IOQ,IOCMDNM),IOFILNM,IODA,(R0),IOWORK              
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         TM    IOERR,IOERRS-IOEDEL                                              
         BNZ   IOXCX                                                            
*                                                                               
         ICM   R1,15,IOAREAD       POINT TO I/O AREA                            
         BZ    IOXCX                                                            
         MVC   0(L'IODA,R1),IODA   MOVE IN D/A AND WORK                         
         LA    R1,L'IODA(R1)                                                    
         MVC   0(L'IOWORK,R1),IOWORK                                            
         B     IOXCX               EXIT TO CALLER                               
*                                                                               
IOXC24   TM    IOFILI,NFIIIS       TEST INDEX SEQUENTIAL FILE                   
         BZ    IOXC28                                                           
         MVC   IOKEYSAV,IOKEY      SAVE CURRENT I/O KEY                         
         LA    R0,IOKEY            FL I/S READS INTO IOKEY                      
         TM    IOFILI2,NFIIID      TEST I/S FILE HAS D/A ATTACHED               
         BNZ   IOXC26                                                           
         TM    IOFILI,NFIIVL       TEST I/S FILE IS V/L                         
         BZ    IOXC26                                                           
         ICM   R0,15,IOADDR        VL I/S MUST READ INTO IOAREA ALSO            
         BNZ   IOXC26                                                           
         DC    H'0'                NO IOAREA SET UP                             
*                                                                               
IOXC26   GOTOX VDMGR,RTPARM,(IOQ,IOCMDNM),IOFILNM,IOKEY,(R0)                    
         MVC   IOERR,8(R1)         SAVE ERROR RETURN BYTE                       
         TM    IOERR,IOERRS-IOEDEL                                              
         BNZ   IOXCX                                                            
*                                                                               
         ICM   R1,15,IOAREAD       POINT TO I/O AREA                            
         BZ    IOXCX                                                            
         XR    RF,RF                                                            
         IC    RF,IOFILKL          KEY LENGTH                                   
         XR    RE,RE                                                            
         IC    RE,IOFILCL          CONTROL LENGTH                               
         LA    RF,0(RE,RF)                                                      
         LA    RF,IOKEY(RF)        POINT TO DISK ADDRESS                        
         MVC   IODA,0(RF)                                                       
*                                                                               
         TM    IOFILI2,NFIIID      TEST D/A FILE ATTACHED TO THIS FILE          
         BZ    *+10                                                             
         MVC   0(L'IODA,R1),IODA                                                
         LA    R1,L'IODA(R1)       BUMP BY D/A LENGTH                           
         MVC   0(L'IOWORK,R1),IOWORK                                            
         B     IOXCX                                                            
*                                                                               
IOXC28   ICM   R0,15,IOADDR                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOX VDMGR,BCPARM,(IOQ,IOCMDNM),IOFILNM,(R0),(R0)                     
         MVC   IOERR,8(R1)                                                      
         B     IOXCX                                                            
*                                                                               
IOXCX    TM    IOERR,IOERRS                                                     
         BZ    EXITOK              NO ERRORS                                    
         MVC   FVMSGNO,=AL2(FVFIOER)                                            
         TM    IOERR,IOEEOF+IOERNF+IOEDEL                                       
         BNZ   EXITH               SET IRRECOVERABLE ERROR                      
         B     EXITL               SET LOGICAL I/O ERROR                        
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE DDPFXTBLE                                                      
         SPACE 1                                                                
GENSYS   EQU   X'0F'                                                            
FF       EQU   X'FF'                                                            
DMREAD   DC    CL8'DMREAD'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
GETREC   DC    CL8'GETREC'                                                      
DMWRITE  DC    CL8'DMWRT'                                                       
TEMPSTR  DC    CL8'TEMPSTR'                                                     
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
RTWORKD  DSECT                                                                  
RTRELO   DS    A                                                                
RTPARMA  DS    A                   A(INCOMING PARAMETER LIST)                   
*                                                                               
RTPARMS  DS    0XL24               * PARAMETER SAVE AREA *                      
RTPARMS1 DS    A                                                                
RTPARMS2 DS    A                                                                
RTPARMS3 DS    A                                                                
RTPARMS4 DS    A                                                                
RTPARMS5 DS    A                                                                
RTPARMS6 DS    A                                                                
*                                                                               
RTPARM   DS    0XL24               * PARAMETERS 1-6 *                           
RTPARM1  DS    A                                                                
RTPARM2  DS    A                                                                
RTPARM3  DS    A                                                                
RTPARM4  DS    A                                                                
RTPARM5  DS    A                                                                
RTPARM6  DS    A                                                                
*                                                                               
RTFILDIR DS    A                   A(THIS SYSTEM DIRECTORY ENTRY)               
RTFILREC DS    A                   A(THIS SYSTEM FILE ENTRY)                    
*                                                                               
RTTAG    DS    A                                                                
RTFULL   DS    A                                                                
RTHALF   DS    H                                                                
RTBYTE1  DS    X                                                                
RTBYTE2  DS    X                                                                
*                                                                               
RTSOVSYS DS    XL1                 SYSTEM                                       
RTSPRG   DS    XL1                 PROGRAM                                      
RTSREC   DS    XL1                 RECORD TYPE                                  
RTSPAG#  DS    XL1                 PAGE NUMBER                                  
*                                                                               
RTMSG    DS    XL2                                                              
*                                                                               
RTWORK   DS    XL128               FOR ALL TO USE                               
*                                                                               
TITSACTN DS    XL1                 SAVED TSAR ACTION NUMBER                     
RTWORKL  EQU   *-RTWORKD                                                        
         SPACE 1                                                                
* GEFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE GEFILWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* GEGENREF                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENREF                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* CTMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTMSGEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
GEFIL00  CSECT                                                                  
         ORG   GEFIL00+(((*-GEFIL00)/2048)+1)*2048                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038GEFIL00   03/07/12'                                      
         END                                                                    
