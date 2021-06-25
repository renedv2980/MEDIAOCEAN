*          DATA SET DDSPOOF    AT LEVEL 022 AS OF 07/09/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE SPOOFA                                                                   
*INCLUDE MASTER                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE BILLIT                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE BINS31                                                                 
*INCLUDE BUFFAHI                                                                
*INCLUDE CARDS                                                                  
*INCLUDE CASHVAL                                                                
*INCLUDE CENTER                                                                 
*INCLUDE CHOPPER                                                                
*INCLUDE COVAIL                                                                 
*INCLUDE CUREDIT                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DAYVAL                                                                 
*INCLUDE DEJAVU                                                                 
*INCLUDE DICTATE                                                                
*INCLUDE DLFLD                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMPRTQB                                                                
*INCLUDE EXPAND                                                                 
*INCLUDE FAEMUMSG                                                               
*INCLUDE FAGETTXT                                                               
*INCLUDE FAPQSEC                                                                
*INCLUDE FAPROT                                                                 
*INCLUDE FASECRET                                                               
*INCLUDE FATABOFF                                                               
*INCLUDE FAXLINK                                                                
*INCLUDE GETDAY                                                                 
*INCLUDE GETFLD                                                                 
*INCLUDE GETPROF                                                                
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LIMACC                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE LOGOC                                                                  
*INCLUDE MINIO                                                                  
*INCLUDE MOBILE                                                                 
*INCLUDE PARSNIP                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PERVAL                                                                 
*INCLUDE PERVERT                                                                
*INCLUDE PHSCNT                                                                 
*INCLUDE PQPROF                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE SCANNER                                                                
*INCLUDE SCUNKEY                                                                
*INCLUDE SMFOUT                                                                 
*INCLUDE SMTP                                                                   
*INCLUDE SORTER                                                                 
*INCLUDE SQUASHER                                                               
*INCLUDE TIMCON                                                                 
*INCLUDE UNSCAN                                                                 
*INCLUDE UNDERLIN                                                               
*INCLUDE UNTIME                                                                 
*INCLUDE XSORT                                                                  
*INCLUDE WIDE                                                                   
*&&US                                                                           
*INCLUDE CLPACK                                                                 
*INCLUDE CLUNPK                                                                 
*INCLUDE DAYUNPK                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE LOGON                                                                  
*INCLUDE CONFID2                                                                
*INCLUDE CBJOB                                                                  
*INCLUDE DEMVER                                                                 
*&&                                                                             
*&&UK                                                                           
*INCLUDE BLDCUR                                                                 
*INCLUDE BLDMED                                                                 
*INCLUDE BUYTER                                                                 
*INCLUDE DEMVALS                                                                
*INCLUDE EUREKA                                                                 
*INCLUDE FAREPORT                                                               
*INCLUDE GETAUD                                                                 
*INCLUDE GETCPT                                                                 
*INCLUDE GETDPT                                                                 
*INCLUDE GETEQIV                                                                
*INCLUDE GETQRP                                                                 
*INCLUDE GETRAT2                                                                
*INCLUDE GETTARG                                                                
*INCLUDE GETUNV                                                                 
*INCLUDE GETWKS                                                                 
*INCLUDE LOGO                                                                   
*INCLUDE MEDEMREG                                                               
*INCLUDE MEFILT                                                                 
*INCLUDE STABLST                                                                
*INCLUDE SUGGEST                                                                
*INCLUDE TMPACK                                                                 
*INCLUDE TMUNPK                                                                 
*&&                                                                             
*ENTRY SPOOF                                                                    
*                                                                               
SPOOF    TITLE '- SPOOL OFFLINE CONTROLLER'                                     
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         ENTRY PTCHPOOL                                                         
         ENTRY SPFUSER                                                          
*&&US*&& ENTRY DUMMY                                                            
         ENTRY SYSFAC                                                           
*                                                                               
SPOOF    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKL,**SPOOF*,RA,WORK=A(WORKAREA)                               
*                                                                               
         USING WORKD,RC            RC=A(LOCAL WORKING STORAGE)                  
         L     R9,=V(MASTC)                                                     
         USING MASTD,R9            R9=A(MASTC)                                  
         L     R8,MCVLOGOC                                                      
         USING LOGOD,R8            R8=A(LOGOC)                                  
         L     R7,=A(TWA)                                                       
         USING TWAD,R7             R7=A(TWA)                                    
         L     R6,MCBXAREA                                                      
         USING BOXD,R6             R6=A(BOX AREA)                               
         LA    R0,SPOOF                                                         
         STCM  R0,15,MCAWORK                                                    
         MVI   MCS2OSYS,6          GET ACC SYSTEM INFO AS SECONDARY             
*                                                                               
         OI    MCFLAGS,MCFSPOOF    LET V(MASTER) KNOW ITS SPOOF                 
         GOTOR MCVRUNST            CONTROL CARDS AND INITIAL LOAD               
*&&UK*&& MVI   LOGOSTRP,YES                                                     
         MVC   MCDMPEND,ASPOOFND   SET END OF DUMP                              
         ZAP   REQTOT,PZERO                                                     
*                                                                               
         GOTOR OPNFIL              OPEN APPLICATION SYSTEM FILES                
*                                                                               
         LA    RF,FAC005A          COMINTER PHASE                               
         CLI   MCCSMODE,0                                                       
         BNE   SPOOF00                                                          
         MVI   2(RF),0             CLEAR PHASE NUMBER - DON'T LOAD              
         B     SPOOF01                                                          
*                                                                               
SPOOF00  TM    MCFLAGS2,MCISSOON   SOON JOB                                     
         BO    SPOOF01             CONTINUE FOR SOON                            
         GOTOR =V(JOBREG)          REGISTER JOB IN TABS COMJOBS TAB             
*                                                                               
SPOOF01  L     R1,ATYPLST                                                       
SPOOF02  MVC   CONNUMF,3(R1)       LOOK UP CONTROLLER TYPE IN LIST              
         CLI   0(R1),FF                                                         
         BE    SPOOF04                                                          
         CLC   0(3,R1),MCLOAD+1                                                 
         BE    SPOOF04                                                          
         AHI   R1,L'TYPLST                                                      
         B     SPOOF02                                                          
*                                                                               
SPOOF04  ICM   R0,14,R00A          RESOLVE FACILITIES ADDRESSES                 
         L     R2,=A(FACLIST)                                                   
         LA    R3,FACLISTN                                                      
SPOOF06  ICM   R0,1,2(R2)          GET PHASE VALUE                              
         BZ    SPOOF07             IF ZERO THEN SKIP                            
         GOTOR CALLOV,DMCB,0,(R0),0   LOAD INTO MEMORY                          
         SR    RF,RF                                                            
         ICM   RF,3,0(R2)          WHERE IN TO PLACE ADDRESS                    
         LA    RF,SPOOF(RF)        ADD DISP TO LOCAL SYS LIST                   
         SR    RE,RE                                                            
         ICM   RE,3,3(R2)          DISPLACEMENT INTO COMFACS                    
         AR    RF,RE                                                            
         MVC   0(4,RF),0(R1)       SAVE OFF ADDRESS                             
*&&UK                                                                           
         CLC   MCSYSTEM,=C'MP'     DON'T LOAD BEYOND REQTWA IF MPL              
         BNE   *+12                                                             
         CLI   2(R2),QREQTWA                                                    
         BE    *+12                                                             
*&&                                                                             
SPOOF07  AHI   R2,L'FACLIST                                                     
         BCT   R3,SPOOF06                                                       
*                                                                               
         L     RF,ASYSFAC0         SAVE LOCALLY                                 
         ST    RF,DBCOMFCS         SAVE COMFACS IN DBLOCK                       
         MVC   AREQTWA,CREQTWA-COMFACSD(RF)                                     
         MVC   ACOMINTR,CCOMINTR-COMFACSD(RF)                                   
*                                                                               
         TM    MCPRTIND,MCPRTINL   TEST NOT TO PRINT LOGOS                      
         BNZ   SPOOF08                                                          
         CLI   MCCSMODE,C'1'       PASS 1 ?                                     
         BNE   SPOOF07A            NO, SO RUN AS USUAL                          
         TM    MCFLAGS2,MCISSOON   SOON RUN?                                    
         BO    SPOOF08             YES, SO NO LOGOS ON MCCSMODE=1               
         L     RF,=V(PRINT00)                                                   
         MVC   0(2,RF),=X'90EC'    PRINTING (HATE THIS CODE)                    
         GOTOR MCVLOGO,DMCB,LOGOD                                               
         L     RF,=V(PRINT00)                                                   
         MVC   0(2,RF),=X'07FE'    NO-OP PRINTING INCASE RFHDR NOT USED         
         B     SPOOF08                                                          
*                                                                               
SPOOF07A GOTOR MCVLOGO,DMCB,LOGOD                                               
*                                                                               
SPOOF08  GOTOR RUNREQ              HANDLE REQUESTS                              
*                                                                               
         CLI   TWAFIRST,2          TEST APPLICATION WANTS RUN LAST HOOK         
         BNE   RUNEND              NO                                           
*                                                                               
* THIS MAY BE A ISSUE WITH COMSCORE BATCH JOBS                                  
* NEED TO RESEACH THIS CALL TO APPLICATION                                      
*                                                                               
         MVI   TWAFIRST,FF         YES-SET LAST TIME FLAG                       
         GOTOR CALLAP              AND GO TO USER                               
*                                                                               
RUNEND   DS    0H                                                               
*                                                                               
         CLI   MCCSMODE,C'1'       PASS 1 ?                                     
         BL    RUNEND03            NOT COMSCORE ENABLED                         
         TM    MCFLAGS2,MCISSOON   ONLY RUN IF A SOON JOB                       
         BO    RUNEND02            NOT A SOON JOB                               
         GOTOR =V(JOBDONE)         JOB COMPLETED OKAY                           
         B     RUNEND03                                                         
*                                                                               
RUNEND02 XC    DMCB(DMCBL),DMCB                                                 
         LA    RF,=C'CLOSE'        PASS 1                                       
         BE    *+8                                                              
         LA    RF,=C'END'          PASS 2                                       
         ST    RF,DMCB+0           P1                                           
         LA    RE,DBLOCK                                                        
         ST    RE,DMCB+4           P2                                           
         GOTOR ACOMINTR,DMCB                                                    
*                                                                               
RUNEND03 ESTAE 0                                                                
         CP    MCNDUMPS,PZERO      ANY DUMPS THEN TRY RECOVERY                  
         BE    RUNEND04                                                         
         GOTO1 MCVDMGR,DMCB,=C'RECOVR'                                          
*                                                                               
RUNEND04 L     R2,MCSSB                                                         
         USING SSBD,R2                                                          
         TM    SSOSTAT2,SSOSRWRK   TEST FACWK RECOVERY                          
         BZ    RUNEND06                                                         
         ICM   RF,15,SSOFWNDX      CLOSE FACWK FILE                             
         JZ    *+2                                                              
         LA    R0,CLOREG                                                        
         CP    MCNDUMPS,PZERO                                                   
         BE    *+8                                                              
         LA    R0,CLOPUR                                                        
         GOTOR MCVDMGR,DMCB,(R0),FACWRK,(RF),MCIO,SSOFWBUF                      
         JNE   *+2                                                              
         DROP  R2                                                               
*                                                                               
RUNEND06 MVI   LOGOTYPE,C'E'       PRINT END LOGOS                              
         ZAP   LOGOREQS,REQTOT                                                  
         L     RF,MCVLOGO                                                       
         LA    R1,DMCB                                                          
         CLC   NOOP,0(RF)          TEST LOGO NO-OPED                            
         BE    RUNEND08                                                         
         GOTOR (RF),(R1),LOGOD                                                  
         B     RUNEND10                                                         
*                                                                               
RUNEND08 CLI   BOXSTAT,C'I'        TEST INSIDE A BOX                            
         BNE   RUNEND10                                                         
         MVI   BOXREQ,C'C'         YES-CLOSE IT                                 
         GOTOR MCVPRINT,(R1),MCSPACES,PRTBL01                                   
*                                                                               
RUNEND10 LA    RF,CLOERR           CLOSE SYSPRINT                               
         LA    R0,L'CLOERR                                                      
         CP    MCNDUMPS,PZERO                                                   
         BNE   RUNEND12                                                         
         OC    MCRETCD,MCRETCD     TEST USER RETURN CODE SET                    
         BNZ   RUNEND12                                                         
         LA    RF,CLOREG                                                        
         LA    R0,L'CLOREG                                                      
         OC    MCREPPQI,MCREPPQI   TEST USER INFO GIVEN                         
         BZ    RUNEND12                                                         
         LA    RF,CLOUSR                                                        
         LA    R0,L'CLOUSR                                                      
RUNEND12 GOTOR MCVPRINT,DMCB,((R0),(RF)),MCREPPQI                               
*                                                                               
         GOTOR VSORTER,DMCB,SRTEND                                              
         GOTOR CLSFIL              DO DMCLSE FOR SYSTEM FILES                   
         GOTOR MCVNQDQ,DMCB,(C'D',DEQALL)                                       
         ESTAE 0                   CANCEL ESTAE                                 
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,MCRETCD        TEST RETURN CODE SET                         
         BZ    RUNEND20                                                         
         ABEND (1)                 YES-ISSUE ABEND WITH IT                      
*                                                                               
RUNEND20 GOTOR VSHMUSS,DMCB,(0,=CL8'DETACH'),(0,=CL8'PRTQ')                     
*&&US*&& GOTOR VSHMUSS,DMCB,(0,=CL8'DETACH'),(0,=CL8'STAPACK')                  
*                                                                               
         CP    MCNDUMPS,PZERO      TEST ANY DUMPS PRODUCED                      
         BE    SPOOFX                                                           
         ABEND 664                 YES-ISSUE ABEND                              
*                                                                               
SPOOFX   XBASE                     EXIT BACK TO MVS                             
         EJECT                                                                  
***********************************************************************         
* HANDLE REQUESTS                                                     *         
***********************************************************************         
RUNREQ   NTR1                                                                   
         LA    RE,RUNREQX          ESTABLISH STXIT RETURN                       
         STM   R0,RF,MCRUNLST                                                   
         LA    RE,RUNREQ10                                                      
         STM   R0,RF,MCREQLST                                                   
*                                                                               
RUNREQ10 GOTOR MCVREQST            GET FIRST REQUEST CARD OF SET                
         CLC   MCREQREC(2),=C'/*'                                               
         BE    RUNREQX             DONE, END OF CARDS                           
         OI    MCFLAGS,MCFRNXT     SET WANT NEXT REQUEST CARD                   
*                                                                               
         AP    REQTOT,PONE         COUNT NUMBER OF REQUEST                      
         MVC   WORK(L'TWAFIRST),TWAFIRST                                        
         LA    R0,TWAD                                                          
         LHI   R1,TWALNQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   TWAFIRST,WORK                                                    
         GOTOR CALLOV,DMCB,('FF',0),0                                           
         JNE   *+2                                                              
*                                                                               
         GOTOR AREQTWA,DMCB,(1,TWAD),(CONNUMF,MCREQREC)                         
         MVI   TWACOMMN,1          CALL APPLICATION TO PROCESS HEADERS          
         GOTOR CALLAP                                                           
*                                                                               
RUNREQ18 GOTOR AREQTWA,DMCB,(2,TWAD),MCREQREC,MCVPRINT                          
         CLI   0(R1),C'*'          DID REQTWA WANT MORE CARDS?                  
         BE    RUNREQ20                                                         
         GOTOR MCVREQST                                                         
         OI    MCFLAGS,MCFRNXT     SET WANT NEXT REQUEST CARD                   
         B     RUNREQ18                                                         
*                                                                               
RUNREQ20 NI    MCFLAGS,255-MCFRNXT SET END OF REQUEST READING LOOP              
         OI    MCFLAGS,MCFRLST                                                  
         GOTOR MCVREQST            LET MASTER KNOW ITS THE END OF REQ           
         NI    MCFLAGS,255-MCFRLST                                              
         CP    MCREQNO,MCSTART     TEST START= REQUEST REACHED                  
         BL    RUNREQ10            NO-BACK FOR NEXT REQUEST                     
*                                                                               
RUNREQ22 XC    DMCB(DMCBL),DMCB                                                 
         CLI   MCCSMODE,C'1'       PASS 1 ?                                     
         BL    RUNREQ25            NOT A COMSCORE RUN                           
         LA    RF,=C'START'        PASS 2                                       
         BH    RUNREQ24            SHOULD NOT GET HERE                          
         TM    MCFLAGS2,MCISSOON   SOON JOB ?                                   
         BO    RUNREQ23            YES, SKIP REQUEST START                      
         GOTOR =V(JCREQSTR)        START TO PROCESS REQUEST                     
*                                                                               
RUNREQ23 LA    RF,=C'ALLOC'        PASS 1                                       
*                                                                               
RUNREQ24 ST    RF,DMCB+0           P1                                           
         LA    RE,DBLOCK                                                        
         ST    RE,DMCB+4           P2                                           
         BRAS  R2,WTO                                                           
         GOTOR ACOMINTR,DMCB                                                    
*                                                                               
RUNREQ25 MVI   TWACOMMN,0                                                       
         MVI   NOCLRTWA,0                                                       
         GOTOR CALLAP              CALL APPLICATION TO PROCESS REQUEST          
         CLI   DMCB,FF             APPLICATION WANTS MORE REQUEST               
         BNE   *+8                 BUT WITHOUT CLEARING TWA                     
         MVI   NOCLRTWA,FF         YES                                          
*                                                                               
         L     RF,VFAXINFO         RF=A(FAX INFO BLOCK)                         
         USING FAXINFOD,RF                                                      
         CLI   FXISTAT,FXISPEND    IF FAX IS STILL PENDING                      
         BNE   *+12                                                             
         MVI   FXISTAT,FXISINAC    SET IT INACTIVE - NOTHING WAS DONE           
         B     RUNREQ26                                                         
*                                                                               
         CLI   FXISTAT,FXISACT     ELSE IF FAX IS ACTIVE                        
         BNE   RUNREQ26                                                         
         MVI   FXISTAT,FXISCLOS    SET TO CLOSE IT                              
         GOTOR VFAXLINK            LET FAXLINK WRAP THINGS UP                   
         DROP  RF                                                               
*                                                                               
RUNREQ26 XC    DMCB(DMCBL),DMCB                                                 
         TM    MCFLAGS2,MCISSOON   SOON JOB                                     
         BO    RUNREQ40            CONTINUE AS NORMAL                           
         CLI   MCCSMODE,C'1'       PASS 1 ?                                     
         BL    RUNREQ40            NOT A COMSCORE REQUEST                       
         LA    RF,=C'CLOSE'        PASS 1                                       
         BE    *+8                                                              
         LA    RF,=C'END  '        PASS 2                                       
*                                                                               
         ST    RF,DMCB+0           P1                                           
         LA    RE,DBLOCK                                                        
         ST    RE,DMCB+4           P2                                           
         BRAS  R2,WTO                                                           
         GOTOR ACOMINTR,DMCB                                                    
*                                                                               
         CLI   MCCSMODE,C'1'       JUST DID CLOSE                               
         JH    RUNREQ28            MUST BE PASS=2                               
         GOTOR =V(JOBWAIT)         WAIT FOR RESPONSE FROM COMSCORE              
         JNE   *+2                                                              
         B     RUNREQ22            RE-RUN REQUEST                               
*                                                                               
RUNREQ28 GOTOR =V(JCREQEND)        PASS 2                                       
*                                                                               
RUNREQ40 CLI   NOCLRTWA,FF                                                      
         BE    RUNREQ18                                                         
         B     RUNREQ10                                                         
*                                                                               
RUNREQX  J     EXIT                                                             
         EJECT                                                                  
WTO      STM   RE,R2,TEMPREG                                                    
         MVC   MSGACT,0(RF)                                                     
         L     RE,MCAEXTRA                                                      
         USING MCEXTRA,RE                                                       
         MVC   MSGPASS,MCCSMODE                                                 
         MVC   MSGDSN,MCMVSDSN                                                  
         DROP  RE                                                               
         LA    R2,MSGLN                                                         
         WTO   TEXT=(R2),MCSFLAG=HRDCPY                                         
         LM    RE,R2,TEMPREG                                                    
         BR    R2                                                               
*                                                                               
TEMPREG  DS    5A                                                               
MSGLN    DC    AL2(60)                                                          
MSGACT   DC    CL5' '                                                           
         DC    CL4' '                                                           
MSGPASS  DC    CL1' '                                                           
         DC    CL4' '                                                           
MSGDSN   DC    CL44' '                                                          
                                                                                
***********************************************************************         
* ROUTINE TO CALL THE APPLICATION PHASE                               *         
***********************************************************************         
CALLAP   NTR1                                                                   
         MVC   BOXCTRY,MCCTRY                                                   
         MVC   BOXLANG,MCLANG                                                   
*                                                                               
         XC    XTRAINFO,XTRAINFO   BUILD EXTRA INFO (CTRY,LANG ETC.)            
         MVC   XIAGCOPT,MCAGCOPT                                                
         MVC   XIAGCTRY,MCAGCTRY                                                
         MVC   XICTRY,MCCTRY                                                    
         MVC   XILANG,MCLANG                                                    
         MVC   XIAGCURR,MCAGCURR                                                
         MVC   XIUSER,MCORIGID                                                  
         MVC   XIAGYALP,MCUSER                                                  
         MVC   XIAGYSEC,MCAGYSEC                                                
         MVC   XIAGYPER,MCRHSAGP                                                
         MVC   XIPID,MCRHPSWD                                                   
*                                                                               
         L     RF,MCSSB                                                         
         USING SSBD,RF                                                          
         CLI   SSODSPAC,C'T'                                                    
         BNE   *+8                                                              
         OI    XIFLAG1,XITSTADV    SET TEST FACPAK                              
         CLI   SSODSPAC,C'C'                                                    
         BNE   *+12                                                             
         OI    XIFLAG1,XITSTADV    SET TST FACPAK                               
         OI    XIFLAG2,XICSCADV    SET CSC FACPAK                               
         DROP  RF                                                               
*                                                                               
         LLC   RE,XICTRY           GET ACTUAL COUNTRY CODE                      
         CHI   RE,15                                                            
         BNH   *+6                                                              
         SR    RE,RE                                                            
         SLL   RE,4                FOUR TABLE ADDRESSES PER COUNTRY             
         L     RF,=A(CTRYXLAT)                                                  
         AHI   RF,-4                                                            
         L     RF,0(RF)            GET A(EXTENDED TRANSLATE TABLES)             
         L     RF,8(RE,RF)                                                      
         MVC   CTRYTRUC,0(RF)      COPY LOWER TO UPPER TRANSLATE TABLE          
         MVI   CTRYTRUC,C' '                                                    
         LA    RE,CTRYTRUC                                                      
         ST    RE,XIATRUC          SET A(TRANSLATE TABLE)                       
*                                                                               
         L     RF,ASYSFAC0         PASS EXTRA INFO IN COMFACS                   
         LA    RF,CXTRAINF-COMFACSD(RF)                                         
         LA    R0,XTRAINFO                                                      
         ST    R0,0(RF)                                                         
*                                                                               
         XC    DMCB(DMCBL),DMCB    BUILD PARAMETER LIST                         
         XC    DMCB1(DMCB1L),DMCB1                                              
         LA    R1,DMCB                                                          
         LA    R0,XTRAINFO                                                      
         ST    R0,20(R1)                                                        
         MVC   TWAORIG,MCORIGID    USER ID NUMBER                               
         LA    R2,TWAD             R2=A(TWA)                                    
*                                                                               
         CLC   MCSYSTEM,=C'CT'     TEST CONTROL SYSTEM                          
         BNE   CALLAP02                                                         
         MVC   00(4,R1),ASYSFAC    A(SYSFACS)                                   
         MVC   04(4,R1),=A(TIA)    A(TIA)                                       
         MVC   12(4,R1),ASYSFAC0   A(COMFACS)                                   
         ST    R2,20(R1)           A(TWA)                                       
         LA    R0,XTRAINFO                                                      
         ST    R0,32(R1)                                                        
         B     CALLAP12                                                         
*                                                                               
CALLAP02 DS    0H                  SET UP APPROPRIATE FACILITIES LIST           
*&&US                                                                           
         L     R3,ASYSFAC2                                                      
         CLI   MCSYSTEM,C'S'       SPOTPAK                                      
         BE    CALLAP04                                                         
*&&                                                                             
*&&UK                                                                           
         L     R3,ASYSFAC5                                                      
         CLC   MCSYSTEM,=C'ME'     MEDIA                                        
         BE    CALLAP04                                                         
*&&                                                                             
         L     R3,ASYSFAC4                                                      
         CLC   MCSYSTEM,=C'MB'     MEDIABASE                                    
         BE    CALLAP04                                                         
         L     R3,ASYSFAC3         COMMON FACILITIES LIST                       
*                                                                               
CALLAP04 L     R4,=A(TIA)                                                       
         L     R5,ASYSFAC0                                                      
         STM   R2,R5,4(R1)                                                      
*                                                                               
CALLAP06 DS    0H                                                               
*&&UK*&& CLC   MCSYSTEM,=C'ME'     MEDIA                                        
*&&UK*&& BE    CALLAP07                                                         
         CLC   MCSYSTEM,=C'MB'     MEDIABASE                                    
         BE    CALLAP07                                                         
         CLC   MCSYSTEM,=C'AC'     ACCPAK                                       
         BE    CALLAP07                                                         
         CLC   MCSYSTEM,=C'PE'     PERSON                                       
         BNE   CALLAP08                                                         
CALLAP07 MVC   0(1,R1),MCIDAGYB                                                 
         B     CALLAP12                                                         
*                                                                               
CALLAP08 DS    0H                                                               
*&&UK                                                                           
         CLC   MCSYSTEM,=C'FE'     ARTIST FEES                                  
         BE    CALLAP10                                                         
         CLC   MCSYSTEM,=C'MP'     MPL/CRAFT                                    
         BNE   CALLAP12                                                         
         CLC   MCLOAD+1(3),=C'510'                                              
         BE    CALLAP10                                                         
         CLC   MCLOAD+1(3),=C'512'                                              
         BE    CALLAP10                                                         
         CLC   MCLOAD+1(3),=C'513'                                              
         BNE   CALLAP12                                                         
*                                                                               
CALLAP10 MVC   0(4,R1),ASYSFAC5    MEDFACS                                      
*&&                                                                             
CALLAP12 MVC   TWAAGY,MCUSER       AGENCY/REP                                   
         MVI   TWACFLAG,TWACFIDF   SET USING IDF PHASE LOADS                    
         MVI   TWAMODE,1           SET RUNNING OFFLINE                          
         MVC   TWAACCS,MCC1ACCS                                                 
         MVC   TWAVPRNT,MCVPRINT                                                
         MVC   TWAWRITE,MCWRITE                                                 
         MVC   TWAVBUFF,VBUFFALO                                                
         MVC   TWAVSORT,VSORTER                                                 
         LHI   RF,PATCHER-SPOOF                                                 
         LA    RF,SPOOF(RF)                                                     
         STCM  RF,15,TWAPTCHR                                                   
         MVC   TWAVWORK,VWORKER                                                 
         MVC   TWAVBOX,MCBXAREA                                                 
         LA    R0,MASTD                                                         
         STCM  R0,15,TWAMASTC                                                   
         MVC   TWADCONS,=A(ADCONS)                                              
*                                                                               
CALLAP14 GOTOR MCAPHAS1,(R1)                                                    
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OPEN APPLICATION SYSTEM FILES                                       *         
***********************************************************************         
OPNFIL   NTR1                                                                   
*&&UK*&& CLC   MCSYSTEM,=C'FE'     UK FEES OPENS OWN FILES                      
*&&UK*&& BE    OPNFILX                                                          
         L     RF,MCUTL            SET SE NUMBER IN UTL                         
         USING UTLD,RF                                                          
         MVC   TSYS,MCIDSENO                                                    
*                                                                               
OPNFIL04 CLI   MCRECOVR,YES                                                     
         BNE   OPNFIL06                                                         
         L     RF,MCSSB                                                         
         USING SSBD,RF                                                          
         NI    SSOSTAT2,FF-SSOSNRCV CLEAR NO RECOVER                            
*&&UK*&& OI    SSOFLAG1,SSOFRCVR   RECOVER I/S FILES TOO                        
*&&US*&& OI    SSOMTIND,SSOFRCVN   RECOVER I/S FILES TOO                        
         B     OPNFIL10                                                         
         DROP  RF                                                               
*                                                                               
OPNFIL06 CLI   MCRECOVR,C'W'       TEST FACWK RECOVERY                          
         BNE   OPNFIL08                                                         
         GOTOR MCVDTCON,PARMS,(4,MCDATE),(1,FULL)                               
*                                                                               
         L     RF,MCSSB                                                         
         USING SSBD,RF                                                          
         NI    SSOSTAT2,FF-SSOSNRCV                                             
         OI    SSOSTAT2,SSOSRWRK   SET FACWK RECOVERY BIT IN SSB                
         L     R0,SSOFWBUF                                                      
*                                                                               
         L     RF,SSOFWNDX                                                      
         USING UKRECD,RF           SET FACWRK KEY VALUES                        
         MVC   UKUSRID,MCORIGID    USERID/SYSTEM/PROGRAM                        
         MVC   UKSYSPRG+0(1),MCSYSTEM                                           
         MVC   UKSYSPRG+1(2),MCPROG                                             
         CLI   MCFACPAK,C' '                                                    
         BNH   *+10                                                             
         MVC   UKSUBPRG,MCFACPAK   FACPAK SYSTEM ID                             
         MVC   UKDAY,FULL+2        DAY NUMBER                                   
         MVI   UKCLASS,C'R'        CLASS=RECOVERY                               
         MVI   UKFLAG,X'01'        FLAG DUPLICATES ALLOWED                      
         MVC   UKFILNO,=X'FFFF'    LET WRKR KNOW UKFLAG IS VALID                
*&&UK                                                                           
         LA    R1,23               BUILD HEADER REC IN MCIO                     
         SLL   R1,16                                                            
         STCM  R1,15,MCIO                                                       
         MVC   MCIO+04(L'FWKSOON),FWKSOON                                       
         OC    MCREMPQK,MCREMPQK                                                
         BNZ   *+10                                                             
         MVC   MCIO+04(L'FWKTSO),FWKTSO                                         
         MVC   MCIO+08(L'MCREMPQK),MCREMPQK                                     
         MVC   MCIO+15(8),MCSYSTEM  MC-SYS/PRG/USER/MED/ND                      
         GOTOR MCVDMGR,PARMS,DMADD,FACWRK,(RF),MCIO,(R0)                        
         B     OPNFIL10                                                         
*&&                                                                             
         DROP  RF                                                               
*                                                                               
OPNFIL08 DS    0H                  OPTIONALLY TURN OFF RECOVERY                 
*&&US                                                                           
         MVI   RPRECV,C'X'                                                      
         MVI   SPRECV,C'X'                                                      
         MVI   NTRECV,C'X'                                                      
*NOP*    MVI   TARECV,C'X'                                                      
         MVI   TRRECV,C'X'                                                      
         MVI   PPRECV,C'X'                                                      
*&&                                                                             
         MVI   MPRECV,C'X'                                                      
         MVI   ACRECV,C'X'                                                      
         MVI   MBRECV,C'X'                                                      
*                                                                               
OPNFIL10 DS    0H                                                               
*&&US                                                                           
         CLI   MCSYSTEM,C'S'       TEST SPOTPAK                                 
         BNE   OPNFIL18                                                         
         GOTOR MCVDMGR,DMCB,DMREAD,SYSFLES,0                                    
*                                                                               
         ICM   RE,15,12(R1)        A(FILE LIST FOR THIS SPOT SYSTEM)            
         JZ    *+2                                                              
         USING SYSFLSTD,RE                                                      
         SR    R1,R1                                                            
         ICM   R1,3,SYSF#FLS       NUMBER OF FILES IN SYSTEM                    
         LA    RE,SYSFLIST         BUMP TO FIRST FILE IN LIST                   
*                                                                               
OPNFIL12 CLI   SYSFILE#,STRFDRQ    STRFDR?                                      
         BE    OPNFIL14            YES-SEE IF IT'S NOP                          
         AHI   RE,SYSFLNQ          NO-TRY NEXT FILE                             
         BCT   R1,OPNFIL12                                                      
         B     OPNFIL16            NO TRAFFIC FILES - REMOVE FROM LIST          
*                                                                               
OPNFIL14 TM    SYSFIND1,SFNOP      TEST FILE IS NOP                             
         BZ    OPNFIL18                                                         
         DROP  RE                                                               
*                                                                               
OPNFIL16 MVC   SPTRF(10),SPRECV    YES-TAKE THEM OUT OF THE LIST                
*                                                                               
OPNFIL18 CLC   MCSYSTEM,=C'TA'     TALENT                                       
         BNE   OPNFIL19                                                         
*NOP*    TM    XIFLAG1,XITSTADV    TST FACPAK?                                  
*NOP*    BZ    OPNFIL19            NO-SKIP GLOBAL                               
         L     RF,MCSSB                                                         
         USING SSBD,RF                                                          
         OI    SSOSTAT2,SSOSGALO+SSOSLOCK                                       
         OI    SSOFLAG1,SSOFRCVR                                                
         NI    SSOMTIND,FF-SSOFRCVN                                             
         DROP  RF                                                               
*&&                                                                             
OPNFIL19 LA    R2,SYSTAB                                                        
         USING SYSTABD,R2                                                       
OPNFIL20 CLI   SYSTABD,SYSTEOTQ    TEST END OF SYSTEM TABLE                     
         JE    *+2                 YES-INVALID SYSTEM                           
         CLI   SYSTNTPK,FF         TEST CHECK MCNETPAK VALUE                    
         BE    OPNFI20A                                                         
         CLC   SYSTNTPK,MCNETPAK   MATCH ON MCNETPAK VALUE                      
         BNE   OPNFI20B                                                         
OPNFI20A CLC   SYSTCODE,MCSYSTEM   AND MATCH ON SYSTEM CODE                     
         BE    OPNFIL22                                                         
OPNFI20B AHI   R2,SYSTABL          NO MATCH - BUMP TO NEXT ENTRY                
         B     OPNFIL20                                                         
*                                                                               
OPNFIL22 SR    RF,RF               A(FILE LIST)                                 
         ICM   RF,3,SYSTDISP                                                    
         LA    RF,SPOOF(RF)                                                     
         LR    RE,RF               RE=BEGINING OF FILE LIST TO OPEN             
         CLI   MCWRITE,NO                                                       
         BNE   OPNFIL24                                                         
OPNFIL23 CLI   0(RE),C'X'          END OF LIST ?                                
         BE    OPNFIL24            YES-THEN DONE                                
         MVI   0(RE),NO            SET ALL FILES AS READ-ONLY                   
         AHI   RE,8                                                             
         B     OPNFIL23                                                         
*                                                                               
OPNFIL24 GOTOR MCVDMGR,DMCB,DMOPEN,SYSTNAME,(RF),TWAD                           
         DROP  R2                                                               
*&&US                                                                           
***********************************************************************         
* WHEN RUNNING TRAFFIC JOB, OPEN SISTER SPOT SYSTEM                   *         
***********************************************************************         
         CLC   MCSYSTEM,=C'ST'     STRAFFIC SYSTEM                              
         BNE   OPNFILX             DONE                                         
         L     RF,MCUTL                                                         
         USING UTLD,RF                                                          
         MVC   TRFSE#,TSYS                                                      
         DROP  RF                                                               
*                                                                               
         GOTOR MCVDMGR,DMCB,(0,DDNAME),TRFSE,0                                  
         TM    8(R1),X'10'         SYSTEM NOT FOUND                             
         JO    *+2                                                              
         LT    RE,8(R1)            GET A(DDNADATA)                              
         JZ    *+2                                                              
         USING DDNAMED,RE                                                       
*                                                                               
         MVC   TRFOV#,DDNASYNO                                                  
*NOP*    MVC   TRFSYSC,DDNASEN3+1                                               
         MVC   TRFSYSC,DDNASEID                                                 
*                                                                               
         GOTO1 MCVDMGR,DMCB,(0,DDNAME),TRFSYS,0                                 
         TM    8(R1),X'10'         SYSTEM NOT FOUND                             
         JO    *+2                                                              
         LT    RE,8(R1)            GET A(DDNADATA)                              
         JZ    *+2                                                              
         MVC   SPTSE#,DDNASENO                                                  
         MVC   SPTOV#,DDNASYNO                                                  
         DROP  RE                                                               
*                                                                               
         L     RF,MCUTL                                                         
         USING UTLD,RF                                                          
         MVC   TSYS,SPTSE#                                                      
*                                                                               
         L     RE,TUTLXADR         EXTENDED UTL (NOT XA OFF-LINE)               
         USING XAUTLD,RE                                                        
         MVI   XASWNUM+1,2         SAVE OFF SPOT / TRAFFIC SYSTEM               
         L     RE,XAASWTAB         GET SWITCH TABLE                             
         USING XASWTABD,RE                                                      
         MVC   XASWSYS,SPTSE#                                                   
         MVC   XASWSOV,SPTOV#      SPOT OVERLAY NUMBER                          
         AHI   RE,XASWLEN          NEXT ENTRY                                   
         MVC   XASWSYS,TRFSE#                                                   
         MVC   XASWSOV,TRFOV#      TRAFFIC OVERLAY NUMBER                       
         DROP  RE,RF               XASWTABD,UTLD                                
*                                                                               
         LA    RF,SPFLIST          SPOT FILES                                   
         LR    RE,RF                                                            
         CLI   MCWRITE,NO                                                       
         BNE   OPNFIL26                                                         
OPNFIL25 CLI   0(RE),C'X'          END OF LIST ?                                
         BE    OPNFIL26            YES-THEN DONE                                
         MVI   0(RE),NO            SET ALL FILES AS READ-ONLY                   
         AHI   RE,8                                                             
         B     OPNFIL25                                                         
*                                                                               
OPNFIL26 GOTOR MCVDMGR,DMCB,DMOPEN,=CL8'SPOT',(RF),TWAD                         
         L     RF,MCUTL                                                         
         MVC   4(1,RF),TRFSE#      RESTORE TRAFFIC                              
*&&                                                                             
*&&UK                                                                           
         CLC   MCSYSTEM,=C'ME'     MEDIA SYSTEM NEEDS MEDZ+DEMO FILES           
         BE    OPNFIL25                                                         
         CLC   MCSYSTEM,=C'MP'     DITTO MPL/CRAFT                              
         JNE   EXIT                                                             
         CLC   MCLOAD+1(3),=C'510'                                              
         BE    OPNFIL25                                                         
         CLC   MCLOAD+1(3),=C'512'                                              
         BNE   OPNFIL28                                                         
*                                                                               
OPNFIL25 L     R2,MCUTL                                                         
         MVC   WORK(1),4(R2)       SAVE SE NUM                                  
         CLI   4(R2),MEDZSEQ       OPEN MEDDIRZ/MEDFILZ IF REQUIRED             
         BE    OPNFIL26                                                         
         MVI   4(R2),MEDZSEQ                                                    
         LA    RE,MEFLDIR                                                       
         ST    RE,8(R1)                                                         
         GOTOR (RF),(R1)                                                        
*                                                                               
OPNFIL26 LA    RE,DEMOFLS          OPEN EXTENDED MEDZ LIST (DEMO FILES)         
         ST    RE,8(R1)                                                         
         GOTOR (RF),(R1)                                                        
         MVC   4(1,R2),WORK        RESTORE SE                                   
*                                                                               
OPNFIL28 CLC   MCSYSTEM,=C'MP'     MPL NEEDS MPL1 FILES (COMMON)                
         JNE   EXIT                                                             
         L     R2,MCUTL                                                         
         CLI   4(R2),MPL1SEQ       OPEN MPLDIR1/MPLFIL1 IF REQUIRED             
         BE    OPNFILX                                                          
         MVC   WORK(1),4(R2)       SAVE SE NUM                                  
         MVI   4(R2),MPL1SEQ                                                    
         LA    RE,MPFLFIL                                                       
         MVI   MPFLFIL,NO                                                       
         MVI   MPFLDIR,NO                                                       
         MVI   MPFLEND,C'X'                                                     
         ST    RE,8(R1)                                                         
         GOTOR (RF),(R1)                                                        
         MVC   4(1,R2),WORK        RESTORE SE                                   
*&&                                                                             
OPNFILX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CLOSE APPLICATION SYSTEM FILES                                      *         
***********************************************************************         
CLSFIL   NTR1                                                                   
*&&UK*&& CLC   MCSYSTEM,=C'FE'     UK FEES OPENS OWN FILES                      
*&&UK*&& BE    CLSFILX                                                          
         L     RF,MCUTL            SET SE NUMBER IN UTL                         
         MVC   TSYS-UTLD(L'TSYS,RF),MCIDSENO                                    
*&&DO                                                                           
         CLI   MCSYSTEM,C'S'       TEST SPOTPAK                                 
         BNE   CLSFIL18                                                         
         GOTOR MCVDMGR,DMCB,DMREAD,SYSFLES,0                                    
         ICM   RE,15,12(R1)        A(FILE LIST FOR THIS SPOT SYSTEM)            
         JZ    *+2                                                              
         SR    R1,R1                                                            
         ICM   R1,3,2(RE)          NUMBER OF FILES IN SYSTEM                    
         LA    RE,4(RE)            BUMP TO FIRST FILE IN LIST                   
*                                                                               
CLSFIL12 CLI   3(RE),STRFDRQ       STRFDR?                                      
         BE    CLSFIL14            YES-SEE IF IT'S NOP                          
         LA    RE,8(RE)            NO-TRY NEXT FILE                             
         BCT   R1,CLSFIL12                                                      
         B     CLSFIL16            NO TRAFFIC FILES - REMOVE FROM LIST          
*                                                                               
CLSFIL14 TM    0(RE),X'80'         TEST FILE IS NOP                             
         BZ    CLSFIL18                                                         
*                                                                               
CLSFIL16 MVC   SPTRF(10),SPRECV    YES-TAKE THEM OUT OF THE LIST                
*&&                                                                             
CLSFIL18 LA    R2,SYSTAB                                                        
         USING SYSTABD,R2                                                       
CLSFIL20 CLI   SYSTABD,SYSTEOTQ    TEST END OF SYSTEM TABLE                     
         JE    *+2                 YES-INVALID SYSTEM                           
         CLI   SYSTNTPK,FF         TEST CHECK MCNETPAK VALUE                    
         BE    CLSFL20A                                                         
         CLC   SYSTNTPK,MCNETPAK   MATCH ON MCNETPAK VALUE                      
         BNE   CLSFL20B                                                         
CLSFL20A CLC   SYSTCODE,MCSYSTEM   AND MATCH ON SYSTEM CODE                     
         BE    CLSFIL22                                                         
CLSFL20B AHI   R2,SYSTABL          NO MATCH - BUMP TO NEXT ENTRY                
         B     CLSFIL20                                                         
*                                                                               
CLSFIL22 SR    RF,RF               A(FILE LIST)                                 
         ICM   RF,3,SYSTDISP                                                    
         LA    RF,SPOOF(RF)                                                     
         GOTOR MCVDMGR,DMCB,DMCLSE,SYSTNAME,(RF),TWAD                           
         DROP  R2                                                               
*&&US                                                                           
***********************************************************************         
* WHEN IN TRAFFIC CLOSE SISTER SPOT SYSTEM TOO.                       *         
***********************************************************************         
         CLC   MCSYSTEM,=C'ST'     TRAFFIC SYSTEM?                              
         BNE   CLSFILX                                                          
         L     RF,MCUTL                                                         
         MVC   WORK(1),4(RF)       SAVE OFF UTL                                 
         MVC   4(1,RF),SPTSE#                                                   
         GOTOR MCVDMGR,DMCB,DMCLSE,=CL8'SPOT',0,0                               
         L     RF,MCUTL                                                         
         MVC   4(1,RF),WORK        RESTORE SE                                   
*&&                                                                             
*&&UK                                                                           
         CLC   MCSYSTEM,=C'ME'     MEDIA SYSTEM NEEDS MEDZ+DEMO FILES           
         BE    CLSFIL24                                                         
         CLC   MCSYSTEM,=C'MP'     DITTO MPL/CRAFT                              
         JNE   EXIT                                                             
         CLC   MCLOAD+1(3),=C'510'                                              
         BE    CLSFIL24                                                         
         CLC   MCLOAD+1(3),=C'512'                                              
         BNE   CLSFIL28                                                         
*                                                                               
CLSFIL24 L     R2,MCUTL                                                         
         MVC   WORK(1),4(R2)       SAVE SE NUM                                  
         CLI   4(R2),MEDZSEQ       OPEN MEDDIRZ/MEDFILZ IF REQUIRED             
         BE    CLSFIL26                                                         
         MVI   4(R2),MEDZSEQ                                                    
         LA    RE,MEFLDIR                                                       
         ST    RE,8(R1)                                                         
         GOTOR (RF),(R1)                                                        
*                                                                               
CLSFIL26 LA    RE,DEMOFLS          OPEN EXTENDED MEDZ LIST (DEMO FILES)         
         ST    RE,8(R1)                                                         
         GOTOR (RF),(R1)                                                        
         MVC   4(1,R2),WORK        RESTORE SE                                   
*                                                                               
CLSFIL28 CLC   MCSYSTEM,=C'MP'     MPL NEEDS MPL1 FILES (COMMON)                
         JNE   EXIT                                                             
         L     R2,MCUTL                                                         
         CLI   4(R2),MPL1SEQ       OPEN MPLDIR1/MPLFIL1 IF REQUIRED             
         BE    CLSFILX                                                          
         MVC   WORK(1),4(R2)       SAVE SE NUM                                  
         MVI   4(R2),MPL1SEQ                                                    
         LA    RE,MPFLFIL                                                       
         MVI   MPFLFIL,NO                                                       
         MVI   MPFLDIR,NO                                                       
         MVI   MPFLEND,C'X'                                                     
         ST    RE,8(R1)                                                         
         GOTOR (RF),(R1)                                                        
         MVC   4(1,R2),WORK        RESTORE SE                                   
*&&                                                                             
CLSFILX  J     EXIT                                                             
         DROP  RA,RB                                                            
         EJECT                                                                  
***********************************************************************         
* TABLE OF SYSTEM CODES AND NAMES                                     *         
***********************************************************************         
SYSTAB   DS    0XL(SYSTABL)                                                     
*&&US                                                                           
         DC    C'SP',X'0',C'SPOT    '                                           
         DC    AL2(SPFLIST-SPOOF)                                               
         DC    C'SP',AL1(YES),C'SPOT    '                                       
         DC    AL2(NTFLIST-SPOOF)                                               
         DC    C'RE',AL1(FF),C'REP     '                                        
         DC    AL2(REFLIST-SPOOF)                                               
         DC    C'PP',AL1(FF),C'PRINT   '                                        
         DC    AL2(PRFLIST-SPOOF)                                               
         DC    C'TA',AL1(FF),C'TALENT  '                                        
         DC    AL2(TAFLIST-SPOOF)                                               
         DC    C'ST',AL1(FF),C'STRAFFIC'                                        
         DC    AL2(TRFLIST-SPOOF)                                               
*&&                                                                             
         DC    C'AC',AL1(FF),C'ACCOUNT '                                        
         DC    AL2(ACFLIST-SPOOF)                                               
*&&UK                                                                           
         DC    C'ME',AL1(FF),C'MEDIA   '                                        
         DC    AL2(MEFLIST-SPOOF)                                               
*&&                                                                             
         DC    C'PE',AL1(FF),C'PERSON  '                                        
         DC    AL2(PEFLIST-SPOOF)                                               
         DC    C'MP',AL1(FF),C'MPL     '                                        
         DC    AL2(MPFLIST-SPOOF)                                               
         DC    C'CT',AL1(FF),C'CONTROL '                                        
         DC    AL2(CTFLIST-SPOOF)                                               
         DC    C'MB',AL1(FF),C'MBA     '                                        
         DC    AL2(MBFLIST-SPOOF)                                               
*                                                                               
SYSTABX  DC    AL1(SYSTEOTQ)                                                    
*&&US                                                                           
DDNAME   DC    CL8'DDNAME'                                                      
TRFSE    DC    C'SE=',X'0000'                                                   
         ORG   TRFSE+3                                                          
         DS    X                                                                
TRFSE#   DS    X                                                                
TRFOV#   DS    X                                                                
*                                                                               
TRFSYS   DC    C'S=S  '                                                         
         ORG   TRFSYS+3                                                         
TRFSYSC  DS    CL2                                                              
*                                                                               
SPTSE#   DS    X                                                                
SPTOV#   DS    X                                                                
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* LISTS OF FILE NAMES FOR EACH SYSTEM                                 *         
***********************************************************************         
*&&US                                                                           
SPFLIST  DS    0C                                                               
         DC    CL8'USPTFILE'                                                    
         DC    CL8'USPTDIR'                                                     
FXSPDIR  DC    CL8'UXSPDIR'                                                     
FXSPFIL  DC    CL8'UXSPFILE'                                                    
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    CL8'NGENDIR'                                                     
         DC    CL8'NGENFIL'                                                     
         DC    CL8'NDEMDIRA'                                                    
         DC    CL8'NDEMDIRN'                                                    
         DC    CL8'NDEMDIRR'                                                    
         DC    CL8'NPAVDIR'                                                     
         DC    CL8'NL=PAVFL'                                                    
         DC    CL8'NL=DEMFA'                                                    
         DC    CL8'NL=DEMFN'                                                    
         DC    CL8'NL=DEMFR'                                                    
         DC    CL8'NNTIDIR'                                                     
         DC    CL8'NL=NTIFL'                                                    
SPTRF    DC    CL8'USTRFFL'                                                     
         DC    CL8'USTRFDR'                                                     
SPRECV   DC    CL8'URECV'                                                       
         DC    C'X'                                                             
*                                                                               
NTFLIST  DS    0C                                                               
         DC    CL8'USPTFILE'                                                    
         DC    CL8'USPTDIR'                                                     
FXNSDIR  DC    CL8'UXSPDIR'                                                     
FXNSFIL  DC    CL8'UXSPFILE'                                                    
         DC    CL8'USTAFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    CL8'NGENDIR'                                                     
         DC    CL8'NGENFIL'                                                     
         DC    CL8'NDEMDIRA'                                                    
         DC    CL8'NDEMDIRN'                                                    
         DC    CL8'NL=DEMFA'                                                    
         DC    CL8'NL=DEMFN'                                                    
         DC    CL8'NPAVDIR'                                                     
         DC    CL8'NL=PAVFL'                                                    
         DC    CL8'NNTIDIR'                                                     
         DC    CL8'NL=NTIFL'                                                    
         DC    CL8'UUNTDIR'                                                     
         DC    CL8'UUNTFIL'                                                     
NTRECV   DC    CL8'URECV'                                                       
         DC    C'X'                                                             
*                                                                               
REFLIST  DS    0C                                                               
         DC    CL8'UREPFILE'                                                    
         DC    CL8'UREPDIR'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    CL8'NL=PAVFL'                                                    
         DC    CL8'NPAVDIR'                                                     
         DC    CL8'NNTIDIR'                                                     
         DC    CL8'NL=NTIFL'                                                    
         DC    CL8'NDEMDIRA'                                                    
         DC    CL8'NDEMDIRN'                                                    
         DC    CL8'NL=DEMFA'                                                    
         DC    CL8'NL=DEMFN'                                                    
         DC    CL8'NGENDIR '                                                    
         DC    CL8'NGENFIL '                                                    
RPRECV   DC    CL8'UREPRCV'                                                     
         DC    C'X'                                                             
*                                                                               
PRFLIST  DS    0C                                                               
         DC    CL8'NPUBDIR'                                                     
         DC    CL8'NPUBFILE'                                                    
         DC    CL8'UPRTDIR'                                                     
         DC    CL8'UPRTFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    CL8'NGENDIR'                                                     
         DC    CL8'NGENFIL'                                                     
PPRECV   DC    CL8'UPRECV'                                                      
         DC    C'X'                                                             
*                                                                               
TAFLIST  DS    0C                                                               
         DC    CL8'UTALFIL'                                                     
         DC    CL8'UTALDIR'                                                     
         DC    CL8'UCHKFIL'                                                     
         DC    CL8'UCHKDIR'                                                     
         DC    CL8'NCTFILE'                                                     
TARECV   DC    CL8'UTALRCV'                                                     
         DC    C'X'                                                             
*                                                                               
TRFLIST  DS    0C                                                               
         DC    CL8'UTRFFILE'                                                    
         DC    CL8'UTRFDIR'                                                     
         DC    CL8'NSTAFIL'                                                     
         DC    CL8'NSPTFIL'                                                     
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NCTFILE'                                                     
TRRECV   DC    CL8'UTRFRCV'                                                     
         DC    C'X'                                                             
*&&                                                                             
*&&UK                                                                           
MEFLIST  DS    0C                                                               
         DC    CL8'NCTFILE'                                                     
MEFLDIR  DC    CL8'NMEDDIR'                                                     
MEFLFIL  DC    CL8'NMEDFIL'                                                     
         DC    C'X'                                                             
*                                                                               
DEMOFLS  DS    0CL8                ADDITIONAL FILES FOR MEDZ OPEN LIST          
         DC    CL8'NDMNDIR'                                                     
         DC    CL8'NDMNNEW'                                                     
         DC    CL8'NDMNOLD'                                                     
         DC    CL8'NDMODIR'                                                     
         DC    CL8'NDMO1FL'                                                     
         DC    CL8'NDMO2FL'                                                     
         DC    CL8'NDMO3FL'                                                     
         DC    CL8'NDMO4FL'                                                     
         DC    C'X'                                                             
*&&                                                                             
ACFLIST  DS    0C                                                               
         DC    CL8'UACCDIR'                                                     
         DC    CL8'UACCMST'                                                     
         DC    CL8'NACCARC'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    CL8'NGENDIR'                                                     
         DC    CL8'NGENFIL'                                                     
ACRECV   DC    CL8'UACCRCV'                                                     
         DC    C'X'                                                             
*                                                                               
PEFLIST  DS    0C                                                               
         DC    CL8'NPERDIR'                                                     
         DC    CL8'NPERFIL'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
*                                                                               
MPFLIST  DS    0C                                                               
MPFLFIL  DC    CL8'NMPLFIL'                                                     
MPFLDIR  DC    CL8'NMPLDIR'                                                     
MPFLEND  EQU   *                                                                
         DC    CL8'NCTFILE'                                                     
*&&UK                                                                           
MPRECV   DC    CL8'NMPLRCV'                                                     
*&&                                                                             
*&&US                                                                           
         DC    CL8'NBUDDIR'                                                     
         DC    CL8'NBUDFIL'                                                     
MPRECV   DC    CL8'NMPLRCV'                                                     
*&&                                                                             
         DC    C'X'                                                             
*                                                                               
CTFLIST  DS    0C                                                               
         DC    CL8'NGENDIR'                                                     
         DC    CL8'NGENFIL'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
*                                                                               
MBFLIST  DS    0C                                                               
         DC    CL8'UMBADIR'                                                     
         DC    CL8'UMBAFIL'                                                     
         DC    CL8'UMBUDIR'                                                     
         DC    CL8'UMBUFIL'                                                     
         DC    CL8'NCTFILE'                                                     
MBRECV   DC    CL8'UMBARCV'                                                     
         DC    C'X'                                                             
         EJECT                                                                  
***********************************************************************         
* EQUATES, LITERALS AND CONSTANTS                                     *         
***********************************************************************         
K        EQU   1024                                                             
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
STRFDRQ  EQU   X'33'                                                            
FF       EQU   X'FF'                                                            
MEDZSEQ  EQU   X'14'                                                            
MPL1SEQ  EQU   X'05'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
PRTBL01  DC    C'BL01'                                                          
DMOPEN   DC    C'OPEN   '                                                       
DMCLSE   DC    C'DMCLSE '                                                       
DMADD    DC    C'ADD    '                                                       
DMREAD   DC    C'DMREAD '                                                       
SYSFLES  DC    C'SYSFLES'                                                       
FACWRK   DC    C'FACWRK  '                                                      
MEDSYS   DC    C'MEDIA  '                                                       
CLOREG   DC    C'CLOSE'                                                         
CLOUSR   DC    C'CLO/USR'                                                       
CLOPUR   DC    C'CLO/PUR'                                                       
CLOERR   DC    C'CLO/ERR'                                                       
FWKSOON  DC    C'SOON'                                                          
FWKTSO   DC    C'TSO '                                                          
SRTEND   DC    C'END'                                                           
DEQALL   DC    C'IFI'                                                           
R00A     DC    C'R',X'000A'                                                     
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
NOOP     BR    RE                                                               
*                                                                               
ATYPLST  DC    A(TYPLST)                                                        
ASPOOFND DC    A(SPOOFND)                                                       
ASYSFAC  DC    A(SYSFAC)                                                        
ASYSFAC0 DC    A(SYSFAC0)                                                       
ASYSFAC2 DC    A(SYSFAC2)                                                       
ASYSFAC3 DC    A(SYSFAC3)                                                       
ASYSFAC4 DC    A(SYSFAC4)                                                       
ASYSFAC5 DC    A(SYSFAC5)                                                       
*                                                                               
VWORKER  DC    V(WORKER)                                                        
VSORTER  DC    V(SORTER)                                                        
VBUFFALO DC    V(BUFFALO)                                                       
VFAXLINK DC    V(FAXLINK)                                                       
VFAXINFO DC    V(FAXINFO)                                                       
VSHMUSS  DC    V(DMSHMUSS)                                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EMULATE FACALLOV (NOTE PHASENAM IS ACTUALLY MCDUB)       *         
***********************************************************************         
CALLOV   NTR1  BASE=*,LABEL=*,WORK=(RC,WORKL)                                   
         MVI   FLAG,0                                                           
         LR    R3,R1               R3=A(PARAMETER LIST)                         
         L     R9,=V(MASTC)                                                     
         USING MASTD,R9            R9=A(MASTC)                                  
*                                                                               
         MVC   PHASENAM,MCSPACES   BUILD PHASE NAME IN MCDUB                    
         MVI   PHASEPFX,C'T'                                                    
         CLI   4(R3),0             TEST MAINTENANCE CALL                        
         BNE   CALLOV02                                                         
*                                                                               
CALLOV1  MVC   PHASESP,MCLOAD+1    APPLICATION CALL - P1=X'00',P2=X'00'         
         ICM   RF,B'1000',0(R3)    RF=X'00......'                               
         SR    RE,RE                                                            
         SLDL  RE,4                                                             
         IC    RE,CALLOVHT(RE)                                                  
         STC   RE,PHASEOLY                                                      
         SR    RE,RE                                                            
         SLDL  RE,4                                                             
         IC    RE,CALLOVHT(RE)                                                  
         STC   RE,PHASEOLY+1                                                    
         CLI   0(R3),FF            TEST LOADING VIRGIN SCREEN                   
         BNE   CALLOV06                                                         
         L     R0,=A(TWA)                                                       
         AHI   R0,CONHEADH-TWAD    YES-POINT TO START OF TWA                    
         B     CALLOV04                                                         
*                                                                               
CALLOV02 LHI   R0,6                MAINTENENCE CALL - P2=X'D90SPPOO'            
         LA    R1,WORK                                                          
         ICM   RF,B'1110',5(R3)    RF=X'0SPPOO..'                               
CALLOV03 SR    RE,RE                                                            
         SLDL  RE,4                                                             
         IC    RE,CALLOVHT(RE)                                                  
         STC   RE,0(R1)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,CALLOV03                                                      
*                                                                               
         MVC   PHASESPO,WORK+1     WORK=C'0SPPOO'                               
         MVI   FLAG,1                                                           
         CLC   PHASESP(3),=C'00A'  TEST CORE RESIDENT PHASE LOAD                
         BE    CALLOV08                                                         
         CLC   PHASESP(3),=C'00B'                                               
         BE    CALLOV08                                                         
         MVI   FLAG,0                                                           
*                                                                               
         SR    R0,R0               TEST WE ARE LOADING A SCREEN                 
         ICM   R0,7,1(R3)          R0=PHASE LOAD ADDRESS                        
         C     R0,=A(TWA)                                                       
         BL    CALLOV06                                                         
         C     R0,=A(TWAX)                                                      
         BH    CALLOV06                                                         
*                                                                               
CALLOV04 MVC   PHASETST,MCTEST2    SET MCTEST2 FOR SCREENS                      
         GOTOR MCVLOADM,PARMS,(R0)                                              
         JNE   *+2                                                              
         MVC   8(4,R3),0(R1)       PASS USER L'PHASE                            
*                                                                               
         LR    R1,R0               SET ALL OUTPUT LENGTHS TO ZERO               
         SR    R0,R0                                                            
         BASR  RE,0                                                             
         ICM   R0,1,0(R1)          EXIT AT END OF LOADED SCREEN                 
         JZ    EXIT                                                             
         MVI   7(R1),0                                                          
         AR    R1,R0                                                            
         BR    RE                                                               
*                                                                               
CALLOV06 MVC   PHASETST,MCTEST1    USE MCTEST1 FOR ROOT PHASE                   
         CLC   PHASEOLY(2),=C'00'                                               
         BE    CALLOV08                                                         
         MVC   PHASETST,MCTEST2    AND MCTEST2 FOR OVERLAYS                     
*                                                                               
CALLOV08 L     R2,=A(LOADLIST)     LOOK UP IN LOADED PHASE LIST                 
         USING LOADLD,R2                                                        
         LA    R0,LOADLMAX                                                      
CALLOV10 CLI   LOADLD,0            TEST END OF LIST                             
         BE    CALLOV12                                                         
         CLC   LOADLNAM,PHASENAM   MATCH PHASE NAME TO TABLE ENTRY              
         BE    *+14                                                             
         AHI   R2,LOADLLEN         BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,CALLOV10                                                      
         DC    H'0'                INCREASE LOADLMAX                            
*                                                                               
         CLI   FLAG,1              DON'T RELOAD IF T00A OR T00B PHASE           
         BE    CALLOV14                                                         
         DELETE EPLOC=LOADLACT     DELETE THE PHASE                             
*                                                                               
CALLOV12 GOTOR MCVLOADM,PARMS,0    CALL LOADEM TO LOAD PHASE                    
         MVC   LOADLACT,MCDUB      DDMASTER CAN CHANGE PHASE NAME               
         MVC   LOADLPHA,4(R1)      SAVE PHASE ADDRESS IN TABLE                  
         MVC   LOADLPHL,0(R1)      SAVE PHASE LENGTH IN TABLE                   
*                                                                               
CALLOV14 MVC   0(4,R3),LOADLPHA    PASS BACK LOAD ADDRESS                       
         MVC   8(4,R3),LOADLPHL    AND LENGTH OF PHASE                          
         J     EXIT                                                             
         DROP  R2                                                               
*                                                                               
CALLOVHT DC    C'0123456789ABCDEF'                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO APPLY PATCHES (NOTE THIS IS THE TWAPTCHR ROUTINE)        *         
* PATCH CARDS ARE ADDED TO PTCHPOOL BY DDMASTER                       *         
* FORMAT OF EACH PATCH CARD IS:                                       *         
*                                                                     *         
* PATCH NN DDDDDD XXXXXXXX...XXXXXXXX                                 *         
*                                                                     *         
* WHERE NN IS PHASE NUMBER, DD IS DISPLACEMENT AND XX IS PATCH VALUE  *         
***********************************************************************         
PATCHER  NTR1  BASE=*,LABEL=*,WORK=(RC,WORKL)                                   
         L     R9,=V(MASTC)                                                     
         MVC   PARMS(16),0(R1)     EXTRACT PHASE ADDRESSES                      
         L     R2,MCVPATCH                                                      
*                                                                               
PATCHER2 CLC   0(5,R2),=C'PATCH'   TEST ANY MORE PATCH CARDS                    
         JNE   EXIT                                                             
         IC    R3,7(R2)            GET ADDRESS OF PHASE                         
         SLL   R3,28                                                            
         SRL   R3,28                                                            
         LTR   R3,R3               ENSURE PHASE NON-ZERO                        
         BZ    PATCHERR                                                         
         CHI   R3,4                ENSURE PHASE NGR 4                           
         BH    PATCHERR                                                         
         BCTR  R3,0                                                             
         SLL   R3,2                                                             
         LA    R3,PARMS(R3)        R3=PHASE ADDRESS                             
         ICM   R3,15,0(R3)         ENSURE PHASE ADDRESS SET                     
         BZ    PATCHERR                                                         
*                                                                               
         GOTOR MCVHEXIN,DMCB,9(R2),WORK,6                                       
         OC    12(4,R1),12(R1)                                                  
         BZ    PATCHERR                                                         
         SR    RE,RE                                                            
         ICM   RE,7,WORK                                                        
         AR    R3,RE               R3=PATCH ADDRESS                             
*                                                                               
         LA    RF,16(R2)           GET LENGTH OF PATCH                          
         SR    R0,R0                                                            
PATCHER4 CLI   0(RF),C' '                                                       
         BE    *+16                                                             
         AHI   RF,1                                                             
         AHI   R0,1                                                             
         B     PATCHER4                                                         
*                                                                               
         LTR   R0,R0               ENSURE NON-ZERO LENGTH                       
         BZ    PATCHERR                                                         
         TML   R0,X'0001'          AND EVEN NUMBER OF CHARACTERS                
         BNZ   PATCHERR                                                         
         GOTOR MCVHEXIN,DMCB,16(R2),WORK,(R0)                                   
         ICM   RF,15,12(R1)                                                     
         BZ    PATCHERR                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK                                                     
         AHI   R2,L'PTCHPOOL                                                    
         B     PATCHER2                                                         
*                                                                               
PATCHERR DC    H'0',C'YOU BLEW THE PATCH CARD'                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EMULATE FAGETFACT                                        *         
***********************************************************************         
GETFACT  NTR1  BASE=*,LABEL=*                                                   
         L     R9,=V(MASTC)                                                     
         LA    RF,MCWORK                                                        
         ST    RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         XC    FACTSD(FACTSLEN),FACTSD                                          
         MVI   FATFLAG,1           SET OFFLINE APPLICATION                      
         MVC   FADATE,MCDATE                                                    
         MVC   FAASSB,MCSSB                                                     
         MVC   FAAUTL,MCUTL                                                     
         MVC   FASYSLST,=A(SYSLST)                                              
         MVC   FAACTRY,=A(CTRYTAB)                                              
         MVC   FAALANG,=A(LANGTAB)                                              
         MVC   FAXLATES,=A(CTRYXLAT)                                            
         MVC   FACTRY,MCCTRY                                                    
         MVC   FALANG,MCLANG                                                    
         MVC   FAOVSYS,MCOVSYS                                                  
         MVC   FASYS,MCIDSENO                                                   
         MVC   FAPASSWD,MCRFHDR+(RQHPSWD-RQHITRM)                               
         J     EXIT                                                             
         DROP  RF                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMMON EXIT, TABLES AND CONSTANTS                                   *         
***********************************************************************         
EXIT     XIT1                                                                   
*                                                                               
         DS    0D                                                               
         DC    CL16'******SSB******'                                            
SSB      DC    (SSOLNQ)X'00'                                                    
         ORG   SSB+(SSOXTND-SSBD)                                               
         DC    AL1(FF,SSOSNRCV)                                                 
         ORG   SSB+(SSOFWNDX-SSBD)                                              
         DC    A(FWKINDX)                                                       
         DC    A(FWKBUFF)                                                       
         ORG   SSB+(SSOFLAG3-SSBD)                                              
         DC    AL1(SSO3XUTL)       OFFLINE EXTENDED UTL                         
         ORG                                                                    
*                                                                               
FACLIST  DS    0XL5                                                             
         DC    AL2(SYSFAC0-SPOOF)                                               
         DC    AL1(QGENERAL),AL2(CGENERAL-COMFACSD)                             
*&&UK                              NEED TO BE HERE FOR MPL                      
         DC    AL2(SYSFAC5-SPOOF)                                               
         DC    AL1(QNETWORK),AL2(ANETWORK-MEDFACSD)                             
         DC    AL2(SYSFAC5-SPOOF)                                               
         DC    AL1(QSTABLE),AL2(ASTABLE-MEDFACSD)                               
         DC    AL2(SYSFAC5-SPOOF)                                               
         DC    AL1(QTVDIR),AL2(ATVDIR-MEDFACSD)                                 
         DC    AL2(SYSFAC5-SPOOF)                                               
         DC    AL1(QTVDIR2),AL2(ATVDIR2-MEDFACSD)                               
         DC    AL2(SYSFAC5-SPOOF)                                               
         DC    AL1(QDEMAND),AL2(ADEMAND-MEDFACSD)                               
         DC    AL2(SYSFAC5-SPOOF)                                               
         DC    AL1(QGETPGM),AL2(AGETPGM-MEDFACSD)                               
*&&                                                                             
         DC    AL2(SYSFAC0-SPOOF)                                               
         DC    AL1(QREQTWA),AL2(CREQTWA-COMFACSD)                               
         DC    AL2(SYSFAC0-SPOOF)                                               
         DC    AL1(QMINIO),AL2(CMINIO-COMFACSD)                                 
*&&UK                                                                           
         DC    AL2(SYSFAC0-SPOOF)                                               
         DC    AL1(QCONVERT),AL2(CCONVERT-COMFACSD)                             
         DC    AL2(SYSFAC4-SPOOF)                                               
         DC    AL1(QMBVAL),AL2(SMBVAL-MBFACSD)                                  
         DC    AL2(SYSFAC4-SPOOF)                                               
         DC    AL1(QMBVAL1),AL2(SMBVAL1-MBFACSD)                                
         DC    AL2(SYSFAC4-SPOOF)                                               
         DC    AL1(QMBVAL2),AL2(SMBVAL2-MBFACSD)                                
         DC    AL2(SYSFAC4-SPOOF)                                               
         DC    AL1(QMBSPVAL),AL2(SMBSPVAL-MBFACSD)                              
         DC    AL2(SYSFAC4-SPOOF)                                               
         DC    AL1(QMBLVAL),AL2(SMBLVAL-MBFACSD)                                
         DC    AL2(SYSFAC4-SPOOF)                                               
         DC    AL1(QMBQED),AL2(SMBQED-MBFACSD)                                  
         DC    AL2(SYSFAC4-SPOOF)                                               
         DC    AL1(QMBNMVAL),AL2(SMBNMVAL-MBFACSD)                              
         DC    AL2(SYSFAC4-SPOOF)                                               
         DC    AL1(QMBQCOMP),AL2(SMBQCOMP-MBFACSD)                              
         DC    AL2(SYSFAC4-SPOOF)                                               
         DC    AL1(QMBDATER),AL2(SMBDATER-MBFACSD)                              
         DC    AL2(SYSFAC4-SPOOF)                                               
         DC    AL1(QMBDCALC),AL2(SMBDCALC-MBFACSD)                              
         DC    AL2(SYSFAC4-SPOOF)                                               
         DC    AL1(QMBLOOK),AL2(SMBLOOK-MBFACSD)                                
         DC    AL2(SYSFAC4-SPOOF)                                               
         DC    AL1(QMBLOOK),AL2(SMBLOOK-MBFACSD)                                
         DC    AL2(SYSFAC4-SPOOF)                                               
         DC    AL1(QUNDAY),AL2(SUNDAY-MBFACSD)                                  
         DC    AL2(SYSFAC4-SPOOF)                                               
         DC    AL1(QMBVAL3),AL2(SMBVAL3-MBFACSD)                                
         DC    AL2(SYSFAC4-SPOOF)                                               
         DC    AL1(QMBVAL4),AL2(SMBVAL4-MBFACSD)                                
         DC    AL2(SYSFAC5-SPOOF)                                               
         DC    AL1(QGTVDIR),AL2(AGTVDIR-MEDFACSD)                               
*&&                                                                             
*&&US                                                                           
         DC    AL2(SYSFAC0-SPOOF)                                               
         DC    AL1(QSOFDAT),AL2(CSOFDAT-COMFACSD)                               
         DC    AL2(SYSFAC0-SPOOF)                                               
         DC    AL1(QDEMADDR),AL2(CDEMADDR-COMFACSD)                             
         DC    AL2(SYSFAC0-SPOOF)                                               
         DC    AL1(QDEMTABS),AL2(CDEMTABS-COMFACSD)                             
         DC    AL2(SYSFAC0-SPOOF)                                               
         DC    AL1(QDEMAND),AL2(CDEMAND-COMFACSD)                               
         DC    AL2(SYSFAC0-SPOOF)                                               
         DC    AL1(QDDISP),AL2(CT00AD0-COMFACSD)                                
         DC    AL2(SYSFAC0-SPOOF)                                               
         DC    AL1(QDEMEL),AL2(CDEMEL-COMFACSD)                                 
         DC    AL2(SYSFAC0-SPOOF)                                               
         DC    AL1(QDEMAINT),AL2(CDEMAINT-COMFACSD)                             
         DC    AL2(SYSFAC0-SPOOF)                                               
         DC    AL1(QDEMOMTH),AL2(CDEMOMTH-COMFACSD)                             
         DC    AL2(SYSFAC0-SPOOF)                                               
         DC    AL1(QDEMOUT),AL2(CDEMOUT-COMFACSD)                               
         DC    AL2(SYSFAC0-SPOOF)                                               
         DC    AL1(QDEMOVAL),AL2(CDEMOVAL-COMFACSD)                             
FAC005A  DC    AL2(SYSFAC0-SPOOF)                                               
         DC    AL1(QCOMINTR),AL2(CCOMINTR-COMFACSD)                             
*&&                                                                             
FACLISTN EQU   (*-FACLIST)/L'FACLIST                                            
*                                                                               
TYPLST   DS    0XL4                                                             
         DC    C'808',AL1(02)      REP SPOOL                                    
         DC    C'21D',AL1(02)      NET SPOOL                                    
         DC    C'60C',AL1(02)      ACC SPOOL                                    
*&&UK*&& DC    C'40C',AL1(01)      APROG                                        
*&&UK*&& DC    C'40D',AL1(01)      RATIO                                        
*&&UK*&& DC    C'411',AL1(02)      ATTRIBUTION                                  
*&&UK*&& DC    C'413',AL1(05)      MEDIA/FLIST                                  
*&&UK*&& DC    C'415',AL1(02)      MEDIA/DEAL                                   
*&&UK*&& DC    C'510',AL1(01)      MPL/CRAFT (2001)                             
*&&UK*&& DC    C'511',AL1(02)      MPL/ATTRIBUTION                              
*&&UK*&& DC    C'512',AL1(01)      MPL/CRAFT (1991)                             
*&&UK*&& DC    C'513',AL1(01)      MPL/CANADIAN CRAFT                           
*&&UK*&& DC    C'702',AL1(02)      ARTISTE FEES                                 
         DC    C'902',AL1(05)      MEDIABASE-LFM                                
         DC    AL1(FF,FF,FF,4)     OTHERS                                       
*                                                                               
SYSFAC   DS    0A                  SYSTEM FACILITIES LIST                       
         DC    V(DATAMGR)                                                       
         DC    A(CALLOV)                                                        
         DC    39A(0)                                                           
         DC    V(SELIST)                                                        
         DC    29A(0)                                                           
*                                                                               
         ENTRY COMFACS                                                          
COMFACS  DS    0A                                                               
*                                                                               
SYSFAC0  DS    0A                  COMFACS - COMMON FACILITIES LIST             
         DC    (COMFACSL)X'00'                                                  
         ORG   SYSFAC0                                                          
         DC    V(DATAMGR)                                                       
         DC    A(CALLOV)                                                        
         DC    V(GETMSG)                                                        
         DC    V(GETTXT)                                                        
         DC    A(0)                FASWITCH                                     
         DC    V(HELLO)                                                         
         DC    V(SCANNER)                                                       
         DC    V(UNSCAN)                                                        
         DC    V(HEXIN)                                                         
         DC    V(HEXOUT)                                                        
         DC    V(CASHVAL)                                                       
         DC    V(DATVAL)                                                        
         DC    V(DATCON)                                                        
         DC    A(0)                TERMVAL                                      
         DC    V(SCUNKEY)                                                       
         DC    V(ADDAY)                                                         
         DC    V(GETDAY)                                                        
         DC    V(GETPROF)                                                       
         DC    V(PERVERT)                                                       
         DC    A(GETFACT)                                                       
         DC    V(XSORT)                                                         
         DC    A(0)                                                             
         DC    V(GETFLD)                                                        
*&&UK                                                                           
         DC    V(PERVAL)                                                        
         DC    V(DLFLD)                                                         
         DC    A(0)                                                             
         DC    3A(0)                                                            
         DC    V(LIMACC)                                                        
         DC    8A(0)                                                            
         DC    V(COVAIL)                                                        
         DC    5A(0)                                                            
*&&                                                                             
*&&US                                                                           
         DC    A(0)                SOFDAT                                       
         DC    A(0)                DEMADDR                                      
         DC    A(0)                DEMDISP                                      
         DC    A(0)                DEMTABS                                      
         DC    A(0)                DSTATION                                     
         DC    A(0)                DMASTER                                      
         DC    A(0)                DFORMULA                                     
         DC    A(0)                DNAME                                        
         DC    A(0)                DCODE                                        
         DC    A(0)                DCONTROL                                     
         DC    A(0)                DADJUST                                      
         DC    A(0)                DEMOUT                                       
         DC    A(0)                DEMEL                                        
         DC    A(0)                DEMAINT                                      
         DC    A(0)                DEMAND                                       
         DC    A(0)                DEMOMATH                                     
         DC    A(0)                DEMOVAL                                      
         DC    A(0)                GENERAL                                      
         DC    V(PERVAL)                                                        
         DC    V(DLFLD)                                                         
         DC    A(0)                                                             
*&&                                                                             
         DC    A(0)                GLOBBER                                      
         DC    A(0)                                                             
         DC    V(PARSNIP)                                                       
         DC    V(DICTATE)                                                       
         DC    A(0)                EDITOR                                       
         DC    V(MQRPT)            OFFLINE=MQRPT,ONLINE=GETHELP                 
         DC    V(CUREDIT)          CURREDIT                                     
         DC    V(GETRET)           GETRET                                       
*&&UK*&& DC    V(REPORT)           REPORT                                       
*&&US*&& DC    A(0)                                                             
*&&UK*&& DC    V(BLDCUR)           BLDCUR                                       
*&&US*&& DC    A(0)                                                             
         DC    A(0)                GETCUR                                       
         DC    A(0)                GETNARR                                      
         DC    V(DEJAVU)           DEJAVU                                       
         DC    V(SECRET)           SECRET                                       
         DC    V(BILLIT)           BILLIT                                       
         DC    A(0)                LOCKET                                       
         DC    V(PQPROF)           PQPROF                                       
         DC    A(0)                SCRIPT                                       
         DC    A(0)                DATTIM                                       
         DC    V(BINSRCH)          BINSRCH                                      
         DC    V(PROTON)                                                        
         DC    V(PROTOFF)                                                       
         DC    V(HELEN)                                                         
         DC    A(0)                MQIO                                         
*&&US*&& DC    A(0)                                                             
*&&UK*&& DC    V(EUREKA)           EUREKA                                       
         DC    V(LOCKUP)                                                        
         DC    V(MASTC)            MASTC                                        
         DC    V(LOCKSPC)          LOCKSPACE                                    
         DC    2A(0)               SPARE                                        
         DC    V(DB2IO)                                                         
         DC    A(0)                LINKIO                                       
         DC    A(0)                RECUP                                        
         DC    A(0)                GETRAD                                       
         DC    A(0)                OFFLAL                                       
         DC    A(0)                XTRAINF                                      
         DC    A(0)                PERSON                                       
         DC    A(0)                JESMAIL                                      
         DC    A(0)                VEMAIL                                       
         DC    A(0)                FABSAM                                       
         DC    A(0)                WRKIO                                        
         DC    A(0)                DEMOCON                                      
         DC    V(FAPQSEC)          FAPQSEC                                      
         DC    A(0)                SPARE                                        
         DC    V(DYNALLOC)         DYNALLOC                                     
         DC    A(0)                COMINTER                                     
         DC    A(0)                SPARE                                        
         DC    A(0)                SPARE                                        
         DC    A(0)                SPARE                                        
         DC    A(0)                SPARE                                        
         DC    A(0)                SPARE                                        
         DC    A(0)                SPARE                                        
*                                                                               
SYSFAC2  DS    0A                  SPOT/PRINT SYSTEMS                           
         DC    V(DATAMGR)                                                       
         DC    A(CALLOV)                                                        
         DC    V(CASHVAL)                                                       
         DC    V(DATVAL)                                                        
         DC    V(ADDAY)                                                         
         DC    A(0)                DTCNV                                        
         DC    V(GETDAY)                                                        
         DC    V(RECUP)                                                         
         DC    V(GETMSG)                                                        
         DC    V(DATCON)                                                        
         DC    V(SCANNER)                                                       
         DC    A(0)                USCAN                                        
         DC    12A(0)                                                           
*                                                                               
SYSFAC3  DS    0A                  ACC/REP/GAMES/CPP/PER/TAL                    
         DC    V(DATAMGR)                                                       
         DC    A(CALLOV)                                                        
         DC    V(CASHVAL)                                                       
         DC    V(DATVAL)                                                        
         DC    V(DATCON)                                                        
         DC    V(GETMSG)                                                        
         DC    V(RECUP)                                                         
         DC    V(ADDAY)                                                         
         DC    V(GETDAY)                                                        
         DC    A(0)                USCAN                                        
         DC    V(DMACCEMU)                                                      
         DC    13A(0)                                                           
*                                                                               
SYSFAC4  DS    0A                  MEDIABASE                                    
         DC    V(DATAMGR)                                                       
         DC    A(CALLOV)                                                        
         DC    V(CASHVAL)                                                       
         DC    V(DATVAL)                                                        
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    V(GETDAY)                                                        
         DC    V(GETMSG)                                                        
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    V(SCANNER)                                                       
         DC    A(0)                                                             
         DC    12A(0)                                                           
         DC    V(RECUP)                                                         
         DC    V(DAYVAL)                                                        
         DC    34A(0)                                                           
*                                                                               
SYSFAC5  DS    0A                  MEDIA SYSTEM (SYSFAC2 IN FATAB)              
*&&UK                                                                           
         DC    V(DATAMGR)                                                       
         DC    A(CALLOV)                                                        
         DC    V(CASHVAL)                                                       
         DC    V(DATVAL)                                                        
         DC    V(TMPACK)                                                        
         DC    V(TMUNPK)                                                        
         DC    V(GETDAY)                                                        
         DC    V(GETMSG)                                                        
         DC    V(BUDGEP)                                                        
         DC    V(BUDGET)                                                        
         DC    A(0)                DEMAND (PHASE)                               
         DC    V(GETTARG)                                                       
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    V(SCANNER)                                                       
         DC    A(0)                STABLE (PHASE)                               
         DC    V(NETWORK)                                                       
         DC    V(BUYTER)                                                        
         DC    A(0)                UK STABLE TABLES (PHASE)                     
         DC    V(TVLIST)                                                        
         DC    V(MEDEMREG)                                                      
         DC    A(0)                DEFUNCT-WAS RADLIST                          
         DC    V(GETDPT)                                                        
         DC    V(GETEQIV)                                                       
         DC    V(BLDCUR)                                                        
         DC    2A(0)               NOT USED OFFLINE                             
         DC    V(BLDMED)                                                        
         DC    6A(0)               NOT USED OFFLINE                             
         DC    V(DEMVALS)                                                       
         DC    V(GETCPT)                                                        
         DC    2A(0)               NOT USED OFFLINE                             
         DC    V(GETAUD)                                                        
         DC    V(GETUNV)                                                        
         DC    3A(0)               NOT USED OFFLINE                             
         DC    A(0)                GETPGM (PHASE)                               
         DC    A(0)                NOT USED OFFLINE                             
         DC    V(DICTATE)                                                       
         DC    A(0)                NOT USED OFFLINE                             
         DC    A(0)                GERMAN STABLE TABLES (PHASE)                 
         DC    V(TVLIST)                                                        
         DC    V(GETWKS)           MEGETWKS                                     
         DC    V(BINSRCH)          BINSRCH                                      
         DC    A(0)                NOT USED OFFLINE                             
         DC    V(MEFILT)           MEFILT                                       
         DC    4A(0)               NOT USED OFFLINE                             
         DC    V(GETRAT2)          GETRAT2                                      
         DC    3A(0)               NOT USED OFFLINE                             
         DC    V(GETQRP)           MEGETQRP                                     
         DC    A(0)                NOT USED OFFLINE                             
         DC    A(SYSFAC0)          COMFACS                                      
*&&                                                                             
         DS    0D                                                               
         DC    CL16'**FACWRK INDEX*'                                            
FWKINDX  DC    16X'00'             FACWRK INDEX AREA                            
*                                                                               
         DC    CL16'**FACWRK BUFF**'                                            
FWKBUFF  DC    (14*K)X'00'        FACWRK BUFFER AREA                            
*                                                                               
         DC    CL16'******UTL******'                                            
UTL      DC    (TUTLXALN)X'00'                                                  
         ORG   UTL+(TUTLXADR-UTLD)                                              
         DC    A(XAUTL)                                                         
         ORG                                                                    
*                                                                               
         DC    CL16'****XAUTL******'                                            
XAUTL    DC    (XASWTABL)X'00'                                                  
         ORG   XAUTL                                                            
         DC    AL4(XASWTABL)                                                    
         ORG   XAUTL+(XAASWTAB-XAUTLD)                                          
         DC    A(XAUTL+(XASWTAB-XAUTLD))                                        
         ORG                                                                    
*                                                                               
         DC    CL16'******TIA*******'                                           
TIA      DC    (18*K)X'00'                                                      
*                                                                               
         DC    CL16'******TWA*******'                                           
TWA      DC    (18*K)X'00'                                                      
TWAX     EQU   *                                                                
TWALNQ   EQU   *-TWA                                                            
*                                                                               
         DC    CL16'****SPFUSER*****'                                           
SPFUSER  DC    (K)X'00'                                                         
*&&US                                                                           
         DC    CL16'*****DUMMY******'                                           
DUMMY    DC    (275*K)X'00'                                                     
*&&                                                                             
         DC    CL16'***PATCH POOL***'                                           
PTCHPOOL DC    24XL(L'MCREQREC)'00'                                             
*                                                                               
         DC    CL16'***LOAD LIST****'                                           
LOADLIST DC    (LOADLMAX)XL(LOADLLEN)'00',X'00'                                 
*                                                                               
WORKAREA DS    0D                  WORKING STORAGE AREA                         
*&&UK*&& DC    (300*K)X'00'                                                     
*&&US*&& DC    (450*K)X'00'                                                     
*                                                                               
SPOOFND  DS    0X                  END OF SPOOF                                 
         EJECT                                                                  
***********************************************************************         
* TABLES OF COUNTRIES, LANGUAGES, SYSTEMS AND TWA ADCONS              *         
***********************************************************************         
* FALANGTAB                                                                     
       ++INCLUDE FALANGTAB                                                      
* FACTRYTAB                                                                     
       ++INCLUDE FACTRYTAB                                                      
* FACTRYXLAT                                                                    
       ++INCLUDE FACTRYXLAT                                                     
* FASYSLST                                                                      
       ++INCLUDE FASYSLST                                                       
* DDTWADCONS                                                                    
ADCONS   DS    0A                                                               
       ++INCLUDE DDTWADCONS                                                     
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
DMCBL    EQU   *-DMCB                                                           
DMCB1    DS    4F                                                               
DMCB1L   EQU   *-DMCB1                                                          
PARMS    DS    6F                                                               
REQTOT   DC    PL2'0'                                                           
FLAG     DS    X                                                                
CONNUMF  DS    XL1                 N'CONTROL FIELDS IN TWA                      
SKIPREQ  DS    C                   SKIP RESEQUEST BASED ON START=               
NOCLRTWA DS    X                   CLR TWA=0, NO CLR TWA=FF                     
*                                                                               
ACOMINTR DS    A                   A(COMINTOR)                                  
AREQTWA  DS    A                   A(REQTWA)                                    
         DS    0D                                                               
XTRAINFO DS    0XL40               EXTRA INFO PASSED TO APPLICATION             
XIAGCOPT DS    X                   AGENCY OPTIONS                               
XIAGCTRY DS    X                   AGENCY COUNTRY                               
XICTRY   DS    X                   ACTUAL COUNTRY (WHERE TERMINAL IS)           
XILANG   DS    X                   AGENCY LANGUAGE CODE                         
XIAGCURR DS    CL3                 AGENCY CURRENCY CODE                         
XIFLAG1  DS    X                   CONNECT INFO FLAG                            
XIROSYS  EQU   X'80'               CONNECTED TO READ ONLY SYSTEM                
XIROMODE EQU   X'40'               CONNECTED IN READ ONLY MODE                  
XIWRONGF EQU   X'20'               CONNECTED TO WRONG FACPAK                    
XITSTADV EQU   X'10'               CONNECTED TO TEST FACPAK                     
XIDDSPER EQU   X'08'               DDS PERSON                                   
XIDDSTRM EQU   X'04'               DDS TERMINAL                                 
XIPPSAGY EQU   X'02'               PPS AGENCY                                   
XIDDSPOV EQU   X'01'               DDS PERSON OVERRIDE ON AGYPER                
*                                                                               
XIUSER   DS    XL2                 USER ID                                      
XIUPDFAC DS    CL4                 UPDATIVE FACPAK ID                           
XIAGYALP DS    CL2                 AGENCY ALPHA                                 
XIAGYSEC DS    CL2                 AGENCY SECURITY ALPHA                        
XIAGYPER DS    CL2                 AGENCY PERSONID ALPHA                        
XIPID    DS    XL2                 AGENCY PERSON ID NUMBER                      
*                                                                               
XIFLAG2  DS    XL1                 CONNECT INFO FLAG                            
XICSCADV EQU   X'80'               CONNECTED TO CSC SYSTEM                      
XICTUEN  EQU   X'40'               CONNECTED WITH U=N                           
XICTIAM  EQU   X'20'               CONNECTED WITH I=XXX                         
*                                                                               
         DS    XL5                 NOT DEFINED OFFLINE                          
XIATRUC  DS    AL4                 ADDRESS OF TRANSLATE TO UPPER CASE           
         DS    XL8                 SPARE                                        
         ORG   XTRAINFO+L'XTRAINFO                                              
*                                                                               
CTRYTRUC DS    XL256               COUNTRY TRANSLATE TO UPPER CASE COPY         
       ++INCLUDE DEDBLOCK          COMSCORE                                     
*                                                                               
WORKL    EQU   *-WORKD                                                          
                                                                                
SYSTABD  DSECT ,                   DSECT TO COVER SYSTEM TABLE                  
SYSTCODE DS    CL2                 SYSTEM CODE                                  
SYSTNTPK DS    XL1                 MCNETPAK VALUE OR X'FF'=NO CHECK             
SYSTNAME DS    CL8                 SYSTEM NAME (FOR DMOPEN)                     
SYSTDISP DS    AL2                 DISPLACEMENT TO FILE LIST                    
SYSTABL  EQU   *-SYSTABD                                                        
SYSTEOTQ EQU   X'FF'               END OF TABLE INDICATOR                       
                                                                                
LOADLD   DSECT ,                   DSECT TO COVER LOADLIST                      
LOADLACT DS    0CL8                ACTUAL PHASE NAME                            
LOADLNAM DS    CL6                 PHASE NAME                                   
LOADLTST DS    CL1                 TEST LEVEL                                   
         DS    CL1                 N/D                                          
LOADLPHA DS    AL4                 PHASE ADDRESS                                
LOADLPHL DS    AL4                 PHASE LENGTH                                 
LOADLLEN EQU   *-LOADLD            LENGTH OF ENTRY                              
LOADLMAX EQU   256                 MAXIMUM N'TABLE ENTRIES                      
         EJECT                                                                  
***********************************************************************         
* TWA DSECT                                                           *         
***********************************************************************         
TWAD     DSECT                                                                  
         DS    XL64                                                             
CONHEADH EQU   *                                                                
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
***********************************************************************         
* DDMASTER DSECT                                                      *         
***********************************************************************         
       ++INCLUDE DDMASTD                                                        
         ORG   MCDUB                                                            
PHASENAM DS    0CL8                ** REDEFINE MCDUB FOR PHASE NAME **          
PHASEPFX DS    CL1                                                              
PHASESPO DS    0CL5                                                             
PHASESP  DS    0CL3                                                             
PHASESYS DS    CL1                                                              
PHASEPRG DS    CL2                                                              
PHASEOLY DS    CL2                                                              
PHASETST DS    CL1                                                              
         DS    CL1                                                              
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
* DDBIGBOX                                                                      
       ++INCLUDE DDBIGBOX                                                       
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
* DDCOREQUS                                                                     
       ++INCLUDE DDCOREQUS                                                      
* DDFAXINFOD                                                                    
       ++INCLUDE DDFAXINFOD                                                     
* DDLOGOD                                                                       
       ++INCLUDE DDLOGOD                                                        
* DMDDNAMED                                                                     
DDNAMED  DSECT                                                                  
       ++INCLUDE DMDDNAMED                                                      
* DMREQHDRA                                                                     
       ++INCLUDE DMREQHDRA                                                      
* DMSYSFD                                                                       
       ++INCLUDE DMSYSFD                                                        
* DMWRKRK                                                                       
       ++INCLUDE DMWRKRK                                                        
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
* FASSBOFF                                                                      
       ++INCLUDE FASSB                                                          
         ORG   SSBD                                                             
       ++INCLUDE FASSBOFF                                                       
SSOLNQ   EQU   *-SSBD                                                           
         ORG                                                                    
* FAUTL                                                                         
       ++INCLUDE FAUTL                                                          
*&&UK                                                                           
* DDMBFACS                                                                      
       ++INCLUDE DDMBFACS                                                       
* DDMEDFACS                                                                     
       ++INCLUDE DDMEDFACS                                                      
*&&                                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022DDSPOOF   07/09/20'                                      
         END                                                                    
