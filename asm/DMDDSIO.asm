*          DATA SET DMDDSIO    AT LEVEL 050 AS OF 03/04/21                      
*PHASE DDSIOP                                                                   
*INCLUDE DMDMGR                                                                 
*INCLUDE DMDTFS                                                                 
*INCLUDE DMDADDS                                                                
*INCLUDE DMISDDS                                                                
*INCLUDE DMIS20                                                                 
*&&US                                                                           
*INCLUDE DMVSAM     <=== VSAM handler (VSAM version of ISDDS)                   
*INCLUDE DMVSDEM    <=== VSAM DEMO emulator                                     
*&&                                                                             
*INCLUDE DMDDNAME                                                               
*INCLUDE DMDYNDD                                                                
*INCLUDE DMDALINK                                                               
*INCLUDE DMDAPTRS                                                               
*INCLUDE DMLOCKER                                                               
*INCLUDE DMPRTQSH                                                               
*INCLUDE DMPRTQO                                                                
*INCLUDE DMRCVR                                                                 
*INCLUDE DMWRKFSH                                                               
*INCLUDE DMWRKR                                                                 
*INCLUDE DMWRKZ                                                                 
*INCLUDE DMACCEMU                                                               
*INCLUDE DMDABUFF                                                               
*INCLUDE DMDANDX                                                                
*INCLUDE DMENQCTL                                                               
*INCLUDE DMENQDEQ                                                               
*INCLUDE DMISGENQ                                                               
*INCLUDE DMISGQU                                                                
*INCLUDE DMRCVUSS                                                               
*INCLUDE DMSHMUSS                                                               
*INCLUDE DMSYSFIL                                                               
*INCLUDE RECOVR                                                                 
*INCLUDE LOCKSPC                                                                
*INCLUDE LOCKUP                                                                 
*INCLUDE DYNALLOC                                                               
*INCLUDE GETRET                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE ARREDIT                                                                
*INCLUDE GETGIN                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HARDON                                                                 
*INCLUDE UPSOON                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE CPUINFO                                                                
*INCLUDE MQRPT                                                                  
*&&UK                                                                           
*INCLUDE PROMOTE                                                                
*&&                                                                             
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE PERVERT                                                                
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
         TITLE 'DDSIO - MODULE TO LINK TO INDEPENDENT DATA MANAGER'             
***********************************************************************         
* LOADED BY DMDMGRL SUBROUTINE LINKED INTO CALLERS PROGRAM            *         
* CALLED BY DMDMGRL SUBROUTINE WITH PARAMETERS IN R2,R3,R4            *         
*                                                                     *         
* R2 AL4(CALLERS UTL CSECT/ENTRY POINT)                               *         
*   IF HOB OF R2 = X'FF' USE R5 INSTEAD                               *         
* R3 AL4(CALLERS SSB CSECT/ENTRY POINT)                               *         
* R4 AL1 SUBROUTINE NUMBER AND AL3(CALLERS ADWAIT SOUBROUTINE)        *         
***********************************************************************         
         PRINT NOGEN                                                            
DDSIO    CSECT                                                                  
         REQUS                                                                  
         ENTRY UTL                 V(UTL) FOR DATAMGR ROUTINES                  
         ENTRY XPEDITER            RUNNING UNDER XPEDITER                       
         ENTRY AMSOON              JOB IS A SOON JOB                            
         ENTRY SSB                 V(SSB) FOR DATAMGR ROUTINES                  
         ENTRY ADWAIT              V(ADWAIT) FOR DATAMGR ROUTINES               
                                                                                
         USING *,RF                                                             
         ST    RE,SAVERE           SAVE CALLERS RETURN ADDRESS                  
         B     SETUTL                                                           
         DC    C'DDSIODDS'                                                      
*                                                                               
SETUTL   LTR   R2,R2               COPY CALLERS UTL TO MY UTL                   
         BZ    SETUTL4                                                          
         CLM   R2,8,=X'FF'         TEST IF R2 SET BY CALLER                     
         BNE   SETUTL2             NO USE R2 AS NORMAL                          
         MVC   UTL,0(R5)           YES USE R5 INSTEAD OF R2                     
         NILH  GR2,X'00FF'         SO 31-BIT MODULES DON'T COMPLAIN             
         B     SETSSB                                                           
*                                                                               
SETUTL2  MVC   UTL,0(R2)                                                        
         B     SETSSB                                                           
*                                                                               
SETUTL4  MVC   UTL,DEFUTL          SET DEFAULT IF NO UTL PROVIDED               
*                                                                               
DDS      USING SSBD,SSB                                                         
SETSSB   LTR   R3,R3               COPY CALLERS SSB TO MY SSB                   
         BZ    SETADW                                                           
*********TM    5(R3),X'20'         TEST SSOFLAG1,SSOFXCPY                       
*********BO    SETADW              NO MVC IF XCOPY IS SET                       
         MVC   SSB,0(R3)                                                        
         B     SETADW                                                           
*                                                                               
SETADW   ST    R4,AADWAIT          SAVE A(ADWAIT) AND ROUTINE NUMBER            
*                                                                               
SETLOCK  TM    DDS.SSOSTAT2,SSOSLOCK                                            
         BO    SETLOCKX                                                         
         ICM   R4,15,=V(LOCKER)                                                 
         BZ    SETMST                                                           
         MVC   0(2,R4),=XL2'07FE'  NOOP LOCKER IF NO OFFLINE LOCKS              
*                                                                               
SETLOCKX LA    R4,=V(LOCKER)                                                    
         XC    0(4,R4),0(R4)       DO ONLY ONCE                                 
*                                                                               
SETMST   CLM   R7,15,=CL4'ABEA'    SPECIAL XPEDITER CALL FOR LOCKSPC            
         BNE   SETMST1             NO - NOT ANNIES FAULT THEN                   
         LTR   R6,R6               YOU HAD BETTER HAVE A MASTC - IDIOT          
         BZ    SETMST1                                                          
         MVI   MASTDR6,YES                                                      
*                                                                               
         USING MASTD,R6                                                         
         MVC   XPEDITER,MCXPDTR    SET XPEDITER STATUS                          
         MVI   AMSOON,NO                                                        
         OC    MCREMPQK,MCREMPQK   TEST SOON JOB OR OVERNIGHT                   
         BZ    *+8                                                              
         MVI   AMSOON,YES                                                       
         TM    MCRFLAG1,MCRPROF    OR PROFILES FROM DISK SPECIFIED              
         BZ    *+8                                                              
         MVI   AMSOON,YES                                                       
         DROP  R6                                                               
*                                                                               
SETMST1  DS    0H                                                               
         CLI   SCANDONE,YES                                                     
         BE    SETMST2                                                          
         BRAS  RE,SCANTIOT                                                      
         MVC   SVDSPACE,DDS.SSODSPAC                                            
         MVI   SCANDONE,YES                                                     
                                                                                
SETMST2  SLR   RE,RE                                                            
         IC    RE,AADWAIT                                                       
         SLL   RE,2                RE=INDEX INTO ROUTINE TABLE                  
         CLI   AADWAIT,12          ACCEMU                                       
         BNE   *+6                                                              
         LR    R2,R1               ACCEMU - R2 points to callers DMCB           
                                                                                
SETADR   MVI   AADWAIT,0           PASS CONTROL TO DATAMGR MODULE               
         CLC   SVDSPACE,DDS.SSODSPAC                                            
         BE    SETADR20                                                         
         CLI   SVDSPACE,C' '           Was this set yet?                        
         BH    SETADR10                Yes, so must have changed                
         MVC   SVDSPACE,DDS.SSODSPAC   Set value                                
         B     SETADR20                                                         
*                                                                               
*                            01234567890123456789012345678901234556789          
SETADR10 MVC   MSGTEXT,=CL40'DSPACE changed from x to y'                        
         MVC   MSGTEXT+20(1),SVDSPACE                                           
         MVC   MSGTEXT+22(2),=C'TO'                                             
         MVC   MSGTEXT+25(1),DDS.SSODSPAC                                       
         LA    R4,MSG                                                           
         WTO   TEXT=(R4)           Dspace changed                               
         ABEND 666                                                              
                                                                                
SETADR20 L     RF,VDMGRMOD(RE)                                                  
         BASR  RE,RF                                                            
         DROP  DDS                                                              
         DROP  RF                                                               
                                                                                
         USING *,RE                                                             
EXIT     IPM   RF                  SAVE CC                                      
         LTR   R3,R3               COPY BACK SSB                                
         BZ    EXITX                                                            
         CLI   2(R3),X'FF'         ONLY IF EXTENDED SSB                         
         BNE   EXITX                                                            
         MVC   0(L'SSB,R3),SSB                                                  
*                                                                               
EXITX    SPM   RF                  RESTORE CC                                   
         L     RE,SAVERE           RETURN TO CALLER                             
         BR    RE                                                               
         DROP  RE                                                               
                                                                                
         USING *,RF                                                             
ADWAIT   L     RF,AADWAIT          ADWAIT ENTRY                                 
         LTR   RF,RF                                                            
         BNZR  RF                                                               
         LR    RF,RE                                                            
         BR    RF                                                               
         EJECT                                                                  
                                                                                
VDMGRMOD DS    0A                                                               
         DC    V(DATAMGR)          ROUTINE NUM 00                               
         DC    V(DADDS)            ROUTINE NUM 01                               
         DC    V(ISDDS)            ROUTINE NUM 02                               
         DC    V(DMDANDX)          ROUTINE NUM 03                               
         DC    V(DYNALLOC)         ROUTINE NUM 04                               
         DC    V(WORKER)           ROUTINE NUM 05                               
         DC    V(DMDABUFF)         ROUTINE NUM 06                               
         DC    V(DMDALINK)         ROUTINE NUM 07                               
         DC    V(DMDAPTRS)         ROUTINE NUM 08                               
         DC    V(PQOPEN)           ROUTINE NUM 09                               
         DC    V(DMENQDEQ)         ROUTINE NUM 10                               
         DC    V(DMOD000)          ROUTINE NUM 11                               
         DC    V(DMACCEMU)         ROUTINE NUM 12                               
         DC    V(LOCKSPC)          ROUTINE NUM 13                               
         DC    V(DMDDNAME)         ROUTINE NUM 14                               
         DC    V(LOCKUP)           ROUTINE NUM 15                               
         DC    V(MQRPT)            ROUTINE NUM 16                               
         DC    V(DMRCVUSS)         ROUTINE NUM 17                               
         DC    V(DMISGENQ)         ROUTINE NUM 18                               
         DC    V(DMSHMUSS)         ROUTINE NUM 19                               
         DC    A(RTSYSFIL)         ROUTINE NUM 20 off-line only                 
         DC    V(GETRET)           ROUTINE NUM 21                               
         DC    V(DMDYNDD)          ROUTINE NUM 22                               
*&&UK*&& DC    A(PROMOTE)          ROUTINE NUM 23 (PROMOTE)                     
*&&US*&& DC    A(0)                ROUTINE NUM 23 (PROMOTE)                     
         DC    V(ADDAY)            ROUTINE NUM 24                               
         DC    V(GETDAY)           ROUTINE NUM 25                               
         DC    V(DATCON)           ROUTINE NUM 26                               
         DC    V(DATCONX)          ROUTINE NUM 27                               
         DC    V(DATVAL)           ROUTINE NUM 28                               
         DC    V(PERVAL)           ROUTINE NUM 29                               
         DC    V(PERVERT)          ROUTINE NUM 30                               
*                                                                               
SAVERE   DS    A                                                                
AADWAIT  DS    A                                                                
                                                                                
SCANDONE DC    AL1(NO)             DO ONLY ONCE                                 
SVDSPACE DC    C' '                                                             
         DS    XL2                                                              
*                                                                               
         DS    0L                                                               
         DC    C'XPEDITER'                                                      
XPEDITER DC    AL1(NO)                                                          
         DC    XL7'00'                                                          
         DC    C'AMSOON**'                                                      
AMSOON   DC    AL1(NO)                                                          
         DC    XL7'00'                                                          
*                                                                               
         DC    C'DDSIOSSB'                                                      
SSB      DC    XL(SSOOFFX-SSBD)'00'                                             
         ORG   SSB                                                              
         DC    XL2'0000',X'FF'                                                  
*&&UK*&& DC    AL1(SSOSNRCV)       NO OFFLINE RECOVERY                          
         ORG                                                                    
*                                                                               
         DC    C'DDSIOUTL'                                                      
UTL      DC    XL255'00'                                                        
*                                                                               
DEFUTL   DS    0X                                                               
         DC    XL255'00'                                                        
         EJECT ,                                                                
***********************************************************************         
* See  DMATABS                                                                  
* Pass back in P1(SYSTABL)        See DMSYSTABUS or DMSYSTABUK                  
*              P2(FILTABL)        See DMFILTAB                                  
*              P3(SYSTABS)        See DMFILESUS  or DMFILESUK                   
*              P4(FACIDTAB)       See FACIDTAB   or FACIDTABL                   
*              P5(COUNTERS)                                                     
*              P6(MORELIST)                                                     
***********************************************************************         
         USING *,RF                                                             
RTSYSFIL DS    0H                                                               
         MVC   0(24,R1),SYSFIL                                                  
         BR    RE                                                               
*                                                                               
SYSFIL   DC    V(SYSTABL)                                                       
         DC    V(FILTABL)                                                       
         DC    V(SYSTABS)                                                       
FACIDL   DC    A(0)                     Used, do not use                        
         DC    A(COUNTERS)              Counters in modules                     
         DC    A(MORELIST)              Future use                              
*                                                                               
COUNTERS DC    V(DMVSCTRS)              VSAM (IAM) counters                     
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
*                                                                               
MORELIST DC    A(0)                     Future use                              
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(0)                                                             
         DROP  RF                                                               
         EJECT ,                                                                
**********************************************************************          
* NOTE 1: We only go though this code once                           *          
*                                                                    *          
* Scan TIOT for special DD statements                                *          
* DSPACEx  DD will force the DSPACE or die if doesn't match          *          
*                                                                    *          
**********************************************************************          
* NOTE 2:                                                            *          
*                                                                    *          
* If SSODSPAC was set to 'N' then we didn't connect to TABS in       *          
* DMDMGRL. It means this program can not determine the SSODSPAC      *          
* value. It can be determined in DDSIO if you use the specail DD     *          
* statement to set the DSPACE card.                                  *          
* i.e.                                                               *          
* //DSPACEx  DD DUMMY                                                *          
*                                                                    *          
* This also means the application will be responsible for            *          
* determining which DSPACE to connect to if any.                     *          
*                                                                    *          
* NOTE 3:                                                            *          
*                                                                    *          
* The SSODSPAC (DSPACE) value dictates what HLQ will be used         *          
* to dynamically allocate the file. ex. TST. or FQA. etc.            *          
* If you open files in read-only mode they don't need to connect to  *          
* the dataspace. So if you use the DD statement to set the DSPACE    *          
* and had initially set the SSOSPAC value to 'N' then it will be set *          
* in DDSIO and will allow the HLQ to be set base on the new value.   *          
*                                                                    *          
* There for it is possible to connect to the production files in     *          
* read-only mode if you don't open for update                        *          
* It may fail if you run with global files since it compares         *          
* the file name against the stored value in the DMGR dataspace.      *          
*                                                                    *          
**********************************************************************          
                                                                                
SCANTIOT NTR1  BASE=*,LABEL=*                                                   
         L     R1,X'10'(,0)        COMMUNICATION VECTOR TABLE                   
         USING CVT,R1                                                           
         L     R1,CVTSMCA          SYSTEM MANAGEMENT CONTROL AREA               
                                                                                
         USING SMCABASE,R1                                                      
         MVC   SMFSYS,SMCASID      SAVE OF CPU ID (SMF) SYA / SYC               
*&&UK*&& CLC   SMFSYS,=CL4'SY7'                                                 
*&&US*&& CLC   SMFSYS,=CL4'SYA'                                                 
         BNE   *+8                 Not okay if non-prod                         
         MVI   SYSTYPE,DSPTSTQ     Default is prod. This is test                
         DROP  R1                                                               
*                                                                               
         LA    R3,TIOT                                                          
         EXTRACT (3),'S',FIELDS=TIOT                                            
         L     R3,TIOT                                                          
         AHI   R3,24               POINT TO TIOT TABLE                          
*                                                                               
         L     R4,=V(SSB)                                                       
         USING SSBD,R4                                                          
STIOT10  CLI   0(R3),0             TEST END OF TIOT TABLE                       
         BE    STIOT80                                                          
         CLC   =C'DSPACE',4(R3)                                                 
         BE    STIOT30                                                          
         CLC   =C'$$DNDX$$',4(R3)  FORCE DANDX READS FOR DEMOS                  
         BE    STIOT40                                                          
         CLC   =C'$$VSAM$$',4(R3)  FORCE VSAM READS FOR DEMOS                   
         BE    STIOT40                                                          
         CLC   =C'PQDD0',7(R3)     Special PQ DD statement                      
         BE    STIOT50                                                          
         CLC   =C'CTDD0',7(R3)     Special CT DD statement                      
         BE    STIOT50                                                          
         CLC   =C'MEDZNEW',4(R3)   Special MEDZNEW DD statement                 
         BE    STIOT60                                                          
         CLC   =C'SHOWDYN',4(R3)   SHOW DYNALLOC ALLOCATIONS                    
         BE    STIOT70                                                          
*                                                                               
STIOT18  LLC   R0,0(,R3)           BUMP TO NEXT TABLE ENTRY                     
         AR    R3,R0                                                            
         B     STIOT10                                                          
***********************************************************************         
* Extract DSPACE from DSPACEx DD statement where x is the value                 
* validate and set SSODSPAC or die if doesn't match                             
***********************************************************************         
STIOT30  MVC   DSPACE,10(R3)       DSPACE MATCH CHARACTER                       
         B     STIOT18                                                          
                                                                                
***********************************************************************         
* Set the VSAM/DANDX switch accordingly based on the DD statements.             
* Note: $$DNDX$$ and $$VSAM$$ DD statements are mutually exclusive!             
***********************************************************************         
STIOT40  DS    0H                                                               
         CLI   SSODMSTA,C' '       was the switch set already?                  
         BNH   STIOT45                                                          
         WTO   'Cannot have *both* $$DNDX$$ and $$VSAM$$ DD statements'         
         ABEND 666                 yes: that's a no-no                          
*                                                                               
STIOT45  DS    0H                                                               
         CLC   =C'$$DNDX$$',4(R3)         force read via DANDX?                 
         JNE   *+12                                                             
         MVI   SSODMSTA,SSODMSTA_DANDX    yes: set SSB flag to DANDX            
         J     STIOT18                    bump to next TIOT entry               
*                                                                               
         CLC   =C'$$VSAM$$',4(R3)         force read via VSAM?                  
         JNE   *+2                        how did we get here ?!                
         MVI   SSODMSTA,SSODMSTA_VSAM     yes: set SSB flag to VSAM             
         J     STIOT18                    bump to next TIOT entry               
*                                                                               
***********************************************************************         
* EXTRAC DD CSCPQDD0, TSTPQDD0 or FQAPQDD0                                      
* EXTRAC DD CSCCTDD0, TSTCTDD0 or FQACTDD0                                      
***********************************************************************         
STIOT50  CLC   =C'CSC',4(R3)       CSCPQDD0 DD DUMMY                            
         BNE   *+8                                                              
         MVI   DSPACE,C'C'                                                      
         CLC   =C'FQA',4(R3)       FQAPQDD0 DD DUMMY                            
         BNE   *+8                                                              
         MVI   DSPACE,C'Q'                                                      
         CLC   =C'TST',4(R3)       TSTPQDD0 DD DUMMY                            
         BNE   *+8                                                              
         MVI   DSPACE,C'T'                                                      
         CLC   =C'REP',4(R3)       REPPQDD0 DD DUMMY                            
         BNE   *+8                                                              
         MVI   DSPACE,C'R'                                                      
         B     STIOT18             Validate                                     
                                                                                
***********************************************************************         
* MEDZNEW DD DUMMY SETS SSODYNDD to NEW                                         
***********************************************************************         
STIOT60  MVI   SSODYNDD,5          SET TO NEW                                   
         B     STIOT18             Next                                         
                                                                                
***********************************************************************         
* SHOWDYN DD DUMMY                                                              
***********************************************************************         
STIOT70  OI    SSOFLAG3,SSO3SDYN   SET SSO3SDYN                                 
         B     STIOT18             Next                                         
                                                                                
***********************************************************************         
* Validate DSPACE card                                                          
***********************************************************************         
                                                                                
STIOT80  CLI   DSPACE,C' '         Was it set                                   
         BNH   STIOT81             No set, so use SSODSPAC                      
         CLI   SSODSPAC,C' '       Yes.Was Was SSODSPAC set                     
         BH    STIOT80A            Yes, see if they are the same?               
         MVC   SSODSPAC,DSPACE     No, so take value from DSPACE                
         B     STIOT83                                                          
*                                                                               
STIOT80A CLC   DSPACE,SSODSPAC     Yes, Are they the same?                      
         BE    STIOT82             Okay since they have the same value          
         CLI   SSODSPAC,C'N'       Special case. See Note 2 & 3.                
         BE    STIOT81A                                                         
*                                                                               
*                            01234567890123456789012345678901234556789          
         MVC   MSGTEXT,=CL40'DSPACE has two different values, x && y'           
         MVC   MSGTEXT+33(1),DSPACE                                             
         MVC   MSGTEXT+37(1),SSODSPAC                                           
         LA    R6,MSG                                                           
         WTO   TEXT=(R6)                                                        
         ABEND 666                                                              
                                                                                
STIOT81  MVC   DSPACE,SSODSPAC                                                  
STIOT81A CLI   DSPACE,C' '         Still missing?                               
         BH    STIOT83             No                                           
         TM    SYSTYPE,DSPTSTQ     Default is prod. This is test                
         BZ    STIOT82                                                          
         MVC   MSGTEXT,=CL40'DSPACE required on test LPAR,'                     
         MVC   MSGTEXT+30(L'SMFSYS),SMFSYS                                      
         LA    R6,MSG                                                           
         WTO   TEXT=(R6)                                                        
         ABEND 666                                                              
                                                                                
STIOT82  DS    0H                                                               
         CLI   DSPACE,C' '          Not set so will use default                 
         BH    STIOT83                                                          
         MVI   DSPACE,C'A'          Default                                     
         MVC   SSODSPAC,DSPACE                                                  
*                                                                               
STIOT83  LA    R5,DSPCTAB                                                       
         USING DSPTABD,R5                                                       
STIOT83A CLC   DSPACE,DSPCHAR                                                   
         BE    STIOT84                                                          
         AHI   R5,DSPTABQ                                                       
         BRCT  R1,STIOT83A                                                      
*                                                                               
*                            01234567890123456789012345678901234556789          
         MVC   MSGTEXT,=CL40'Invalid DSPACE '                                   
         MVC   MSGTEXT+15(1),SSODSPAC                                           
         LA    R6,MSG                                                           
         WTO   TEXT=(R6)          Invalid                                       
         ABEND 666                                                              
*                                                                               
STIOT84  CLI   SSODSPAC,C'N' C'N' means not connected to DATASPACE              
         BNE   STIOT84A      So don't validate                                  
         MVC   SSODSPAC,DSPACE  Set potential DSPACE. See NOTE 3.               
         B     STIOT85                                                          
*                                                                               
STIOT84A LA    RE,DSPSYS                                                        
         LLC   R1,SYSTYPE                                                       
         EXRL  R1,TESTSYS                                                       
         BO    STIOT85                                                          
*                                                                               
*                            01234567890123456789012345678901234556789          
         MVC   MSGTEXT,=CL40'DSPACE   not valid for LPAR,'                      
         MVC   MSGTEXT+7(1),DSPACE                                              
         MVC   MSGTEXT+29(L'SMFSYS),SMFSYS                                      
         LA    R6,MSG                                                           
         WTO   TEXT=(R6)                                                        
         ABEND 666                                                              
*                                                                               
STIOT85  CLI   MASTDR6,YES                                                      
         BNE   STIOT90                                                          
         USING MASTD,R6                                                         
         MVC   MCUPDID,DSPUPDID    Set potential UPDID                          
         DROP  R5,R6               R5=DSPTABD, R6=MASTC                         
                                                                                
***********************************************************************         
* Find FACIDTAB entry or use old master table                                   
***********************************************************************         
         USING FACITABD,RE                                                      
STIOT90  LA    RE,FACIDTAB                                                      
         CLC   FACISN4,=C'????'    Older FACIDTAB                               
         BE    STIOT98                                                          
         CLI   SSODSPAC,C'N'                                                    
         BNE   STIOT92                                                          
         XR    RE,RE                                                            
         B     STIOT98                                                          
*                                                                               
         USING FACIDD,RE                                                        
STIOT92  CLI   FACIDSPC,X'FF'                                                   
         JE    *+2                                                              
         CLC   FACIDSPC,SSODSPAC                                                
         BE    STIOT94                                                          
         AHI   RE,FACIDLNQ                                                      
         B     STIOT92                                                          
*                                                                               
STIOT94  L     RE,FACAID                                                        
*                                                                               
STIOT98  L     RF,=A(FACIDL)        A(FACIDTAB ENTRY)                           
         ST    RE,0(,RF)                                                        
         ST    RE,SSOAFID           A(FACIDTAB) in SSB                          
*                                                                               
STIOTXIT XIT1  ,                                                                
         DROP  R4                                                               
         EJECT ,                                                                
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
***********************************************************************         
* DSPACE plus UPDID values                                                      
***********************************************************************         
SMFSYS   DC    CL4' '              SY1, SYC, SYT, SY7, SYA                      
SYSTYPE  DC    AL1(DSPPRDQ)                                                     
DSPACE   DC    C' '                                                             
BYTE     DS    C                                                                
                                                                                
DSPCTAB  DC    C'ADM',AL1(DSPPRDQ) Production (ADV)                             
         DC    C'RDM',AL1(DSPPRDQ) Production (REP)                             
         DC    C'PDM',AL1(DSPPRDQ) Production (ADV & REP)                       
         DC    C'QFQ',AL1(DSPTSTQ) FQA                                          
         DC    C'CFC',AL1(DSPTSTQ) CSC                                          
         DC    C'TFT',AL1(DSPTSTQ) TST                                          
         DC    C'TFM',AL1(DSPTSTQ) MEL                                          
         DC    C'NXX',AL1(DSPPRDQ+DSPTSTQ) No DSPACE                            
DSPCTAB# EQU   ((*-DSPCTAB)/DSPTABQ)                                            
*                                                                               
TIOT     DS    D                                                                
*                                                                               
MASTDR6  DC    AL1(NO)                                                          
*                                                                               
MSG      DC    AL2(MSGEND-*-2)                                                  
MSGTEXT  DC    CL40' '                                                          
MSGEND   DS    0X                                                               
                                                                                
TESTSYS  TM    0(RE),0                                                          
                                                                                
         LTORG                                                                  
*************************************************                               
*FACIDTAB - List of FACPAKs and internal values *                               
*************************************************                               
       ++INCLUDE FACIDTAB                                                       
*      ++INCLUDE FACIDTABL                                                      
         EJECT ,                                                                
*************************************************                               
* LOCAL DSECT for DSPACE                        *                               
*************************************************                               
DSPTABD  DSECT                                                                  
DSPCHAR  DS    C           Dataspace character                                  
DSPUPDID DS    CL2         For MCUPDID                                          
DSPSYS   DS    X           TEST or PROD                                         
DSPPRDQ  EQU   X'80'                                                            
DSPTSTQ  EQU   X'40'                                                            
DSPTABQ  EQU   *-DSPTABD                                                        
                                                                                
**********************************************************************          
* IBM DSECTS - LOW CORE PRIVITE REGION INFORMATION                              
*              CVT  - COMMUNICATION VECTOR TABLE                                
*              SMCA - SYSTEM MANAGEMENT CONTROL AREA                            
**********************************************************************          
         CVT   DSECT=YES                                                        
*                                                                               
         IEESMCA                                                                
                                                                                
***********************************************************************         
* DSECTS                                                                        
***********************************************************************         
*FACIDTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*FASSBOFF                                                                       
SSBD     DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
*DMDYNDDD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDYNDDD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050DMDDSIO   03/04/21'                                      
         END                                                                    
