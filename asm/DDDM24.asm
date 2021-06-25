*          DATA SET DDDM24     AT LEVEL 007 AS OF 03/04/21                      
*PHASE DDDM24A                                                                  
*INCLUDE DMDMGR                                                                 
*INCLUDE DMDTFS                                                                 
*INCLUDE DMDADDS                                                                
*INCLUDE DMISDDS                                                                
*INCLUDE DMIS20                                                                 
*&&US                                                                           
*INCLUDE DMVSAM     <=== VSAM HANDLER (VSAM VERSION OF ISDDS)                   
*INCLUDE DMVSDEM    <=== VSAM DEMO EMULATOR                                     
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
*INCLUDE DDWTO                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE ARREDIT                                                                
*INCLUDE GETGIN                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE UPSOON                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE CPUINFO                                                                
*INCLUDE MQRPT                                                                  
*&&UK                                                                           
*INCLUDE PROMOTE                                                                
*&&                                                                             
         TITLE 'DDDM24 - MODULE TO LINK TO 24BIT DATA MANAGER'                  
         MACRO                                                                  
&ENTRY   MODENTRY                                                               
         ENTRY &ENTRY                                                           
         USING *,RF                                                             
         USING MLFIXEDD,MLFIX                                                   
&ENTRY   STM   R1,R3,24(RD)                                                     
         ICM   RF,7,ML&ENTRY+1                                                  
         J     CALLMOD                                                          
         MEND                                                                   
***********************************************************************         
* LOADED BY DDMODLINK SUBROUTINE LINKED INTO CALLERS PROGRAM          *         
* CALLED BY DDMODLINK SUBROUTINE WITH PARMS IN R1 (DSECT DDMODLINKD)  *         
*                                                                     *         
* WARNING:                                                            *         
* THIS MODULE *DOES NOT* SAVE REGISTERS APART FROM R1 TO R3. IT       *         
* RELIES ON ITS CALLER'S SAVE (MODLINK)                               *         
***********************************************************************         
         PRINT NOGEN                                                            
DDDM24   CSECT                                                                  
         REQUS                                                                  
         ENTRY UTL                 V(UTL) FOR DATAMGR ROUTINES                  
         ENTRY AMSOON              JOB IS A SOON JOB                            
         ENTRY SSB                 V(SSB) FOR DATAMGR ROUTINES                  
         ENTRY ADWAIT              V(ADWAIT) FOR DATAMGR ROUTINES               
                                                                                
         USING *,RF                                                             
         ST    RE,SAVERE           SAVE CALLERS RETURN ADDRESS                  
         J     *+12                SKIP OVER SENTINAL                           
         DC    C'*DDDM24*'                                                      
*                                                                               
         LR    R2,R1                                                            
         USING MLPARMS,R2                                                       
*                                                                               
LCL      USING SSBD,SSB                                                         
         CLI   MLFIX,0             ALREADY SAVED FIXED MODLINK DATA?            
         JNE   SETADDRS            SKIP IF YES - NOT FIRST TIME IN              
                                                                                
*==================================================================             
*        FIRST TIME ONLY CODE                                                   
*==================================================================             
SETFIRST L     R3,MLAFIXED                                                      
         MVC   MLFIX,0(R3)         SAVE FIXED MODLINK DATA                      
         USING MLFIXEDD,MLFIX                                                   
*                                                                               
EXT      USING SSBD,R3                                                          
SETFILCK LT    R3,MLVSSB           TEST SSB PASSED                              
         JZ    *+2                 Got to have SSB                              
         TM    EXT.SSOSTAT2,SSOSLOCK YES, OFFLINE LOCKS RQD?                    
         JO    SETADDRS            YES, SKIP                                    
         DROP  EXT                                                              
                                                                                
         L     R3,=V(LOCKER)       DMLOCKER LINKED IN                           
         MVC   0(2,R3),=XL2'07FE'  NOOP LOCKER IF NO OFFLINE LOCKS              
*==================================================================             
*        END OF FIRST TIME ONLY CODE                                            
*==================================================================             
                                                                                
SETADDRS LT    R3,MLVUTL           TEST IF UTL PROVIDED                         
         JZ    SETADD10            NO USE DEFAULT                               
         MVC   UTL,0(R3)                                                        
         J     SETADD20                                                         
                                                                                
SETADD10 XC    UTL(255),UTL        SET DEFAULT IF NO UTL PROVIDED               
*                                                                               
EXT      USING SSBD,R3                                                          
SETADD20 L     R3,MLVSSB           Check if need to toggle AMSOON               
         LT    R3,EXT.SSOMASTC                                                  
         JZ    SETADD30                                                         
         DROP  EXT                                                              
                                                                                
         USING MASTD,R3                                                         
         MVI   AMSOON,NO                                                        
         OC    MCREMPQK,MCREMPQK   TEST SOON JOB OR OVERNIGHT                   
         JZ    *+8                                                              
         MVI   AMSOON,YES                                                       
         TM    MCRFLAG1,MCRPROF    OR PROFILES FROM DISK SPECIFIED              
         BZ    *+8                                                              
         MVI   AMSOON,YES                                                       
         DROP  R3                                                               
                                                                                
EXT      USING SSBD,R3                                                          
SETADD30 LT    R3,MLVSSB           COPY CALLERS SSB TO MY SSB                   
         JZ    SETTIOT                                                          
         MVC   LCL.SSBD(255),EXT.SSBD                                           
         DROP  EXT                                                              
*                                                                               
SETTIOT  CLI   SCANDONE,YES                                                     
         JE    CHKDSP                                                           
         BRAS  RE,SCANTIOT                                                      
         MVC   SVDSPACE,LCL.SSODSPAC                                            
         MVI   SCANDONE,YES                                                     
                                                                                
CHKDSP   CLC   SVDSPACE,LCL.SSODSPAC   CHECK DATASPACE                          
         JE    CALLDMGR                                                         
         CLI   SVDSPACE,C' '           WAS THIS SET YET?                        
         JH    CHKDSP10                YES, SO MUST HAVE CHANGED                
         MVC   SVDSPACE,LCL.SSODSPAC   SAVE VALUE                               
         J     CALLDMGR                                                         
                                                                                
*                            01234567890123456789012345678901234556789          
CHKDSP10 MVC   MSGTEXT,=CL40'DSPACE changed from x to y'                        
         MVC   MSGTEXT+20(1),SVDSPACE                                           
         MVC   MSGTEXT+22(2),=C'TO'                                             
         MVC   MSGTEXT+25(1),LCL.SSODSPAC                                       
         LA    R4,MSG                                                           
         WTO   TEXT=(R4)           Dspace changed                               
         ABEND 666                                                              
*                                                                               
CALLDMGR L     R1,MLAPARM          CALLERS PARMS                                
         LLH   RE,MLNTRYNO                                                      
         BCTR  RE,0                                                             
         SLL   RE,2                RE=INDEX INTO ROUTINE TABLE                  
         LR    R2,R1               IF ACCEMU, R2 POINTS TO CALLERS DMCB         
         L     RF,VDDDM24(RE)                                                   
         BASR  RE,RF               ROUTINE MUST NOT RELY ON R0, R2-RC           
                                                                                
RETNDMGR SAM24 ,                   MAY BE LEFT IN 31 BIT AND/OR AR MODE         
         SAC   0                                                                
         LARL  RF,DDDM24           RESET OUR BASE                               
         IPM   RE                  SAVE CC                                      
         LT    R3,MLVSSB           COPY BACK SSB                                
         JZ    RETNDM10                                                         
EXT      USING SSBD,R3                                                          
         CLI   EXT.SSOXTND,X'FF'   ONLY IF EXTENDED SSB                         
         JNE   *+2                                                              
*        JNE   RETNDM10                                                         
         MVC   EXT.SSBD(255),LCL.SSBD                                           
         DROP  EXT,LCL                                                          
                                                                                
*===================================================================            
*        COPY BACK UTL? ARE WE SURE WE KNOW LENGTH?                             
*===================================================================            
RETNDM10 SPM   RE                  RESTORE CC                                   
         L     RE,SAVERE           RETURN TO DDMODLINK                          
         BR    RE                                                               
         DROP  RF                                                               
                                                                                
         USING *,RF                                                             
         USING MLFIXEDD,MLFIX                                                   
ADWAIT   LT    RF,MLVADWT      ADWAIT ENTRY                                     
         BNZR  RF                                                               
         BR    RE                                                               
         DROP  RF                                                               
                                                                                
**********************************************************************          
*  MODULES CALLED FROM WITHIN THIS LOAD MODULE BUT NOT LINKED IN                
*  THESE WILL ROUTE THROUGH MODLINK VIA ENTRY POINTS IN MLFIXEDD                
**********************************************************************          
ADDAY    MODENTRY ,                                                             
GETDAY   MODENTRY ,                                                             
DATCON   MODENTRY ,                                                             
DATCONX  MODENTRY ,                                                             
*                                                                               
         DROP  RF                                                               
CALLMOD  LARL  R1,MLFIX                                                         
         USING MLFIXEDD,R1                                                      
         LT    R2,MLVSSB                                                        
         JZ    *+2                                                              
*        JZ    CALMOD10                                                         
EXT      USING SSBD,R2                                                          
                                                                                
         CLI   EXT.SSOXTND,X'FF'         ONLY IF EXTENDED SSB                   
         JNE   CALMOD10                                                         
         LARL  R3,SSB                                                           
LCL      USING SSBD,R3                                                          
                                                                                
         MVC   EXT.SSBD(255),LCL.SSBD                                           
CALMOD10 LM    R1,R3,24(RD)                                                     
         BR    RF                                                               
         DROP  EXT,LCL                                                          
         DROP  R1                                                               
                                                                                
******************************************************************              
*  MODLINK MODULES IN THIS LOAD MODULE IN ENTRY NUMBER SEQUENCE                 
******************************************************************              
VDDDM24  DC    (MLNTRYSN)A(0)                                                   
         ORG   VDDDM24+(4*(DATAMGRQ-1))                                         
         DC    V(DATAMGR)                        ROUTINE NO. 01                 
         ORG   VDDDM24+(4*(DADDSQ-1))                                           
         DC    V(DADDS)                          ROUTINE NO. 02                 
         ORG   VDDDM24+(4*(ISDDSQ-1))                                           
         DC    V(ISDDS)                          ROUTINE NO. 03                 
         ORG   VDDDM24+(4*(DMDANDXQ-1))                                         
         DC    V(DMDANDX)                        ROUTINE NO. 04                 
         ORG   VDDDM24+(4*(DYNALLOCQ-1))                                        
         DC    V(DYNALLOC)                       ROUTINE NO. 05                 
         ORG   VDDDM24+(4*(WORKERQ-1))                                          
         DC    V(WORKER)                         ROUTINE NO. 06                 
         ORG   VDDDM24+(4*(DMDABUFFQ-1))                                        
         DC    V(DMDABUFF)                       ROUTINE NO. 07                 
         ORG   VDDDM24+(4*(DMDALINKQ-1))                                        
         DC    V(DMDALINK)                       ROUTINE NO. 08                 
         ORG   VDDDM24+(4*(DMDAPTRSQ-1))                                        
         DC    V(DMDAPTRS)                       ROUTINE NO. 09                 
         ORG   VDDDM24+(4*(PQOPENQ-1))                                          
         DC    V(PQOPEN)                         ROUTINE NO. 10                 
         ORG   VDDDM24+(4*(DMENQDEQQ-1))                                        
         DC    V(DMENQDEQ)                       ROUTINE NO. 11                 
         ORG   VDDDM24+(4*(DMOD000Q-1))                                         
         DC    V(DMOD000)                        ROUTINE NO. 12                 
         ORG   VDDDM24+(4*(DMACCEMUQ-1))                                        
         DC    V(DMACCEMU)                       ROUTINE NO. 13                 
         ORG   VDDDM24+(4*(LOCKSPCQ-1))                                         
         DC    V(LOCKSPC)                        ROUTINE NO. 14                 
         ORG   VDDDM24+(4*(DMDDNAMEQ-1))                                        
         DC    V(DMDDNAME)                       ROUTINE NO. 15                 
         ORG   VDDDM24+(4*(LOCKUPQ-1))                                          
         DC    V(LOCKUP)                         ROUTINE NO. 16                 
         ORG   VDDDM24+(4*(MQRPTQ-1))                                           
         DC    V(MQRPT)                          ROUTINE NO. 17                 
         ORG   VDDDM24+(4*(DMRCVUSSQ-1))                                        
         DC    V(DMRCVUSS)                       ROUTINE NO. 18                 
         ORG   VDDDM24+(4*(DMISGENQQ-1))                                        
         DC    V(DMISGENQ)                       ROUTINE NO. 19                 
         ORG   VDDDM24+(4*(DMSHMUSSQ-1))                                        
         DC    V(DMSHMUSS)                       ROUTINE NO. 20                 
         ORG   VDDDM24+(4*(DMSYSFILQ-1))                                        
         DC    A(RTSYSFIL)                       ROUTINE NO. 21                 
         ORG   VDDDM24+(4*(GETRETQ-1))                                          
         DC    V(GETRET)                         ROUTINE NO. 22                 
         ORG   VDDDM24+(4*(DMDYNDDQ-1))                                         
         DC    V(DMDYNDD)                        ROUTINE NO. 23                 
*&&UK                                                                           
         ORG   VDDDM24+(4*(PROMOTEQ-1))          ROUTINE NO. 24                 
         DC    V(PROMOTE)                                                       
*&&                                                                             
         ORG   ,                                                                
*                                                                               
SAVERE   DS    A                   ALWAYS SAME ADDRESS IN MODLINK FOR           
*                                  ANY GIVEN CALL TO MODLINK                    
                                                                                
SCANDONE DC    AL1(NO)             DO ONLY ONCE                                 
SVDSPACE DC    C' '                                                             
         DS    XL2                                                              
*                                                                               
         DS    0L                                                               
         DC    C'AMSOON**'                                                      
AMSOON   DC    AL1(NO)                                                          
         DC    XL7'00'                                                          
*                                                                               
         DC    C'DM24SSB '                                                      
SSB      DC    XL(SSOOFFX-SSBD)'00'                                             
         ORG   SSB                                                              
         DC    XL2'0000',X'FF'                                                  
*&&UK*&& DC    AL1(SSOSNRCV)       NO OFFLINE RECOVERY                          
         ORG                                                                    
*                                                                               
         DC    C'DM24UTL '                                                      
UTL      DC    XL255'00'                                                        
*                                                                               
         DS    0L                                                               
MLFIX    DC    XL(MLFIXEDL)'00'    SEE DDMODLINKD                               
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
         JE    STIOT90                                                          
         CLC   =C'DSPACE',4(R3)                                                 
         JE    STIOT30                                                          
         CLC   =C'$$DNDX$$',4(R3)  FORCE DANDX READS FOR DEMOS                  
         JE    STIOT40                                                          
         CLC   =C'$$VSAM$$',4(R3)  FORCE VSAM READS FOR DEMOS                   
         JE    STIOT40                                                          
         CLC   =C'PQDD0',7(R3)     Special PQ DD statement                      
         JE    STIOT50                                                          
         CLC   =C'CTDD0',7(R3)     Special CT DD statement                      
         JE    STIOT50                                                          
         CLC   =C'MEDZNEW',4(R3)   Special MEDZNEW DD statement                 
         JE    STIOT60                                                          
*&&UK*&& CLC   =C'SHOWDYN',4(R3)   SHOW DYNALLOC ALLOCATIONS                    
*&&UK*&& JE    STIOT70                                                          
*                                                                               
STIOT18  LLC   R0,0(,R3)           BUMP TO NEXT TABLE ENTRY                     
         AR    R3,R0                                                            
         J     STIOT10                                                          
                                                                                
***********************************************************************         
* Extract DSPACE from DSPACEx DD statement where x is the value                 
* validate and set SSODSPAC or die if doesn't match                             
***********************************************************************         
STIOT30  MVC   DSPACE,10(R3)       DSPACE MATCH CHARACTER                       
         J     STIOT18                                                          
                                                                                
***********************************************************************         
* Set the VSAM/DANDX switch accordingly based on the DD statements.             
* Note: $$DNDX$$ and $$VSAM$$ DD statements are mutually exclusive!             
***********************************************************************         
STIOT40  DS    0H                                                               
         CLI   SSODMSTA,C' '       was the switch set already?                  
         JNH   STIOT45                                                          
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
         JNE   STIOT18                                                          
         MVI   DSPACE,C'R'                                                      
         J     STIOT18             Validate                                     
                                                                                
***********************************************************************         
* MEDZNEW DD DUMMY SETS SSODYNDD to NEW                                         
***********************************************************************         
STIOT60  MVI   SSODYNDD,5          SET TO NEW                                   
         J     STIOT18             Next                                         
                                                                                
***********************************************************************         
* SHOWDYN DD DUMMY                                                              
***********************************************************************         
*&&UK                                                                           
STIOT70  OI    SSOFLAG3,SSO3SDYN   SET SSO3SDYN                                 
         J     STIOT18             NEXT                                         
*&&                                                                             
                                                                                
***********************************************************************         
* Validate DSPACE card                                                          
***********************************************************************         
STIOT80  CLI   DSPACE,C' '         Was it set                                   
         JNH   STIOT81             No set, so use SSODSPAC                      
         CLI   SSODSPAC,C' '       Yes.Was Was SSODSPAC set                     
         JH    STIOT80A            Yes, see if they are the same?               
         MVC   SSODSPAC,DSPACE     No, so take value from DSPACE                
         J     STIOT83                                                          
*                                                                               
STIOT80A CLC   DSPACE,SSODSPAC     Yes, Are they the same?                      
         JE    STIOT82             Okay since they have the same value          
         CLI   SSODSPAC,C'N'       Special case. See Note 2 & 3.                
         JE    STIOT81A                                                         
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
         JH    STIOT83             No                                           
         TM    SYSTYPE,DSPTSTQ     Default is prod. This is test                
         JZ    STIOT82                                                          
         MVC   MSGTEXT,=CL40'DSPACE required on test LPAR,'                     
         MVC   MSGTEXT+30(L'SMFSYS),SMFSYS                                      
         LA    R6,MSG                                                           
         WTO   TEXT=(R6)                                                        
         ABEND 666                                                              
                                                                                
STIOT82  DS    0H                                                               
         CLI   DSPACE,C' '          Not set so will use default                 
         JH    STIOT83                                                          
         MVI   DSPACE,C'A'          Default                                     
         MVC   SSODSPAC,DSPACE                                                  
*                                                                               
STIOT83  LA    R5,DSPCTAB                                                       
         USING DSPTABD,R5                                                       
STIOT83A CLC   DSPACE,DSPCHAR                                                   
         JE    STIOT84                                                          
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
         J     STIOT85                                                          
*                                                                               
STIOT84A LA    RE,DSPSYS                                                        
         LLC   R1,SYSTYPE                                                       
         EXRL  R1,TESTSYS                                                       
         JO    STIOT85                                                          
*                                                                               
*                            01234567890123456789012345678901234556789          
         MVC   MSGTEXT,=CL40'DSPACE   not valid for LPAR,'                      
         MVC   MSGTEXT+7(1),DSPACE                                              
         MVC   MSGTEXT+29(L'SMFSYS),SMFSYS                                      
         LA    R6,MSG                                                           
         WTO   TEXT=(R6)                                                        
         ABEND 666                                                              
*                                                                               
         USING MASTD,R6                                                         
STIOT85  LT    R6,SSOMASTC                                                      
         JZ    STIOT90                                                          
         MVC   MCUPDID,DSPUPDID    Set potential UPDID                          
         DROP  R5,R6               R5=DSPTABD, R6=MASTC                         
                                                                                
***********************************************************************         
* Find FACIDTAB entry or use old master table                                   
***********************************************************************         
         USING FACITABD,RE                                                      
STIOT90  LA    RE,FACIDTAB                                                      
         CLC   FACISN4,=C'????'    Older FACIDTAB                               
         JE    STIOT98                                                          
         CLI   SSODSPAC,C'N'                                                    
         JNE   STIOT92                                                          
         XR    RE,RE                                                            
         J     STIOT98                                                          
*                                                                               
         USING FACIDD,RE                                                        
STIOT92  CLI   FACIDSPC,X'FF'                                                   
         JE    *+2                                                              
         CLC   FACIDSPC,SSODSPAC                                                
         JE    STIOT94                                                          
         AHI   RE,FACIDLNQ                                                      
         J     STIOT92                                                          
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
MSG      DC    AL2(MSGEND-*-2)                                                  
MSGTEXT  DC    CL40' '                                                          
MSGEND   DS    0X                                                               
                                                                                
TESTSYS  TM    0(RE),0                                                          
                                                                                
         LTORG                                                                  
                                                                                
*************************************************                               
*FACIDTAB - List of FACPAKs and internal values *                               
*************************************************                               
       ++INCLUDE FACIDTAB                                                       
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
*                                                                               
MLFIXEDD DSECT                                                                  
       ++INCLUDE DDMODLINKD                                                     
*FACIDTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
*FATABSDEQU                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSDEQU                                                     
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
*FAUTL                                                                          
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
*DMDYNDDD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDYNDDD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007DDDM24    03/04/21'                                      
         END                                                                    
