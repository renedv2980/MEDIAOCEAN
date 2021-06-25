*          DATA SET DMDDSION   AT LEVEL 039 AS OF 12/19/18                      
*PHASE DDSIONB      <===                                                        
*INCLUDE DMDMGR     <=== DEIS: VSAM FLAGS IN DATASPACE                          
*INCLUDE DMDTFSN    <=== TEST SYSTEM FILE DEFINTIONS                            
*INCLUDE DMDADDS                                                                
*INCLUDE DMISDDS                                                                
*INCLUDE DMIS20                                                                 
*INCLUDE DMVSAM     <=== VSAM handler (VSAM version of ISDDS)                   
*INCLUDE DMVSDEM    <=== VSAM DEMO emulator                                     
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
*INCLUDE DMISGENQ   <AWIL> Increase number of enqueues per task                 
*INCLUDE DMRCVUSS                                                               
*INCLUDE DMSHMUSS                                                               
*INCLUDE DMSYSFIL                                                               
*INCLUDE GETGIN                                                                 
*INCLUDE RECOVR                                                                 
*INCLUDE LOCKSPC    <MHER> RCVB shared memory                                   
*INCLUDE LOCKUP                                                                 
*INCLUDE DYNALLOC                                                               
*INCLUDE GETRET                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE ARREDIT                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HARDON                                                                 
*INCLUDE UPSOON                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE CPUINFO                                                                
*INCLUDE MQRPT                                                                  
*INCLUDE DATCON                                                                 
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
         ENTRY ALCTLBLK            Allocation control block                     
*                                                                               
         USING *,RF                                                             
         ST    RE,SAVERE           SAVE CALLERS RETURN ADDRESS                  
         B     SETUTL                                                           
         DC    C'DDSIODDS'                                                      
                                                                                
SETUTL   LTR   R2,R2               COPY CALLERS UTL TO MY UTL                   
         BZ    SETUTL4                                                          
         CLM   R2,8,=X'FF'         TEST IF R2 SET BY CALLER                     
         BNE   SETUTL2             NO USE R2 AS NORMAL                          
         MVC   UTL,0(R5)           YES USE R5 INSTEAD OF R2                     
         NILH  GR2,X'00FF'         R2- 31-BIT MODULES DON'T COMPLAIN            
         B     SETSSB                                                           
                                                                                
SETUTL2  MVC   UTL,0(R2)                                                        
         B     SETSSB                                                           
                                                                                
SETUTL4  MVC   UTL,DEFUTL          SET DEFAULT IF NO UTL PROVIDED               
*                                                                               
DDS      USING SSBOFFD,SSB                                                      
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
         MVC   0(2,R4),=XL2'07FE'  NO-OP LOCKER IF NO OFFLINE LOCKS             
                                                                                
SETLOCKX LA    R4,=V(LOCKER)                                                    
         XC    0(4,R4),0(R4)       DO ONLY ONCE                                 
*                                                                               
SETMST   CLM   R7,15,=CL4'ABEA'    SPECIAL XPEDITER CALL FOR LOCKSPC            
         BNE   SETMST1             NO - NOT ANNIES FAULT THEN                   
         LTR   R6,R6               YOU HAD BETTER HAVE A MASTC                  
         BZ    SETMST1                                                          
*                                                                               
         USING MASTD,R6                                                         
         MVC   XPEDITER,MCXPDTR    SET XPEDITER STATUS                          
         MVI   AMSOON,C'N'                                                      
         OC    MCREMPQK,MCREMPQK   TEST SOON JOB OR OVERNIGHT                   
         BZ    *+8                                                              
         MVI   AMSOON,C'Y'                                                      
         TM    MCRFLAG1,MCRPROF    OR PROFILES FROM DISK SPECIFIED              
         BZ    *+8                                                              
         MVI   AMSOON,C'Y'                                                      
         DROP  R6                                                               
*                                                                               
SETMST1  DS    0H                                                               
         CLI   SCANDONE,C'Y'                                                    
         BE    SETMST2                                                          
         BRAS  RE,SCANTIOT                                                      
         MVC   SVDSPACE,DDS.SSODSPAC                                            
         MVI   SCANDONE,C'Y'                                                    
                                                                                
SETMST2  SLR   RE,RE                                                            
         IC    RE,AADWAIT                                                       
         SLL   RE,2                RE=INDEX INTO ROUTINE TABLE                  
         CLI   AADWAIT,12          ACCEMU                                       
         BNE   *+6                                                              
         LR    R2,R1               ACCEMU - R2 points to callers DMCB           
*                                                                               
SETADR   MVI   AADWAIT,0           Pass control to DATAMGR module               
         CLI   DDS.SSODSPAC,C'T'   Must be "T"                                  
SETADRX  BE    SETADR20                                                         
         STM   RE,R1,SVRER1                                                     
         WTO   'ABEND 666 - Warning - DDSION only supports DSPACE=T'            
         LARL  R1,SVRER1           RF base reg has been destroied               
         LM    RE,R1,0(R1)         Get back addresses                           
         MVI   SETADRX+1,X'F0'     Do only once                                 
*        ABEND 666                                                              
         DROP  DDS                                                              
                                                                                
SETADR20 L     RF,VDMGRMOD(RE)                                                  
         BASR  RE,RF                                                            
         DROP  RF                                                               
                                                                                
         USING *,RE                                                             
EXIT     IPM   RF                  Save CC                                      
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
*                                                                               
         USING *,RF                                                             
ADWAIT   L     RF,AADWAIT          ADWAIT ENTRY                                 
         LTR   RF,RF                                                            
         BNZR  RF                                                               
         LR    RF,RE                                                            
         BR    RF                                                               
         EJECT                                                                  
*                                                                               
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
         DC    A(RTSYSFIL)         ROUTINE NUM 20                               
         DC    V(GETRET)           ROUTINE NUM 21                               
         DC    V(DMDYNDD)          ROUTINE NUM 22                               
*                                                                               
SAVERE   DS    A                                                                
SVRER1   DS    4A                                                               
AADWAIT  DS    A                                                                
*                                                                               
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
SSB      DS    XL(SSOOFFX-SSBOFFD)'00'                                          
         ORG   SSB+(SSOXTND-SSBOFFD)                                            
         DC    X'FF'                                                            
*&&UK*&& DC    AL1(SSOSNRCV)       NO OFFLINE RECOVERY                          
         ORG   SSB+(SSOAFID-SSBOFFD)                                            
         DC    A(FACIDTST)         TST  Fac ID table                            
         ORG                                                                    
*                                                                               
         DC    C'DDSIOUTL'                                                      
UTL      DC    XL255'00'                                                        
*                                                                               
DEFUTL   DS    0X                                                               
         DC    XL255'00'                                                        
         EJECT ,                                                                
***********************************************************************         
* Pass back in P1 to P6 list of entry points or tables                          
* See DMATABS                                                                   
***********************************************************************         
         USING *,RF                                                             
RTSYSFIL DS    0H                                                               
         MVC   0(24,R1),SYSFIL                                                  
         BR    RE                                                               
*                                                                               
SYSFIL   DC    V(SYSTABL)                                                       
         DC    V(FILTABL)                                                       
         DC    V(SYSTABS)                                                       
FACIDL   DC    A(FACIDTST)              Fac ID table                            
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
***********************************************************************         
* NOTE: We only go though this code once                                        
*                                                                               
* First see if we care about DDS.TESTLIB                                        
* Scan TIOT for special DD statements                                           
* DSPACEx  DD will force the DSPACE or die if doesn't match                     
* HLQxxxx  DD will allow xxxx to be HLQ in DMDYNDD                              
* Code to be determined for control files and QDQ vs QDC                        
***********************************************************************         
SCANTIOT NTR1  BASE=*,LABEL=*                                                   
         L     RF,PSAAOLD-PSA(,0)       GET CURRENT/HOME ASCB                   
         L     RF,(ASCBASXB-ASCB)(,RF)  GET ASXB ADDRESS                        
         L     RF,(ASXBSENV-ASXB)(,RF)  GET ACEE ADDRESS                        
         CLC   =C'ACEE',0(RF)           VALID ACEE?                             
         BE    *+6                      YES: EXTRACT RACF USERID                
         DC    H'0'                     NO: IMPOSSIBLE                          
         CLC   =C'JCLCNGMN',(ACEEUSRI-ACEE)(RF)  Submitted by =D.3.J ?          
         BNE   STIOT08             NO                                           
*                                                                               
         USING S99RB,R4                                                         
         XC    INFRBLK,INFRBLK     DYNAMIC INFORMATION RETRIEVAL BLOCK          
         LA    R4,INFRBLK                                                       
         ST    R4,AINFRBLK         A(INFORMATION RETRIEVAL BLOCK)               
         MVI   AINFRBLK,X'80'      EOL MARKER                                   
                                                                                
         MVI   S99RBLN,20          L'BLOCK                                      
         MVI   S99VERB,S99VRBIN    INFORMATION RETRIEVAL VERB CODE              
         LA    RF,AINFRTXT         A(TEXT UNITS)                                
         ST    RF,S99TXTPP                                                      
                                                                                
         LA    RF,TXTIDSN          A(DDNAME TEXT UNIT)                          
         ST    RF,AINFRTXT                                                      
         LA    RF,TXTITYPE         A(TYPE SPECIFICATION TEXT UNIT)              
         ST    RF,AINFRTXT+4                                                    
         MVI   AINFRTXT+4,X'80'    EOL MARKER                                   
                                                                                
         MVC   TXTIDSN(2),=AL2(DINDSNAM)  DSN TEXT UNIT                         
         MVC   TXTIDSN+2(2),=X'0001'                                            
         MVC   TXTIDSN+4(2),=AL2(11)                                            
         MVC   TXTIDSN+6(11),=C'DDS.TESTLIB'                                    
         MVC   TXTITYPE(2),=AL2(DINRTTYP) TYPE SPECIFICATION TEXT UNIT          
         MVC   TXTITYPE+2(5),=X'0001000100'                                     
*                                                                               
         LA    R1,AINFRBLK         A(DYNAMIC INF. RETRIEVAL BLOCK)              
         DYNALLOC                                                               
         LTR   RF,RF               ANY ERRORS?                                  
         BNZ   STIOT06             YES: DSN not ALLOCATED                       
         WTO   '*** ERROR *** DDS.TESTLIB not allowed for =D.3.J'               
         DC    H'00'                                                            
*                                                                               
STIOT06  CHI   RF,4                Return code of 04?                           
         JNE   *+2                 No, cause dump and look at it                
         CLC   =X'0440',S99ERROR   'DSNAME not found' ?                         
         BE    STIOT08             Yes                                          
         DC    H'00'                                                            
*                                                                               
STIOT08  LA    R3,TIOT                                                          
         EXTRACT (3),'S',FIELDS=TIOT                                            
         L     R3,TIOT                                                          
         AHI   R3,24               POINT TO TIOT TABLE                          
         MVI   EUREKA,NO                                                        
*                                                                               
         USING ALCTLD,ALCTLBLK     Allocation control block                     
         USING SSBOFFD,R4                                                       
         L     R4,=V(SSB)                                                       
                                                                                
STIOT10  CLI   0(R3),0             TEST END OF TIOT TABLE                       
         BE    STIOTXIT                                                         
         CLC   =C'HLQ',4(R3)       FIND HIGH LEVEL QUALIFIER                    
         BE    STIOT20                                                          
         CLC   =C'DSPACE',4(R3)                                                 
         BE    STIOT30                                                          
         CLC   =C'$$DNDX$$',4(R3)  FORCE DANDX READS FOR DEMOS                  
         BE    STIOT40                                                          
         CLC   =C'$$VSAM$$',4(R3)  FORCE VSAM READS FOR DEMOS                   
         BE    STIOT40                                                          
         CLC   =C'TSTCTDD0',4(R3)  TEST DDNAME=TSTCTDD0                         
         BNE   STIOT18                                                          
         MVI   EUREKA,YES                                                       
         CLI   SSODSPAC,C' '                                                    
         BH    *+8                                                              
         MVI   SSODSPAC,C'T'                                                    
                                                                                
STIOT18  LLC   R0,0(,R3)           BUMP TO NEXT TABLE ENTRY                     
         AR    R3,R0                                                            
         B     STIOT10                                                          
                                                                                
***********************************************************************         
* Extract HLQ from DD statement                                                 
***********************************************************************         
STIOT20  LA    R1,L'ALCHLQ                                                      
         LA    R6,6(R1,R3)         Point beyond HLQ to get HLQ less 1           
STIOT22  CLI   0(R6),C' '                                                       
         BH    STIOT24                                                          
         BCTR  R6,0                                                             
         BRCT  R1,STIOT22                                                       
         MVC   MSGHDR,=CL15'INVALID HLQ'                                        
         MVC   MSGINFO(8),4(R3)                                                 
         LA    R4,MSG                                                           
         WTO   TEXT=(R4)           Invalid HLQ                                  
         ABEND 666                 HLQ definition is missing                    
*                                                                               
STIOT24  STC   R1,ALCHLQLN                                                      
         MVC   ALCHLQ,7(R3)        Save of HLQ                                  
         B     STIOT18                                                          
                                                                                
***********************************************************************         
* Extract DSPACE from DSPACEx DD statement where x is the value                 
* validate and set SSODSPAC or die if doesn't match                             
***********************************************************************         
STIOT30  MVC   ALCDSPAC,10(R3)     DSPACE MATCH CHARACTER                       
         MVC   SSODSPAC,ALCDSPAC                                                
         CLI   ALCDSPAC,C'T'                                                    
         BE    STIOT18                                                          
         WTO   'DDSION only supports DSPACE=T'                                  
         ABEND 666                                                              
                                                                                
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
STIOTXIT CLI   EUREKA,YES          TEST control files are dynamically           
*AH3     BNE   STIOTNO             allocated, so comment out.                   
         XIT1  ,                                                                
                                                                                
STIOTNO  WTO   'Missing INCLUDE MEMBER=TSTCTDD'                                 
         ABEND 666                 IT'S A BEASTLY PROBLEM                       
EUREKA   DS    C                                                                
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
         EJECT ,                                                                
***********************************************************************         
* DSPACE plus UPDID values                                                      
***********************************************************************         
DSPCTAB  DC    C'TFT'              TST                                          
         DC    C'TFM'              MEL                                          
         DC    C'NXX'              No DSPACE                                    
DSPCTAB# EQU   ((*-DSPCTAB)/L'DSPCTAB)                                          
                                                                                
***********************************************************************         
* Allocation block based on TIOT                                                
***********************************************************************         
ALCTLBLK DS    0F                  Allocation control block                     
         DC    C'*ALCTLB*'                                                      
         DC    AL1(ALCTLLNQ)       Allocation control block length              
         DC    XL(ALCTLLNQ-1)'00'                                               
*&&DO                                                                           
         ORG   ALCTLBLK+(ALCQDQ-ALCTLD)                                         
         DC    CL30'FAC.DDSQDQ'                                                 
         ORG   ALCTLBLK+(ALCQDC-ALCTLD)                                         
         DC    CL30'FAC.DDSQDC'                                                 
         ORG   ALCTLBLK+(ALCGEND-ALCTLD)                                        
         DC    CL30'CON.GENDIR'                                                 
         ORG   ALCTLBLK+(ALCGENF-ALCTLD)                                        
         DC    CL30'CON.GENFIL'                                                 
         ORG   ALCTLBLK+(ALCCTF-ALCTLD)                                         
         DC    CL30'CON.CTFILE'                                                 
         ORG                                                                    
*&&                                                                             
AINFRBLK DS    A                   A(DYNAMIC INF. RETRIEVAL BLOCK)              
INFRBLK  DS    XL20                DYNAMIC INFORMATION RETRIEVAL BLOCK          
AINFRTXT DS    3A                                                               
TXTIDDN  DS    XL6,CL8             DDNAME TEXT UNIT                             
TXTIDSN  DS    XL6,CL44            DSN    TEXT UNIT                             
TXTITYPE DS    XL6,XL1             TYPE SPECIFICATION TEXT UNIT                 
*                                                                               
TIOT     DS    D                                                                
*                                                                               
MSG      DC    AL2(MSGEND-*-2)                                                  
MSGHDR   DC    CL15' '                                                          
         DC    C' '                                                             
MSGINFO  DC    CL20' '                                                          
         DC    C'.'                                                             
MSGEND   DS    0X                                                               
         LTORG                                                                  
       ++INCLUDE FACIDTABL                                                      
         EJECT ,                                                                
***********************************************************************         
* IBM                                                                           
***********************************************************************         
* PSA -  Prefixed Save Area                                                     
         IHAPSA                                                                 
* ACEE - Accessor Environment Element                                           
         IHAACEE                                                                
* ASCB - Address Space Control Block                                            
         IHAASCB                                                                
* ASXB - Address Space Control Block Extension                                  
         IHAASXB                                                                
* DSECT to parameters for DYNALLOC                                              
         IEFZB4D0                                                               
* Mnemonics for the text units                                                  
         IEFZB4D2                                                               
***********************************************************************         
* DDS DSECTS                                                                    
***********************************************************************         
*DDMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*FACIDTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
*FASSBOFF                                                                       
SSBOFFD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
*DMDYNDDD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDYNDDD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039DMDDSION  12/19/18'                                      
         END                                                                    
