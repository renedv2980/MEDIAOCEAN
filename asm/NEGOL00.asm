*          DATA SET NEGOL00    AT LEVEL 020 AS OF 02/25/19                      
*PHASE T31400B,+0                                                               
*INCLUDE KHDUMMY                                                                
         TITLE 'T31400 - NETWORK GOAL SYSTEM CONTROLLER'                        
T31400   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 3500,T31400,RR=R2                                                
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING T31400+4096,R7                                                   
         ST    R2,RELO                                                          
         LR    R9,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         LR    RE,R8                                                            
         LA    RF,2000             CLEAR SELECTED STORAGE                       
         SLL   RF,3                                                             
         XCEF                                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R9,SYSPARMS                                                      
         L     RA,4(R1)                                                         
         ST    RA,ATWA                                                          
         USING TWAD,RA                                                          
         LA    R9,IO                                                            
         AH    R9,=H'18000'        GRABBING 3 6000 BYTE I/O AREAS               
         LA    R9,24(R9)           NEED SPACE FOR 3 8BYTE LABELS                
         USING SYSD,R9                                                          
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         L     R9,SYSPARMS                                                      
         L     R2,8(R9)            A(SYSLIST)                                   
         L     RF,4(R2)            A(CALLOV)                                    
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A30'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF               GO TO CALLOV                                 
         L     RF,DMCB             PICK UP A(GENERAL CONTROLLER)                
         GOTO1 (RF),DMCB,(R8)      GO THERE - PASS A(WORKING STORAGE)           
         B     XIT                 THEN WE'RE THROUGH                           
         EJECT                                                                  
* INITIALIZE SYSTEM DEPENDENT VALUES *                                          
         SPACE 1                                                                
SYSINIT  NTR1                                                                   
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
         SPACE 1                                                                
         SPACE 1                                                                
SYS2     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS2                                                          
         SPACE 1                                                                
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,VCOUNT                                                        
         SPACE 1                                                                
SYS4     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS4                                                          
*                                                                               
         L     RE,SYSPARMS                                                      
         L     R2,8(RE)            A(SYSLIST)                                   
         L     RF,4(R2)            A(CALLOV)                                    
         ST    RF,CALLOV                                                        
                                                                                
         GOTO1 CALLOV,DMCB,0,X'D9000AE0'   DEMOCON                              
         L     RF,DMCB                                                          
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,DEMOCON                                                       
         GOTO1 CALLOV,DMCB,0,X'D9000A14'   CLPACK                               
         L     RF,DMCB                                                          
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,CLPACK                                                        
         GOTO1 CALLOV,DMCB,0,X'D9000A15'   CLUNPK                               
         L     RF,DMCB                                                          
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,CLUNPK                                                        
         XC    CALLOV,CALLOV                                                    
         SPACE 1                                                                
* SET SYSTEM DEPENDENT VALUES *                                                 
         SPACE 1                                                                
         MVI   SYSTEM,C'F'         FILE MAINT                                   
         MVI   MAXIOS,3            USES 3 I/O AREAS                             
         MVI   NTWA,1                                                           
*        MVC   SIZEIO,=F'2000'     EACH I/O IS 2000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'20'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'27'     USUALLY NETFILE                              
         MVC   SYSFIL,=C'UNTFIL  '                                              
         MVC   SYSDIR,=C'UNTDIR  '                                              
         MVI   GETMSYS,5           USES GETMSG FOR SYSTEM 5                     
         MVC   SIZEIO,=F'6000'     EACH I/O IS 6000 BYTES                       
         MVC   LWORK,=F'16000'     WE TOOK 16000 BYTES IN NMOD                  
         MVC   RCPROG(2),=C'FM'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9031400'    PRESET FOR SYSTEM CALLOVS               
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
         LA    R0,SVSTART          SET SAVED STORAGE START                      
         ST    R0,ASTARTSV                                                      
*                                                                               
         MVC   LKEY,=H'13'         SET VALUES FOR SPTFILE                       
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSFIL,=C'SPTFILE '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   USEIO,0                                                          
         XC    FILENAME,FILENAME                                                
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY *                              
         SPACE 1                                                                
         DS    0H                                                               
         USING *,RF                                                             
VCOMMON  NTR1  BASE=SYSRB                                                       
         DROP  RF                                                               
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         ST    RD,COMMRD                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SRL   RF,24                                                            
         BAS   RE,VBRANCH(RF)                                                   
* NOTE NOT TO INSERT ANY RTNS HERE THAT CHANGE CONDITION CODE                   
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
VBRANCH  B     VUSER                                                            
         B     VFLD                                                             
         B     VMED                                                             
         B     VCLI                                                             
         B     VPROD                                                            
         B     VEST                                                             
         B     VNTWK                                                            
         B     VDPT                                                             
         B     VPKG                                                             
         B     VDTE                                                             
         B     VPRO                                                             
         B     VSPT                                                             
         B     VSTA                                                             
         B     VBFM                                                             
         B     GCLI                                                             
         B     VREP                                                             
         B     VMKT                                                             
         B     VPER                                                             
         B     DEST                                                             
         B     VXSP                                                             
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         LTORG                                                                  
         EJECT                                                                  
* READ AGENCY HEADER FOR USER NAME AND ADDRESS *                                
         SPACE 1                                                                
         PRINT GEN                                                              
VUSER    NTR1                                                                   
         PRINT NOGEN                                                            
         MVI   NFILE,C'S'                                                       
         L     R3,ATWA                                                          
         CLI   29(R3),0            TEST FIRST TIME                              
         BE    VUSER2              YES - READ DATA                              
         MVC   USERNAME(66),SVUSER ELSE MOVED SAVED DATA                        
         B     VUSX                                                             
*                                                                               
VUSER2   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
*                                                                               
         MVI   ERROR,INVMED                                                     
         BAS   RE,NREAD                                                         
         BNZ   TRAPERR                                                          
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         BAS   RE,NGETREC                                                       
*                                                                               
         DROP  R4                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   VMED4                                                            
         USING AGYEL,R6                                                         
         MVC   USERNAME,AGYNAME                                                 
         MVC   USERADDR,AGYADDR                                                 
         MVC   USERPROF,AGYPROF                                                 
         MVC   SVUSER(66),USERNAME SAVE FOR FUTURE REF                          
         MVC   SVAGYFL2,AGYFLAG2                                                
VUSX     BAS   RE,INITSEC                                                       
         L     RF,ATWA                                                          
         USING T314FFD,RF                                                       
         CLC   =C'BARULE',CONREC                                                
         BE    XIT                                                              
         DROP  RF                                                               
*                                                                               
         MVC   LKEY,=H'32'         SET VALUES FOR XSPFILE                       
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL  '                                              
         MVC   SYSDIR,=C'XSPDIR  '                                              
         MVI   USEIO,0                                                          
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* INITIALIZE SECRET                                                             
*                                                                               
INITSEC  NTR1                                                                   
         L     RE,ATWA                                                          
         A     RE,=A(BASETWA)                                                   
         USING SAVAREA,RE                                                       
*                                                                               
         SPACE 1                                                                
         OC    TWASAGN,TWASAGN          ON NEW SECURITY                         
         BNZ   *+14                                                             
         OC    TWAACCS(2),TWAACCS       OR HAVE LIMIT ACCESS                    
         BZ    INITSCEX                                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',SVSECRET),0                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INITSCEX B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
         SPACE                                                                  
************************************************************                    
* VALIFLD EXTRACTS DATA  FROM SCREEN FIELD                 *                    
*  NOPTFLG SET TO 1 IF INPUT IS OPTIONAL                   *                    
*                                                                               
*  OUTPUT                                                  *                    
*      NFLDH  CONTAINS FIELD HEADER                        *                    
*      NFLD   CONTAINS EXTRACTED FIELD DATA SPACE FILLED   *                    
*      R0     CONTAINS BINARY VALUE OF DATA IF FIELD IS NUMERIC                 
*      R1     CONTAINS FIELD LENGTH                        *                    
*                                                          *                    
*      CONDITION CODE = 0 IF R1=0                          *                    
************************************************************                    
VFLD     DS    0H                                                               
         NTR1                                                                   
         MVC   NFLDH,0(R2)                                                      
         MVI   NFLD,C' '                                                        
         MVC   NFLD+1(L'NFLD-1),NFLD  FILL WITH SPACES                          
         SR    R0,R0              PRE-CLEAR REGISTER FOR NUMERIC VALUE          
         ZIC   R1,NFLDH                                                         
         SH    R1,=H'9'                                                         
         EX    R1,FLDMOVE                                                       
         LA    RE,NFLD(R1)          POINT RE AT LAST EXTRACTED BYTE             
         LA    R1,1(R1)            RESTORE DATA LENGTH                          
         SPACE 1                                                                
GETFLD1  CLI   0(RE),C' '          FIND ACTUAL DATA LENGTH                      
         BH    GETFLD2                                                          
         MVI   0(RE),C' '          MAKE SURE BINARY ZERO GOES TO BLANK          
         BCTR  RE,0                                                             
         BCT   R1,GETFLD1                                                       
         SPACE 1                                                                
GETFLD2  STC   R1,NFLDH+5           SET ACTUAL DATA LENGTH                      
         LTR   R1,R1               TEST FOR EMPTY FIELD                         
         BZ    GTFERRCK                                                         
GETFLD4  LR    R3,R1               GET FLD LENGTH-1 IN R3                       
         BCTR  R3,0                                                             
         MVC   WORK(6),=6X'F0'     TEST FOR NUMERIC FIELD                       
         EX    R3,MOVEZONE                                                      
         CLC   WORK(6),=6X'F0'                                                  
         BNE   GETFLDX                                                          
         EX    R3,FLDPACK                                                       
         CVB   R0,DUB                                                           
         LTR   R0,R0               CK IF INPUT=0                                
         BZ    GTNOTNUM                                                         
         B     GETFLDX                                                          
*                                                                               
GTNOTNUM SR    R0,R0               NON-NUMERIC. SO SET R0 TO 0                  
         B     GETFLDX                                                          
*                                                                               
GTFERRCK CLI   NOPTFLG,1          IS THIS OK?                                   
         BE    GETFLDX                                                          
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
GETFLDX  LTR   R1,R1                                                            
         XIT1  REGS=(R0,R1)                                                     
         SPACE 1                                                                
FLDMOVE  MVC   NFLD(0),8(R2)                                                    
FLDPACK  PACK  DUB,NFLD(0)                                                      
MOVEZONE MVZ   WORK(0),8(R2)                                                    
*                                                                               
         EJECT                                                                  
         SPACE                                                                  
***************************************                                         
* VALIDATE MEDIA CODE                 *                                         
*    OUTPUT - BAGYMD,MEDNM,MEDCAPT    *                                         
***************************************                                         
         SPACE 1                                                                
VMED     NTR1                                                                   
         MVI   NFILE,C'S'                                                       
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
         BAS   RE,NREAD                                                         
         BNZ   VMED4                                                            
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         BAS   RE,NGETREC                                                       
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
VMED2    BAS   RE,NEXTEL                                                        
         BNE   VMED4                                                            
         CLC   2(1,R6),8(R2)                                                    
         BNE   VMED2                                                            
         MVC   QMED,8(R2)          SAVE INPUT MEDIA CODE                        
         MVC   BAGYMD,3(R6)        DIG OUT AGENCY/MEDIA                         
         MVC   MEDNM,4(R6)         MEDIA NAME                                   
         MVC   MEDCAPT,14(R6)      AND CAPTION                                  
*                                                                               
         B     XIT                                                              
         SPACE 1                                                                
VMED4    MVI   ERROR,INVMED                                                     
         B     TRAPERR                                                          
         EJECT                                                                  
*********************************                                               
* GET CLIENT,                   *                                               
* VALIDATE CLIENT               *                                               
*   OUTPUT - QCLT,BCLT,CLTNM    *                                               
*            SVCLIST            *                                               
*********************************                                               
         SPACE 1                                                                
GCLI     NTR1                                                                   
         MVI   NFILE,C'S'                                                       
*                                                                               
         MVC   QCLT(3),0(R1)                                                    
         B     CLT3                                                             
*                                                                               
VCLI     NTR1                                                                   
         MVI   NFILE,C'S'                                                       
*                                                                               
         MVI   ERROR,INVCLI                                                     
         CLI   5(R2),3                                                          
         BH    CLIERR                                                           
         CLI   5(R2),2                                                          
         BL    CLIERR                                                           
         BAS   RE,VFLD                                                          
         MVC   QCLT(3),NFLD                                                     
*                                                                               
CLT3     GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   CLIERR                                                           
         SPACE 1                                                                
* READ CLIENT HEADER *                                                          
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
*                                                                               
         BAS   RE,NREAD                                                         
         BNZ   CLIERR                                                           
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         BAS   RE,NGETREC                                                       
         SPACE 1                                                                
* SAVE CLIENT PRODUCT LIST *                                                    
         SPACE 1                                                                
         LA    R4,CLIST                                                         
         LA    R5,880                                                           
         LA    RE,SVCLIST                                                       
         LA    RF,880                                                           
         MVCL  RE,R4                                                            
         MVC   SVCLIST+880(140),CLIST2                                          
*                                                                               
         MVC   CLTNM,CNAME         AND CLIENT NAME                              
         MVC   QOFFICE,COFFICE     AND OFFICE CODE                              
         MVC   ROTSTAT,CSCJROT     OUT OF WEEK ROTATOR DAY                      
*                                                                               
*  CHECK CLIENT SECURITY                                                        
*                                                                               
         BAS   RE,CALLOFCR                                                      
*                                                                               
***         L     RA,ATWA                                                       
***         USING T314FFD,RA                                                    
*                                                                               
***         MVI   ERROR,SECLOCK                                                 
***         OC    T314FFD+6(2),T314FFD+6  TEST ANY SECURITY LIMIT               
***         BZ    CLT8                                                          
***         CLI   T314FFD+6,C'$'          TEST OFFICE LOCKOUT                   
***         BE    CLT4                                                          
***         CLI   T314FFD+6,C'*'          TEST OFFICE LOCKOUT                   
***         BE    CLT5                                                          
***         CLC   T314FFD+6(2),BCLT       ELSE SINGLE CLIENT ACCESS             
***         BNE   CLIERR                                                        
***         B     CLT8                                                          
*                                                                               
***CLT4     DS    0H               * TEST OFFICE LIST SECURITY *                
***         XC    DMCB(8),DMCB                                                  
***         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                   
***         L     R1,SYSPARMS                                                   
***         L     RE,8(R1)            A(SYSLIST)                                
***         L     RF,4(RE)            A(CALLOV)                                 
***         GOTO1 (RF),DMCB                                                     
***         CLI   4(R1),X'FF'                                                   
***         BNE   *+6                                                           
***         DC    H'0'                                                          
***         XC    DUB,DUB                                                       
***         LA    R1,DUB                                                        
***         USING OFFICED,R1                                                    
***         MVI   OFCSYS,C'S'         SYSTEM ID                                 
***         MVC   OFCAUTH,T314FFD+6   ID AUTH VALUE                             
***         MVC   OFCAGY,AGENCY                                                 
***         MVC   OFCOFC,COFFICE                                                
*                                                                               
***         L     RF,DMCB                                                       
***         GOTO1 (RF),DMCB,DUB,ACOMFACS,0                                      
***         CLI   0(R1),0                                                       
***         BNE   CLIERR                                                        
***         B     CLT8                                                          
*                                                                               
***CLT5     CLC   T314FFD+7(1),COFFICE   MATCH OFFICE CODE                      
***         BNE   CLIERR                                                        
*                                                                               
*--READ N0 PROFILE                                                              
CLT8     XC    KEY,KEY             GET USER PROFILE INTO NBUSER                 
         MVC   KEY(4),=C'S0N0'                                                  
         MVC   KEY+4(2),AGENCY                                                  
         MVI   KEY+6,C'N'                                                       
         MVC   KEY+7(3),QCLT                                                    
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),QOFFICE                                                
         GOTO1 GETPROF,DMCB,KEY,N0PROF,DATAMGR                                  
         MVI   KEY+3,C'1'          GET N1 PROFILE INTO N1PROF                   
         GOTO1 GETPROF,DMCB,KEY,N1PROF,DATAMGR                                  
         MVI   KEY+3,C'2'          GET N2 PROFILE INTO N1PROF                   
         GOTO1 GETPROF,DMCB,KEY,N2PROF,DATAMGR                                  
*                                                                               
CLTX     B     XIT                                                              
         DROP  R6                                                               
         SPACE                                                                  
*                                                                               
CLIERR   B     TRAPERR                                                          
         EJECT                                                                  
*=========================================================                      
* CALL OFFICER TO VALIDATE LIMIT ACCESS                                         
*=========================================================                      
         SPACE 1                                                                
CALLOFCR NTR1                                                                   
         L     R4,AIO                                                           
         USING CLTHDRD,R4                                                       
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING OFFICED,R1                                                       
*                                                                               
         L     RE,ATWA                                                          
         A     RE,=A(BASETWA)                                                   
         USING SAVAREA,RE                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,TWAACCS                                                  
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,COFFICE                                                   
         MVC   OFCCLT,QCLT                                                      
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),TWAACCS                                                
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         LA    RF,SVSECRET                                                      
         ST    RF,OFCSECD                                                       
         DROP  R1,RE                                                            
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK),ACOMFACS                                   
         CLI   0(R1),0                                                          
         BE    CALLOFEX                                                         
*                                                                               
         MVI   ERROR,SECLOCK                                                    
         B     TRAPERR                                                          
*                                                                               
CALLOFEX B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*********************************************************************           
* VALIDATE PRD                                                      *           
*    OUTPUT - QPRD,BPRD                                             *           
* IF SECOND PRODUCT IS NUMERIC IT IS ASSUMED TO BE A PACKAGE NUMBER *           
*    OUTPUT - BPAKG                                                 *           
*********************************************************************           
         SPACE                                                                  
* PRODUCT CODE(S) (SYNTAX IS CODE/CODE)                                         
*                                                                               
VPROD    NTR1                                                                   
         XC    QPRD,QPRD                                                        
         XC    BPRD,BPRD                                                        
*        XC    QPRD2,QPRD2                                                      
         XC    BPRD2,BPRD2                                                      
         MVI   NFILE,C'S'                                                       
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=*/'                              
         MVI   ERROR,INVPROD                                                    
         CLI   4(R1),0                                                          
         BE    TRAPERR             INVALID INPUT                                
         CLI   4(R1),2             SEE IF PRIORITY BRAND SET                    
         BNE   VPRD40                                                           
*  THE FOLLOWING 4 LINES WOULD CHECK TO SEE IF THE SECOND FIELD                 
*  WAS NUMERIC. IF IT WAS NUMERIC IT WOULD ASSUME A PACKAGE                     
*  NUMBER HAD BEEN INPUTTED. WE NO LONGER WANT TO HAVE PACKAGE                  
*  GOALS SO THE 4 LINES HAVE BEEN COMMENTED OUT.                                
*                                                                               
*        TM    BLOCK+34,X'80'      TEST IF SECOND FIELD IS A PACKAGE            
*        BZ    *+12                                                             
*        BAS   RE,CONVPKG          CONVERT THE PACKAGE TO BINARY                
*        B     VPRD40                                                           
         MVC   BLOCK(32),BLOCK+32                                               
VPRD40   LA    R4,BLOCK            POINT R4 AT BLOCK                            
         CLI   0(R4),2                                                          
         BE    *+12                                                             
         CLI   0(R4),3                                                          
         BNE   TRAPERR             PRODUCT CODE IS 2 OR 3 ALPHA CHARS           
         ZIC   RF,0(R4)                                                         
         LA    R1,12(R4)                                                        
         BAS   RE,PRDFORM          CHECK PRODUCT IN RIGHT FORMAT                
         BNZ   TRAPERR                                                          
         MVC   THREE,12(R4)                                                     
         BAS   RE,VALPRDP          VALIDATE VIA PRODUCT PASSIVE POINTER         
         CLI   ERROR,INVPROD                                                    
         BE    TRAPERR                                                          
         MVC   QPRD,THREE                                                       
         MVC   BPRD,BYTE                                                        
         SPACE                                                                  
         CLI   1(R4),0             TEST FOR SECOND PRODUCT                      
         BE    XIT                 NO EXIT                                      
*                                                                               
         CLI   1(R4),2                                                          
         BE    *+12                                                             
         CLI   1(R4),3                                                          
         BNE   TRAPERR                                                          
         ZIC   RF,1(R4)                                                         
         LA    R1,22(R4)                                                        
         BAS   RE,PRDFORM          CHECK PRODUCT IN RIGHT FORMAT                
         BNZ   TRAPERR                                                          
         MVC   THREE,22(R4)                                                     
         BAS   RE,VALPRDP          VALIDATE VIA PRODUCT PASSIVE POINTER         
         CLI   ERROR,INVPROD                                                    
         BE    TRAPERR                                                          
*        MVC   QPRD2,THREE         TEST IF SAME CODE INPUT TWICE                
         MVC   BPRD2,BYTE                                                       
         B     XIT                                                              
         EJECT                                                                  
* PRDFORM-CHECKS FOR ALPHA IN FIRST PRODUCT FIELD, AND ALPHA-NUMERIC            
* IN THE REST.                                                                  
*        RF=LENGTH OF FIELD                                                     
*        R1=FIELD                                                               
*                                                                               
PRDFORM  NTR1                                                                   
         BCTR  RF,0                                                             
*--CHECK FIRST CHARACTER FOR ALPHA                                              
         CLI   0(R1),X'C1'                                                      
         BL    PRDBFRM                                                          
         CLI   0(R1),X'E9'                                                      
         BH    PRDBFRM                                                          
         LA    R1,1(R1)                                                         
*                                                                               
PRDFM20  CLI   0(R1),X'F0'                                                      
         BL    PRDFM30                                                          
         CLI   0(R1),X'F9'                                                      
         BNH   PRDFM40                                                          
         B     PRDBFRM                                                          
PRDFM30  CLI   0(R1),X'C1'                                                      
         BL    PRDBFRM                                                          
         CLI   0(R1),X'E9'                                                      
         BH    PRDBFRM                                                          
PRDFM40  LA    R1,1(R1)                                                         
         BCT   RF,PRDFM20                                                       
*                                                                               
         SR    R1,R1               SET GOOD RETURN                              
PRDBFRM  LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
*        VALIDATE PRODUCT VIA PASSIVE POINTER                                   
*                 INPUT:  THREE (3 CHARACTER PRODUCT CODE)                      
*                 OUTPUT: QPRD  (3 CHARACTER PRODUCT CODE)                      
*                         BPRD  (1 BYTE EQUATE OR ZERO IF OVER 252)             
*******************************************************************             
VALPRDP  NTR1                                                                   
         MVI   NFILE,C'S'                                                       
         MVI   ERROR,0                                                          
         MVI   BYTE,0                                                           
*                                                                               
         CLC   THREE,=C'POL'       TEST FOR POOL                                
         BE    INVPRD              YES                                          
         CLC   THREE,=C'AAA'       TEST FOR AAA                                 
         BE    INVPRD                                                           
*                                                                               
VPRDP10  DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING PRDHDRD,R3                                                       
         MVC   KEY(2),=X'0DF1'                                                  
         MVC   PLSTAM,BAGYMD                                                    
         MVC   PLSTCLT,BCLT                                                     
         MVC   PLSTPRD,THREE                                                    
         BAS   RE,NHIGH                                                         
         CLC   KEY(9),KEYSAVE                                                   
         BNE   INVPRD                                                           
*                                                                               
         MVC   BYTE,PLSTBPRD+1      IF < 252, SAVE AWAY EQUATE                  
         MVC   QPRD,PLSTPRD         3 CHAR PRODUCT CODE                         
VPRDPX   B     XIT                                                              
*                                                                               
INVPRD   MVI   ERROR,INVPROD       NOT A VALID ALLOCATION                       
         B     XIT                                                              
         DROP  R3                                                               
*                                                                               
* SUB-ROUTINE TO CHECK INPUT BRAND CODE AGAINST CLIENT LIST (AT ENTRY           
* THREE CONTAINS INPUT, AT EXIT BRAND NUMBER IN BYTE)                           
*                                                                               
VALPRD   NTR1                                                                   
         MVI   ERROR,0                                                          
         LA    R0,255              COUNTER                                      
         LA    RF,SVCLIST                                                       
         MVI   BYTE,0                                                           
         CLC   THREE,=C'POL'       TEST FOR POOL                                
         BE    *+14                YES                                          
         CLC   THREE,=C'AAA'       TEST FOR AAA                                 
         BNE   VALPRD2             NO                                           
         MVI   ERROR,INVPROD       NOT A VALID ALLOCATION                       
         B     VALPRDX                                                          
         SPACE                                                                  
VALPRD2  OC    0(4,RF),0(RF)       TEST FOR E-O-L.                              
         BZ    VALPRD3             YES                                          
         CLC   THREE,0(RF)         COMPARE INPUT VS ENTRY                       
         BE    VALPRD4             FOUND IT                                     
         LA    RF,4(RF)                                                         
         BCT   R0,VALPRD2                                                       
         SPACE                                                                  
VALPRD3  MVI   ERROR,INVPROD                                                    
         B     VALPRDX                                                          
         SPACE                                                                  
VALPRD4  MVC   BYTE,3(RF)          EXTRACT PRODUCT NUMBER                       
         CLI   NOPTFLG,1           IS THIS FOR LIST                             
         BE    VALPRDX             IF YES END EDIT HERE                         
         SPACE                                                                  
VALPRD6  XC    KEY,KEY             NOW VALIDATE BRAND ESTIMATE                  
         LA    R3,KEY                                                           
         USING EKEY,R3                                                          
         MVI   EKEYTYPE,X'00'                                                   
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,THREE                                                    
*        MVC   EKEYEST,BEST                                                     
         BAS   RE,NREAD                                                         
         BNZ   VALPRD3                                                          
VALPRDX  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO CONVERT PACKAGE NUMBER TO BINARY                               
* BLOCK+32 CONTAINS PACKAGE BLOCK FROM SCANNER                                  
* BLOCK+32 IS CLEARED SO THE VPROD LOGIC CAN FLOW SMOOTHLY                      
*                                                                               
CONVPKG  NTR1                                                                   
         ZIC   R3,BLOCK+32         LENGTH OF INPUT                              
         BCTR  R3,0                                                             
         EX    R3,PACKPKG                                                       
         CVB   R0,DUB                                                           
         STC   R0,BPAKG                                                         
         CLI   BPAKG,0                                                          
         BE    CNVPK100                                                         
         CLI   BPAKG,255                                                        
         BNH   CNVPKEX                                                          
*                                                                               
CNVPK100 MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
CNVPKEX  XC    BLOCK+32(32),BLOCK+32                                            
         B     XIT                                                              
PACKPKG  PACK  DUB,BLOCK+44(0)     PACKAGE NUMBER                               
         EJECT                                                                  
*******************************                                                 
* VALIDATE ESTIMATE           *                                                 
*   OUTPUT QEST,BEST,ESTNAME  *                                                 
*******************************                                                 
         SPACE                                                                  
VEST     NTR1                                                                   
         MVI   ERROR,INVEST                                                     
         XC    DMCB,DMCB                                                        
         GOTO1 VALIFLD,DMCB,255                                                 
         LTR   R0,R0               R0=0 IF NOT NUMERIC                          
         BZ    TRAPERR                                                          
         STC   R0,BEST                                                          
         MVI   NFILE,C'S'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING EKEY,R4                                                          
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,BEST                                                     
         BAS   RE,NREAD                                                         
         BNZ   TRAPERR                                                          
         MVC   AIO,AIO1                                                         
         BAS   RE,NGETREC                                                       
         DROP  R4                                                               
         L     R6,AIO                                                           
         USING ESTHDR,R6                                                        
         MVC   ESTNAME,EDESC                                                    
         MVC   ESTDEMO,EDEMOS                                                   
         MVC   ESTTRGL,ETRGLST                                                  
         MVC   ESTUSNS,EUSRNMS                                                  
         MVC   SVBEGIN,ESTART                                                   
         MVC   SVEND,EEND                                                       
         MVC   QEST,NFLD                                                        
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
************************************                                            
* VALIDATE NETWORK                 *                                            
*   OUTPUT - QNET(CL4),QNETMKT(BL2)*                                            
************************************                                            
         SPACE                                                                  
VNTWK    NTR1                                                                   
         BAS   RE,VFLD                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STAREC,R4                                                        
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),NFLD    ASSUMES AFTER VFLD                           
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,=CL3'000'   FILL WITH CHAR ZERO                          
         MVC   STAKFILL,=CL5'00000'                                             
         MVC   FILENAME,=C'STATION'                                             
         MVI   USEIO,C'Y'                                                       
         GOTO1 READ                                                             
         BE    VNTWKOK                                                          
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
VNTWKOK  DS    0H                                                               
         MVC   QNET,NFLD                                                        
         LA    R4,IO              GET STATION MARKET NUMBER                     
         PACK  DUB,SMKT                                                         
         CVB   R1,DUB                                                           
         STH   R1,QNETMKT                                                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              DAYPART ROUTINES                                                 
*                                                                               
* VALDPT  - DOES ALL USER DAYPART HANDLING                                      
*                                                                               
* AT ENTRY P1 CONTAINS BYTE 1 = 1(DAYPART RETURN)                               
*                      BYTE 1 = 0(DAYPART VALIDATION)                           
*                                                                               
*                      A(OF DAYPART CODE)                                       
* RETURN IF KEY = KAESAVE DAYPART VALID                                         
*                                                                               
* ON RETURN                                                                     
* DPTNAME  = 14 BYTE DAYPART DESCRIPTION                                        
* DPTCODE  = 2 BYTE DAYPART CODE                                                
* DPTVALUE = 1 BYTE DAYPART EQUATE                                              
*                                                                               
VDPT     NTR1                                                                   
         MVI   NFILE,C'U'           SET TO READ UNIT FILE                       
*                                                                               
         L     R3,0(R1)                                                         
*                                                                               
         USING NDPTHDR,R4                                                       
*                                                                               
         CLI   0(R1),1                                                          
         BE    VALDP200             DO LOOKUP                                   
*                                                                               
*  VALIDATE A DAYPART                                                           
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   NDPTKTYP,=XL2'0D07'                                              
         MVC   NDPTAGM,BAGYMD                                                   
*                                                                               
VALDP010 GOTO1 NHIGH                                                            
         B     VALDP040                                                         
VALDP020 GOTO1 NSEQ                                                             
                                                                                
VALDP040 CLC   KEY(5),KEYSAVE      CHECK UP TO CLIENT                           
         BNE   VALDP100            YES                                          
         CLC   NDPTDPTA,0(R3)      CHECK FOR MATCH ON CODE                      
         BNE   VALDP020            GET NEXT RECORD                              
*  MOVE DATA OUT AND EXIT                                                       
         B     VALDP220                                                         
*                                                                               
* CHECK KEY FOR AGENCY OR CLIENT LEVEL CHECK                                    
* IF AGENCY LEVEL RESET KEY MOVE CLIENT CODE IN RESTART SEARCH                  
* IF CLIENT LEVEL EXIT ROUTINE DEMO WAS INVALID                                 
*                                                                               
VALDP100 OC    KEYSAVE+3(2),KEYSAVE+3                                           
         BNZ   BADDPT                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(5),KEYSAVE                                                   
         MVC   KEY+3(2),BCLT                                                    
         B     VALDP010                                                         
*                                                                               
* FOLLOWING ROUTINE FINDS THE 2 CHARACTER CODE AND EXPANSION                    
*                                                                               
*                                                                               
VALDP200 LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   NDPTKTYP,=XL2'0D07'                                              
         MVC   NDPTAGM,BAGYMD                                                   
         CLI   0(R3),127            CHECK CLIENT LEVEL DAYPART                  
         BH    *+10                                                             
         MVC   NDPTCLT,BCLT                                                     
         MVC   NDPTDPTE,0(R3)                                                   
         GOTO1 NHIGH                                                            
         CLC   KEY(6),KEYSAVE                                                   
         BNE   BADDPT                                                           
*                                                                               
VALDP220 MVC   DPTCODE,NDPTDPTA                                                 
         MVC   DPTVALUE,NDPTDPTE                                                
         MVC   DPTNAME,NDPTDES                                                  
         B     VALDPEX                                                          
         DROP  R4                                                               
         EJECT                                                                  
BADDPT   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
VALDPEX  B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*******   OLD DAYPART VALIDATION   ***********                                  
*******************************                                                 
**** VALIDATE A DAYPART       *                                                 
****    OUTPUT - DPTNAME,QDPT *                                                 
*******************************                                                 
***VDPT     NTR1                                                                
***         DS    0H                                                            
***         LA    R3,DAYPARTS                                                   
***VDPT5    CLC   8(1,R2),0(R3)                                                 
***         BE    DPTOK                                                         
***         LA    R3,9(R3)                                                      
***         CLI   0(R3),X'FF'                                                   
***         BNE   VDPT5                                                         
***         MVI   ERROR,INVALID                                                 
***         B     TRAPERR                                                       
***DPTOK    MVC   DPTNAME,1(R3)                                                 
***         MVC   QDPT,8(R2)                                                    
***         B     XIT                                                           
***         SPACE                                                               
*                                                                               
***DAYPARTS DS    0H                                                            
*         DC    C'D',CL8'DAYTIME'                                               
*         DC    C'B',CL8'CBLSPORT'                                              
*         DC    C'U',CL8'UNWIRED'                                               
*         DC    C'F',CL8'FRINGE'                                                
*         DC    C'P',CL8'PRIME'                                                 
*         DC    C'K',CL8'KIDS'                                                  
*         DC    C'T',CL8'TEENS'                                                 
*         DC    C'Y',CL8'YOUTH'                                                 
*         DC    C'S',CL8'SPORTS'                                                
*         DC    C'N',CL8'NEWS'                                                  
*         DC    C'E',CL8'EARLY'                                                 
*         DC    C'L',CL8'LATE'                                                  
*         DC    C'H',CL8'OTHER'                                                 
*         DC    C'J',CL8'PROMO-ID'                                              
*         DC    C'C',CL8'CABLE'                                                 
*         DC    C'O',CL8'OLYMPICS'                                              
*         DC    C'R',CL8'RADIO'                                                 
*         DC    C'X',CL8'SYND.'                                                 
*         DC    C'I',CL8'SPECIAL'                                               
*         DC    C'V',CL8'OVERNITE'                                              
*         DC    C'W',CL8'WKNDPM'                                                
*         DC    C'M',CL8'WKNDAM'                                                
*         DC    C'A',CL8'ACCESS'                                                
*         DC    C'Q',CL8'INTRACTV'                                              
*         DC    XL1'FF',CL8' '      END OF TABLE                                
*                                                                               
         EJECT                                                                  
****************************                                                    
* VALIDATE PACKAGE         *                                                    
*   OUTPUT - PAKGNAM,BPAKG *                                                    
****************************                                                    
         SPACE                                                                  
VPKG     NTR1                                                                   
         MVI   ERROR,NOTFOUND                                                   
         BAS   RE,VFLD                                                          
         STC   R0,BPAKG                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPKEY,R4                                                         
         MVI   KEY,X'02'                                                        
         MVC   NPKAM,BAGYMD                                                     
         MVC   NPKCLT,BCLT                                                      
         MVC   NPKNET(3),QNET     (QNET=WABC/NPKNET=ABCX'40')                   
         MVI   NPKNET+3,C' '                                                    
         MVC   NPKEST,BEST                                                      
         MVC   NPKPACK,BPAKG                                                    
         GOTO1 READ                                                             
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         USING NPAKEL,R6                                                        
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         MVC   PAKGNAM,NPAKNAME                                                 
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***************************************************                             
* DATE VALIDATION                                                               
*      OUTPUT: YYMMDD IN QDATE                                                  
*                                                                               
VDTE     NTR1                                                                   
         BAS   RE,VFLD                                                          
         MVI   ERROR,INVDATE                                                    
         GOTO1 DATVAL,DMCB,(0,NFLD),QDATE                                       
         OC    DMCB(4),DMCB                                                     
         BZ    TRAPERR                                                          
         B     XIT                                                              
         EJECT                                                                  
**********************************************************                      
* READ PROGRAM RECORD TO VALIDATE PROGRAM EXCEPTION NAME *                      
*      EXPECTS R2 TO POINT TO PROG CODE HEADER           *                      
**********************************************************                      
VPRO     NTR1                                                                   
         MVI   NFILE,C'S'                                                       
*                                                                               
         MVI   ERROR,INVALID                                                    
*--MAKE SURE CODE IS ALPHA-NUMERIC                                              
         LR    R3,R2                                                            
         LA    R3,8(R3)                                                         
         ZIC   R4,5(R2)                                                         
VPRO10   CLI   0(R3),C'&&'                                                      
         BE    VPRO20                                                           
         CLI   0(R3),C'/'                                                       
         BE    VPRO20                                                           
         CLI   0(R3),C'.'                                                       
         BE    VPRO20                                                           
         CLI   0(R3),C'-'                                                       
         BE    VPRO20                                                           
         CLI   0(R3),C'?'                                                       
         BE    VPRO20                                                           
         CLI   0(R3),C':'                                                       
         BE    VPRO20                                                           
         CLI   0(R3),C''''                                                      
         BE    VPRO20                                                           
         CLI   0(R3),X'C1'                                                      
         BL    TRAPERR                                                          
         CLI   0(R3),X'F9'                                                      
         BH    TRAPERR                                                          
VPRO20   LA    R3,1(R3)                                                         
         BCT   R4,VPRO10                                                        
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    XIT                                                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D20'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),QNETMKT                                                 
         MVC   KEY+5(6),8(R2)       PROG CODE                                   
         OC    KEY+5(6),=6X'40'                                                 
         BAS   RE,NREAD                                                         
         CLC   KEY(11),KEYSAVE                                                  
         BNE   TRAPERR                                                          
         MVC   AIO,AIO1                                                         
         BAS   RE,NGETREC                                                       
         B     XIT                                                              
         EJECT                                                                  
******************************************                                      
* SETS TO READ XSPOT FILE                                                       
*                                                                               
******************************************                                      
VXSP     NTR1                                                                   
         MVC   LKEY,=H'32'         SET VALUES FOR XSPFILE                       
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL  '                                              
         MVC   SYSDIR,=C'XSPDIR  '                                              
         MVI   USEIO,0                                                          
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         SPACE 3                                                                
******************************************                                      
* SETS TO READ SPOT FILE                                                        
*                                                                               
******************************************                                      
VSPT     NTR1                                                                   
         MVC   LKEY,=H'13'         SET VALUES FOR SPTFILE                       
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSFIL,=C'SPTFILE '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   USEIO,0                                                          
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         SPACE 3                                                                
*********************************************                                   
* SETS TO READ STATION FILE                                                     
*                                                                               
**********************************************                                  
VSTA     NTR1                                                                   
         MVC   LKEY,=H'17'         SET VALUES FOR STAFILE                       
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'23'                                                  
         XC    SYSFIL,SYSFIL                                                    
         MVC   SYSDIR,=C'STATION '                                              
         MVI   USEIO,C'Y'                                                       
         MVC   AIO,AIO1                                                         
         XC    FILENAME,FILENAME                                                
         B     XIT                                                              
         EJECT                                                                  
***************************************                                         
* VALIDATE BILL FORMULA               *                                         
*   OUTPUT - WORK(7)                  *                                         
*   INPUT  - R2 PTS TO THE BILL BASIS *                                         
*   INPUT  - P1 E=EST,P=PROD          *                                         
***************************************                                         
         SPACE 1                                                                
VBFM     NTR1                                                                   
         XC    WORK(7),WORK                                                     
         MVC   WORK+7(1),DMCB      RECORD TYPE                                  
*--CHECK BILL BASIS                                                             
         CLI   8(R2),C'G'          SEE IF GROSS                                 
         BNE   VBFM020                                                          
         MVC   8(5,R2),=C'GROSS'                                                
         OI    6(R2),X'80'                                                      
         B     VBFM030                                                          
VBFM020  MVI   ERROR,INVBAS                                                     
         CLI   8(R2),C'N'          SEE IF NET                                   
         BNE   TRAPERR                                                          
         XC    8(5,R2),8(R2)                                                    
         MVC   8(3,R2),=C'NET'                                                  
         OI    6(R2),X'80'                                                      
         OI    WORK+2,X'10'                                                     
*--CHECK COMMISSION PERCENT                                                     
VBFM030  SR    RE,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
*                                                                               
         MVI   ERROR,INVBAS                                                     
         LA    R9,8(R2)                                                         
         GOTO1 CASHVAL,DMCB,(4,(R9))                                            
         CLI   DMCB,X'FF'                                                       
         BE    TRAPERR                                                          
         MVC   WORK+3(4),DMCB+4                                                 
*--CHECK COMM BASIS                                                             
         SR    RE,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
*                                                                               
         CLI   8(R2),C'G'          SEE IF GROSS                                 
         BNE   VBFM080                                                          
         MVC   8(5,R2),=C'GROSS'                                                
         OI    6(R2),X'80'                                                      
         B     VBFM100                                                          
VBFM080  MVI   ERROR,INVBAS                                                     
         CLI   8(R2),C'N'          SEE IF NET                                   
         BNE   TRAPERR                                                          
         XC    8(5,R2),8(R2)                                                    
         MVC   8(3,R2),=C'NET'                                                  
         OI    6(R2),X'80'                                                      
         OI    WORK+2,X'01'                                                     
*--CHECK EFFECTIVE DATE                                                         
VBFM100  CLI   WORK+7,C'E'                                                      
         BE    VBFMEX                                                           
         CLI   WORK+7,C'P'                                                      
         BE    *+6                                                              
         DS    H'0'                                                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,0(R2)                                                       
         AR    R2,RE                                                            
*                                                                               
         LA    R9,8(R2)                                                         
         GOTO1 DATVAL,DMCB,(2,(R9)),QDATE                                       
         OC    DMCB(4),DMCB                                                     
         BZ    TRAPERR                                                          
*                                                                               
         MVC   FULL,DMCB+4                                                      
         GOTO1 DATCON,DMCB,(0,FULL),(2,WORK)                                    
*                                                                               
VBFMEX   B     XIT                                                              
         EJECT                                                                  
************************************                                            
* VALIDATE REP                     *                                            
************************************                                            
         SPACE                                                                  
VREP     NTR1                                                                   
         BAS   RE,VFLD                                                          
         BNZ   VREP10                                                           
         MVC   8(3,R2),=CL3'000'   DEFAULT VALUE                                
         B     VREPREK                                                          
VREP10   CLC   NFLD(3),=CL3'000'   DEFAULT VALUE                                
         BE    VREPREK                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING REPREC,R4                                                        
         MVI   REPKTYPE,C'R'                                                    
         MVI   REPKMED,C'N'                                                     
         MVC   REPKREP,NFLD        ASSUMES AFTER VFLD                           
         MVC   REPKAGY,AGENCY                                                   
         MVC   REPKFILL,=CL10'0000000000'                                       
         MVC   FILENAME,=C'STATION'                                             
         MVI   USEIO,C'Y'                                                       
         GOTO1 READ                                                             
         BE    VREPREK                                                          
         MVI   ERROR,INVREP                                                     
         B     TRAPERR                                                          
VREPREK  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
************************************                                            
* VALIDATE MARKET                  *                                            
************************************                                            
         SPACE                                                                  
VMKT     NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MKTREC,R4                                                        
         MVI   NFILE,C'T'                                                       
         MVC   QNETMKT,=XL2'0309'  INITIALIZE TO MARKET 777                     
         MVI   MKTKTYPE,C'M'                                                    
         MVI   MKTKMED,C'N'                                                     
         MVC   MKTKMKT,=CL4'0777'  ASSUMES AFTER VFLD                           
         MVC   MKTKAGY,AGENCY                                                   
         MVC   MKTKFILL,=CL9'000000000'                                         
         MVC   FILENAME,=C'STATION'                                             
         MVI   USEIO,C'Y'                                                       
         BAS   RE,NREAD                                                         
         BZ    VMKTREK                                                          
         MVC   QNETMKT,=XL2'1E61'  INITIALIZE TO MARKET 7777                    
         MVI   MKTKTYPE,C'M'                                                    
         MVI   MKTKMED,C'N'                                                     
         MVC   MKTKMKT,=CL4'7777'   ASSUMES AFTER VFLD                          
         MVC   MKTKAGY,AGENCY                                                   
         MVC   MKTKFILL,=CL9'000000000'                                         
         MVC   FILENAME,=C'STATION'                                             
         MVI   USEIO,C'Y'                                                       
         BAS   RE,NREAD                                                         
         BZ    VMKTREK                                                          
         MVI   ERROR,INVMKT                                                     
         B     TRAPERR                                                          
VMKTREK  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
************************************                                            
* VALIDATE PERIOD                  *                                            
************************************                                            
         SPACE                                                                  
VPER     NTR1                                                                   
* CHECK FOR OUT OF WEEK ROTATOR                                                 
         MVI   ROTDAY,1             ROTATION DEFAULT IS MONDAY                  
         CLI   ROTSTAT,0                                                        
         BE    PER2                                                             
         MVC   ROTDAY,ROTSTAT       GET ROTATION DAY FROM CLIENT                
PER2     XC    BWEEKS,BWEEKS                                                    
         MVC   MYWORKD(6),SVBEGIN  MOVE EST START/END                           
         MVC   MYWORKD+6(6),SVEND    MOVE EST START/END                         
         CLI   5(R2),0                                                          
         BE    PER30                                                            
*                                                                               
         MVC   WORK(17),8(R2)      MOVE DATA TO WORK                            
         LA    R4,WORK                                                          
* 3270'S LEAVE INPUT LEN 17 WITH NO DATA - SO CHECK FOR IT                      
         OC    WORK(17),SPACES                                                  
         CLC   WORK(17),SPACES                                                  
         BE    PER30                                                            
*                                                                               
         CLC   =C'S-',0(R4)                                                     
         BNE   PER3                                                             
         LA    R4,2(R4)                                                         
         B     PER7                                                             
*                                                                               
PER3     XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK),C',=,-'                              
*                                                                               
         GOTO1 DATVAL,DMCB,(0,BLOCK+12),MYWORKD                                 
         MVI   ERROR,SDTERR                                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   PER4                                                             
*                                                                               
         GOTO1 DATVAL,DMCB,(1,BLOCK+12),MYWORKD                                 
         MVI   ERROR,SDTERR                                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    TRAPERR                                                          
*                                                                               
PER4     CLI   BLOCK+1,0           SEE IF SINGLE DATE REQUESTED                 
         BNE   PER5                                                             
         MVC   MYWORKD+6(6),MYWORKD                                             
         B     PER30                                                            
PER5     ZIC   RE,BLOCK                                                         
         AR    R4,RE               POINT TO SEPARATOR                           
         CLI   0(R4),C'-'                                                       
         BNE   TRAPERR                                                          
         LA    R4,1(R4)                                                         
*                                                                               
PER7     CLC   =C'E ',0(R4)                                                     
         BE    PER30                                                            
*                                                                               
PER8     GOTO1 DATVAL,DMCB,(0,BLOCK+22),MYWORKD+6                               
         OC    0(4,R1),0(R1)       IF NOT VALID,                                
         BZ    PER10                 CHECK FOR OTHER EXPRESSIONS                
         B     PER30                                                            
*                                                                               
PER10    GOTO1 DATVAL,DMCB,(1,BLOCK+22),MYWORKD+6                               
         OC    0(4,R1),0(R1)       IF NOT VALID,                                
         BZ    PER12                 CHECK FOR OTHER EXPRESSIONS                
         B     PER30                                                            
*                                                                               
PER12    LA    R0,3                CHECK FOR 'NW OR NNW'                        
         LR    R1,R4                                                            
         MVI   ERROR,EDTERR                                                     
PER13    CLI   0(R1),C' '                                                       
         BE    TRAPERR                                                          
         CLI   0(R1),C'W'                                                       
         BE    PER14                                                            
         CLI   0(R1),C'0'                                                       
         BL    TRAPERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    TRAPERR                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,PER13                                                         
         B     TRAPERR                                                          
PER14    LA    R5,2                SET MAX LEN-1                                
         SR    R5,R0               SET FOR EX                                   
         BM    TRAPERR                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)    * EXECUTED *                                      
         CVB   R0,DUB                                                           
         STC   R0,BYTE             SAVE NUMBER OF WEEKS                         
         LA    R0,7                                                             
         CLI   1(R1),C' '                                                       
         BE    PER16                                                            
*                                                                               
         LA    R0,14                                                            
         CLI   1(R1),C'A'                                                       
         BE    PER16                                                            
         LA    R0,21                                                            
         CLI   1(R1),C'T'                                                       
         BE    PER16                                                            
         LA    R0,28                                                            
         CLI   1(R1),C'F'                                                       
         BE    PER16                                                            
         MVI   ERROR,DTERR                                                      
         B     TRAPERR                                                          
PER16    ST    R0,FULL             SAVE DAYS SKIPPED                            
         CLC   MYWORKD(2),=C'00'   DO WE HAVE START YEAR                        
         BNE   PER18               YES                                          
         MVC   MYWORKD(2),SVBEGIN  MOVE EST START YEAR                          
         CLC   SVBEGIN(2),SVEND    TEST EST ALL IN ONE YEAR                     
         BE    PER18                                                            
         CLC   MYWORKD+2(4),SVBEGIN+2 INPUT MONTH TO EST START MONTH            
         BNL   *+10                  IF HI OR EQ USE START YEAR                 
         MVC   MYWORKD(2),SVEND      ELSE USE END YEAR                          
*                                                                               
PER18    MVI   ERROR,PERERR                                                     
         CLC   MYWORKD(6),SVBEGIN                                               
         BL    TRAPERR                                                          
         CLC   MYWORKD(6),SVEND                                                 
         BH    TRAPERR                                                          
         MVC   WORK(6),MYWORKD                                                  
* FIND START DAY                                                                
         GOTO1 GETDAY,DMCB,MYWORKD,MYWORKD+12                                   
         CLC   0(1,R1),ROTDAY                                                   
         BE    PER20                                                            
         MVI   ERROR,SDAYERR                                                    
         CLC   MYWORKD(6),SVBEGIN                                               
         BNE   TRAPERR                                                          
* GET PREVIOUS MONDAY IN 'WORK'                                                 
         SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         BCTR  R0,0                                                             
         LCR   R0,R0                                                            
         GOTO1 ADDAY,DMCB,MYWORKD,WORK,(R0)                                     
*        SR    R0,R0                                                            
*        IC    R0,STDAY            START DAY OF GOALS                           
*        BCTR  R0,0                                                             
*        GOTO1 ADDAY,DMCB,WORK NORK,(R0)                                        
* BUILD DATES - FIRST DATE FROM MYWORKD, REST FROM WORK                         
PER20    GOTO1 DATCON,DMCB,MYWORKD,(2,BWEEKS)                                   
*                                                                               
         LA    R5,BWEEKS+2                                                      
         SR    R0,R0                                                            
         IC    R0,BYTE             GET NUMBER OF WEEKS                          
         MVI   ERROR,PERERR                                                     
         B     PER24                                                            
PER22    MVC   DMCB+8(4),FULL                                                   
         GOTO1 ADDAY,DMCB,WORK,WORK                                             
         CLC   WORK(6),SVEND                                                    
         BH    TRAPERR                                                          
         GOTO1 DATCON,DMCB,WORK,(2,(R5))                                        
         LA    R5,2(R5)                                                         
PER24    BCT   R0,PER22                                                         
         B     PER42                                                            
         EJECT                                                                  
PER30    CLC   MYWORKD(2),=C'00'   DO WE HAVE START YEAR                        
         BNE   PER32                                                            
         MVC   MYWORKD(2),SVBEGIN  MOVE EST START YEAR                          
         CLC   SVBEGIN(2),SVEND    TEST EST ALL IN ONE YEAR                     
         BE    PER32                                                            
         CLC   MYWORKD+2(4),SVBEGIN+2 INPUT MONTH TO EST START MONTH            
         BNL   *+10                  IF HI OR EQ USE START YEAR                 
         MVC   MYWORKD(2),SVEND      ELSE USE END YEAR                          
*                                                                               
PER32    CLC   MYWORKD+6(2),=C'00' DO WE HAVE END YEAR                          
         BNE   PER34                                                            
         MVC   MYWORKD+6(2),SVBEGIN                                             
         CLC   SVBEGIN(2),SVEND                                                 
         BE    PER34                                                            
         CLC   MYWORKD+8(4),SVBEGIN+2                                           
         BNL   *+10                                                             
         MVC   MYWORKD+6(2),SVEND                                               
*                                                                               
PER34    MVI   ERROR,STENDERR                                                   
         CLC   MYWORKD(6),MYWORKD+6                                             
         BH    TRAPERR                                                          
         MVI   ERROR,PERERR                                                     
         CLC   MYWORKD(6),SVBEGIN                                               
         BL    TRAPERR                                                          
         CLC   MYWORKD+6(6),SVEND                                               
         BH    TRAPERR                                                          
         CLI   SVPRD2,0            TEST PIGGYBACK                               
         BE    PER34X                                                           
         SPACE 1                                                                
* TEST DATES IN PARTNER ESTIMATE PERIOD *                                       
         SPACE 1                                                                
         MVI   ERROR,BADPR2DT                                                   
         CLC   MYWORKD(6),SVBEGIN2                                              
         BL    TRAPERR                                                          
         CLC   MYWORKD+6(6),SVEND2                                              
         BH    TRAPERR                                                          
*                                                                               
PER34X   MVC   WORK(6),MYWORKD     ASSUME MONDAY START                          
*                                                                               
         GOTO1 GETDAY,DMCB,MYWORKD,MYWORKD+12                                   
         CLC   0(1,R1),ROTDAY                                                   
         BE    PER35                                                            
         MVI   ERROR,SDAYERR                                                    
         CLC   MYWORKD(6),SVBEGIN                                               
         BNE   TRAPERR                                                          
* GET PREVIOUS MONDAY IN WORK                                                   
         SR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         BCTR  R0,0                                                             
         LCR   R0,R0                                                            
         GOTO1 ADDAY,DMCB,MYWORKD,WORK,(R0)                                     
PER35    DS    0H                                                               
*        SR    R0,R0                                                            
*        IC    R0,STDAY            START DAY OF GOALS                           
*        BCTR  R0,0                                                             
*        GOTO1 ADDAY,DMCB,WORK,WORK,(R0)                                        
*                                                                               
* NOW CHECK END DAY IS MONDAY OR EST END                                        
*                                                                               
PER36    GOTO1 GETDAY,DMCB,MYWORKD+6,MYWORKD+12                                 
         CLC   0(1,R1),ROTDAY                                                   
         BE    PER38                                                            
         MVI   ERROR,EDAYERR                                                    
         CLC   MYWORKD(6),MYWORKD+6                                             
         BE    PER38               IF SINGLE INPUT OK                           
         CLC   MYWORKD+6(6),SVEND                                               
         BNE   TRAPERR                                                          
*                                                                               
PER38    DS    0H                                                               
         GOTO1 DATCON,DMCB,MYWORKD,(2,BWEEKS)                                   
*                                                                               
         LA    R5,BWEEKS+2                                                      
PER40    GOTO1 ADDAY,DMCB,WORK,WORK,7                                           
         CLC   WORK(6),MYWORKD+6                                                
         BH    PER42                                                            
         GOTO1 DATCON,DMCB,WORK,(2,(R5))                                        
         LA    R5,2(R5)                                                         
         B     PER40                                                            
*                                                                               
PER42    B     XIT                                                              
         EJECT                                                                  
DEST     NTR1                                                                   
         MVI   ERROR,INVEST                                                     
         MVC   BEST,0(R1)          BINARY ESTIMATE CODE                         
         MVI   NFILE,C'S'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING EKEY,R4                                                          
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,BEST                                                     
         BAS   RE,NREAD                                                         
         BNZ   TRAPERR                                                          
         MVC   AIO,AIO3                                                         
         BAS   RE,NGETREC                                                       
         DROP  R4                                                               
         L     R6,AIO                                                           
         USING ESTHDR,R6                                                        
         MVC   ESTNAME,EDESC                                                    
         MVC   ESTDEMO,EDEMOS                                                   
         MVC   ESTTRGL,ETRGLST                                                  
         MVC   ESTUSNS,EUSRNMS                                                  
         MVC   SVBEGIN,ESTART                                                   
         MVC   SVEND,EEND                                                       
         EDIT  (1,BEST),(3,QEST),ALIGN=LEFT                                     
         B     XIT                                                              
         DROP  R4,R6                                                            
************************************                                            
**********************************************                                  
*              DATAMGR INTERFACE                                                
         SPACE 3                                                                
NHIGH    NTR1                                                                   
         MVC   COMMAND,=CL8'DMRDHI'          HANDLE COMMANDS                    
         B     DIRALL                                                           
         SPACE 1                                                                
NSEQ     NTR1                                                                   
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         B     DIRALL                                                           
         SPACE 1                                                                
NREAD    NTR1                                                                   
         MVC   COMMAND,=CL8'DMREAD'                                             
         SPACE 1                                                                
DIRALL   MVC   SYSFIL,=C'SPTDIR  '         DIRECTORIES                          
         CLI   NFILE,C'S'                                                       
         BE    DRL2                                                             
         MVC   SYSFIL,=C'UNTDIR  '                                              
         CLI   NFILE,C'U'                                                       
         BE    DRL2                                                             
         MVC   SYSFIL,=C'STATION '                                              
         CLI   NFILE,C'T'                                                       
DRL2     MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,COMMAND,SYSFIL,KEY,KEY,0                            
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
         SPACE 1                                                                
NGETREC  NTR1                                                                   
         LA    R3,KEY+14                                                        
         MVC   SYSFIL,=C'SPTFILE '     FILE                                     
         MVC   DATADISP,=H'24'                                                  
         CLI   NFILE,C'S'                                                       
         BE    GETREC2                                                          
         MVC   SYSFIL,=C'UNTFILE '                                              
         MVC   DATADISP,=H'27'                                                  
         LA    R3,KEY+21                                                        
         SPACE 1                                                                
GETREC2  L     R2,AIO                                                           
         GOTO1 DATAMGR,DMCB,(X'00',=C'GETREC'),SYSFIL,(R3),(R2),DMWORK          
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
         SPACE 1                                                                
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'92'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1  REGS=(R0,R1)                                                     
         SPACE 2                                                                
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
         PRINT GEN                                                              
TRAPERR  GOTO1 ERREX                                                            
         PRINT NOGEN                                                            
         EJECT                                                                  
* CONSTANTS TABLES, ETC *                                                       
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
         SPACE 1                                                                
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         EJECT                                                                  
*              DIRECTORY OF RECORDS AND ACTIONS                                 
         SPACE 1                                                                
RECACT   DS    0D                                                               
         SPACE 1                                                                
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
         SPACE 1                                                                
         DC    X'01',C'GOAL    ',AL1(02),X'00C2'                                
         DC    X'01',C'HISTORY ',AL1(03),X'00C2' GOAL HISTORY RECS              
         DC    X'01',C'BARULES ',AL1(04),X'00C2' BARULES RECORD                 
         DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
         SPACE 1                                                                
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,02,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,03,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,04,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,06,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,05,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         EJECT                                                                  
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
         SPACE 1                                                                
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS                     
*                                      01=USER MAINTENANCE                      
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
         SPACE 1                                                                
         DC    X'03',AL1(02,01),X'F1020000F8',C'    '  GOAL     ADD             
         DC    X'03',AL1(02,02),X'F1020000F8',C'    '           CHANGE          
         DC    X'03',AL1(02,03),X'F1020000F8',C'    '           DISPLAY         
         DC    X'03',AL1(02,04),X'F1020000F8',C'    '           SELECT          
         DC    X'03',AL1(02,05),X'F1020000F8',C'    '           DELETE          
         DC    X'03',AL1(02,06),X'F1020000F8',C'    '           RESTORE         
         DC    X'03',AL1(02,10),X'F2020000F8',C'GLNI'           LIST            
*                                                                               
         DC    X'03',AL1(03,03),X'F303000080',C'    '  HISTORY  DISPLAY         
         DC    X'03',AL1(03,04),X'F303000080',C'    '           SELECT          
         DC    X'03',AL1(03,10),X'F403000080',C'    '           LIST            
*                                                                               
         DC    X'03',AL1(04,01),X'F5040000F8',C'    '  BARULES  ADD             
         DC    X'03',AL1(04,02),X'F5040000F8',C'    '           CHANGE          
         DC    X'03',AL1(04,03),X'F5040000F8',C'    '           DISPLAY         
         DC    X'03',AL1(04,04),X'F5040000F8',C'    '           SELECT          
         DC    X'03',AL1(04,05),X'F5040000F8',C'    '           DELETE          
         DC    X'03',AL1(04,06),X'F5040000F8',C'    '           RESTORE         
         DC    X'03',AL1(04,10),X'F6040000F8',C'    '           LIST            
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE NEGOLWORK                                                      
         EJECT                                                                  
         ORG   SYSSPARE                                                         
         SPACE 2                                                                
MYWORKD  DS    CL20                                                             
THREE    DS    CL3                                                              
ROTDAY   DS    CL1                  FIRST DAY OF ROTATION                       
*                                **** MYWORK AREA ***                           
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
STAHDRD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
REPHDRD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
MKTHDRD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE NEGENPACK                                                      
         EJECT                                                                  
       ++INCLUDE NEGENDPT                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
       ++INCLUDE FATWA                                                          
       ++INCLUDE NEGOLFFD                                                       
*                                                                               
*  BASE TWA SAVE AREA                                                           
         ORG   CONHEAD-64+BASETWA                                               
SAVAREA  DS    0H                                                               
SVSECRET DS    CL1024              SECRET BLOCK FOR OFFICER                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020NEGOL00   02/25/19'                                      
         END                                                                    
