*          DATA SET SPSFM00    AT LEVEL 194 AS OF 06/15/20                      
*PHASE T21700A                                                                  
*INCLUDE MEDGET                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE DPTRD                                                                  
*INCLUDE EQVRD                                                                  
*INCLUDE KHDUMMY                                                                
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                           
* ---- ----------  -------- -------------------------------------------         
* WHOA SPEC-39079  03/24/20 CLFRZ REPORT                                        
* WHOA SPEC-33177  03/23/20 SAP Brand ID - Merged MHER's with APPL's            
* AKAT SPEC-38510  02/24/20 FCONTROL SUPPORT                                    
***********************************************************************         
         TITLE 'T21700 - SUPER FILE MAINTENANCE CONTROLLER'                     
T21700   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,T21700,R7,RR=R2,CLEAR=YES                                
         ST    R2,RELO                                                          
         LR    R9,R1                                                            
         LR    RE,RC                                                            
*                                                                               
         USING SFMPARMD,RE         ESTABLISH A(BASE PARM LIST)                  
         LA    R8,SFMPARMX                                                      
         DROP  RE                                                               
*                                                                               
         USING SPOOLD,R8                                                        
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AHI   R9,LENIOAS          GRABBING 3 I/O AREAS PLUS LABELS             
         USING SYSD,R9                                                          
         ST    RE,ASFMPARM         SAVE A(BASE PARM LIST)                       
         L     RA,SYSPARMS                                                      
         L     RA,4(RA)            A(TWA)                                       
         ST    RA,ATWA                                                          
         USING T217FFD,RA                                                       
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   IF OFFLINE                                   
         BNZ   FM20                THEN SKIP DDLINK STUFF                       
                                                                                
*===========================================================                    
* FIRST NEED TO SEE IF THIS IS AN ESTHDR UPLOAD                                 
* IF SO, NEED TO CALL SPSFM67 TO DO DDLINK STUFF                                
* THEN RETURN HERE AND DO GENCON STUFF                                          
*===========================================================                    
                                                                                
         L     R2,SYSPARMS         TEST IF CALLED FROM DDLINK UPLOAD            
         L     R2,16(R2)           R2=A(COMFACS)                                
                                                                                
* TEST IF THIS IS A TRANSFER OF CONTROL FROM ANOTHER PROGRAM                    
                                                                                
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)                                                        
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         BZ    FM20                                                             
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',BLOCK,24,GLVXCTL                              
         CLI   8(R1),0                                                          
         BNE   FM20                                                             
*                                                                               
         LARL  RE,CALLBASE                                                      
         ST    RE,VCALLBAS                                                      
*                                                                               
         LA    R1,BLOCK            SAVE CALLING SYS/PRG FOR OVERLAYS            
         USING GLVXFRSY,R1                                                      
         L     RE,ASFMPARM                                                      
         USING SFMPARMD,RE                                                      
         MVC   SFMXFRSY,GLVXFRSY                                                
         MVC   SFMXFRPR,GLVXFRPR                                                
*                                                                               
         CLC   SFMXFRSY(6),=C'SPOLIN' TEST FROM SPOT/LINK                       
         BNE   FM10                NO                                           
         DROP  R1,RE                                                            
                                                                                
* AT THIS TIME ONLY ESTHDR UPLOAD COMES FROM CONTROL LINK                       
* IF MORE RECORDS ADDED, CHANGE GLVXFRPR TO SOMETHING ELSE                      
* AND CHANGE IT BACK BEFORE EXIT                                                
                                                                                
         MVI   GCMODE,C'S'   THIS MAKES GENCON EXIT AFTER INITIALING            
         GOTO1 GENCON,DMCB,(R8)    GO THERE - PASS A(WORKING STORAGE)           
         MVI   GCMODE,C' '                                                      
                                                                                
         OI    GENSTAT1,APPLIC     TELL GENCON TO OBSERVE NODES                 
                                                                                
         MVC   CONREC(8),=CL8'EST'                                              
         MVI   CONRECH+5,3                                                      
         L     RF,CCALLOV-COMFACSD(R2)                                          
         GOTO1 (RF),DMCB,(X'67',0),ATWA                                         
         CLI   4(R1),X'FF'                                                      
         JE    *+2                 DIE ON ERROR                                 
         L     RF,0(R1)            GET ADDRESS OF T21767                        
         GOTO1 (RF),DMCB,(RC)                                                   
         J     FM32                                                             
*                                                                               
FM10     GOTO1 (RF),(R1),=C'GETF',CONRECH,3,GLVXREC                             
         CLI   8(R1),0                                                          
         BNE   FM20                FORGET ABOUT IT THEN                         
*                                                                               
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
         GOTO1 (RF),(R1),=C'GETF',CONACTH,3,GLVXACT                             
         CLI   8(R1),0                                                          
         BNE   FM20                FORGET ABOUT IT THEN                         
*                                                                               
         GOTO1 (RF),(R1),=C'DELE'                                               
*                                                                               
         MVI   CONKEY,C'*'         FAKE INPUT FOR GENCON                        
         MVI   CONKEYH+5,1                                                      
         EJECT                                                                  
*                                                                               
*  CODE FOR PFM                                                                 
*                                                                               
FM20     CLC   =C'PFM',CONREC                                                   
         BNE   FM22                                                             
         CLI   T217FFD+1,C'*'        TEST DDS TERMINAL                          
         BNE   FM22                  NO                                         
         BRAS  RE,PFMXFR                                                        
         B     XIT                                                              
********************************************************                        
*  SPECIAL CODE FOR LIMITING WITEST AND SJR                                     
********************************************************                        
FM22     CLC   =C'C2',CONREC                                                    
         BNE   FM30                                                             
         LA    R1,C2AGCYS                                                       
FM24     CLC   0(2,R1),T217FFD+14                                               
         BE    FM30                                                             
         LA    R1,2(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   FM24                                                             
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'** RECORD NOT AVAILABLE **'                       
         OI    CONHEADH+6,X'88'  TRANSMIT WITH HIGH INTENSITY                   
         B     XIT                                                              
C2AGCYS  DS    0CL2                                                             
         DC    C'WJ'               WITEST                                       
         DC    C'WI'               WILA                                         
         DC    C'WR'               INITIATIVE NA                                
         DC    C'SJ'               SJR                                          
         DC    C'TM'               TMNY  QA                                     
         DC    AL1(0)                                                           
********************************************************                        
*                                                                               
FM30     CLC   =C'SEL',CONACT                                                   
         BNE   FM32                                                             
         CLC   =C'DET',CONREC      TEST SIR DETAIL SELECT MODE                  
         BE    SLAVE               YES                                          
         CLC   =C'NSI',CONREC      TEST SIR NSID SELECT MODE                    
         BE    SLAVE               NO                                           
         CLC   =C'COMP',CONREC     TEST SIR COMPETITION SELECT MODE             
         BE    SLAVE               YES                                          
         CLC   =C'CES',CONREC      TEST SIR COMPETITIVE ESTIMATE SELECT         
         BNE   FM32                NO                                           
SLAVE    MVI   GCMODE,C'S'         YES - SLAVE MODE FOR GENCON                  
*                                                                               
FM32     LA    R2,CONRECH                                                       
         LA    RF,L'CONREC         RF = L(FIELD)                                
         LA    RE,7(RF,R2)         RE = A(LAST BYTE IN THE FIELD                
FM34     CLI   0(RE),C' '                                                       
         BH    FM36                                                             
         BCTR  RE,0                                                             
         BCT   RF,FM34                                                          
FM36     STC   RF,5(R2)                                                         
*                                                                               
         LARL  RE,RECACT                                                        
         ST    RE,ARECACT                                                       
*                                                                               
         BAS   RE,GOGENCON                                                      
*                                                                               
T21700X  TM    VCALLBAS,X'80'      TEST CALLED BY SPSFM67                       
         JZ    XIT                                                              
         L     RD,SFM67RD                                                       
         NI    VCALLBAS,X'7F'                                                   
         L     RD,4(RD)                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
GOGENCON NTR1                                                                   
         BAS   RE,SETRD                                                         
*                                                                               
GOG00    MVI   GOAGAIN,C'N'                                                     
         GOTO1 GENCON,DMCB,(R8)    GO THERE - PASS A(WORKING STORAGE)           
*                                                                               
         CLI   GOAGAIN,C'Y'        REQUEST BY APPLICATION TO GO BACK?           
         BE    GOG00               YES                                          
*                                                                               
         CLI   GCMODE,C'S'         TEST WE WERE SLAVED                          
         BNE   GOGX                NO                                           
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         OI    CONHEADH+6,X'80'    XMIT CLEARED MESSAGE FIELD                   
         L     RF,VDUMMY                                                        
         ST    RF,DMCB                                                          
*                                                                               
         CLC   =C'DET',CONREC      TEST DETAIL SELECT                           
         BE    GOG10                                                            
         CLC   =C'COMP',CONREC     TEST COMPETITION SELECT                      
         BE    GOG10                                                            
         CLC   =C'CES',CONREC      TEST COMPETITIVE ESTIMATE SELECT             
         BNE   GOG20               NO                                           
*                                                                               
GOG10    MVC   DMCB+4(4),=X'D902171F'                                           
         B     GOG30                                                            
*                                                                               
GOG20    CLC   =C'NSI',CONREC      TEST NSID SELECT                             
         BE    *+6                 YES                                          
         DC    H'0'                SOMEONE ELSE SLAVED GENCON                   
         MVC   DMCB+4(4),=X'D902170E'                                           
*                                                                               
GOG30    GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,SPOOLEND  SIR NSID/DETAIL MINI-CONTROLLER              
*                                                                               
GOGX     B     XIT                                                              
         SPACE 3                                                                
SETRD    NTR1                                                                   
         ST    RD,SYSRD            SET RD SO WE GET CONTROL BACK                
         B     XIT                                                              
         DROP  RA                                                               
         EJECT                                                                  
* INITIALIZE SYSTEM DEPENDENT VALUES *                                          
         SPACE 1                                                                
SYSINIT  NTR1                                                                   
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
         SPACE 1                                                                
SYS2     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS2                                                          
*                                                                               
         L     RE,SYSPARMS                                                      
         L     RE,8(RE)            GET A(SPOT FACS)                             
         USING SPSYSFAC,RE                                                      
         MVC   RECUP,SRECUP                                                     
         DROP  RE                                                               
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
         SPACE 1                                                                
         BAS   RE,GETCORE                                                       
         SPACE 1                                                                
* SET SYSTEM DEPENDENT VALUES *                                                 
         SPACE 1                                                                
         MVI   SYSTEM,C'S'         SPOT                                         
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 2000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'     USUALLY SPOTFILE                             
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   GETMSYS,23          USES GETMSG FOR SYSTEM 23                    
         MVC   LWORK,=AL4(LENWORK) SET WORK AREA LENGTH                         
         MVC   RCPROG(2),=C'SP'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9021700'    PRESET FOR SYSTEM CALLOVS               
         MVI   ALTPROG,X'97'       USE T297## FOR SCREENS                       
         LARL  R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
         MVI   LRECACT,13                                                       
         LA    R1,SVSTART          SET SAVED STORAGE START                      
         ST    R1,ASTARTSV                                                      
         MVI   NTWA,0              NO EXTRA TWAS                                
         MVC   LSVTWA0,=AL2(6144)   L'STORAGE TO SAVE IN TWA0                   
         OI    GENSTAT3,RESTXE00                                                
         OI    GENSTAT1,USKYMRG                                                 
         OI    GENSTAT2,DISTHSPG                                                
         OI    GENSTAT4,NEWSCRM    WANT TO BE CALLED WITH NEWSCREEN             
         OI    GENSTAT7,GES7DDS    DDS ONLY RECORDS                             
*                                                                               
         L     RA,ATWA                                                          
         USING T217FFD,RA                                                       
         CLI   TWAFIRST,0          FIRST TIME IN?                               
         BNE   *+8                                                              
         OI    GENSTAT4,SVTWA0FF   DON'T RESTORE TWA0 ON 'FF' SCREEN            
*                                                                               
         BRAS  RE,CHKCOMS          SPECIAL FOR COMMENT RECORDS                  
*                                                                               
         CLC   =C'BHOLD',CONREC    TEST ADDRESS RECORD                          
         BE    SYS10                                                            
         CLC   =C'AD',CONREC       TEST ADDRESS RECORD                          
         BE    SYS10                                                            
         CLC   =C'MAS',CONREC      TEST MASTER RECORD                           
         BE    SYS10                                                            
         CLC   =C'M2',CONREC       TEST MASTER RECORD                           
         BE    SYS10                                                            
         CLC   =C'MAS2',CONREC       TEST MASTER RECORD                         
         BE    SYS10                                                            
         CLC   =C'MAR',CONREC      TEST MARKET RECORD                           
         BE    SYS10                                                            
         CLC   =C'MKT',CONREC      TEST MARKET RECORD                           
         BE    SYS10                                                            
         CLC   =C'RE',CONREC       TEST REP RECORD                              
         BE    SYS10                                                            
         CLC   =C'I2',CONREC       TEST I2 COM REC                              
         BE    SYS10                                                            
         CLC   =C'FAUTH',CONREC    TEST AUTH REC                                
         BE    SYS10                                                            
         CLC   =C'INCH',CONREC     TEST INVOICE CHECKING REC                    
         BE    SYS10                                                            
         CLC   =C'BUCH',CONREC     BUYING GUIDELINE REC                         
         BE    SYS10                                                            
         CLC   =C'STLOCK',CONREC   STATION LOCKIN RECORD                        
         BE    SYS10                                                            
         CLC   =C'STALOCK',CONREC  STATION LOCKIN RECORD                        
         BE    SYS10                                                            
         CLC   =C'CABLE',CONREC    TEST CABLE RECORD                            
         BE    SYS10                                                            
         CLC   =C'BPCT',CONREC     TEST BILL PCT RECORD                         
         BE    SYS10                                                            
         CLC   =C'XAUT',CONREC     XAUTOPAY RECORD                              
         BE    SYS10                                                            
         CLC   =C'PURP',CONREC     TEST PURPOSE REC                             
         BE    SYS15                                                            
         CLC   =C'RCODE',CONREC    TEST RCODE REC                               
         BE    SYS15                                                            
         CLC   =C'UD',CONREC       TEST USER RECORD                             
         BE    SYS15                                                            
         CLC   =C'BYR',CONREC                                                   
         BE    SYS15                                                            
         CLC   =C'BUYER',CONREC                                                 
         BE    SYS15                                                            
         CLC   =C'SUPV',CONREC                                                  
         BE    SYS15                                                            
         CLC   =C'SUPERV',CONREC                                                
         BE    SYS15                                                            
         CLC   =C'AGENCY',CONREC                                                
         BE    SYS15                                                            
         CLC   =C'AGY',CONREC                                                   
         BE    SYS15                                                            
         CLC   =C'SPLIT',CONREC    SPLIT RECORD?                                
         BE    SYS15               YES - NO DELETE FROM LIST!                   
         CLC   =C'OMBUYER',CONREC  OMBUYER RECORD?                              
         BE    SYS15               YES - NO DELETE FROM LIST!                   
         CLC   =C'MGR',CONREC      MGROUP/MGRDEF RECORD?                        
         BE    SYS15               YES - NO DELETE FROM LIST!                   
         CLC   =C'AOR',CONREC      AOR RECORD?                                  
         BE    SYS15               YES - NO DELETE FROM LIST!                   
         CLC   =C'SHOWDEF',CONREC  SHOWDEF REC                                  
         BNE   *+8                                                              
         OI    GENSTAT4,CONFDEL    NEED TO CONF DELETE FROM LIST                
         B     SYS20                                                            
*                                                                               
SYS10    OI    GENSTAT2,DISTHSPG   RE-DISPLAY THIS PAGE AFTER SELECT            
         OI    GENSTAT3,MULTFILS   GET MODE SETFILE                             
*                                                                               
SYS15    OI    GENSTAT4,NODELLST   DON'T ALLOW DELETE FROM LIST                 
*                                                                               
SYS20    LHI   R6,SVSPARE-T217FFD                                               
         AR    R6,RA                                                            
         ST    R6,ATMPSTOR                                                      
*                                                                               
         OC    TWASAGN,TWASAGN     NEW SECURITY                                 
         JNZ   *+14                YES                                          
         OC    TWAACCS(2),TWAACCS  LIMIT ACCESS?                                
         JZ    SYS30               NO - USER HAS FULL ACCESS                    
         LHI   R6,SECBLK-T217FFD                                                
         AR    R6,RA                                                            
         ST    R6,ASECBLK                                                       
*                                                                               
SYS30    L     R1,SYSPARMS         RF = A(TIOB)                                 
         L     RF,0(R1)                                                         
         USING TIOBD,RF                                                         
         LLC   R0,TIOBAID                                                       
         CHI   R0,12                                                            
         JNH   *+8                                                              
         SHI   R0,12                                                            
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY                          
         MVC   CURDISP,TIOBCURD    SAVE CURSOR DISPLACEMENT                     
         DROP  RF                                                               
*                                                                               
SYSX     B     XIT                                                              
         DROP  RA                                                               
         EJECT                                                                  
*                                                                               
GETCORE  NTR1                                                                   
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R4,OFFICER          POINT TO ADDRESS AREA                        
         L     R1,SYSPARMS                                                      
         L     R1,8(R1)                                                         
         USING COMFACSD,R1                                                      
         L     RF,CCALLOV                                                       
         DROP  R1                                                               
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QGENCON                                                   
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   GENCON,DMCB         SAVE MODULE ADDRESS                          
*                                                                               
SYS6     MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R4),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R4,4(R4)            NEXT ADDRESS                                 
         BCT   R0,SYS6                                                          
*                                                                               
         MVI   DMCB+7,QSTAPACK                                                  
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VSTAPACK,DMCB        SAVE MODULE ADDRESS                         
*                                                                               
         MVI   DMCB+7,QCLPACK                                                   
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   CLPACK,DMCB         SAVE MODULE ADDRESS                          
*                                                                               
         MVI   DMCB+7,QCLUNPK                                                   
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   CLUNPK,DMCB         SAVE MODULE ADDRESS                          
*                                                                               
         MVI   DMCB+7,QLINKIO                                                   
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LINKIO,DMCB         SAVE MODULE ADDRESS                          
*                                                                               
         L     R0,=A(GOMSPACK)                                                  
         A     R0,RELO                                                          
         ST    R0,MSPACK                                                        
         L     R0,=A(GOMSUNPK)                                                  
         A     R0,RELO                                                          
         ST    R0,MSUNPK                                                        
         B     XIT                                                              
         EJECT                                                                  
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY *                              
         SPACE 1                                                                
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         ST    RD,COMMRD                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R7,4095(RB)         INITIALIZE MANUALLY!                         
         LA    R7,1(R7)                                                         
         L     R8,ASPOOLD                                                       
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     VUSER                                                            
         B     VMED                                                             
         B     VCLI                                                             
         B     VPROD                                                            
         B     VMKT                                                             
         B     VSTAT                                                            
         B     VSLN                                                             
         B     VUPG                                                             
         B     VSPARE              SPARE                                        
         B     VMGRPID                                                          
         B     VMGRPNO                                                          
         B     VSOURCE                                                          
         B     VCLEARF                                                          
         B     VLOADAPP                                                         
         B     VRMKT                                                            
         B     VRSTA                                                            
         B     VEST                                                             
         B     VCNREC                                                           
         B     VINITPFK                                                         
         B     VCURSERR                                                         
         B     VAUTH                                                            
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
VSPARE   DC    H'0'                                                             
*                                                                               
* GET AGENCY NAME/ADDRESS FROM CONTROL FILE ID RECORD *                         
* THIS ROUTINE GETS CALLED BY GENCON ON EVERY TRANACTION                        
* BEFORE THE APPLICATION GETS CALLED                                            
*                                                                               
VUSER    L     RA,ATWA                                                          
         USING T217FFD,RA                                                       
*                                                                               
         MVI   USEIONUM,1          DEFAULT TO AIO1                              
         MVI   USRIDFLG,0                                                       
*                                                                               
         CLI   MODE,NEWSCR         NEW SCREEN WAS JUST LOADED?                  
         BNE   *+8                                                              
         BRAS  RE,CHKSCRN          CHECK IF NEED TO DO ANYTHING SPECIAL         
*                                                                               
         CLI   TWASCR,0            ARE WE ON X'FF' SCREEN?                      
         BNE   *+12                                                             
         CLI   CONRECH+5,0                                                      
         BE    PLSENTER            PLEASE ENTER FIELDS AS REQUIRED              
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    VUSER10                                                          
         CLI   TWAFIRST,0          TEST FIRST TIME                              
         BE    VUSER10             YES - READ DATA                              
         MVC   USERNAME(66),SVUSER ELSE MOVED SAVED DATA                        
         B     VUSER40                                                          
*                                                                               
VUSER10  MVI   TWAFIRST,1          WE'VE BEEN THROUGH HERE                      
         OC    TWASAGN,TWASAGN     NEW SECURITY                                 
         JNZ   *+14                YES                                          
         OC    TWAACCS(2),TWAACCS  LIMIT ACCESS?                                
         JZ    VUSER20             NO - USER HAS FULL ACCESS                    
         GOTO1 SECRET,DMCB,('SECPINIT',ASECBLK),0                               
         JNE   *+2                 BLOCK NOT BIG ENOUGH                         
*                                                                               
VUSER20  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4           BUILD ID RECORD KEY                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),TWAORIG FROM TWA                                     
         MVC   AIO,AIO1                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,AIO                 
         L     R4,AIO                                                           
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'36'        ORIGIN DETAILS                               
         BAS   RE,FIRSTEL                                                       
         BE    VUSER30                                                          
         CLI   TWAOFFC,C'*'        DON'T DIE IF DDS TERMINAL                    
         JNE   *+2                                                              
         DROP  R4                                                               
*                                                                               
         USING CTORGD,R6                                                        
VUSER30  MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         MVC   SVUSER(66),USERNAME SAVE FOR FUTURE REF                          
         DROP  R6                                                               
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   DON'T SET ARECACT1 OFFLINE                   
         BNZ   VUSER40                                                          
         CLI   TWAOFFC,C'*'        TEST DDS TERMINAL                            
         BE    VUSER40             YES                                          
         L     R1,=A(RECACT1A)     RESTRICT DDS-ONLY RECORD TYPES               
         A     R1,RELO                                                          
         ST    R1,ARECACT1                                                      
         XC    WORK,WORK           READ SID PROFILE                             
         MVC   WORK+16(4),=C'S0SI'                                              
         MVC   WORK+20(2),AGENCY                                                
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         MVC   SVUSECMP,WORK       SAVE ESTIMATE/COMPETITION USER FLAG          
         MVC   SVUSEDDS,WORK+3     SAVE DDS FLAG                                
         CLI   SVUSECMP,C'Y'       ESTIMATE/COMPETITION USER?                   
         BE    VUSER40             YES                                          
         L     R1,=A(RECACT1)      NO - MAKE ESTIMATE/COMP INVALID              
         A     R1,RELO                                                          
         ST    R1,ARECACT1                                                      
*                                                                               
VUSER40  BAS   RE,GETCORE                                                       
*                                                                               
         CLI   TWASCR,X'32'        IS COMMENT MAINT SCREEN LOADED?              
         BNE   VUSER50                                                          
         CLC   =C'COMM',CONREC      RECORD TYPE COMMENT?                        
         BNE   VUSER50                                                          
         CLC   =C'SEL',CONACT      ACTION SELECT?                               
         BNE   VUSER50                                                          
         CLI   THISLSEL,C'C'       'C' ENTERED AS LIST SELECTION?               
         BNE   VUSER50                                                          
         CLI   PFKEY,3             ERASE A LINE?                                
         BE    *+12                                                             
         CLI   PFKEY,4             INSERT A LINE?                               
         BNE   VUSER50                                                          
         MVC   CONACT(3),=C'CHA'   FORCE ACTION TO CHANGE                       
                                                                                
VUSER50  TM    TRNSTAT,RCHANG      IF RECORD FIELD HAS CHANGED                  
         BNZ   VUSER60                                                          
         TM    TRNSTAT,ACHANG      TEST ACTION CHANGED TO LIST                  
         BZ    *+8                                                              
VUSER60  MVI   CALLSP,0            CLEAR CALLPROG STACK                         
         BRAS  RE,GETTOKEN         GET TOKEN RECORD FOR RENTRAK                 
*                                                                               
VUSERX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PFKEY INITIALIZATION                                                          
*                                                                               
* ON ENTRY:    PARAM 1             A(PFKEY VAL. TABLE) OR ZEROS                 
***********************************************************************         
VINITPFK DS    0H                                                               
         ICM   R3,7,1(R1)                                                       
         GOTO1 =A(PFKYINIT),DMCB,(RC),(R3),RR=RELO                              
         B     XIT                                                              
         EJECT                                                                  
* POSITION CURSOR TO CORRECT FIELD IN ERRORS                                    
* INPUT  : R2 = A(SCREEN HEADER)                                                
*          FIELDERR = NUMBER OF FIELD IN ERROR                                  
*                                                                               
VCURSERR CLI   FIELDERR,0          APPLICATION MUST SET FIELD NUMBER            
         BE    TRAPERR                                                          
         CLI   OFFLINE,C'Y'                                                     
         BE    TRAPERR                                                          
         L     R1,SYSPARMS         R4 = A(TIOB)                                 
         L     R4,0(R1)                                                         
         USING TIOBD,R4                                                         
         OI    6(R2),X'80'         TRANSMIT ERROR FIELD HEADER                  
         OI    TIOBINDS,TIOBSETC   INSTRUCT CURSOR SETTING                      
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD       DISPLACEMENT FROM START OF TWA               
         LA    RE,8(R2)                                                         
         SR    R1,R1               COMPUTE FIELD DISPLACEMENT INTO R1           
         SR    R0,R0                                                            
         ICM   R0,1,5(R2)          R0 HAS FIELD LENGTH                          
         BZ    CURSERR5                                                         
         LLC   RF,FIELDERR                                                      
         BCT   RF,CURSERR2         CHECK IF ERROR IS IN FIELD 1                 
         B     CURSERR4                                                         
*                                                                               
CURSERR2 CLI   0(RE),C','          SCAN FOR THE COMMAS                          
         BNE   CURSERR4                                                         
         BCT   RF,CURSERR4                                                      
         LA    R1,1(R1)            FOUND ENOUGH - SPACE PAST LAST               
         B     CURSERR6                                                         
*                                                                               
CURSERR4 LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,CURSERR2                                                      
CURSERR5 SR    R1,R1               ERROR - DIDN'T FIND ENOUGH COMMAS            
*                                                                               
CURSERR6 STC   R1,TIOBCURI         SET CURSOR DISPLACEMENT WITHIN FIELD         
         B     TRAPERR                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE AUTHORIZATION NUMBER                                                 
*                                                                               
* ON ENTRY:    PARAM 1             A(SECURITY TABLE)   OR ZEROS                 
*              SECURITY AGENCY(2),AUTHORIZATION #(2),PERSONAL ID(8)             
***********************************************************************         
VAUTH    DS    0H                                                               
         ICM   R3,7,1(R1)                                                       
         GOTO1 =A(AUTH),DMCB,(RC),(R3),RR=RELO                                  
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE MEDIA CODE *                                                         
         SPACE 1                                                                
VMED     GOTO1 ANY                                                              
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         BAS   RE,SETIONUM                                                      
         L     R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING AGYKEY,R6                                                        
         MVC   SVAPROF,AGYPROF     SAVE AGENCY PROFILE                          
         MVI   ERROR,INVMED                                                     
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
VMED2    BAS   RE,NEXTEL                                                        
         BNE   TRAPERR                                                          
         CLC   2(1,R6),8(R2)                                                    
         BNE   VMED2                                                            
         MVC   QMED,8(R2)          SAVE INPUT MEDIA CODE                        
         MVC   BAGYMD,3(R6)        DIG OUT AGENCY/MEDIA                         
         MVC   MEDNM,4(R6)         MEDIA NAME                                   
         MVC   MEDCAPT,14(R6)      AND CAPTION                                  
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE CLIENT - ON EXIT QCLT AND BCLT CONTAIN VALUES                        
         SPACE 1                                                                
VCLI     GOTO1 ANY                 CLIENT                                       
*                                                                               
         MVI   ERROR,INVCLI                                                     
         MVC   QCLT(3),WORK                                                     
         CLI   5(R2),3                                                          
         BH    TRAPERR                                                          
         CLI   5(R2),2                                                          
         BL    TRAPERR                                                          
*                                                                               
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   TRAPERR                                                          
         SPACE 1                                                                
* READ CLIENT HEADER *                                                          
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   ERROR,NOCLTFND                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   TRAPERR                                                          
                                                                                
         BAS   RE,SETIONUM                                                      
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         SPACE 1                                                                
* SAVE CLIENT PRODUCT LIST *                                                    
         SPACE 1                                                                
         LA    R4,CLIST                                                         
         LA    R5,880                                                           
         LA    RE,SVCLIST                                                       
         LA    RF,880                                                           
         MVCL  RE,R4                                                            
*                                                                               
         MVC   SVCPROF,CPROF       SAVE CLIENT PROFILES                         
         MVC   SVCXTRA,CEXTRA                                                   
         MVC   CLTNM,CNAME         AND CLIENT NAME                              
         MVC   SVCACCS,CACCESS                                                  
         MVC   SVOFFC,COFFICE                                                   
*                                                                               
         L     RA,ATWA                                                          
         USING T217FFD,RA                                                       
*                                                                               
         CLI   T217FFD+6,C'+'      TEST MARKET LIMIT ACCESS                     
         BE    *+8                                                              
         BRAS  RE,CALLOFCR                                                      
         B     XIT                                                              
         DROP  R6,RA                                                            
         EJECT                                                                  
* VALIDATE PRD (-SLN)   - ON EXIT WORK(3)   = EBCDIC PRODUCT                    
*                                 WORK+3(1) = PRODUCT CODE                      
*                                 WORK+4(1) = SPOT LENGTH (IF ENTERED)          
*                                 BPRD      = PRODUCT CODE                      
*                                 QPRD      = EBCDIC PRODUCT                    
*                                 PRDNM     = PRODUCT NAME                      
VPROD    XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         LA    R4,BLOCK                                                         
*                                                                               
         MVI   ERROR,INVPROD                                                    
         CLI   AAAOK,C'Y'          IS PRODUCT AAA OK?                           
         BE    *+14                                                             
         CLC   =C'AAA',12(R4)                                                   
         BE    TRAPERR                                                          
         CLI   0(R4),2                                                          
         BL    TRAPERR                                                          
         CLI   0(R4),3                                                          
         BH    TRAPERR                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),12(R4)                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
*                                                                               
         BAS   RE,SETIONUM                                                      
         L     R6,AIO                                                           
         USING PRDHDRD,R6                                                       
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   WORK(3),12(R4)      RETURN EBCDIC PRD CODE                       
         MVC   WORK+3(1),PCODE+1      AND BINARY PRD CODE                       
         MVC   BPRD,PCODE+1                                                     
         MVC   QPRD,WORK                                                        
         MVC   PRDNM,PNAME                                                      
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE SPOT LENGTH IF INPUT *                                               
         SPACE 1                                                                
         MVI   WORK+4,0                                                         
         CLI   1(R4),0             TEST INPUT                                   
         BE    XIT                                                              
         MVC   WORK+4(1),11(R4)    RETURN SLN IN BINARY                         
         MVI   ERROR,BADSLN                                                     
         CLI   1(R4),3                                                          
         BH    TRAPERR                                                          
         TM    3(R4),X'80'         TEST NUMERIC                                 
         BZ    TRAPERR                                                          
*                                                                               
VPRD8    MVC   DMCB+4(4),=X'D9000A57'                                           
         GOTO1 CALLOV,DMCB,0         GET SPSLENTAB                              
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R1,DMCB             POINT TO START OF PHASE                      
         LH    RE,0(R1)            ENTRY LENGTH                                 
         L     RF,2(R1)            EOT DSPL                                     
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         MVI   BYTE,C'T'                                                        
         CLI   QMED,C'T'                                                        
         BE    VPRD1                                                            
         CLI   QMED,C'N'                                                        
         BE    VPRD1                                                            
         CLI   QMED,C'C'                                                        
         BE    VPRD1                                                            
*                                                                               
         MVI   BYTE,C'R'                                                        
         CLI   QMED,C'R'                                                        
         BE    VPRD1                                                            
         CLI   QMED,C'X'                                                        
         BE    VPRD1                                                            
         DC    H'0'                                                             
*                                                                               
VPRD1    CLC   =C'00',0(R1)        TEST DEFAULT TABLE                           
         BE    VPRD2                                                            
         CLC   0(2,R1),AGENCY      ELSE MATCH AGY                               
         BNE   *+14                                                             
VPRD2    CLC   BYTE,2(R1)          AND MEDIA                                    
         BE    VPRD3                                                            
*                                                                               
         BXLE  R1,RE,VPRD1                                                      
         DC    H'0'                                                             
*                                                                               
VPRD3    AHI   R1,4                POINT BEYOND HEADER                          
*                                                                               
         LLC   RE,WORK+4           GET SLN                                      
         AR    RE,RE               X 2                                          
         AR    RE,R1               POINT TO ENTRY                               
         CLI   1(RE),0             SLN VALID?                                   
         BE    TRAPERR             NO                                           
*                                                                               
VPRD10   MVC   WORK+4(1),11(R4)                                                 
         B     XIT                                                              
*                                                                               
*                                                                               
* VALIDATE ESTIMATE - ON EXIT QEST  = EBCDIC ESTIMATE                           
*                             BEST  = BINARY ESTIMATE                           
*                             ESTNM = ESTIMATE NAME                             
*                                                                               
VEST     GOTO1 ANY                                                              
         MVI   ERROR,INVEST                                                     
         MVC   QEST,WORK                                                        
         TM    4(R2),X'08'         TEST VALID NUMERIC                           
         BZ    TRAPERR                                                          
         CLI   5(R2),3                                                          
         BH    TRAPERR                                                          
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         CHI   RE,1                TEST IN RANGE 1-255                          
         BL    TRAPERR                                                          
         CHI   RE,255                                                           
         BH    TRAPERR                                                          
         STC   RE,BEST             SET BINARY ESTIMATE                          
         XC    KEY,KEY                                                          
         LA    R6,KEY              READ ESTIMATE HEADER                         
         USING ESTHDRD,R6                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,BEST                                                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
*                                                                               
         BAS   RE,SETIONUM                                                      
         L     R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   ESTNM,EDESC         SET ESTIMATE NAME                            
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE STATION CALL LETTERS - ON EXIT QSTA QMKT BMKTSTA                     
*                       QSTANEW STAPRNTN AND STAPRINT ARE SET                   
         SPACE 1                                                                
VSTAT    LA    R4,BLOCK                                                         
         USING STABLKD,R4                                                       
         XC    0(STBLNQ,R4),0(R4)  CLEAR INTERFACE BLOCK                        
         XC    KEY,KEY                                                          
         MVC   STBMED,QMED         SET MEDIA                                    
         ST    R2,STBADDR          SET A(STATION FIELD)                         
         MVI   STBCTRY,C'U'                                                     
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+8                                                              
         MVI   STBCTRY,C'C'                                                     
         MVC   STBACOM,ACOMFACS                                                 
         GOTO1 STAVAL,DMCB,(R4)                                                 
         MVI   ERROR,INVSTAT                                                    
         CLI   STBERR,0                                                         
         BNE   TRAPERR                                                          
         MVC   QSTA,STBSTA         SET OUTPUT STATION                           
         MVC   QSTANEW,STBSTA      SET 8 CHAR OUTPUT STATION                    
         SPACE 1                                                                
* FORMAT STATION FOR PRINTING (EG WABC-FM) *                                    
         SPACE 1                                                                
VSTA10   MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNTN,SPACES                                                  
         MVC   STAPRNT(4),QSTA                                                  
         MVC   STAPRNTN(4),QSTANEW                                              
         CLI   QSTANEW,C'0'        IF THIS IS CABLE                             
         BL    VSTA11                                                           
         LA    RE,STAPRNTN+3       SET PRINTABLE STATION WITH                   
         CLI   0(RE),C' '                                                       
         BNE   *+6                 '/' & NETWORK                                
         BCTR  RE,0                                                             
         CLC   QSTANEW+4(3),SPACES                                              
         BNH   VSTA13                                                           
         MVI   1(RE),C'/'                                                       
         MVC   2(3,RE),QSTANEW+5                                                
         B     VSTA13                                                           
*                                                                               
VSTA11   LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),QSTANEW+4   MOVE SUB-MEDIA                               
         MVI   3(RE),C'V'          ASSUME TV                                    
         CLI   QMED,C'T'                                                        
         BE    VSTA12                                                           
         MVI   3(RE),C'M'          ASSUME RADIO                                 
         CLI   QMED,C'R'                                                        
         BE    VSTA12                                                           
         MVI   3(RE),C' '                                                       
         SPACE 1                                                                
*                                                                               
VSTA12   LA    RE,STAPRNTN+3                                                    
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),QSTANEW+4      MOVE SUB-MEDIA                            
         MVI   3(RE),C'V'          ASSUME TV                                    
         CLI   QMED,C'T'                                                        
         BE    VSTA13                                                           
         MVI   3(RE),C'M'          ASSUME RADIO                                 
         CLI   QMED,C'R'                                                        
         BE    VSTA13                                                           
         MVI   3(RE),C' '                                                       
         SPACE 1                                                                
* READ STATION MASTER RECORD *                                                  
         SPACE 1                                                                
VSTA13   DS    0H                                                               
         MVC   KEY(17),ZEROES                                                   
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(5),QSTANEW                                                 
         CLI   KEY+6,C' '                                                       
         BNE   *+8                                                              
         MVI   KEY+6,C'T'                                                       
         MVC   KEY+7(2),AGENCY                                                  
         CLC   QCLT,SPACES                                                      
         BH    *+10                                                             
         MVC   QCLT,ZEROES                                                      
         MVC   KEY+9(3),QCLT                                                    
*                                                                               
         BAS   RE,SETIONUM                                                      
*                                                                               
VSTA13A  BAS   RE,RDSTA                                                         
         CLI   8(R1),0                                                          
         BNE   TRAPERR                                                          
*                                                                               
VSTA13B  L     R6,AIO                                                           
         USING STARECD,R6                                                       
         MVC   QMKT,SMKT                                                        
         GOTO1 MSPACK,DMCB,QMKT,QSTANEW,BMKTSTA                                 
         EJECT                                                                  
* READ MARKET RECORD TO IO+500                                                  
         SPACE 1                                                                
VSTA13C  LA    R6,500(R6)                                                       
*                                                                               
VSTA14   ST    R6,AIO                                                           
         USING MKTRECD,R6                                                       
         MVC   KEY(17),ZEROES                                                   
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         MVI   ERROR,INVMKT                                                     
         BAS   RE,HISTA                                                         
         L     R6,AIO              SAVE I/O AREA ADDRESS                        
*                                                                               
         BAS   RE,SETIONUM         RESTORE IO ADDRESS                           
*                                                                               
         CLC   KEY(15),0(R6)       TEST MARKET FOUND                            
         BE    VSTA16                                                           
         CLI   QMED,C'C'           ERROR EXCEPT FOR MEDIA=C, MKT=0000           
         BNE   TRAPERR                                                          
         CLC   QMKT,ZEROES                                                      
         BNE   TRAPERR                                                          
         MVC   MKTNM(7),=C'NETWORK'                                             
         B     XIT                                                              
*                                                                               
VSTA16   MVC   SVMACCS,MKTLTACC                                                 
         L     RA,ATWA                                                          
         USING T217FFD,RA                                                       
*                                                                               
****     OC    T217FFD+6(2),T217FFD+6 TEST ANY LIMIT ACCESS                     
****     BZ    *+8                                                              
****     BRAS  RE,CALLOFCR                                                      
*                                                                               
         CLI   T217FFD+6,C'+'      TEST MARKET LOCKOUT                          
         BNE   VSTA20                                                           
         BRAS  RE,CALLOFCR                                                      
         DROP  RA                                                               
*                                                                               
VSTA20   MVC   MKTNM,MKTNAME       RETURN MARKET NAME TO USER                   
         MVI   BYTE,C'0'           FIND RATING SERVICE MARKET                   
         CLI   BKVALSRC,C'N'                                                    
         BE    *+16                                                             
         CLI   BKVALSRC,C'C'                                                    
         BE    *+8                                                              
         MVI   BYTE,C'1'                                                        
***                                                                             
* NEED TO CHECK IF RATING SERVICE FIELD IS SET FOR CANADIAN SOFT DEMOS          
***                                                                             
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   VSTA25              NO                                           
         CLI   QMED,C'T'           MEDIA T?                                     
         BNE   VSTA25              NO                                           
         CLI   MKTRSVC,0           RATING SERVICE SET?                          
         BE    VSTA25              NO                                           
         MVC   BYTE,MKTRSVC        SET RATING SERVICE                           
*                                                                               
VSTA25   CLC   MKTRS1,BYTE                                                      
         BNE   *+10                                                             
         MVC   MKTRS,MKTRSM1                                                    
         CLC   MKTRS2,BYTE                                                      
         BNE   *+10                                                             
         MVC   MKTRS,MKTRSM2                                                    
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE MARKET CODE *                                                        
         SPACE 1                                                                
VMKT     DS    0H                                                               
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BO    *+12                                                             
         MVI   ERROR,INVMKT                                                     
         B     TRAPERR                                                          
*                                                                               
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2) *EXECUTED*                                           
*                                                                               
         CVB   R0,DUB                                                           
         STCM  R0,3,BMKT                                                        
*                                                                               
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB            SET FOR USER TOO                             
*                                                                               
         BAS   RE,SETIONUM                                                      
         L     R6,AIO                                                           
         B     VSTA14                                                           
         EJECT                                                                  
*                                                                               
* VALIDATE SPOT LENGTH *                                                        
         SPACE 1                                                                
VSLN     MVI   ERROR,BADSLN                                                     
         GOTO1 VALINUM                                                          
         MVC   WORK(1),ACTUAL                                                   
         MVC   WORK+4(1),ACTUAL    MOVE HERE FOR EDIT COMPATABILITY             
         B     VPRD8                                                            
         EJECT                                                                  
* VALIDATE MARKET GROUP ID - ON EXIT MGRPID AND MGRPLEN ARE SET                 
         SPACE 1                                                                
VMGRPID  GOTO1 ANY                                                              
         MVI   ERROR,INVMGRP                                                    
         CLI   8(R2),C'A'          TEST MKTGRP ID ALPHA                         
         BL    TRAPERR                                                          
         CLI   8(R2),C'Z'                                                       
         BH    TRAPERR                                                          
         MVC   MGRPID,8(R2)        MARKET GROUP ID                              
         XC    KEY,KEY             GET MKTGRP X'0000' RECORD                    
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         CLI   MGRPID,C'F'                                                      
         BH    *+10                                                             
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+8(1),MGRPID                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   TRAPERR                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL            GET MKTGRP BREAK DESCRIPTION                 
         BE    *+6                                                              
         DC    H'0'                                                             
         USING MKGEL01,R6                                                       
         MVC   MGRPLEN,MKGBK1LN    MKTGRP BREAK 1 LEN                           
         DROP  R6                                                               
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE MARKET GROUP NO - ON EXIT MGRPNO AND MGRPNM ARE SET                  
         SPACE 1                                                                
VMGRPNO  MVI   ERROR,INVMGRP                                                    
         LLC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         CLM   R3,1,MGRPLEN        TEST FOR CORRECT BREAK 1 LENGTH              
         BNE   TRAPERR                                                          
         XC    KEY,KEY             GET MKTGRP RECORD                            
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT       FIRST TRY WITH CLIENT                        
         MVC   KEY+8(1),MGRPID                                                  
         MVC   WORK(4),ZEROES                                                   
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),9(R2)                                                    
         PACK  DUB(3),WORK(5)                                                   
         MVC   MGRPNO,DUB          MARKET GROUP NUMBER                          
         MVC   KEY+9(2),MGRPNO                                                  
*                                                                               
VMGRPN2  DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         UNPK  DUB(5),KEY+9(3)                                                  
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),DUB         TEST RECORD EXISTS                           
         BE    VMGRPN4                                                          
         OC    KEYSAVE+3(2),KEYSAVE+3  NO - TEST FOR CLIENT SPECIFIC            
         BZ    TRAPERR                      NO - ERROR                          
         MVC   KEY,KEYSAVE                  YES- TRY WITHOUT CLIENT             
         XC    KEY+3(2),KEY+3                                                   
         B     VMGRPN2                                                          
*                                                                               
VMGRPN4  DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING MKGEL10,R6                                                       
         MVC   MGRPNM,MKGNAM1      MARKET GROUP NAME                            
         DROP  R6                                                               
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE REP STATION CALL LETTERS - ON EXIT QSTA, QMKT, BMKTSTA,              
*                                         STAPRINT, MKTNM ARE SET               
*                                                                               
VRSTA    MVI   ERROR,INVSTAT                                                    
         CLI   5(R2),3                                                          
         BL    TRAPERR                                                          
         CLI   5(R2),4                                                          
         BH    TRAPERR                                                          
*                                                                               
         MVC   QSTA(4),8(R2)       SAVE CALL LETTERS                            
         OC    QSTA,SPACES                                                      
         MVC   QSTA+4(1),QMED      ALWAYS MEDIA 'T'                             
*                                                                               
         MVC   STAPRNT,SPACES      PRINTABLE VERSION (E.G. WABC-T)              
         MVC   STAPRNT(4),QSTA                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),QSTA+4      MOVE SUB-MEDIA                               
         MVI   3(RE),C'V'          ASSUME TV                                    
*                                                                               
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   VRSTA10             NO                                           
         L     R6,ATWA             GET A(MASTC)                                 
         L     R6,TWAMASTC-T217FFD(R6)                                          
         L     R6,MCUTL-MASTD(R6)  GET A(UTL)                                   
         MVC   SVUTLSYS,4(R6)      SAVE SPOT SENUM                              
         MVI   4(R6),X'08'         PUT REP SENUM IN UTL                         
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',REPFLIST,AIO                     
         B     VRSTA20                                                          
*                                                                               
VRSTA10  GOTO1 SWITCH,DMCB,=C'REP',0                                            
         CLI   DMCB+4,2            TEST REP SYSTEM IS STARTED                   
         BNE   *+12                YES                                          
         MVI   ERROR,REPISOFF                                                   
         B     TRAPERR                                                          
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
VRSTA20  LA    R6,KEY              READ PASSIVE POINTER                         
         USING RMKTRECD,R6                                                      
         XC    KEY,KEY                                                          
         MVI   RMKTSTYP,X'8B'                                                   
         MVC   RMKTSREP,AGENCY                                                  
         MVC   RMKTSSTA(4),QSTA                                                 
         MVI   RMKTSSTA+4,C' '                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'REPDIR',KEY,KEY,0                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(23),KEYSAVE     TEST KEY FOUND                               
         BE    *+12                YES                                          
         MVI   ERROR,INVSTAT                                                    
         B     TRAPERR                                                          
         MVC   QMKT,RMKTSMKT                                                    
         GOTO1 MSPACK,DMCB,QMKT,QSTA,BMKTSTA                                    
*                                                                               
VRSTA30  GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'REPFIL',KEY+28,AIO,DMWORK         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVC   MKTNM(L'RMKTNAME),RMKTNAME  RETURN MARKET NAME TO USER           
         DROP  R6                                                               
*                                                                               
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   VRSTA40             NO                                           
         GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'REP'                                  
         L     R6,ATWA             GET A(MASTC)                                 
         L     R6,TWAMASTC-T217FFD(R6)                                          
         L     R6,MCUTL-MASTD(R6)  GET A(UTL)                                   
         MVC   4(1,R6),SVUTLSYS    SWITCH BACK TO SPOT SYSTEM                   
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',SPTFLIST,AIO                    
         B     XIT                                                              
*                                                                               
VRSTA40  GOTO1 SWITCH,DMCB,=C'SPOT',0                                           
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    XIT                 YES                                          
         DC    X'00'                                                            
         SPACE                                                                  
SVUTLSYS DS    X'00'                                                            
         SPACE 3                                                                
SPTFLIST DC    CL8'NSPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         SPACE                                                                  
REPFLIST DC    CL8'NREPFILE'                                                    
         DC    CL8'NREPDIR'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         EJECT                                                                  
* VALIDATE REP MARKET CODE *                                                    
*                                                                               
VRMKT    TM    4(R2),X'08'         TEST NUMERIC                                 
         BO    *+12                                                             
         MVI   ERROR,INVMKT                                                     
         B     TRAPERR                                                          
*                                                                               
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2) *EXECUTED*                                           
*                                                                               
         CVB   R0,DUB                                                           
         STCM  R0,3,BMKT                                                        
*                                                                               
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB            SET FOR USER TOO                             
*                                                                               
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   VRMKT10             NO                                           
         L     R6,ATWA             GET A(MASTC)                                 
         L     R6,TWAMASTC-T217FFD(R6)                                          
         L     R6,MCUTL-MASTD(R6)  GET A(UTL)                                   
         MVC   SVUTLSYS,4(R6)      SAVE SPOT SENUM                              
         MVI   4(R6),X'08'         PUT REP SENUM IN UTL                         
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',REPFLIST,AIO                     
         B     VRMKT20                                                          
*                                                                               
VRMKT10  GOTO1 SWITCH,DMCB,=C'REP',0                                            
         CLI   DMCB+4,2            TEST REP SYSTEM IS STARTED                   
         BNE   *+12                YES                                          
         MVI   ERROR,REPISOFF                                                   
         B     TRAPERR                                                          
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
VRMKT20  LA    R6,KEY              READ MARKET KEY                              
         USING RMKTRECD,R6                                                      
         XC    KEY,KEY                                                          
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,AGENCY                                                  
         MVC   RMKTKMKT,QMKT                                                    
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'REPDIR',KEY,KEY,0                 
         CLI   8(R1),0             TEST VALID MARKET                            
         BE    VRSTA30             YES - READ THE RECORD                        
         MVI   ERROR,INVMKT                                                     
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
*  WHEN CHANGING CANADIAN TV STATION/CHANGE MEDIA C & N                         
*                                                                               
*  P1 - C'A' - ADDING A RECORD                                                  
*       C'C' - CHANGING A RECORD                                                
*       C'D' - DELETING A RECORD                                                
*       C'R' - RESTORING A RECORD                                               
*                                                                               
VCNREC   DS    0H                                                               
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY                              
         BNE   CNRXIT                                                           
         CLI   QMED,C'T'           TV ONLY                                      
         BNE   CNRXIT                                                           
*                                                                               
CNR20    MVC   SVKEY,KEY                                                        
         MVC   BYTE,0(R1)                                                       
*                                                                               
         USING STARECD,R3                                                       
         L     R3,AIO                                                           
         LA    R5,KEY+3                                                         
         CLI   0(R3),C'C'          DISTRICTS                                    
         BE    CNR30                                                            
         CLI   0(R3),C'V'          DIVISIONS                                    
         BE    CNR30                                                            
         CLI   0(R3),C'G'          REGIONS                                      
         BE    CNR30                                                            
         LA    R5,KEY+1                                                         
*                                                                               
CNR30    MVI   0(R5),C'N'          NETWORK                                      
         CLI   0(R3),C'S'          STA MASTERS                                  
         BE    CNR40                                                            
         CLI   0(R3),C'A'          ADDR RECS                                    
         BNE   CNR50                                                            
*                                                                               
CNR40    MVI   KEY+6,C'N'          ALSO HAVE MEDIA IN REC+6                     
*                                                                               
CNR50    BAS   RE,CNCHK                                                         
*                                                                               
CNR80    MVC   KEY,SVKEY                                                        
         MVI   0(R5),C'C'          COMBINED                                     
         CLI   KEY,C'A'                                                         
         BE    CNR60                                                            
         CLI   KEY,C'S'            ADDR AND STA MASTER RECS                     
         BNE   CNR70               ALSO HAVE MEDIA IN KEY+6                     
*                                                                               
CNR60    MVI   KEY+6,C'C'                                                       
*                                                                               
CNR70    BAS   RE,CNCHK                                                         
*                                                                               
CNRX     MVC   0(STAKEYLN,R3),SVKEY                                             
         MVC   KEY,SVKEY           RESTORE KEY                                  
*                                                                               
CNRXIT   B     XIT                                                              
         SPACE 2                                                                
*                                                                               
CNCHK    NTR1                                                                   
         CLI   BYTE,C'A'           ADDING A NEW STATION                         
         BNE   CNC10                                                            
         MVC   0(STAKEYLN,R3),KEY                                               
         BAS   RE,ADDSTA                                                        
         B     CNCX                                                             
*                                                                               
CNC10    CLI   BYTE,C'C'           CHANGE                                       
         BE    CNC40                                                            
         CLI   BYTE,C'R'           RESTORE                                      
         BE    CNC40                                                            
         CLI   BYTE,C'D'           DELETE                                       
         BNE   CNCX                                                             
*                                                                               
CNC40    MVC   AIO,AIO2                                                         
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         BAS   RE,HISTA            READ RECORD                                  
         NI    DMINBTS,X'F7'                                                    
         L     R4,AIO2                                                          
         MVC   AIO,AIO1                                                         
         CLC   KEY(STAKEYLN),0(R4) IS IT FOUND                                  
         BE    CNC50                                                            
         CLI   BYTE,C'D'           NO - DELETING SO IGNORE                      
         BE    CNCX                                                             
         MVC   0(STAKEYLN,R3),KEY                                               
         BAS   RE,ADDSTA           ELSE ADD IT                                  
         B     CNCX                                                             
*                                                                               
CNC50    MVC   0(STAKEYLN,R3),0(R4)   YES - CHANGE KEYS                         
         CLI   BYTE,C'D'           RECORD WAS FOUND                             
         BNE   CNC60                                                            
         OI    SCNTL,X'80'         MARK IT DELETED                              
         B     CNC70                                                            
*                                                                               
CNC60    CLI   BYTE,C'R'           RESTORE                                      
         BNE   CNC70                                                            
         NI    SCNTL,X'7F'         MARK IT UN-DELETED                           
*                                                                               
CNC70    BAS   RE,CHGSTA                                                        
*                                                                               
CNCX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
RDSTA    MVC   COMMAND,=C'DMREAD'                                               
         B     STA                                                              
*                                                                               
HISTA    MVC   COMMAND,=C'DMRDHI'                                               
         B     STA                                                              
*                                                                               
ADDSTA   MVC   COMMAND,=C'DMADD '                                               
         B     STA                                                              
*                                                                               
CHGSTA   MVC   COMMAND,=C'DMWRT '                                               
*                                                                               
STA      NTR1                          SET NTR1                                 
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'STATION',KEY,AIO               
         B     XIT                                                              
         EJECT                                                                  
*  VALIDATE RATING SOURCE                                                       
*                                                                               
VSOURCE  XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELAGY,AGENCY     FOR MARKET SECURITY                          
*                                                                               
         GOTO1 ANY                                                              
         LLC   RE,5(R2)            GET INPUT LENGTH                             
         BCTR  RE,0                SET FOR EX                                   
*                                                                               
         MVI   ERROR,INVSRC                                                     
         LA    R4,SRCLIST                                                       
         USING SRCLISTD,R4                                                      
*                                                                               
SRC10    EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),SRCINPUT                                                 
         BE    SRC20                                                            
         LA    R4,SRCNEXT                                                       
         CLI   0(R4),X'FF'                                                      
         BNE   SRC10                                                            
         B     TRAPERR                                                          
*                                                                               
SRC20    MVC   DBSELSRC,SRCDBSRC                                                
         MVC   DBFILE,SRCDBFIL                                                  
         MVC   8(6,R2),SRCINPUT                                                 
         OI    6(R2),X'80'         TRANSMIT FULL SOURCE NAME                    
         MVC   DBSELMED,SRCSELMD                                                
         MVC   BKVALSRC,SRCBKSRC                                                
*                                                                               
         MVC   BYTE,DBSELMED                                                    
         CLI   BYTE,C'C'                                                        
         BNE   *+8                                                              
         MVI   BYTE,C'T'                                                        
         GOTO1 MEDGET,DMCB,(BYTE,AGENCY),DATAMGR,WORK                           
         MVC   BAGYMD,WORK         SAVE AGENCY/MEDIA BYTE                       
         CLI   8(R1),X'FF'                                                      
         BE    TRAPERR                                                          
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
SRCLIST  DS    0CL12                                                            
         DC    C'ARB   ',C'T',C'TP ',C'AA'                                      
         DC    C'BBM   ',C'C',C'TP ',C'BB'                                      
         DC    C'CSI   ',C'C',C'TP ',C'CC'                                      
         DC    C'NSI   ',C'T',C'TP ',C'NN'                                      
         DC    C'RARB  ',C'R',C'TP ',C'AA'                                      
         DC    C'RBIR  ',C'R',C'TP ',C'NN' **SHOULD BE SOURCE B                 
         DC    C'BIRCH ',C'R',C'TP ',C'NN' **                                   
         DC    C'DRARB ',C'R',C'RDP',C'AA'                                      
         DC    C'DRBIR ',C'R',C'RDP',C'NN' **                                   
         DC    X'FF'                                                            
*                                                                               
SRCLISTD DSECT                                                                  
SRCINPUT DS    CL6                 VALID INPUT                                  
SRCSELMD DS    CL1                 DBSELMED VALUE                               
SRCDBFIL DS    CL3                 DBSELFIL VALUE                               
SRCDBSRC DS    CL1                 DBSELSRC VALUE                               
SRCBKSRC DS    CL1                 BOOKVAL SOURCE VALUE                         
SRCNEXT  EQU   *                                                                
*                                                                               
T21700   CSECT                                                                  
         EJECT                                                                  
****************************************************                            
* EDIT INPUT PARAMETERS FOR DEMO UPGRADES          *                            
* UPP= (OR UPT=), BK=,  DT=  , ST=                 *                            
*                                                  *                            
* NOTE -- SCANNER ENTRIES USE  NON-STANDARD LENGTH *                            
*                                                  *                            
* NOTE - DMCB MUST CONTAIN ADDRESS OF SPDEMUP AREA *                            
*                                                  *                            
****************************************************                            
         SPACE 1                                                                
VUPG     L     RA,DMCB             GET UPGRADE AREA ADDRESS                     
         USING SPDEMUPD,RA                                                      
         XC    0(SPDEMUPL,RA),0(RA)                                             
         MVI   ERROR,BADUPGD                                                    
         XC    BLOCK(256),BLOCK    CLEAR SCANNER BLOCK                          
         LA    R4,BLOCK                                                         
         XC    ELEM,ELEM           USE FOR FAKE TWA FIELDS                      
*                                                                               
         CLC   =C'UPP=BK/',8(R2)   CANADIAN UPGRADE EXPRESSION?                 
         BE    *+14                                                             
         CLC   =C'UPT=BK/',8(R2)                                                
         BNE   VUPG1               NO                                           
*                                                                               
         MVC   SPUPFIL,10(R2)      MOVE P OR T TO SPUP BLOCK                    
*                                                                               
         MVC   ELEM+136(4),8(R2)   BUILD TWA FIELD AT ELEM+128                  
         MVC   ELEM+140(7),=C'IX/100,'  UPT=IX/100,                             
         LLC   RE,5(R2)            COMPUTE EXPRESSION LENGTH                    
         SHI   RE,5                SUBTRACT L'UPT= AND 1 FOR EX                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+147(0),12(R2)  MOVE DATA                                    
         MVI   ELEM+149,C'='       REPLACE 'BK/' WITH 'BK='                     
         LA    RE,12(RE)                                                        
         STC   RE,ELEM+133         SET INPUT STRING LENGTH                      
         LA    RE,8(RE)            ADJUST LEN TO INCLUDE FLDHDR                 
         STC   RE,ELEM+128         AND SET IN FLDHDR                            
         GOTO1 SCANNER,DMCB,(30,ELEM+128),BLOCK,0                               
         B     VUPG4                                                            
*                                                                               
VUPG1    GOTO1 SCANNER,DMCB,(30,(R2)),BLOCK,0                                   
         CLI   0(R4),3                                                          
         BNE   TRAPERR                                                          
         CLC   12(3,R4),=C'UPP'                                                 
         BE    *+14                                                             
         CLC   12(3,R4),=C'UPT'                                                 
         BNE   TRAPERR                                                          
         SPACE 1                                                                
         MVC   SPUPFIL,14(R4)      MOVE P OR T TO ELEM                          
         MVC   SPUPPRG,22(R4)      MOVE DATA TO SAVE AREA                       
         SPACE 1                                                                
* READ INPUT STRING FOR UPGRADE EXPRESSION AND BUILD FLDHDR *                   
         SPACE 1                                                                
VUPG4    LLC   RE,1(R4)            GET EXPRESSION LENGTH                        
         STC   RE,ELEM+5           SET INPUT STRING LENGTH                      
         BCTR  RE,0                SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),22(R4)    MOVE DATA                                    
*                                                                               
         LA    RE,9(RE)            ADJUST LEN TO INCLUDE FLDHDR                 
         STC   RE,ELEM             AND SET IN FLDHDR                            
         SPACE 1                                                                
* GET UPVAL ADDRESS *                                                           
         SPACE 1                                                                
         GOTO1 UPVAL,(R1),ELEM,WORK,(C'/',ACOMFACS)                             
         CLI   0(R1),0             TEST VALID EXPRESSION                        
         BE    TRAPERR                                                          
         MVC   SPUPTYPE(8),WORK+4                                               
*                                                                               
VUPG8    LA    R4,52(R4)           *** NOTE NON-STANDARD SCANNER ***            
         OC    0(2,R4),0(R4)       TEST ANY MORE FIELDS                         
         BZ    XIT                 NO -DONE                                     
*                                                                               
VUPG8X   CLC   =C'BK',12(R4)                                                    
         BE    VUPG10                                                           
         CLC   =C'DT',12(R4)                                                    
         BE    VUPG20                                                           
         CLC   =C'ST',12(R4)                                                    
         BE    VUPG30                                                           
*                                                                               
         L     RF,ATWA                                                          
         LA    RF,CONHEAD-T217FFD(RF)   POINT TO MESSAGE AREA                   
         MVC   0(29,RF),=C'* ERROR * UNKNOWN INPUT FIELD'                       
         LLC   RE,0(R4)            GET LENGTH                                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   30(0,RF),12(R4) *EXECUTED*                                       
         GOTO1 ERREX2              HOPEFULLY, NEVER TO RETURN                   
         B     XIT                 ELSE GENCON WAS SLAVED                       
         EJECT                                                                  
* EDIT OVERRIDE FOR SHARE BOOK *                                                
         SPACE 1                                                                
VUPG10   MVI   ERROR,BOOKERR                                                    
         CLI   1(R4),0                                                          
         BE    TRAPERR                                                          
         XC    ELEM,ELEM                                                        
         LLC   RE,1(R4)                                                         
         STC   RE,ELEM+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),22(R4) *EXECUTED*                                      
         LA    RE,9(RE)                                                         
         STC   RE,ELEM                                                          
*                                                                               
         LLC   RE,ELEM+5           LENGTH OF BOOK(S) EXPRESSION                 
         LA    R1,ELEM+8           A(EXPRESSION)                                
VUPG15   CLI   0(R1),C'/'          ALTER SLASHES TO COMMAS IF THEY. . .         
         BNE   *+16                . . .SEPARATE BOOKS                          
         CLI   1(R1),C'0'                                                       
         BNL   *+8                                                              
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
         BCT   RE,VUPG15                                                        
*                                                                               
         GOTO1 BOOKVAL,(R1),(C'N',ELEM),(4,ELEM+128),SCANNER                    
         SR    RF,RF                                                            
         ICM   RF,1,4(R1)          NUMBER OF BOOKS                              
         BZ    TRAPERR                                                          
         TM    ELEM+128,X'BF'      ANY GARBAGE OPTIONS SPECIFIED?               
         BNZ   TRAPERR                                                          
         MVC   SPUPFBK,ELEM+129                                                 
         CLI   4(R1),1             2 OR MORE BOOKS?                             
         BNH   VUPG8               NO                                           
*                                                                               
         BCTR  RF,0                                                             
         LA    R1,SPUPFBKL         BOOK LIST                                    
         LA    RE,ELEM+131         A(2ND BOOK)                                  
VUPG17   TM    0(RE),X'BE'                                                      
         BNZ   TRAPERR                                                          
         MVC   0(2,R1),1(RE)       PUT BOOK IN LIST                             
         LA    R1,2(R1)                                                         
         LA    RE,3(RE)                                                         
         BCT   RF,VUPG17                                                        
         B     VUPG8                                                            
         SPACE 1                                                                
* EDIT OVERRIDE DAY/TIME *                                                      
         SPACE 1                                                                
VUPG20   MVI   ERROR,INVDAY                                                     
         LLC   R0,1(R4)            GET INPUT FIELD LEN                          
         LA    R5,22(R4)           POINT TO INPUT STRING                        
         SR    R6,R6               CLEAR COUNTER                                
VUPG22   CLI   0(R5),C'/'          FIND DELIMITER                               
         BE    VUPG22X                                                          
         LA    R5,1(R5)                                                         
         LA    R6,1(R6)                                                         
         BCT   R0,VUPG22                                                        
         B     TRAPERR                                                          
VUPG22X  DS    0H                                                               
         ST    R0,DUB              SAVE REMAINING LENGTH                        
         ST    R5,DUB+4            AND INPUT ADDRESS                            
*                                                                               
         GOTO1 DAYVAL,DMCB,((R6),22(R4)),WORK,WORK+1                            
         CLI   WORK,0                                                           
         BE    TRAPERR                                                          
         MVC   SPUPUDAY,WORK                                                    
* EDIT TIME *                                                                   
         MVI   ERROR,INVTIME                                                    
         L     R0,DUB              PICK UP REMAINING STRING LENGTH              
         BCTR  R0,0                ADJUST LENGTH FOR DELIMITER                  
         LTR   R0,R0                                                            
         BNP   TRAPERR                                                          
         L     R5,DUB+4            RESTORE INPUT POINTER                        
         LA    R5,1(R5)            ADJUST FOR DELIMITER                         
         GOTO1 TIMVAL,DMCB,((R0),(R5)),WORK                                     
         CLI   0(R1),X'FF'                                                      
         BE    TRAPERR                                                          
         MVC   SPUPUTIM,WORK                                                    
         B     VUPG8                                                            
         SPACE 1                                                                
* EDIT STATION OVERRIDE *                                                       
         SPACE 1                                                                
VUPG30   MVI   ERROR,INVSTAT                                                    
         CLI   1(R4),3                                                          
         BL    TRAPERR                                                          
         CLI   1(R4),4                                                          
         BH    TRAPERR                                                          
         MVC   SPUPSTA,22(R4)                                                   
         MVI   SPUPSTA+4,C'T'      FORCE MEDIA=T                                
         B     VUPG8                                                            
         DROP  RA                                                               
         EJECT                                                                  
* VCLEARF - CLEAR AND FOUT FIELDS                                               
*                                                                               
* ON ENTRY                                                                      
*        P1    BYTE 0    = 0 UNPROTECTED FIELDS                                 
*                        = 1 PROTECTED FIELDS                                   
*              BYTES 1-3 = A(START FIELD HEADER)                                
*        P2    BYTES 1-3 = A(END FIELD HEADER)                                  
*                                                                               
VCLEARF  LM    R2,R3,0(R1)                                                      
         SR    RE,RE                                                            
         LA    R4,X'10'            BRANCH CONDITION                             
         LA    R5,MOVESPA          CLEAR FIELD INSTRUCTION                      
         CLI   0(R1),0             TEST FOR UNPROTECTED FIELDS                  
         BE    *+12                YES                                          
         LA    R4,X'80'            SET BRANCH CONDITION AND CLEAR               
         LA    R5,ZEROFLD          INSTRUCTION FOR PROTECTED FIELDS             
         SPACE 1                                                                
VCLEARF2 IC    RE,0(R2)            LENGTH OF FIELD PLUS HEADER                  
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         EX    R4,TSTBRAN          BRANCH ACCORDINGLY                           
         LR    R1,RE                                                            
         SHI   R1,9                SET EXECUTE LENGTH FOR FIELD DATA            
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         SHI   R1,8                LESS 8 MORE FOR EXTENDED FIELD               
         EX    R1,0(R5)            CLEAR FIELD                                  
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         SPACE 1                                                                
VCLEARF4 LA    R2,0(RE,R2)                                                      
         CR    R2,R3               TEST IF END FIELD REACHED                    
         BL    VCLEARF2            NO-CONTINUE                                  
         B     XIT                 YES-ALL DONE                                 
         SPACE 2                                                                
MOVESPA  MVC   8(0,R2),SPACES                                                   
ZEROFLD  XC    8(0,R2),8(R2)                                                    
TSTBRAN  BC    0,VCLEARF4                                                       
         SPACE 5                                                                
* SUBROUTINE SETS A COMMA AFTER LAST DATA CHAR *                                
         SPACE 1                                                                
SETCOMMA CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
* VLOADAPP - LOADS NSID APPLICATION                                             
*                                                                               
*        P1    BYTE    0 = PHASE TO BE LOADED (X'0F',X'16',X'17')               
*              BYTES 1-3 = LOAD ADDRESS                                         
*                                                                               
VLOADAPP L     RA,ATWA                                                          
         USING T217FFD,RA                                                       
*                                                                               
         CLI   DMCB,X'0F'          TEST DETAIL LOAD                             
         BNE   VLAPP10                                                          
         MVI   DMCB,0                                                           
         MVC   DMCB+4(4),=X'D902170F'                                           
         GOTO1 CALLOV,DMCB         LOAD OVERLAY ON TOP OF NSID PROGRAM          
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CONREC(8),=C'DETAIL  '                                           
         OI    CONRECH+6,X'80'     XMIT                                         
         MVC   CONACT(8),=C'SELECT  '                                           
         OI    CONACTH+6,X'80'     XMIT                                         
*                                                                               
         L     RF,DMCB             A(OVERLAY)                                   
         GOTO1 (RF),DMCB,(RC)      GO TO OVERLAY                                
         B     VLAPPX                                                           
*                                                                               
VLAPP10  CLI   DMCB,X'16'          TEST COMPETITION LOAD                        
         BNE   VLAPP20                                                          
         MVI   DMCB,0                                                           
         MVC   DMCB+4(4),=X'D9021716'                                           
         GOTO1 CALLOV,DMCB         LOAD OVERLAY ON TOP OF NSID PROGRAM          
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CONREC(8),=C'COMPETE '                                           
         OI    CONRECH+6,X'80'     XMIT                                         
         MVC   CONACT(8),=C'SELECT  '                                           
         OI    CONACTH+6,X'80'     XMIT                                         
*                                                                               
         L     RF,DMCB             A(OVERLAY)                                   
         MVI   MODE,VALKEY                                                      
         GOTO1 (RF),DMCB,(RC)      VALIDATE COMPETITION KEY                     
         MVI   MODE,DISPREC                                                     
         GOTO1 (RF),DMCB,(RC)      DISPLAY COMPETITION SCREEN                   
         B     VLAPPX                                                           
*                                                                               
VLAPP20  CLI   DMCB,X'17'          TEST ESTIMATE LOAD                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DMCB,0                                                           
         MVC   DMCB+4(4),=X'D9021717'                                           
         GOTO1 CALLOV,DMCB         LOAD OVERLAY ON TOP OF NSID PROGRAM          
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CONREC(8),=C'CEST    '                                           
         OI    CONRECH+6,X'80'     XMIT                                         
         MVC   CONACT(8),=C'SELECT  '                                           
         OI    CONACTH+6,X'80'     XMIT                                         
*                                                                               
         L     RF,DMCB             A(OVERLAY)                                   
         MVI   MODE,VALKEY                                                      
         GOTO1 (RF),DMCB,(RC)      VALIDATE ESTIMATE KEY                        
         MVI   MODE,DISPREC                                                     
         GOTO1 (RF),DMCB,(RC)      DISPLAY ESTIMATE SCREEN                      
*                                                                               
VLAPPX   XC    DMCB(24),DMCB                                                    
         ST    RF,DMCB                                                          
         MVI   DMCB,0                                                           
         MVC   DMCB+4(4),=X'D902170E'                                           
         GOTO1 CALLOV,DMCB         RESTORE NSID PROGRAM                         
         CLI   DMCB+4,X'FF'                                                     
         BNE   XIT                                                              
         DC    H'0'                                                             
         DROP  RA                                                               
         EJECT                                                                  
PLSENTER OI    GENSTAT2,USGETTXT   PLEASE ENTER FIELDS AS REQUIRED              
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMSGNO,0                                                        
         MVI   GTMSGNO+1,2                                                      
         MVI   GTMTYP,C'I'                                                      
         MVI   GTMSYS,X'FF'                                                     
         DROP  RF                                                               
*                                                                               
         L     RA,ATWA                                                          
         USING T217FFD,RA                                                       
         LA    R2,CONRECH          CURSOR TO RECORD FIELD                       
         DROP  RA                                                               
         GOTO1 ERREX                                                            
         B     XIT                                                              
*                                                                               
PFERR    MVI   ERROR,ERINVPFK      INVALID PF KEY                               
TRAPERR  DS    0H                                                               
         TM    TRNSTAT,NOVALERR    NO ERREX ON VALI RTNS                        
         JZ    *+12                                                             
         OI    TRNSTAT,BASERR      SET FLAG FOR ERROR TO APPL                   
         J     XIT                                                              
         GOTO1 ERREX               HOPEFULLY, NEVER TO RETURN                   
         J     XIT                 ELSE GENCON WAS SLAVED                       
*                                                                               
DUMMYERR MVI   GOAGAIN,C'Y'        SET TO RETURN WITH NEW RECORD/ACTION         
DUMYERR1 L     RA,ATWA             CURSOR TO RECORD FIELD                       
         USING T217FFD,RA                                                       
         LA    R2,CONRECH          CURSOR TO RECORD FIELD                       
         GOTO1 ERREX2                                                           
         DROP  RA                                                               
*                                                                               
YES      SR    RC,RC               SET CC EQ                                    
NO       LTR   RC,RC               SET CC NEQ                                   
XIT      XIT1                                                                   
         SPACE 1                                                                
* IF SF USER PUTS  A SF NUMBER IN USEIONUM, SET THE                             
* CORRESPONDING SF IOAREA ADDRESS.                                              
* ELSE SET AIO1                                                                 
                                                                                
SETIONUM DS    0H                                                               
         MVC   AIO,AIO1                                                         
         CLI   USEIONUM,1                                                       
         BNH   SETIOX                                                           
         MVC   AIO,AIO2                                                         
         CLI   USEIONUM,2                                                       
         BE    SETIOX                                                           
         MVC   AIO,AIO3                                                         
         CLI   USEIONUM,3                                                       
         BE    SETIOX                                                           
         DC    H'0'                SONIA WAS HERE FIRST                         
SETIOX   BR    RE                                                               
         SPACE 1                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
         DROP  RB,R7                                                            
         EJECT                                                                  
* CONSTANTS TABLES, ETC *                                                       
         SPACE 1                                                                
ZEROES   DC    17C'0'                                                           
RELO     DS    A                                                                
         SPACE 1                                                                
SYSVCON  DS    0F                                                               
         DC    V(CLPACK)                                                        
         DC    V(CLUNPK)                                                        
         DC    A(0)                A(CORERES MSPACK) GOES HERE                  
         DC    A(0)                A(CORERES MSUNPK) GOES HERE                  
         DC    V(DUMMY)                                                         
         DC    V(MEDGET)                                                        
         DC    A(0)                A(LINKED RECUP GOES HERE)                    
         DC    V(BINSRCH)                                                       
         DC    V(DPTRD)                                                         
         DC    V(EQVRD)                                                         
         SPACE 1                                                                
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         SPACE 1                                                                
CORETAB  DS    0X                                                               
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QUPVAL)                                                      
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QRANSID)                                                     
         DC    AL1(QQSORT)                                                      
         DC    AL1(QSTAVAL)                                                     
CORES    EQU   (*-CORETAB)                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT GEN                                                              
*              DIRECTORY OF RECORDS AND ACTIONS                                 
         SPACE 1                                                                
RECACT   DS    0D                                                               
*                                                                               
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
*        GENSTAT7 = GES7DDS        XL1 STATUS BYTE  X'80'=DDS ONLY              
*                                                                               
*                                                                               
         DC    X'01',C'INCH    ',AL1(70),X'00C2',X'00'                          
         DC    X'01',C'BUCH    ',AL1(71),X'00C2',X'00'                          
         DC    X'01',C'DMTEST  ',AL1(111),X'00C2',X'80'                         
RECACT1A DC    X'01',C'*COMPETE',AL1(21),X'00C2',X'00'                          
         DC    X'01',C'*CEST   ',AL1(22),X'00C2',X'00'                          
RECACT1  DC    X'01',C'NVL     ',AL1(09),X'00C2',X'00'                          
         DC    X'01',C'ACNEQ   ',AL1(10),X'00C2',X'00'                          
         DC    X'01',C'SCHEME  ',AL1(12),X'00C2',X'00'                          
         DC    X'01',C'PERIOD  ',AL1(13),X'00C2',X'00'                          
         DC    X'01',C'STATION ',AL1(14),X'00C2',X'00'                          
         DC    X'01',C'*NSID   ',AL1(15),X'00C2',X'00'                          
         DC    X'01',C'*DETAIL ',AL1(16),X'00C2',X'00'                          
         DC    X'01',C'AOR     ',AL1(17),X'00C2',X'00'                          
         DC    X'01',C'MTR     ',AL1(18),X'00C2',X'00'                          
         DC    X'01',C'BHOLD   ',AL1(19),X'00C2',X'00'                          
         DC    X'01',C'BUY     ',AL1(20),X'00C2',X'00'                          
         DC    X'01',C'STALIST ',AL1(23),X'00C2',X'00'                          
         DC    X'01',C'ELIST   ',AL1(24),X'00C2',X'00'                          
         DC    X'01',C'PSR     ',AL1(25),X'00C2',X'00'                          
         DC    X'01',C'ESR     ',AL1(26),X'00C2',X'00'                          
* FOLLOWING DISABLED 21JAN98 MHER - NO LONGER EXISTS                            
**NOP**  DC    X'01',C'MKTCORR ',AL1(27),X'00C2' MARKET CORRESPONDENCE          
         DC    X'01',C'SPB     ',AL1(29),X'00C2',X'00'  SPLIT BILLING           
         DC    X'01',C'XRATE   ',AL1(30),X'00C2',X'00'  EXCHANGE RATE           
         DC    X'01',C'SGROUP  ',AL1(31),X'00C2',X'00'  STATN GROUP             
         DC    X'01',C'SGASSIGN',AL1(32),X'00C2',X'00'  STATN GRP ASSGN         
         DC    X'01',C'SGDEF   ',AL1(33),X'00C2',X'00'  STATN GRP DEF           
         DC    X'01',C'CGROUP  ',AL1(34),X'00C2',X'00'  CLT GROUP               
         DC    X'01',C'CGASSIGN',AL1(35),X'00C2',X'00'  CLT GRP ASSGN           
         DC    X'01',C'CGDEF   ',AL1(36),X'00C2',X'00'  CLT GRP DEF             
         DC    X'01',C'DPT     ',AL1(37),X'00C2',X'00'  DAYPT MENUS             
         DC    X'01',C'DAYPART ',AL1(37),X'00C2',X'00'                          
         DC    X'01',C'CLRST   ',AL1(39),X'00C2',X'00'  CLRST STATUS            
         DC    X'01',C'MASTER  ',AL1(40),X'00C2',X'00'                          
         DC    X'01',C'ADDRESS ',AL1(41),X'00C2',X'00'  ADDRESS                 
         DC    X'01',C'MARKET  ',AL1(42),X'00C2',X'00'                          
         DC    X'01',C'MKT     ',AL1(42),X'00C2',X'00'                          
         DC    X'01',C'REP     ',AL1(43),X'00C2',X'00'  REP                     
         DC    X'01',C'UDEF    ',AL1(44),X'00C2',X'00'  USER DEF                
         DC    X'01',C'CABLE   ',AL1(45),X'00C2',X'00' 'Y'-RECORDS              
         DC    X'01',C'REASON  ',AL1(46),X'00C2',X'00'                          
         DC    X'01',C'DEMOVER ',AL1(47),X'00C2',X'00'                          
         DC    X'01',C'DEMDEF  ',AL1(48),X'00C2',X'00'                          
         DC    X'01',C'DEMODEF ',AL1(48),X'00C2',X'00'                          
         DC    X'01',C'NETDEF  ',AL1(49),X'00C2',X'00'                          
         DC    X'01',C'CBLPRO  ',AL1(27),X'00C2',X'00'  CBL NWK PRO-CDN         
         DC    X'01',C'CBLMKT  ',AL1(28),X'00C2',X'00'  CBL NWK MKT-CND         
         DC    X'01',C'CBLDEF  ',AL1(28),X'00C2',X'00'  CBLMKT SYN -CDN         
*DISABLED 10DEC03 PWES - CONVERSIONS DONE, CODE STILL IN SFM5F IF NEED          
*10DEC03 DC    X'01',C'CBLCONV ',AL1(28),X'00C2' TEMP NWK->CBL CONV-CDN         
         DC    X'01',C'SHOWDEF ',AL1(50),X'00C2',X'00'                          
****     DC    X'01',C'BILLFORM',AL1(51),X'00C2',X'00'                          
         DC    X'01',C'INFO    ',AL1(52),X'00C2',X'00'  INFOMERCIAL             
         DC    X'01',C'INFB    ',AL1(53),X'00C2',X'00'  INFO BUY                
         DC    X'01',C'IB      ',AL1(53),X'00C2',X'00'                          
         DC    X'01',C'BGL     ',AL1(54),X'00C2',X'00'                          
         DC    X'01',C'INFC    ',AL1(55),X'00C2',X'00'  INFO BUY COUNTS         
         DC    X'01',C'IC      ',AL1(55),X'00C2',X'00'                          
         DC    X'01',C'PW      ',AL1(56),X'00C2',X'00'                          
         DC    X'01',C'A2COM   ',AL1(57),X'00C2',X'00'  A2 COMMENT              
         DC    X'01',C'A3COM   ',AL1(58),X'00C2',X'00'  A3 COMMENT              
         DC    X'01',C'BCOM    ',AL1(59),X'00C2',X'00'                          
         DC    X'01',C'RSCOM   ',AL1(60),X'00C2',X'00'                          
         DC    X'01',C'NVTEXT  ',AL1(61),X'00C2',X'00'                          
         DC    X'01',C'MCOM    ',AL1(62),X'00C2',X'00'                          
         DC    X'01',C'SDR     ',AL1(63),X'00C2',X'00'                          
         DC    X'01',C'B4COM   ',AL1(64),X'00C2',X'00'                          
         DC    X'01',C'B5COM   ',AL1(65),X'00C2',X'00'                          
         DC    X'01',C'B6COM   ',AL1(66),X'00C2',X'00'                          
         DC    X'01',C'B7COM   ',AL1(67),X'00C2',X'00'                          
         DC    X'01',C'CONTRCTR',AL1(68),X'00C2',X'00'                          
         DC    X'01',C'CTA     ',AL1(69),X'00C2',X'00'                          
         DC    X'01',C'BUYGRP  ',AL1(72),X'00C2',X'00'  BUY GROUP RECS          
         DC    X'01',C'BUYER   ',AL1(73),X'00C2',X'00'  BUYER RECORDS           
         DC    X'01',C'BYR     ',AL1(73),X'00C2',X'00'  BUYER RECORDS           
         DC    X'01',C'SUPV    ',AL1(74),X'00C2',X'00'  SUPERVISOR RECS         
         DC    X'01',C'SUPERV  ',AL1(74),X'00C2',X'00'  SUPERVISOR RECS         
         DC    X'01',C'PGROUP  ',AL1(76),X'00C2',X'00'  PRD GRP RECS            
         DC    X'01',C'PGRDEF  ',AL1(75),X'00C2',X'00'  PRD GRP DEF             
         DC    X'01',C'MGROUP  ',AL1(78),X'00C2',X'00'  MKT GROUP               
         DC    X'01',C'MGRDEF  ',AL1(77),X'00C2',X'00'  MKT GRP DEF             
* THIS ENTRY IS NOT USED MUCH SO LET MGROUP GO FIRST !  MHER 11NOV97            
         DC    X'01',C'MGREQ   ',AL1(11),X'00C2',X'00'                          
         DC    X'01',C'SPILLDE ',AL1(79),X'00C2',X'00'  SPILL DEF               
         DC    X'01',C'SPLDEF  ',AL1(79),X'00C2',X'00'  SPILL DEF               
         DC    X'01',C'STLOCK  ',AL1(80),X'00C2',X'00'  STATION LOCKIN          
         DC    X'01',C'STALOCK ',AL1(80),X'00C2',X'00'  STATION LOCKIN          
         DC    X'01',C'FAUTH   ',AL1(81),X'00C2',X'80'  AUTHORIZATION           
         DC    X'01',C'SCOM    ',AL1(82),X'00C2',X'00'  SCOM                    
         DC    X'01',C'AUTOPAY ',AL1(83),X'00C2',X'80'  AUTOPAY RECS            
         DC    X'01',C'PXC     ',AL1(84),X'00C2',X'00'  PRD EXCLUSION           
         DC    X'01',C'CLT     ',AL1(85),X'00C2',X'00'  CLIENT RECORD           
         DC    X'01',C'CLIENT  ',AL1(85),X'00C2',X'00'  CLIENT RECORD           
         DC    X'01',C'PRD     ',AL1(86),X'00C2',X'00'  PRODUCT RECORD          
         DC    X'01',C'PRODUCT ',AL1(86),X'00C2',X'00'  PRODUCT RECORD          
         DC    X'01',C'CL2     ',AL1(87),X'00C2',X'00'  CLIENT2 RECORD          
         DC    X'01',C'CLIENT2 ',AL1(87),X'00C2',X'00'  CLIENT2 RECORD          
         DC    X'01',C'ESTIMATE',AL1(88),X'00C2',X'00'  ESTIMATE RECORD         
         DC    X'01',C'ESTDOLS ',AL1(89),X'00C2',X'00'  ESTIMATE DOLLAR         
         DC    X'01',C'EST$    ',AL1(89),X'00C2',X'00'  ESTIMATE DOLLAR         
         DC    X'01',C'AUTHDOL ',AL1(89),X'00C2',X'00'  ESTIMATE DOLLAR         
         DC    X'01',C'SQAD    ',AL1(90),X'00C2',X'00'  SQAD DAYPT EQV          
         DC    X'01',C'ESTLIST ',AL1(24),X'00C2',X'00'                          
         DC    X'01',C'I2COM   ',AL1(91),X'00C2',X'00'  I2 COMMENT              
         DC    X'01',C'NEWCOMP ',AL1(92),X'00C2',X'00'  NEW COMPETE             
         DC    X'01',C'C2      ',AL1(93),X'00C2',X'00'  COST2                   
         DC    X'01',C'UCOMM   ',AL1(94),X'00C2',X'00'  USER COMMENTS           
         DC    X'01',C'MASTER2 ',AL1(95),X'00C2',X'00'  MASTER REC 2            
         DC    X'01',C'M2      ',AL1(95),X'00C2',X'00'  MASTER REC 2            
         DC    X'01',C'MAS2    ',AL1(95),X'00C2',X'00'  MASTER REC 2            
         DC    X'01',C'RCODE   ',AL1(96),X'00C2',X'00'  REASON CODES            
         DC    X'01',C'PURPOSE ',AL1(97),X'00C2',X'00'  PURPOSE CODES           
         DC    X'01',C'PCOM    ',AL1(98),X'00C2',X'00'  PARAMOUNT COMMS         
         DC    X'01',C'PGEST   ',AL1(99),X'00C2',X'00'  PGEST                   
         DC    X'01',C'AGENCY  ',AL1(100),X'00C2',X'80'  AGENCY                 
         DC    X'01',C'AGY     ',AL1(100),X'00C2',X'80'  AGENCY                 
         DC    X'01',C'SPLIT   ',AL1(101),X'00C2',X'00'  SPLIT                  
         DC    X'01',C'MENU    ',AL1(102),X'00C2',X'00'  MENU                   
         DC    X'01',C'EQU     ',AL1(103),X'00C2',X'00'  EQUHDR                 
         DC    X'01',C'OCOM    ',AL1(104),X'00C2',X'00'  O COMMENTS             
         DC    X'01',C'DSTN    ',AL1(105),X'00C2',X'00'  DESTINE                
         DC    X'01',C'DESTINE ',AL1(105),X'00C2',X'00'  DESTINE                
         DC    X'01',C'GFEST   ',AL1(106),X'00C2',X'00'  GFEST                  
         DC    X'01',C'MKTFIX  ',AL1(107),X'00C2',X'00'  MKTFIX                 
         DC    X'01',C'SDUDEF  ',AL1(108),X'00C2',X'00'  SDUDEF                 
         DC    X'01',C'STAFIX  ',AL1(109),X'00C2',X'00'  STAFIX                 
         DC    X'01',C'BFORM   ',AL1(110),X'00C2',X'00'  NEW BILLFORM           
         DC    X'01',C'EQLEN   ',AL1(112),X'00C2',X'00'  EQLEN                  
         DC    X'01',C'BPCT    ',AL1(113),X'00C2',X'00'  BILLING PCT            
         DC    X'01',C'DEAL    ',AL1(114),X'00C2',X'00'  DEAL                   
         DC    X'01',C'ACOM    ',AL1(115),X'00C2',X'00'  ACOM                   
         DC    X'01',C'OMBUYER ',AL1(116),X'00C2',X'00'  OMBUYER                
         DC    X'01',C'COMMENT ',AL1(117),X'00C2',X'00'  COMMENT                
         DC    X'01',C'FLIGHT  ',AL1(118),X'00C2',X'00'  FLIGHT                 
         DC    X'01',C'BYRMOVE ',AL1(119),X'00C2',X'00'  BYRMOVE                
         DC    X'01',C'XAUTOPAY',AL1(120),X'00C2',X'80'  AUTOPAY RECS           
         DC    X'01',C'BILLADR ',AL1(121),X'00C2',X'00'  BILL ADDRESS           
         DC    X'01',C'CLFRZ   ',AL1(122),X'00C2',X'00'  CLFRZ                  
*        DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
*                                  CL1 SPARE                                    
         SPACE 1                                                                
         DC    X'02',C'ADD     ',AL1(01,01,00),X'00'                            
         DC    X'02',C'CHANGE  ',AL1(02,02,00),X'00'                            
         DC    X'02',C'DISPLAY ',AL1(03,03,00),X'00'                            
         DC    X'02',C'DELETE  ',AL1(04,04,00),X'00'                            
         DC    X'02',C'SELECT  ',AL1(05,05,00),X'00'                            
         DC    X'02',C'RESTORE ',AL1(06,06,00),X'00'                            
         DC    X'02',C'COPY    ',AL1(07,07,00),X'00'                            
         DC    X'02',C'RANK    ',AL1(08,08,00),X'00'                            
         DC    X'02',C'LIST    ',AL1(10,10,00),X'00'                            
         DC    X'02',C'REPORT  ',AL1(12,12,00),X'00'                            
         DC    X'02',C'TRANSFER',AL1(13,13,00),X'00'                            
         DC    X'02',C'PRINT   ',AL1(14,14,00),X'00'                            
         DC    X'02',C'PCOPY   ',AL1(15,15,00),X'00'                            
         DC    X'02',C'MAINTAIN',AL1(16,16,00),X'00'                            
         DC    X'02',C'BILL    ',AL1(17,17,00),X'00'                            
         DC    X'02',C'UNLOCK  ',AL1(18,18,00),X'00'                            
         DC    X'02',C'LOCK    ',AL1(19,19,00),X'00'                            
         DC    X'02',C'USAGE   ',AL1(20,20,00),X'00'                            
         DC    X'02',C'BLIST   ',AL1(10,21,00),X'00'                            
         DC    X'02',C'PREVIEW ',AL1(22,22,00),X'00'                            
         DC    X'02',C'MOVE    ',AL1(23,23,00),X'00'                            
         DC    X'02',C'HELP    ',AL1(00,00,00),X'00'                            
         EJECT                                                                  
* TABLE OF USED PHASES -- INDEXED BY PHASE NUMBER                               
* A NON-BLANK ENTRY INDICATES C'P' FOR PROGRAM, C'S' FOR SCREEN                 
*RECTAB          **** T217 ******                                               
*                0123456789ABCDEF                                               
*                PPPPPPPPPPPPPPPP      00-0F                                    
*                PPPPPPPPPPPPPPPP      10-1F                                    
*                PPPPPPPPPPPPPPPP      20-2F                                    
*                PPPPPPPPPPPPPPPP      30-3F                                    
*                PPPPPPPPPPPPPPPP      40-4F                                    
*                PPPPPPPPPPPPPPPP      50-5F                                    
*                PPPPP***PPPPPPP*      60-6F                                    
*                PPPPPPP PPP           70-7F                                    
*                  PPP                 80-8F                                    
*                  PP  P               90-9F                                    
*                                      A0-AF                                    
*                                      B0-BF                                    
*                                      C0-CF                                    
*                                      D0-DF                                    
*                                      E0-EF                                    
*                                      F0-FF                                    
*                                                                               
*                                                                               
*                **** T297 ******   NOTE SCREENS WITH A C ARE SCSFM             
*                0123456789ABCDEF                                               
*                                      00-0F                                    
*                                      10-1F                                    
*                         CCCCCCC      20-2F                                    
*                CCCCCCCCCCCCCCCC      30-3F                                    
*                CCCCCCCCCCCCCCCC      40-4F                                    
*                CCCCCCCCCCCCCCCC      50-5F                                    
*                CCCCCSSSCCSCCCCS      60-6F                                    
*                CCCCCCCCCCCCCCCC      70-7F                                    
*                CCCCCCCCCCCCCCCC      80-8F                                    
*                CCCCCCCCCCCCCCCS      90-9F                                    
*                SSSSSSSSSSSSSSSS      A0-AF                                    
*                SSSSSSSSSSSSSSSS      B0-BF                                    
*                SSCSSSSSSSSSCSSS      C0-CF                                    
*                SSSSSSSSSCSSSSSS      D0-DF                                    
*                SSSSSSSSSSSSSCSS      E0-EF                                    
*                SSSSCCSSSSSSSSSS      F0-FF                                    
*                0123456789ABCDEF                                               
*                                                                               
*        DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                         
*                                                                               
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
*        GENSTAT7 = GES7DDS        XL1 STATUS BYTE  X'80'=DDS ONLY              
*                                                                               
*                                                      NVL RECORD               
         DC    X'03',AL1(09,01),X'F9090000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(09,02),X'F9090000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(09,03),X'F9090000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(09,04),X'F9090000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(09,05),X'F9090000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(09,06),X'F9090000C0',C'    ',X'00'     RESTORE         
*                                                      ACNEQ REC                
         DC    X'03',AL1(10,01),X'FA0A0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(10,02),X'FA0A0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(10,03),X'FA0A0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(10,04),X'FA0A0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(10,05),X'FA0A0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(10,06),X'FA0A0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(10,10),X'EA0A000AF8',C'AEAE',X'00'     LIST            
*                                                      MGREQ REC                
         DC    X'03',AL1(11,01),X'FA0A0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(11,02),X'FA0A0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(11,03),X'FA0A0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(11,04),X'FA0A0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(11,05),X'FA0A0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(11,06),X'FA0A0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(11,10),X'EA0A000AF8',C'MEME',X'00'     LIST            
*                                                      SCHEME REC               
         DC    X'03',AL1(12,01),X'BB0B0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(12,02),X'BB0B0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(12,03),X'BB0B0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(12,04),X'BB0B0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(12,05),X'BB0B0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(12,06),X'BB0B0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(12,10),X'AB0B000080',C'    ',X'00'     LIST            
         DC    X'03',AL1(12,12),X'CB0B000058',C'SCSF',X'00'     REPORT          
*                                                      PERIOD REC               
         DC    X'03',AL1(13,01),X'BC0C0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(13,02),X'BC0C0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(13,03),X'BC0C0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(13,04),X'BC0C0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(13,05),X'BC0C0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(13,06),X'BC0C0000C0',C'    ',X'00'     RESTORE         
*                                                      STATION                  
         DC    X'03',AL1(14,01),X'BD0D0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(14,02),X'BD0D0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(14,03),X'BD0D0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(14,04),X'BD0D0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(14,05),X'BD0D0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(14,06),X'BD0D0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(14,10),X'AD0D000080',C'    ',X'00'     LIST            
         DC    X'03',AL1(14,12),X'CD0D000058',C'STSF',X'00'     REPORT          
*                                                      NSID REC                 
         DC    X'03',AL1(15,01),X'BE0E0000C1',C'    ',X'00'     ADD             
         DC    X'03',AL1(15,02),X'BE0E0000C1',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(15,03),X'BE0E0000C1',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(15,04),X'BE0E0000C1',C'    ',X'00'     DELETE          
         DC    X'03',AL1(15,05),X'BE0E0000C1',C'    ',X'00'     SELECT          
         DC    X'03',AL1(15,06),X'BE0E0000C1',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(15,07),X'BE0E0000C1',C'    ',X'00'     COPY            
         DC    X'03',AL1(15,08),X'B8180000C1',C'    ',X'00'     RANK            
         DC    X'03',AL1(15,10),X'AE0E0000C1',C'    ',X'00'     LIST            
         DC    X'03',AL1(15,12),X'CE1E001E58',C'DTSF',X'00'     REPORT          
         DC    X'03',AL1(15,14),X'B818001878',C'RARA',X'00'     PRINT           
         DC    X'03',AL1(15,15),X'AC12001298',C'PCSN',X'00'     PCOPY           
*                                                      DETAIL REC               
         DC    X'03',AL1(16,01),X'BF0F0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(16,02),X'BF0F0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(16,03),X'BF0F0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(16,04),X'BF0F0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(16,05),X'BF0F0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(16,06),X'BF0F0000C0',C'    ',X'00'     RESTORE         
*                                                      AOR REC                  
         DC    X'03',AL1(17,01),X'C1110000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(17,02),X'C1110000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(17,03),X'C1110000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(17,04),X'C1110000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(17,05),X'C1110000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(17,06),X'C1110000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(17,10),X'D1110000C0',C'AOSF',X'00'     LIST            
         DC    X'03',AL1(17,12),X'D111000058',C'AOSF',X'00'     REPORT          
*                                                      MTR REC                  
         DC    X'03',AL1(18,01),X'B3130000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(18,02),X'B3130000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(18,03),X'B3130000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(18,04),X'B3130000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(18,05),X'B3130000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(18,06),X'B3130000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(18,10),X'A3130000C0',C'    ',X'00'     LIST            
         DC    X'03',AL1(18,12),X'C313000058',C'MTSF',X'00'     REPORT          
*                                                      BHOLD REC                
         DC    X'03',AL1(19,01),X'56610000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(19,02),X'56610000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(19,03),X'56610000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(19,04),X'56610000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(19,05),X'56610000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(19,06),X'56610000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(19,10),X'57610000C0',C'    ',X'00'     LIST            
*                                                      BUY REC                  
         DC    X'03',AL1(20,13),X'C020002018',C'BXBX',X'00'     TRANSFR         
         DC    X'03',AL1(20,23),X'2A78007838',C'PVPV',X'00'     MOVE            
*                                                      COMPETE REC              
         DC    X'03',AL1(21,01),X'B6160000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(21,02),X'B6160000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(21,03),X'B6160000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(21,04),X'B6160000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(21,05),X'B6160000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(21,06),X'B6160000C0',C'    ',X'00'     RESTORE         
*                                                      CESTIMATE REC            
         DC    X'03',AL1(22,01),X'B7170000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(22,02),X'B7170000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(22,03),X'B7170000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(22,04),X'B7170000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(22,05),X'B7170000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(22,06),X'B7170000C0',C'    ',X'00'     RESTORE         
*                                                      STALIST REC              
         DC    X'03',AL1(23,01),X'CA1A0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(23,02),X'CA1A0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(23,03),X'CA1A0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(23,04),X'CA1A0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(23,05),X'CA1A0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(23,06),X'CA1A0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(23,10),X'DA1A0000C0',C'    ',X'00'     LIST            
         DC    X'03',AL1(23,12),X'DA1A001A58',C'SLSF',X'00'     REPORT          
*                                                      ESTLIST REC              
         DC    X'03',AL1(24,12),X'DB1B001BE0',C'EHSF',X'00'     REPORT          
*                                                      PSR REC                  
         DC    X'03',AL1(25,01),X'DC1C0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(25,02),X'DC1C0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(25,03),X'DC1C0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(25,04),X'DC1C0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(25,05),X'DC1C0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(25,06),X'DC1C0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(25,10),X'DD1C0000C0',C'    ',X'00'     LIST            
*                                                      ESR REC                  
         DC    X'03',AL1(26,01),X'DE1D0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(26,02),X'DE1D0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(26,03),X'DE1D0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(26,04),X'DE1D0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(26,05),X'DE1D0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(26,06),X'DE1D0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(26,10),X'EE1D0000C0',C'    ',X'00'     LIST            
*                                                      MKTCORR                  
**NOP**  DC    X'03',AL1(27,01),X'E1210000C0',C'    ',X'00'     ADD             
**NOP**  DC    X'03',AL1(27,02),X'E1210000C0',C'    ',X'00'     CHANGE          
**NOP**  DC    X'03',AL1(27,03),X'E1210000C0',C'    ',X'00'     DISPLAY         
**NOP**  DC    X'03',AL1(27,04),X'E1210000C0',C'    ',X'00'     DELETE          
**NOP**  DC    X'03',AL1(27,05),X'E1210000C0',C'    ',X'00'     SELECT          
**NOP**  DC    X'03',AL1(27,06),X'E1210000C0',C'    ',X'00'     RESTORE         
**NOP**  DC    X'03',AL1(27,10),X'E3210000C0',C'    ',X'00'     LIST            
*                                                      SPB REC                  
         DC    X'03',AL1(29,01),X'E5230000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(29,02),X'E5230000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(29,03),X'E5230000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(29,04),X'E5230000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(29,05),X'E5230000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(29,06),X'E5230000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(29,10),X'D5230000C0',C'    ',X'00'     LIST            
         DC    X'03',AL1(29,12),X'D623000058',C'SBSF',X'00'     REPORT          
*                                                      XRATE REC                
         DC    X'03',AL1(30,01),X'FB240000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(30,02),X'FB240000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(30,03),X'FB240000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(30,04),X'FB240000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(30,05),X'FB240000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(30,06),X'FB240000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(30,10),X'EB240000C0',C'    ',X'00'     LIST            
*                                                      SGROUP REC               
         DC    X'03',AL1(31,01),X'C6260000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(31,02),X'C6260000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(31,03),X'C6260000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(31,04),X'C6260000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(31,05),X'C6260000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(31,06),X'C6260000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(31,10),X'B426000080',C'    ',X'00'     LIST            
         DC    X'03',AL1(31,12),X'B926000038',C'SGSF',X'00'     REPORT          
*                                                      SGASSIGN REC             
         DC    X'03',AL1(32,01),X'C8270000C1',C'    ',X'00'     ADD             
         DC    X'03',AL1(32,02),X'C8270000C1',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(32,03),X'C8270000C1',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(32,04),X'C8270000C1',C'    ',X'00'     DELETE          
         DC    X'03',AL1(32,05),X'C8270000C1',C'    ',X'00'     SELECT          
         DC    X'03',AL1(32,06),X'C8270000C1',C'    ',X'00'     RESTORE         
*                                                      SGDEF REC                
         DC    X'03',AL1(33,01),X'C5250000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(33,02),X'C5250000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(33,03),X'C5250000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(33,04),X'C5250000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(33,05),X'C5250000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(33,06),X'C5250000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(33,10),X'B5250000C0',C'    ',X'00'     LIST            
*                                                      CGROUP REC               
         DC    X'03',AL1(34,01),X'C7260000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(34,02),X'C7260000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(34,03),X'C7260000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(34,04),X'C7260000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(34,05),X'C7260000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(34,06),X'C7260000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(34,10),X'B426000080',C'    ',X'00'     LIST            
         DC    X'03',AL1(34,12),X'B926000038',C'CGSF',X'00'     REPORT          
*                                                      CGASSIGN REC             
         DC    X'03',AL1(35,01),X'C9270000C1',C'    ',X'00'     ADD             
         DC    X'03',AL1(35,02),X'C9270000C1',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(35,03),X'C9270000C1',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(35,04),X'C9270000C1',C'    ',X'00'     DELETE          
         DC    X'03',AL1(35,05),X'C9270000C1',C'    ',X'00'     SELECT          
         DC    X'03',AL1(35,06),X'C9270000C1',C'    ',X'00'     RESTORE         
*                                                      CGDEF REC                
         DC    X'03',AL1(36,01),X'C5250000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(36,02),X'C5250000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(36,03),X'C5250000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(36,04),X'C5250000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(36,05),X'C5250000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(36,06),X'C5250000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(36,10),X'B5250000C0',C'    ',X'00'     LIST            
*                                                      CLRST REC                
         DC    X'03',AL1(39,10),X'CC2A0000C0',C'    ',X'00'     LIST            
         DC    X'03',AL1(39,12),X'CC2A002A40',C'    ',X'00'     REPORT          
*                                                      MASTER REC               
         DC    X'03',AL1(40,01),X'D4140000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(40,02),X'D4140000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(40,03),X'D4140000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(40,05),X'D4140000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(40,10),X'EC390000F8',C'MSMS',X'00'     LIST            
*                                                      ADDRESS REC              
         DC    X'03',AL1(41,01),X'B2150000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(41,02),X'B2150000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(41,03),X'B2150000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(41,05),X'B2150000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(41,10),X'A2320000F8',C'ADAD',X'00'     LIST            
*                                                      MARKET REC               
         DC    X'03',AL1(42,01),X'D2340000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(42,02),X'D2340000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(42,03),X'D2340000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(42,05),X'D2340000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(42,10),X'D3330000F8',C'MTMT',X'00'     LIST            
*                                                      REP REC                  
         DC    X'03',AL1(43,01),X'D7370000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(43,02),X'D7370000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(43,03),X'D7370000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(43,05),X'D7370000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(43,10),X'D8380000F8',C'RLRL',X'00'     LIST            
*                                                      UDEF REC                 
         DC    X'03',AL1(44,02),X'B0350000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(44,03),X'B0350000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(44,05),X'B0350000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(44,10),X'B1360000C0',C'    ',X'00'     LIST            
*                                                      CABLE REC                
         DC    X'03',AL1(45,02),X'6F690000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(45,03),X'6F690000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(45,10),X'BA3A0000F8',C'CACA',X'00'     LIST            
*                                                      REASON REC               
         DC    X'03',AL1(46,01),X'E0100000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(46,02),X'E0100000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(46,03),X'E0100000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(46,04),X'E0100000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(46,05),X'E0100000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(46,06),X'E0100000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(46,10),X'A0300000F8',C'RSRS',X'00'     LIST            
*                                                      DEMOVER REC              
         DC    X'03',AL1(47,01),X'A12B0000C1',C'    ',X'00'     ADD             
         DC    X'03',AL1(47,02),X'A12B0000C1',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(47,03),X'A12B0000C0',C'    ',X'00'     DISPLAY         
*        DC    X'03',AL1(47,04),X'A12B0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(47,05),X'A12B0000C0',C'    ',X'00'     SELECT          
*        DC    X'03',AL1(47,06),X'A12B0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(47,10),X'A42B0000C0',C'    ',X'00'     LIST            
*                                                      DEMODEF REC              
         DC    X'03',AL1(48,01),X'A72C0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(48,02),X'A72C0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(48,03),X'A72C0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(48,07),X'EF2C0000C1',C'    ',X'00'     COPY            
         DC    X'03',AL1(48,04),X'A72C0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(48,05),X'A72C0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(48,06),X'A72C0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(48,10),X'AA2C0000C0',C'    ',X'00'     LIST            
*                                                      NETWORK REC              
         DC    X'03',AL1(49,01),X'AF2D0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(49,02),X'AF2D0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(49,03),X'AF2D0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(49,05),X'AF2D0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(49,10),X'D02D000080',C'    ',X'00'     LIST            
         DC    X'03',AL1(49,12),X'AF2D0000C0',C'4444',X'00'     REPORT          
*                                                                               
* NOTE: TEMP RECTYPE OF CBLDEF ALSO USES THE CBLMKT CODE                        
*                                                      CBLMKT REC               
         DC    X'03',AL1(28,01),X'535F0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(28,02),X'535F0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(28,03),X'535F0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(28,05),X'535F0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(28,07),X'535F0000C0',C'    ',X'00'     COPY            
         DC    X'03',AL1(28,10),X'525F000080',C'    ',X'00'     LIST            
         DC    X'03',AL1(28,12),X'535F0000C0',C'4444',X'00'     REPORT          
*                                                      CBLPRO REC               
         DC    X'03',AL1(27,01),X'55600000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(27,02),X'55600000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(27,03),X'55600000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(27,05),X'55600000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(27,07),X'55600000C0',C'    ',X'00'     COPY            
         DC    X'03',AL1(27,10),X'5460000080',C'    ',X'00'     LIST            
         DC    X'03',AL1(27,12),X'55600000C0',C'4444',X'00'     REPORT          
*                                                      SHOWDEF REC              
         DC    X'03',AL1(50,01),X'F02E0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(50,02),X'F02E0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(50,03),X'F02E0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(50,04),X'F02E0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(50,05),X'F02E0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(50,06),X'F02E0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(50,10),X'F12E0000C0',C'    ',X'00'     LIST            
*                                                      BILLFORM REC             
*****    DC    X'03',AL1(51,16),X'9F3B000081',C'    ',X'00'     MAINT           
*                                                      INFO REC                 
         DC    X'03',AL1(52,01),X'9E2F0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(52,02),X'9E2F0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(52,03),X'9E2F0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(52,04),X'9E2F0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(52,05),X'9E2F0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(52,06),X'9E2F0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(52,10),X'9D3F0000C0',C'    ',X'00'     LIST            
*                                                      INFOBUY REC              
         DC    X'03',AL1(53,02),X'9C3C0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(53,03),X'9C3C0000C0',C'    ',X'00'     DISPLAY         
*                                                      BUYING RULES REC         
         DC    X'03',AL1(54,01),X'89490000C0',C'    ',X'00'                     
         DC    X'03',AL1(54,02),X'89490000C0',C'    ',X'00'                     
         DC    X'03',AL1(54,03),X'89490000C0',C'    ',X'00'                     
         DC    X'03',AL1(54,04),X'89490000C0',C'    ',X'00'                     
         DC    X'03',AL1(54,05),X'89490000C0',C'    ',X'00'                     
         DC    X'03',AL1(54,06),X'89490000C0',C'    ',X'00'                     
         DC    X'03',AL1(54,10),X'87490000C0',C'    ',X'00'                     
         DC    X'03',AL1(54,12),X'87490000F8',C'RUSF',X'00'                     
*                                                      INFOCNT REC              
         DC    X'03',AL1(55,02),X'9B3D0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(55,03),X'9B3D0000C0',C'    ',X'00'     DISPLAY         
*                                                      PW REC                   
         DC    X'03',AL1(56,10),X'9942000080',C'    ',X'00'     LIST            
         DC    X'03',AL1(56,05),X'9A3E000080',C'    ',X'00'     SELECT          
         DC    X'03',AL1(56,16),X'9A3E000099',C'SLLK',X'00'     MAINT           
         DC    X'03',AL1(56,17),X'9843000081',C'    ',X'00'     BILL            
*^^NOP   DC    X'03',AL1(56,18),X'9744000081',C'    ',X'00'     UNLOCK          
*^^NOP   DC    X'03',AL1(56,19),X'9744000081',C'    ',X'00'     LOCK            
         DC    X'03',AL1(56,21),X'9942000080',C'    ',X'00'     BLIST           
*                                                      A2COM REC                
         DC    X'03',AL1(57,01),X'95450000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(57,02),X'95450000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(57,03),X'95450000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(57,04),X'95450000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(57,05),X'95450000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(57,06),X'95450000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(57,10),X'96460000C0',C'    ',X'00'     LIST            
*                                                      A3COM REC                
         DC    X'03',AL1(58,01),X'95450000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(58,02),X'95450000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(58,03),X'95450000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(58,04),X'95450000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(58,05),X'95450000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(58,06),X'95450000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(58,10),X'96460000C0',C'    ',X'00'     LIST            
*                                                      BCOM REC                 
         DC    X'03',AL1(59,01),X'90450000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(59,02),X'90450000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(59,03),X'90450000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(59,04),X'90450000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(59,05),X'90450000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(59,06),X'90450000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(59,10),X'83460000C0',C'    ',X'00'     LIST            
*                                                      RSCOM REC                
         DC    X'03',AL1(60,01),X'95450000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(60,02),X'95450000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(60,03),X'95450000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(60,04),X'95450000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(60,05),X'95450000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(60,06),X'95450000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(60,10),X'96460000C0',C'    ',X'00'     LIST            
*                                                      NVTEXT REC               
         DC    X'03',AL1(61,01),X'94450000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(61,02),X'94450000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(61,03),X'94450000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(61,04),X'94450000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(61,05),X'94450000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(61,06),X'94450000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(61,10),X'84460000C0',C'    ',X'00'     LIST            
*                                                      MCOM REC                 
         DC    X'03',AL1(62,01),X'93450000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(62,02),X'93450000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(62,03),X'93450000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(62,04),X'93450000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(62,05),X'93450000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(62,06),X'93450000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(62,10),X'83460000C0',C'    ',X'00'     LIST            
*                                                      SDR REC                  
         DC    X'03',AL1(63,01),X'92450000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(63,02),X'92450000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(63,03),X'92450000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(63,04),X'92450000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(63,05),X'92450000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(63,06),X'92450000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(63,10),X'82460000C0',C'    ',X'00'     LIST            
*                                                      B4COM REC                
         DC    X'03',AL1(64,01),X'90450000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(64,02),X'90450000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(64,03),X'90450000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(64,04),X'90450000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(64,05),X'90450000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(64,06),X'90450000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(64,10),X'83460000C0',C'    ',X'00'     LIST            
*                                                      B5COM REC                
         DC    X'03',AL1(65,01),X'90450000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(65,02),X'90450000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(65,03),X'90450000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(65,04),X'90450000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(65,05),X'90450000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(65,06),X'90450000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(65,10),X'83460000C0',C'    ',X'00'     LIST            
*                                                      B6COM                    
         DC    X'03',AL1(66,01),X'90450000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(66,02),X'90450000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(66,03),X'90450000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(66,04),X'90450000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(66,05),X'90450000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(66,06),X'90450000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(66,10),X'83460000C0',C'    ',X'00'     LIST            
*                                                      B7COM                    
         DC    X'03',AL1(67,01),X'90450000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(67,02),X'90450000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(67,03),X'90450000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(67,04),X'90450000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(67,05),X'90450000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(67,06),X'90450000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(67,10),X'83460000C0',C'    ',X'00'     LIST            
*                                                      CONTRCTR REC             
         DC    X'03',AL1(68,01),X'85050000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(68,02),X'85050000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(68,03),X'85050000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(68,04),X'85050000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(68,05),X'85050000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(68,06),X'85050000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(68,10),X'81050000C0',C'    ',X'00'     LIST            
*                                                      CTA REC                  
         DC    X'03',AL1(69,05),X'8619000080',C'    ',X'00'     SELECT          
         DC    X'03',AL1(69,10),X'8019000080',C'    ',X'00'     LIST            
         DC    X'03',AL1(69,16),X'8619000081',C'    ',X'00'     MAINT           
         DC    X'03',AL1(69,20),X'D919000081',C'    ',X'00'     USAGE           
*                                                      INCH REC                 
         DC    X'03',AL1(70,03),X'7C47000080',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(70,04),X'7C470000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(70,05),X'7C47000080',C'    ',X'00'     SELECT          
         DC    X'03',AL1(70,06),X'7C470000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(70,10),X'7D47000080',C'    ',X'00'     LIST            
*                                                      BUCH REC                 
         DC    X'03',AL1(71,03),X'7E48000080',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(71,04),X'7E480000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(71,05),X'7E48000080',C'    ',X'00'     SELECT          
         DC    X'03',AL1(71,06),X'7E480000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(71,10),X'7F48000080',C'    ',X'00'     LIST            
*                                                      BUYGRP REC               
         DC    X'03',AL1(72,01),X'E64A0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(72,02),X'E64A0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(72,03),X'E64A0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(72,04),X'E64A0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(72,05),X'E64A0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(72,06),X'E64A0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(72,10),X'F64A0000C0',C'    ',X'00'     LIST            
*                                                      SUPV REC                 
         DC    X'03',AL1(74,01),X'E74B0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(74,02),X'E74B0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(74,03),X'E74B0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(74,04),X'E74B0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(74,05),X'E74B0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(74,06),X'E74B0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(74,10),X'F74B0000C0',C'    ',X'00'     LIST            
         DC    X'03',AL1(74,12),X'5F4B000038',C'SVSF',X'00'     REPORT          
*                                                      BUYER REC                
         DC    X'03',AL1(73,01),X'E84C0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(73,02),X'E84C0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(73,03),X'E84C0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(73,04),X'E84C0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(73,05),X'E84C0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(73,06),X'E84C0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(73,10),X'F84C0000C0',C'    ',X'00'     LIST            
         DC    X'03',AL1(73,12),X'584C000038',C'BYBY',X'00'     REPORT          
***      DC    X'03',AL1(73,13),X'E94C0000C1',C'    ',X'00'     XFER            
*                                                      PGRDEF REC               
         DC    X'03',AL1(75,01),X'E44D0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(75,02),X'E44D0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(75,03),X'E44D0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(75,05),X'E44D0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(75,10),X'E24D0000F8',C'PDPD',X'00'     LIST            
*                                                      PRGROUP REC              
         DC    X'03',AL1(76,01),X'FE4E0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(76,02),X'FE4E0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(76,03),X'FE4E0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(76,05),X'FE4E0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(76,10),X'FD4E0000F8',C'PGPG',X'00'     LIST            
*                                                      MGRDEF REC               
         DC    X'03',AL1(77,01),X'CF4F0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(77,02),X'CF4F0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(77,03),X'CF4F0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(77,04),X'CF4F0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(77,05),X'CF4F0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(77,06),X'CF4F0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(77,10),X'DF4F0000F8',C'MDMD',X'00'     LIST            
*                                                      MGROUP REC               
         DC    X'03',AL1(78,01),X'A8280000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(78,02),X'A8280000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(78,03),X'A8280000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(78,04),X'A8280000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(78,05),X'A8280000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(78,06),X'A8280000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(78,10),X'A9280000F8',C'MRMR',X'00'     LIST            
*                                                      SPILLDEF REC             
         DC    X'03',AL1(79,01),X'A5400000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(79,02),X'A5400000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(79,03),X'A5400000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(79,05),X'A5400000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(79,10),X'A6400000C0',C'    ',X'00'     LIST            
         DC    X'03',AL1(79,04),X'A5400000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(79,06),X'A5400000C0',C'    ',X'00'     RESTORE         
*                                                      STLOCKIN REC             
         DC    X'03',AL1(80,02),X'F3290000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(80,03),X'F3290000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(80,05),X'F3290000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(80,10),X'F2220000C0',C'    ',X'00'     LIST            
*                                                      FAUTH REC                
         DC    X'03',AL1(81,02),X'C2020000C0',C'    ',X'80'     CHANGE          
         DC    X'03',AL1(81,03),X'C2020000C0',C'    ',X'80'     DISPLAY         
         DC    X'03',AL1(81,05),X'C2020000C0',C'    ',X'80'     SELECT          
         DC    X'03',AL1(81,10),X'91020000C0',C'    ',X'80'     LIST            
         DC    X'03',AL1(81,01),X'C2020000C0',C'    ',X'80'     ADD             
         DC    X'03',AL1(81,04),X'C2020000C0',C'    ',X'80'     DELETE          
         DC    X'03',AL1(81,06),X'C2020000C0',C'    ',X'80'     RESTORE         
*                                                      SCOM REC                 
         DC    X'03',AL1(82,01),X'F4450000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(82,02),X'F4450000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(82,03),X'F4450000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(82,04),X'F4450000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(82,05),X'F4450000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(82,06),X'F4450000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(82,10),X'F5460000C0',C'    ',X'00'     LIST            
*                                                      AUTOPAY REC              
         DC    X'03',AL1(83,01),X'ED030000C0',C'    ',X'80'     ADD             
         DC    X'03',AL1(83,02),X'ED030000C0',C'    ',X'80'     CHANGE          
         DC    X'03',AL1(83,03),X'ED030000C0',C'    ',X'80'     DISPLAY         
         DC    X'03',AL1(83,04),X'ED030000C0',C'    ',X'80'     DELETE          
         DC    X'03',AL1(83,05),X'ED030000C0',C'    ',X'80'     SELECT          
         DC    X'03',AL1(83,06),X'ED030000C0',C'    ',X'80'     RESTORE         
         DC    X'03',AL1(83,10),X'7B030000C0',C'    ',X'80'     LIST            
*                                                      PXC REC                  
         DC    X'03',AL1(84,01),X'79040000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(84,02),X'79040000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(84,03),X'79040000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(84,04),X'79040000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(84,05),X'79040000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(84,06),X'79040000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(84,10),X'7A04000080',C'    ',X'00'     LIST            
         DC    X'03',AL1(84,12),X'7A04000448',C'PXPX',X'00'     REPORT          
*                                                      CLIENT REC               
         DC    X'03',AL1(85,02),X'76060000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(85,03),X'76060000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(85,05),X'76060000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(85,10),X'77070000C0',C'    ',X'00'     LIST            
         DC    X'03',AL1(85,01),X'76060000C0',C'    ',X'00'     ADD             
*                                                      PRODUCT REC              
         DC    X'03',AL1(86,02),X'70500000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(86,03),X'70500000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(86,05),X'70500000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(86,10),X'71510000C0',C'    ',X'00'     LIST            
         DC    X'03',AL1(86,01),X'70500000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(86,04),X'70500000C0',C'    ',X'00'     DELETE          
*                                                      CLIENT2 REC              
         DC    X'03',AL1(87,02),X'78080000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(87,03),X'78080000C0',C'    ',X'00'     DISPLAY         
*                                                      ESTIMATE REC             
         DC    X'03',AL1(88,02),X'72520000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(88,03),X'72520000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(88,01),X'72520000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(88,05),X'72520000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(88,10),X'73530000C0',C'    ',X'00'     LIST            
         DC    X'03',AL1(88,07),X'74540000C0',C'    ',X'00'     COPY            
*                                                      ESTD REC                 
         DC    X'03',AL1(89,02),X'75550000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(89,03),X'75550000C0',C'    ',X'00'     DISPLAY         
*                                                      SQAD REC                 
         DC    X'03',AL1(90,02),X'60560000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(90,03),X'60560000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(90,01),X'60560000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(90,10),X'64560000C0',C'    ',X'00'     LIST            
         DC    X'03',AL1(90,05),X'60560000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(90,04),X'60560000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(90,06),X'60560000C0',C'    ',X'00'     RESTORE         
*                                                      I2COM REC                
         DC    X'03',AL1(91,01),X'61450000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(91,02),X'61450000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(91,03),X'61450000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(91,04),X'61450000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(91,05),X'61450000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(91,06),X'61450000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(91,10),X'62460000C0',C'    ',X'00'     LIST            
*                                                      NEWCOMP REC              
         DC    X'03',AL1(92,01),X'63570000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(92,02),X'63570000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(92,03),X'63570000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(92,04),X'63570000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(92,05),X'63570000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(92,06),X'63570000C0',C'    ',X'00'     RESTORE         
*                                                      COST2 REC                
         DC    X'03',AL1(93,10),X'6558000080',C'    ',X'00'     LIST            
         DC    X'03',AL1(93,05),X'6659000080',C'    ',X'00'     SELECT          
         DC    X'03',AL1(93,16),X'6659000099',C'SLLK',X'00'     MAINT           
         DC    X'03',AL1(93,21),X'6558000080',C'    ',X'00'     BLIST           
         DC    X'03',AL1(93,17),X'675A000081',C'    ',X'00'     BILL            
*                                                      UCOMM REC                
         DC    X'03',AL1(94,01),X'685B0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(94,02),X'685B0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(94,03),X'685B0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(94,04),X'685B0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(94,05),X'685B0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(94,06),X'685B0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(94,10),X'695B0000C0',C'    ',X'00'     LIST            
*                                                      MASTER2 REC              
         DC    X'03',AL1(95,02),X'6A5C0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(95,03),X'6A5C0000C0',C'    ',X'00'     DISPLAY         
*                                                      REASON REC/RCODE         
         DC    X'03',AL1(96,01),X'6D5D0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(96,02),X'6D5D0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(96,03),X'6D5D0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(96,04),X'6D5D0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(96,05),X'6D5D0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(96,06),X'6D5D0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(96,10),X'6C5D0000C0',C'    ',X'00'     LIST            
*                                                      PURPOSE REC              
         DC    X'03',AL1(97,01),X'6B5D0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(97,02),X'6B5D0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(97,03),X'6B5D0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(97,04),X'6B5D0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(97,05),X'6B5D0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(97,06),X'6B5D0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(97,10),X'6E5D0000C0',C'    ',X'00'     LIST            
*                                                      PCOM REC                 
         DC    X'03',AL1(98,01),X'93450000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(98,02),X'93450000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(98,03),X'93450000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(98,04),X'93450000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(98,05),X'93450000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(98,06),X'93450000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(98,10),X'83460000C0',C'    ',X'00'     LIST            
*                                                      PGEST REC                
         DC    X'03',AL1(99,01),X'505E0000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(99,02),X'505E0000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(99,03),X'505E0000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(99,04),X'505E0000C0',C'    ',X'00'     DELETE          
         DC    X'03',AL1(99,05),X'505E0000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(99,06),X'505E0000C0',C'    ',X'00'     RESTORE         
         DC    X'03',AL1(99,10),X'515E0000C0',C'    ',X'00'     LIST            
*                                                      DAYPART REC              
         DC    X'03',AL1(37,01),X'5A620000C0',C'    ',X'00'     ADD             
         DC    X'03',AL1(37,02),X'5A620000C0',C'    ',X'00'     CHANGE          
         DC    X'03',AL1(37,03),X'5A620000C0',C'    ',X'00'     DISPLAY         
         DC    X'03',AL1(37,05),X'5A620000C0',C'    ',X'00'     SELECT          
         DC    X'03',AL1(37,10),X'59620000C0',C'    ',X'00'     LIST            
*                                                       AGENCY REC              
         DC    X'03',AL1(100,01),X'5C630000C0',C'    ',X'80'     ADD            
         DC    X'03',AL1(100,02),X'5C630000C0',C'    ',X'80'     CHANGE         
         DC    X'03',AL1(100,03),X'5C630000C0',C'    ',X'80'     DISP           
         DC    X'03',AL1(100,05),X'5C630000C0',C'    ',X'80'     SELECT         
         DC    X'03',AL1(100,10),X'5B630000C0',C'    ',X'80'     LIST           
*                                                       SPLIT REC               
         DC    X'03',AL1(101,01),X'5E640000C0',C'    ',X'00'     ADD            
         DC    X'03',AL1(101,02),X'5E640000C0',C'    ',X'00'     CHANGE         
         DC    X'03',AL1(101,03),X'5E640000C0',C'    ',X'00'     DISP           
         DC    X'03',AL1(101,05),X'5E640000C0',C'    ',X'00'     SELECT         
         DC    X'03',AL1(101,10),X'5D640000C0',C'    ',X'00'     LIST           
*                                                       MENU REC                
         DC    X'03',AL1(102,01),X'42830000C0',C'    ',X'00'     ADD            
         DC    X'03',AL1(102,02),X'42830000C0',C'    ',X'00'     CHANGE         
         DC    X'03',AL1(102,03),X'42830000C0',C'    ',X'00'     DISP           
         DC    X'03',AL1(102,05),X'42830000C0',C'    ',X'00'     SELECT         
         DC    X'03',AL1(102,10),X'43830000C0',C'    ',X'00'     LIST           
*                                                       EQUHDR REC              
         DC    X'03',AL1(103,01),X'40820000C0',C'    ',X'00'     ADD            
         DC    X'03',AL1(103,02),X'40820000C0',C'    ',X'00'     CHANGE         
         DC    X'03',AL1(103,03),X'40820000C0',C'    ',X'00'     DISP           
         DC    X'03',AL1(103,05),X'40820000C0',C'    ',X'00'     SELECT         
         DC    X'03',AL1(103,10),X'41820000C0',C'    ',X'00'     LIST           
*                                                       OCOMMENT REC            
         DC    X'03',AL1(104,01),X'47920000C0',C'    ',X'00'     ADD            
         DC    X'03',AL1(104,02),X'47920000C0',C'    ',X'00'     CHANGE         
         DC    X'03',AL1(104,03),X'47920000C0',C'    ',X'00'     DISP           
         DC    X'03',AL1(104,04),X'47920000C0',C'    ',X'00'     DELETE         
         DC    X'03',AL1(104,05),X'47920000C0',C'    ',X'00'     SELECT         
         DC    X'03',AL1(104,06),X'47920000C0',C'    ',X'00'     RESTOR         
         DC    X'03',AL1(104,10),X'48920000C0',C'    ',X'00'     LIST           
*                                                       DESTINE REC             
         DC    X'03',AL1(105,01),X'49930000C0',C'    ',X'00'     ADD            
         DC    X'03',AL1(105,02),X'49930000C0',C'    ',X'00'     CHANGE         
         DC    X'03',AL1(105,03),X'49930000C0',C'    ',X'00'     DISP           
         DC    X'03',AL1(105,04),X'49930000C0',C'    ',X'00'     DELETE         
         DC    X'03',AL1(105,05),X'49930000C0',C'    ',X'00'     SELECT         
         DC    X'03',AL1(105,06),X'49930000C0',C'    ',X'00'     RESTOR         
         DC    X'03',AL1(105,10),X'4A930000C0',C'    ',X'00'     LIST           
         DC    X'03',AL1(105,12),X'4A93009360',C'DESF',X'00'     REPORT         
*                                                       GFEST REC               
         DC    X'03',AL1(106,01),X'44680000C0',C'    ',X'00'    ADD             
         DC    X'03',AL1(106,02),X'44680000C0',C'    ',X'00'    CHANGE          
         DC    X'03',AL1(106,03),X'44680000C0',C'    ',X'00'    DISPLAY         
         DC    X'03',AL1(106,04),X'44680000C0',C'    ',X'00'    DELETE          
         DC    X'03',AL1(106,05),X'44680000C0',C'    ',X'00'    SELECT          
         DC    X'03',AL1(106,06),X'44680000C0',C'    ',X'00'    RESTORE         
         DC    X'03',AL1(106,10),X'45680000C0',C'    ',X'00'    LIST            
*                                                       MKTFIX REC              
         DC    X'03',AL1(107,10),X'4B6A0000C0',C'    ',X'00'    LIST            
*                                                       SDUDEF REC              
         DC    X'03',AL1(108,01),X'4C840000C0',C'    ',X'00'    ADD             
         DC    X'03',AL1(108,02),X'4C840000C0',C'    ',X'00'    CHANGE          
         DC    X'03',AL1(108,03),X'4C840000C0',C'    ',X'00'    DISPLAY         
         DC    X'03',AL1(108,04),X'4C840000C0',C'    ',X'00'    DELETE          
         DC    X'03',AL1(108,05),X'4C840000C0',C'    ',X'00'    SELECT          
         DC    X'03',AL1(108,06),X'4C840000C0',C'    ',X'00'    RESTORE         
         DC    X'03',AL1(108,10),X'4D840000C0',C'    ',X'00'    LIST            
*                                                                               
*                                                       STAFIX  REC             
         DC    X'03',AL1(109,10),X'466B0000C0',C'    ',X'00'    LIST            
*                                                                               
*                                                       BFORM   REC             
         DC    X'03',AL1(110,01),X'4E6C0000C0',C'    ',X'00'    ADD             
         DC    X'03',AL1(110,02),X'4E6C0000C0',C'    ',X'00'    CHANGE          
         DC    X'03',AL1(110,03),X'4E6C0000C0',C'    ',X'00'    DISPLAY         
         DC    X'03',AL1(110,04),X'4E6C0000C0',C'    ',X'00'    DELETE          
         DC    X'03',AL1(110,05),X'4E6C0000C0',C'    ',X'00'    SELECT          
         DC    X'03',AL1(110,06),X'4E6C0000C0',C'    ',X'00'    RESTORE         
         DC    X'03',AL1(110,10),X'4F6C006CF8',C'BFSF',X'00'    LIST            
         DC    X'03',AL1(110,12),X'4F6C006C78',C'BFSF',X'00'    REPORT          
*                                                                               
*                                                       BPCT    REC             
         DC    X'03',AL1(113,01),X'3A960000C0',C'    ',X'00'    ADD             
         DC    X'03',AL1(113,02),X'3A960000C0',C'    ',X'00'    CHANGE          
         DC    X'03',AL1(113,03),X'3A960000C0',C'    ',X'00'    DISPLAY         
         DC    X'03',AL1(113,04),X'3A960000C0',C'    ',X'00'    DELETE          
         DC    X'03',AL1(113,05),X'3A960000C0',C'    ',X'00'    SELECT          
         DC    X'03',AL1(113,06),X'3A960000C0',C'    ',X'00'    RESTORE         
         DC    X'03',AL1(113,10),X'39960000C0',C'    ',X'00'    LIST            
         DC    X'03',AL1(113,12),X'3996000030',C'BPSF',X'00'    REPORT          
*                                                                               
*                                                       DMTEST REC              
         DC    X'03',AL1(111,12),X'3F6D000078',C'DMDM',X'80'    REPORT          
*                                                                               
*                                                       EQLEN   REC             
         DC    X'03',AL1(112,03),X'3E6E0000C0',C'    ',X'00'    DISPLAY         
*                                                                               
*                                                       DEAL    REC             
         DC    X'03',AL1(114,01),X'3D700000C0',C'    ',X'00'    ADD             
         DC    X'03',AL1(114,02),X'3D700000C0',C'    ',X'00'    CHANGE          
         DC    X'03',AL1(114,03),X'3D700000C0',C'    ',X'00'    DISPLAY         
         DC    X'03',AL1(114,05),X'3D700000C0',C'    ',X'00'    SELECT          
         DC    X'03',AL1(114,10),X'3C700000C0',C'    ',X'00'    LIST            
         DC    X'03',AL1(114,12),X'3B70007040',C'DLDL',X'00'    REPORT          
*                                                                               
*                                                       ACOM    REC             
         DC    X'03',AL1(115,01),X'38450000C0',C'    ',X'00'    ADD             
         DC    X'03',AL1(115,02),X'38450000C0',C'    ',X'00'    CHANGE          
         DC    X'03',AL1(115,03),X'38450000C0',C'    ',X'00'    DISPLAY         
         DC    X'03',AL1(115,04),X'38450000C0',C'    ',X'00'    DELETE          
         DC    X'03',AL1(115,05),X'38450000C0',C'    ',X'00'    SELECT          
         DC    X'03',AL1(115,06),X'38450000C0',C'    ',X'00'    RESTORE         
         DC    X'03',AL1(115,10),X'37460000C0',C'    ',X'00'    LIST            
*                                                                               
*                                                       OMBUYER REC             
         DC    X'03',AL1(116,01),X'3571000080',C'    ',X'00'    ADD             
         DC    X'03',AL1(116,02),X'3571000080',C'    ',X'00'    CHA             
         DC    X'03',AL1(116,03),X'3571000080',C'    ',X'00'    DIS             
*        DC    X'03',AL1(116,04),X'3571000080',C'    ',X'00'    DELETE          
         DC    X'03',AL1(116,05),X'3571000080',C'    ',X'00'    SEL             
*        DC    X'03',AL1(116,06),X'3571000080',C'    ',X'00'    RESTORE         
         DC    X'03',AL1(116,10),X'36710000C0',C'    ',X'00'    LIST            
*                                                                               
*                                                       COMMENT REC             
         DC    X'03',AL1(117,01),X'32720000C0',C'    ',X'00'    ADD             
         DC    X'03',AL1(117,02),X'32720000C0',C'    ',X'00'    CHA             
         DC    X'03',AL1(117,03),X'32720000C0',C'    ',X'00'    DIS             
         DC    X'03',AL1(117,04),X'32720000C0',C'    ',X'00'    DELETE          
         DC    X'03',AL1(117,05),X'32720000C0',C'    ',X'00'    SEL             
         DC    X'03',AL1(117,06),X'32720000C0',C'    ',X'00'    RESTORE         
         DC    X'03',AL1(117,10),X'33720000C0',C'    ',X'00'    LIST            
         DC    X'03',AL1(117,22),X'34730000C1',C'    ',X'00'    PREVIEW         
*                                                                               
*                                                       FLIGHT  REC             
         DC    X'03',AL1(118,01),X'3074000080',C'    ',X'00'    ADD             
         DC    X'03',AL1(118,02),X'3074000080',C'    ',X'00'    CHA             
         DC    X'03',AL1(118,03),X'3074000080',C'    ',X'00'    DIS             
         DC    X'03',AL1(118,05),X'3074000080',C'    ',X'00'    SEL             
         DC    X'03',AL1(118,10),X'3174000080',C'    ',X'00'    LIST            
*                                                                               
*                                                       BYRMOVE                 
         DC    X'03',AL1(119,23),X'2F75000041',C'BMBV',X'00'    MOVE            
         DC    X'03',AL1(119,12),X'2F75000098',C'BMBV',X'00'    REPORT          
*                                                                               
*                                                      XAUTOPAY REC             
         DC    X'03',AL1(120,01),X'2E760000C0',C'    ',X'80'    ADD             
         DC    X'03',AL1(120,02),X'2E760000C0',C'    ',X'80'    CHANGE          
         DC    X'03',AL1(120,03),X'2E760000C0',C'    ',X'80'    DISPLAY         
         DC    X'03',AL1(120,04),X'2E760000C0',C'    ',X'80'    DELETE          
         DC    X'03',AL1(120,05),X'2E760000C0',C'    ',X'80'    SELECT          
         DC    X'03',AL1(120,06),X'2E760000C0',C'    ',X'80'    RESTORE         
         DC    X'03',AL1(120,10),X'2D760000C0',C'    ',X'80'    LIST            
*                                                                               
*                                                      BILLADR REC              
         DC    X'03',AL1(121,01),X'29790000C0',C'    ',X'00'     ADD            
         DC    X'03',AL1(121,02),X'29790000C0',C'    ',X'00'    CHANGE          
         DC    X'03',AL1(121,03),X'29790000C0',C'    ',X'00'    DISPLAY         
         DC    X'03',AL1(121,04),X'29790000C0',C'    ',X'00'    DELETE          
         DC    X'03',AL1(121,05),X'29790000C0',C'    ',X'00'    SELECT          
         DC    X'03',AL1(121,06),X'29790000C0',C'    ',X'00'    RESTORE         
         DC    X'03',AL1(121,10),X'2B790000C0',C'    ',X'00'    LIST            
*                                                                               
*                                                      CLFRZ                    
         DC    X'03',AL1(122,12),X'2C7A000030',C'CVCV',X'00'    REPORT          
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* GET TOKEN RECORD TO VERIFY ACCESS TO RENTRAK DEMOS                            
*                                                                               
         USING GTTOKD,R2                                                        
GETTOKEN NTR1  BASE=*,LABEL=*,WORK=(R2,GTTOKL)                                  
         L     RA,ATWA                                                          
         USING T217FFD,RA                                                       
         NI    USRIDFLG,X'FF'-USRRNTKQ                                          
         XC    CSUTOKEN,CSUTOKEN                                                
         MVC   HALF,TWAAGY                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CT5KEY,R4                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,TWAAGY                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,GTTOKIO             
         LA    R4,GTTOKIO                                                       
         LA    R6,CT5DATA                                                       
         USING CTSEAD,R6                                                        
         MVI   ELCODE,CTSEAELQ     SECURITY AGENCY ALPHA                        
         BRAS  RE,FIRSTEL                                                       
         BNE   *+10                                                             
         MVC   HALF,CTSEAAID                                                    
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TOKRECD,R4          BUILD TOKEN RECORD KEY                       
         MVI   TOKKMIN,TOKKMINQ                                                 
         MVI   TOKKTYP,TOKKRTRK                                                 
         MVC   TOKKSAGY,HALF       SECURITY AGENCY                              
         MVC   TOKKAAGY,TWAAGY                                                  
         MVI   TOKKSYS,X'02'       SPOT SYSTEM                                  
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI '),=C'GENDIR ',KEY,KEY                 
         CLC   KEY(L'TOKKEY),KEYSAVE                                            
         JNE   GTOKENX                                                          
*                                                                               
         LA    R4,GTTOKIO                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL ',KEY+36,(R4),WORK             
*                                                                               
         AHI   R4,TOKFIRST                                                      
         USING RTAUTHD,R4                                                       
         CLI   0(R4),RTAUTELQ      X'0A' ELEMENT                                
         JNE   GTOKENX                                                          
         OC    RTAUTID(L'RTAUTID+L'RTAUTSEC),RTAUTID                            
         JZ    GTOKENX                                                          
         OI    USRIDFLG,USRRNTKQ   HAS ACCESS TO RENTRAK DEMOS                  
         MVC   CSUTOKEN,RTAUTID    SAVE USER TOKEN FOR DEMO VALIDATION          
*                                                                               
GTOKENX  DS    0H                                                               
         J     XIT                                                              
         DROP  R4                                                               
         LTORG                                                                  
*                                                                               
GTTOKD   DSECT                                                                  
GTTOKIO  DS    XL2000                                                           
GTTOKL   EQU   *-GTTOKD                                                         
T21700   CSECT                                                                  
*                                                                               
*=============================================================*                 
* PROVIDE MSPACK/MSUNPK ENTRY POINTS FOR LINKAGE TO STAPACK                     
*=============================================================*                 
         SPACE 1                                                                
GOMSPACK NTR1  BASE=*,LABEL=*,WORK=(R4,8)                                       
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R5)            GET A(MKT)                                   
         MVC   STAPQMKT,0(RE)                                                   
         L     RE,4(R5)            GET A(STA)                                   
         MVC   STAPQSTA(8),0(RE)                                                
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    GOMSP10                                                          
         MVI   ERROR,INVSTAT       INVALID STATION                              
         CLI   STAPERR,QSTP_INVCBL                                              
         JNE   TRAPERR                                                          
         MVI   ERROR,CBLNETER      INVALID CABLE NETWORK                        
         J     TRAPERR                                                          
GOMSP10  L     RE,8(R5)            GET A(MKTSTA)                                
         MVC   0(5,RE),STAPMKST                                                 
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
GOMSUNPK NTR1  BASE=*,LABEL=*,WORK=(R4,8)                                       
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R5)            GET A(MKTSTA)                                
         MVC   STAPMKST,0(RE)                                                   
*                                                                               
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         B     *+6    <--- MHER 1/17/95  IGNORE ERRORS                          
         DC    H'0'                                                             
*                                                                               
         L     RE,4(R5)            GET A(MKT)                                   
         MVC   0(4,RE),STAPQMKT                                                 
         L     RE,8(R5)            GET A(STA)                                   
         MVC   0(5,RE),STAPQSTA    ALWAYS MOVE 5 STATION BYTES                  
         TM    0(R5),X'80'         DOES USER WANT 8 BYTES                       
         BZ    *+10                                                             
         MVC   0(8,RE),STAPQSTA                                                 
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE AUTHORIZATION NUMBER                                       *         
* ON ENTRY     PARAM 1             HAD BETTER HAVE A(GEND) !!!        *         
* ON ENTRY:    PARAM 2             A(SECURITY BLOCK)                  *         
* ON ENTRY:    AIO                 ADDRESS OF IO TO USE               *         
***********************************************************************         
AUTH     DS    0H                                                               
         NMOD1 0,**AUTH**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         USING T217FFD,RA                                                       
*                                                                               
         L     R4,DMCB+4           SECURITY BLOCK                               
         USING AUTHD,R4                                                         
         XC    PRSNLID,PRSNLID                                                  
         OC    PASSWD,PASSWD       CHANGED BY FIELD                             
         BZ    AUTHX                                                            
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CT0REC,R6                                                        
         MVI   CT0KTYP,CT0KTEQU    RECORD TYPE '0'                              
         MVC   CT0KAGY,SECRAGY     GET SECURITY AGENCY                          
         CLC   SECRAGY,SPACES                                                   
         BH    *+10                                                             
         MVC   CT0KAGY,AGENCY                                                   
         MVC   CT0KNUM,PASSWD      PERSON AUTH NUMBER                           
*                                                                               
         L     R6,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,(R6)                      
         CLC   CT0KEY,KEY                                                       
         BNE   AUTHX                                                            
         LA    RE,28(R6)                                                        
*                                                                               
AUTH10   DS    0H                                                               
         CLC   =X'C30A',0(RE)      NEW SECURITY - PERSON ELEMENT                
         BE    AUTH20                                                           
         LLC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BNE   AUTH10                                                           
         B     AUTHX                                                            
*                                                                               
AUTH20   MVC   PRSNLID,2(RE)       PERSONAL ID                                  
AUTHX    B     XIT2                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* PFKEY INITIALIZATION                                                          
* ON ENTRY     PARAM 1             HAD BETTER HAVE A(GEND) !!!                  
* ON ENTRY:    PARAM 2             A(PFKEY VAL. TABLE) OR ZEROS                 
***********************************************************************         
PFKYINIT DS    0H                                                               
         NMOD1 0,**PFIN**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R9,ASYSD                                                         
         L     RA,ATWA                                                          
         USING T217FFD,RA                                                       
*                                                                               
         ICM   R3,7,5(R1)          IF PFKEY VALIDATION TABLE PASSED             
         BZ    INIT10                                                           
         BAS   RE,TESTSEL          TEST FOR SPECIAL SELECT CODE                 
         GOTO1 PFVAL,DMCB,(R3)     HANDLE LOCAL PFKEY PRESENCE                  
         BNE   INITX               TAKE DUMMY ERROR EXIT FOR GOAGAIN            
* THIS CODE REPLICATED FROM UNREACHABLE INSTRUCTION IN DUMMYERR                 
         MVI   GOAGAIN,C'Y'        SET TO RETURN WITH NEW RECORD/ACTION         
         L     RA,ATWA             CURSOR TO RECORD FIELD                       
         USING T217FFD,RA                                                       
         LA    R2,CONRECH          CURSOR TO RECORD FIELD                       
         GOTO1 ERREX2                                                           
*                                                                               
INIT10   DS    0H                                                               
*&&DO                                                                           
INIT10   MVI   SCRSTAT,0           CLEAR SCREEN STATUS BYTE                     
*                                                                               
         CLC   TWASCR,SVSCR        TEST SCREEN CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,SCRCHG                                                   
*                                                                               
         CLC   RECNUM,SVREC        TEST RECORD CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,RECCHG                                                   
*                                                                               
         MVC   BYTE,ACTNUM         MOVE CURRENT ACTION TO TEMP. W/S             
         CLI   BYTE,ACTCHA         IF CURRENT ACTION IS CHANGE                  
         BNE   *+16                                                             
         CLI   SVACT,ACTSEL        AND SAVED ACTION WAS SELECT                  
         BNE   *+8                                                              
         MVI   BYTE,ACTSEL         PRETEND CURRENT ACTION IS SELECT             
*                                                                               
         CLC   BYTE,SVACT          TEST ACTION CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,ACTCHG                                                   
*                                                                               
         TM    SCRSTAT,RECCHG      ALWAYS CLEAR IF RECORD TYPE CHANGED          
         BO    INIT20                                                           
         TM    SCRSTAT,SCRCHG      NEVER CLEAR IF SCREEN DIDN'T CHANGE          
         BZ    INIT30                                                           
         CLI   BYTE,ACTREP         ALWAYS CLEAR IF ACTION IS NOW REPORT         
         BE    INIT20                                                           
         CLI   SVACT,ACTSEL        IF LAST ACTION NOT SELECT                    
         BE    INIT30                                                           
         CLI   BYTE,ACTSEL         AND THIS ACTION NOT SELECT                   
         BE    INIT30                                                           
*                                                                               
         TM    CTLRFLG1,CF1NOCLR       DON'T CLEAR APPLICATION STORAGE?         
         BZ    *+12                                                             
         NI    CTLRFLG1,X'FF'-CF1NOCLR  YES, BUT CLEAR IT NEXT TIME             
         B     INIT30                                                           
*                                                                               
INIT20   LA    RE,SYSSPARE         CLEAR APPLICATION STORAGE                    
         LH    RF,=AL2(L'SYSSPARE)                                              
         XCEFL                                                                  
         LA    RE,CONHEADH         FIND END OF SCREEN                           
         SR    RF,RF                                                            
         ICM   RF,1,0(RE)                                                       
         BZ    *+10                                                             
         AR    RE,RF                                                            
         B     *-10                                                             
         LA    RE,3(RE)            BUMP PAST CONTROL BYTES                      
         LR    RF,RE                                                            
         SR    RF,RA                                                            
         SHI   RF,3520+64          L'AVAIL TWA0 AS DEFINED IN DDGENTWA          
         LCR   RF,RF                                                            
         XCEFL ,                   CLEAR AREA AFTER SCREEN END                  
*                                                                               
INIT30   MVC   SVSCR,TWASCR        SAVE CURRENT SCREEN                          
         MVC   SVREC,RECNUM                     RECORD                          
         MVC   SVACT,BYTE                       ACTION                          
*&&                                                                             
INITX    B     XIT2                                                             
         EJECT                                                                  
*              LOCAL ROUTINE TO HANDLE PFKEY PRESENCE                           
*                                                                               
*                                  P1  BYTES 1-3 = A(PFKEY VAL. TABLE)          
PFVAL    NTR1                                                                   
         CLI   PFKEY,0             USER HIT ENTER?                              
         BE    NO2                 YES                                          
*                                                                               
         L     RF,0(R1)            RF=A(PFKEY TABLE)                            
         USING PFTABD,RF           LOOK UP PFKEY NUMBER IN TABLE                
PFV2     CLI   0(RF),X'FF'                                                      
         BE    PFERR2                                                           
         CLC   PFKEY,PFTAID        MATCH ON NUMBER                              
         BE    PFV3                                                             
         LLC   RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    RF,RE                                                            
         B     PFV2                                                             
*                                                                               
PFV3     TM    PFTSTAT2,PFTRETRN   TEST RETURN TO APPLICATION                   
         BO    NO2                                                              
*                                                                               
         BAS   RE,PFINVOKE         OK TO INVOKE PFKEY                           
         B     YES2                IF RETURNS, RETURN CC EQUAL                  
         EJECT                                                                  
*              ROUTINE TO PROCESS PFKEY REQUEST                                 
*                                                                               
         USING PFTABD,RF           RF=A(PFTABLE ENTRY)                          
PFINVOKE NTR1                                                                   
         TM    PFTSTAT2,PFTCLRKY   DO WE CLEAR THE PFKEY?                       
         BNZ   *+8                 NO                                           
         MVI   PFKEY,0             CLEAR PFKEY FOR NEXT SCREEN                  
*                                                                               
         L     R1,SYSPARMS         RE=A(TRANSLATOR I/O BLOCK)                   
         L     RE,0(R1)                                                         
         MVI   TIOBAID-TIOBD(RE),0 CLEAR PF KEY HERE AS WELL                    
*                                                                               
         TM    PFTSTAT,PFTCPROG    TEST PFKEY GENERATES CALLPROG CALL           
         BZ    *+8                                                              
         BAS   RE,CPROG                                                         
*                                                                               
         CLI   PFTNKEYS,0          TEST KEY FIELDS PRESENT                      
         BE    *+8                                                              
         BAS   RE,EXPNDKEY         EXPAND THEM INTO 'KEY' FIELD                 
*                                                                               
         TM    PFTSTAT,PFTRPROG    POP NESTED CALL SEQUENCE                     
         BZ    *+12                                                             
         BAS   RE,RPROG            ROUTINE TO RESTORE PREV. SCREEN              
         B     DUMYERR2            TAKE DUMMY ERROR XIT FOR GOAGAIN             
*                                                                               
         CLI   PFTREC,C' '         IF NEW RECORD TYPE DEFINED                   
         BE    PFI8                                                             
         MVC   CONREC,PFTREC       MOVE IT OUT                                  
         OI    CONRECH+6,X'80'     TRANSMIT                                     
         MVI   CONRECH+5,8         SET L'I/P                                    
*                                                                               
         L     RE,EFHKEY           RE=A(KEY FIELD)                              
         CLI   5(RE),0             IF THERE'S NO INPUT IN KEY FIELD             
         BNE   *+12                                                             
         MVI   8(RE),C','          MOVE A COMMA TO KEY FIELD SO THAT            
         MVI   5(RE),1             APPLICATION GETS CONTROL                     
*                                                                               
PFI8     CLI   PFTACT,C' '         TEST FOR ACTION CHANGE                       
         BE    PFIX                                                             
         MVC   CONACT,PFTACT       MOVE IT OUT                                  
         OI    CONACTH+6,X'80'     TRANSMIT                                     
         MVI   CONACTH+5,5         SET L'I/P - NOTE ONLY 5                      
*                                                                               
PFIX     B     XIT2                                                             
         EJECT                                                                  
*              ROUTINE TO TEST FOR SPECIAL SELECT CODE ON LISTS                 
*              R3=A(PFKEY TABLE)                                                
TESTSEL  NTR1                                                                   
         CLI   ACTNUM,ACTLIST      IF LIST SCREEN                               
         BNE   TSELX                                                            
*                                                                               
         L     R2,AFRSTREC         LOOP THROUGH SELECT FIELDS                   
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
*                                                                               
TSEL2    STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         CLI   5(R2),0             TEST SELECT CODE INPUT                       
         BE    TSEL6                                                            
         OC    8(3,R2),SPACES                                                   
         LR    RF,R3               RF=A(START OF TABLE)                         
         USING PFTABD,RF                                                        
TSEL4    CLI   0(RF),X'FF'                                                      
         BE    TSEL6                                                            
         CLC   PFTSEL,8(R2)        MATCH ON EXACT SELECT CODE                   
         BE    TSEL8                                                            
         LLC   RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    RF,RE                                                            
         B     TSEL4                                                            
*                                                                               
TSEL6    LLC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    TSELX               (E-O-S)                                      
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE           WHEN ROW CHANGES, PROCESS NEXT               
         BNE   TSEL2               SELECT FIELD                                 
         B     TSEL6                                                            
*                                                                               
TSEL8    MVC   8(3,R2),SPACES      FOUND A MATCH - CLEAR SELECT FIELD           
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LR    RE,R2               SAVE A(FIELD)                                
         LLC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   7(R2),0             TEST THERE'S SOMETHING TO SELECT             
         BE    TSEL6               (NO, SO IGNORE)                              
*                                                                               
         MVC   PFKEY,PFTAID        SET CORRESPONDING PFKEY NUMBER               
         SR    RE,RA                                                            
         STH   RE,CURDISP          SAVE DISP. TO FIELD                          
*                                                                               
TSELX    B     XIT2                                                             
         EJECT                                                                  
* THIS ROUTINE SAVES THE CURRENT TWA IN THE FIRST HALF OF TEMPSTR               
* RECORD NUMBERS 2.  IT THEN SAVES THE SCREEN NUMBER FOUND IN                   
* TWASCR ONTO A STACK.  THE USE OF THIS ROUTINE IN CONJUNCTION WITH             
* THE CHANGING OF THE RECORD, ACTION, AND KEY FIELDS ALLOWS USERS TO            
* CALL UP A NEW SCREEN AND THEN LATER RETURN TO THE SCREEN THEY WERE            
* WORKING ON.  WHEN THE USER WANTS TO RETURN TO A SCREEN, RETPROG WILL          
* BE CALLED TO RESTORE THE SCREEN.                                              
*                                                                               
CPROG    NTR1                                                                   
         MVC   CALLSTCK,TWASCR     SAVE SCREEN NUMBER ON STACK                  
         MVI   CALLSP,1                                                         
**       LR    R1,RA               WE HAVE TO DO THIS INSTEAD BECAUSE           
**       AHI   R1,TWAENDLQ-2       SPSFM HAS NO ROOM IN ITS SAVE AREA           
**       MVC   0(1,R1),TWASCR        FOR CALLSTCK AND CALLSP                    
**       MVI   1(R1),1                                                          
*                                                                               
         L     RE,ATIA             SAVE SCREEN IN FIRST HALF OF TWA             
         LHI   RF,TWAMXLEN                                                      
         L     R0,ATWA                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,X'82'            WRITE TWA RECORD #2                          
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
*                                                                               
         LA    R2,X'83'            WRITE TWA RECORD #3                          
         GOTO1 GETTWA,DMCB,((R2),ASTARTSV)                                      
*                                                                               
         B     XIT2                                                             
         EJECT                                                                  
* THIS ROUTINE RESTORES THE USER TO THE SCREEN THEY WERE WORKING ON             
* BEFORE CALLING ANOTHER SCREEN WHICH WAS SAVED IN TEMPSTR BY CALLPROG.         
*                                                                               
RPROG    NTR1                                                                   
**       LR    R1,RA               WE HAVE TO DO THIS INSTEAD BECAUSE           
**       AHI   R1,TWAENDLQ-2         SPSFM HAS NO ROOM IN ITS SAVE AREA         
**       CLI   1(R1),0                                                          
         CLI   CALLSP,0                                                         
         BE    PFERR2              ERROR IF STACK IS EMPTY                      
*                                                                               
         LA    R2,2                READ TWA RECORD #2                           
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
*                                                                               
         L     RE,ATIA             RESTORE SCREEN FROM 1ST HALF OF TWA          
         LHI   RF,TWAMXLEN                                                      
         L     R0,ATWA                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   ACTNUM,TWALACT      SPECIAL CODE TO KEEP SELECT GOING            
         MVC   RECNUM,TWALREC                                                   
         MVC   CONHEAD(30),=CL30'Came back from another screen.'                
         MVC   CONHEAD+30(30),=CL30'  Please continue ...'                      
*                                                                               
         LA    R2,3                READ TWA RECORD #3                           
*^^NOP   GOTO1 GETTWA,DMCB,((R2),ASTARTSV)                                      
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
         L     RE,ATIA             RESTORE SAVE AREA FROM TIA                   
         LHI   RF,SYSDEND-SVSTART                                               
         L     R0,ASTARTSV                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   CALLSP,0            DECREMENT STACK POINTER                      
         MVC   TWASCR,CALLSTCK     EXTRACT TWASCR                               
**       LR    R1,RA               WE HAVE TO DO THIS INSTEAD BECAUSE           
**       AHI   R1,TWAENDLQ-2         SPSFM HAS NO ROOM IN ITS SAVE AREA         
**       MVI   1(R1),0               FOR CALLSTCK AND CALLSP                    
**       MVC   TWASCR,0(R1)        EXTRACT TWASCR                               
*                                                                               
         L     R2,ATWA             MUST SET INDICTOR TO XMIT ALL FIELDS         
         LA    R2,64(R2)               OR SCREEN WILL BE MESSED UP              
         CLI   0(R2),0                                                          
         BE    *+16                FIND END OF TWA                              
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-16                                                             
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT ALL FIELDS             
*                                                                               
         OI    TRNSTAT,RETURNED    SET THAT RETPROG HAS BEEN CALLED             
*                                                                               
         B     XIT2                                                             
         EJECT                                                                  
*              ROUTINE TO EXPAND KEY FIELDS INTO TMPKEY FIELD                   
*                                                                               
         USING PFTABD,RF           RF=A(PFTABLE ENTRY)                          
EXPNDKEY NTR1                                                                   
         MVC   WORK,SPACES         BUILD KEY FIELD IN WORK FIRST                
         LA    R2,WORK             R2=A(WORK)                                   
         LLC   R3,PFTNKEYS         R3=N'KEY FIELDS                              
         LA    R4,PFTKEYS          SET R4=A(1ST KEY FIELD)                      
         USING KEYD,R4                                                          
         L     R6,ATMPSTOR                                                      
         USING TMPSTORD,R6                                                      
*                                                                               
EXP10    CLI   KEYTYPE,KEYTYCOM    TEST SIMPLY PLACE IMBEDDED COMMA             
         BE    EXP20                                                            
         LR    RF,RA               SET WHERE DATA IS                            
         CLI   KEYTYPE,KEYTYTWA    TWA                                          
         BE    EXP15                                                            
         L     RF,ASTARTSV                                                      
         LA    RF,SYSSPARE                                                      
         CLI   KEYTYPE,KEYTYWS     W/S (SYSSPARE)                               
         BE    EXP15                                                            
         CLI   KEYTYPE,KEYTYCUR    CURSOR LOCATION                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    RF,CURDISP          ASSUME THIS IS A SELECT FIELD                
         AR    RF,RA               RF=A(FLD WHERE CURSOR IS)                    
         BAS   RE,BMPTOROW         BUMP TO FIRST FIELD FOR THIS ROW             
         BNE   PFERR2                                                           
         L     RF,FULL             RETURNS ADDRESS IN FULL                      
*                                                                               
EXP15    AH    RF,KEYDISP          RF=A(DATA)                                   
         LLC   RE,KEYLEN           RE=L'DATA-1                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)       MOVE TO WORK                                 
         AR    R2,RE               BUMP TO LAST CHARACTER OF FIELD              
*                                                                               
         CLI   0(R2),C' '          SHUFFLE BACK TO 1ST NON-SPACE                
         BH    *+10                                                             
         BCTR  R2,0                                                             
         BCT   RE,*-10                                                          
         LA    R2,1(R2)            BUMP TO 1ST POSITION PAST                    
*                                                                               
         CHI   R3,1                TEST THIS IS LAST KEY FIELD                  
         BE    EXPX                SO FINISH UP                                 
*                                                                               
EXP20    MVI   0(R2),C','          INSERT COMMA BEFORE NEXT FIELD               
         LA    R2,1(R2)            BUMP PAST COMMA TO NEXT POSITION             
         LA    R4,KEYNEXT          BUMP TO NEXT KEY FIELD                       
         BCT   R3,EXP10            AND PROCESS                                  
*                                                                               
EXPX     LA    R3,WORK                                                          
         SR    R2,R3               R2=L'TMPKEY FIELD                            
         CLM   R2,1,=AL1(L'TMPKEY)                                              
         BNH   *+6                                                              
         DC    H'0'                MAKE TMPKEY BIGGER                           
*                                                                               
         STC   R2,TMPKEYH+5        STORE LENGTH IN FIELD HEADER                 
         MVI   TMPKEYH,L'TMPKEY+L'TMPKEYH SET LENGTH OF FIELD                   
         LA    RE,TMPKEYH                                                       
         ST    RE,EFHKEY           TELL GENCON TO USE TMPKEY FIELD              
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     XIT2                                                             
         MVC   TMPKEY(0),WORK      MOVE DATA TO FAKE KEY FIELD                  
         SPACE 5                                                                
BMPTOROW NTR1                      BUMP TO FIRST FIELD IN ROW                   
         LR    R2,RF               R2=A(CURRENT FIELD)                          
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         L     R2,AFRSTREC                                                      
BMPT2    LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE                                                        
         BE    BMPT4                                                            
         LLC   R1,0(R2)            TRY NEXT FIELD                               
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BNE   BMPT2                                                            
         B     NO2                 RETURN CC NE IF REACHED E-O-S                
*                                                                               
BMPT4    LLC   R1,0(R2)                                                         
         AR    R2,R1               ASSUMING SELECT FIELD -- BUMP PAST           
         LA    R2,8(R2)            AND PAST HEADER OF (FIRST) DATA FLD          
         ST    R2,FULL             MATCH-RETURN A(FLD HEADER) IN FULL           
         B     YES2                                                             
         DROP  R6,RA                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ/WRITE TEMPSTR PAGES                                           
*                                                                               
* ON ENTRY:    PARAM 1  BYTE  0    BIT SETTINGS/PAGE NUMBER                     
*              PARAM 1  BYTES 1-3  READ/WRITE ADDRESS                           
***********************************************************************         
GETTWA   NTR1                                                                   
         L     RA,ATWA                                                          
         USING T217FFD,RA                                                       
*                                                                               
         MVC   BYTE,0(R1)          BIT SETTINGS/PAGE NUMBER                     
         L     R2,0(R1)            READ/WRITE ADDRESS                           
*                                                                               
         MVC   COMMAND(6),=C'DMWRT '                                            
         TM    BYTE,X'80'          X'80'=1 IS WRITE, ELSE READ                  
         BO    GTWA10                                                           
         MVC   COMMAND(6),=C'DMRDIR'                                            
         TM    BYTE,X'40'          X'40'=1 IS 2304 BYTE TWAS ELSE 6144          
         BNZ   GTWA10                                                           
         MVC   COMMAND(6),=C'DMREAD'                                            
*                                                                               
GTWA10   NI    BYTE,X'0F'          TURN OFF HIGH ORDER BITS                     
*                                                                               
         MVC   DMCB+8(1),BYTE      PAGE NUMBER                                  
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM   TERMINAL NUMBER                              
*                                                                               
         GOTO1 DATAMGR,DMCB,COMMAND,=C'TEMPSTR',,(R2),0                         
*                                                                               
         CLI   8(R1),0             IF COULDN'T DO IT, DIE                       
         BE    XIT2                                                             
         DC    H'0'                                                             
         DROP  RA                                                               
*                                                                               
PFERR2   MVI   ERROR,ERINVPFK      INVALID PF KEY                               
         TM    TRNSTAT,NOVALERR    NO ERREX ON VALI RTNS                        
         BZ    *+12                                                             
         OI    TRNSTAT,BASERR      SET FLAG FOR ERROR TO APPL                   
         B     XIT2                                                             
         GOTO1 ERREX               HOPEFULLY, NEVER TO RETURN                   
         B     XIT2                ELSE GENCON WAS SLAVED                       
*                                                                               
DUMYERR2 MVI   GOAGAIN,C'Y'        SET TO RETURN WITH NEW RECORD/ACTION         
         L     RA,ATWA             CURSOR TO RECORD FIELD                       
         USING T217FFD,RA                                                       
         LA    R2,CONRECH          CURSOR TO RECORD FIELD                       
         GOTO1 ERREX2                                                           
         DROP  RA                                                               
*                                                                               
YES2     SR    RC,RC               SET CC EQ                                    
NO2      LTR   RC,RC               SET CC NEQ                                   
XIT2     XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
************************************************************                    
*  CHECK IF COMMENT RECORD TYPE                                                 
************************************************************                    
CHKCOMS  NTR1  BASE=*,LABEL=*                                                   
         L     RA,ATWA                                                          
         USING T217FFD,RA                                                       
         LA    R3,COMTAB                                                        
*                                                                               
CC10     CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    CCX                                                              
         CLC   0(2,R3),CONREC                                                   
         BE    CC20                                                             
         LA    R3,L'COMTAB(R3)                                                  
         B     CC10                                                             
*                                                                               
CC20     OI    GENSTAT2,DISTHSPG   RE-DISPLAY THIS PAGE AFTER SELECT            
*                                                                               
CCX      XIT1                                                                   
         DROP  RA                                                               
*                                                                               
COMTAB   DS    0CL2                                                             
         DC    C'A2'                                                            
         DC    C'A3'                                                            
         DC    C'BC'                                                            
         DC    C'I2'                                                            
         DC    C'FA'                                                            
         DC    C'RS'                                                            
         DC    C'NV'                                                            
         DC    C'MC'                                                            
         DC    C'SD'                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
************************************************************                    
*  CODE FOR PFM                                                                 
************************************************************                    
PFMXFR   NTR1  BASE=*,LABEL=*                                                   
         L     RA,ATWA             RESET ADDRESS OF TWA                         
         USING T217FFD,RA                                                       
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)                                                        
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         BZ    PFMXT                                                            
*                                                                               
         LR    R3,RF               R3 HAS A(GLOBBER)                            
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING GLPFMFIL,R5                                                      
         MVC   GLPFMFIL(6),=C'SPTFIL'                                           
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)                                                        
         ICM   RF,15,CDATAMGR-COMFACSD(RF)                                      
         MVC   KEYSAVE,TWAKEYSV                                                 
*                                                                               
         CLI   TWALREC,40           CK IF REC ON STATION FILE                   
         BL    PFMXFR13                                                         
         CLI   TWALREC,43                                                       
         BH    PFMXFR13                                                         
         MVC   GLPFMFIL(7),=C'STATION'                                          
         GOTO1 (RF),DMCB,(0,=C'DMRDHI'),=C'STATION',KEYSAVE,KEY                 
         XC    GLPFMDA,GLPFMDA                                                  
         MVC   GLPFMKEY(15),KEY                                                 
         B     PFMXFR17                                                         
*                                                                               
PFMXFR13 CLI   TWALREC,91          CK IF REC ON XSPFIL                          
         BNE   PFMXFR15                                                         
         MVC   GLPFMFIL(6),=C'XSPFIL'                                           
         GOTO1 (RF),DMCB,(0,=C'DMRDHI'),=C'XSPDIR',KEYSAVE,KEY                  
         MVC   GLPFMDA,KEY+36                                                   
         MVC   GLPFMKEY(4),=C'*   '                                             
         B     PFMXFR17                                                         
*                                                                               
PFMXFR15 GOTO1 (RF),DMCB,(0,=C'DMRDHI'),=C'SPTDIR',KEYSAVE,KEY                  
         MVC   GLPFMDA,KEY+14                                                   
         MVC   GLPFMKEY(4),=C'*   '                                             
*                                                                               
PFMXFR17 LR    RF,R3                                                            
         GOTO1 (RF),DMCB,=C'PUTD',ELEM,54,GLPFMCDQ                              
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING GLVXFRSY,R5                                                      
         MVC   GLVXFRSY,=C'SPO'                                                 
         MVC   GLVXTOSY,=C'SPO'                                                 
PFMXFR20 MVC   GLVXFRPR,=C'FIL'                                                 
         MVC   GLVXTOPR,=C'PFM'                                                 
         OI    GLVXFLG1,GLV1SEPS                                                
         LR    RF,R3                                                            
         GOTO1 (RF),DMCB,=C'PUTD',ELEM,24,GLVXCTL                               
         DROP  R5                                                               
*                                                                               
PFMXT          XIT1                                                             
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* CALL OFFICER TO VALIDATE LIMIT ACCESS                                         
*=========================================================                      
         SPACE 1                                                                
CALLOFCR NTR1  BASE=*,LABEL=*,WORK=(R4,8)                                       
*                                                                               
         L     RA,ATWA                                                          
         USING T217FFD,RA                                                       
*                                                                               
         OC    T217FFD+6(2),T217FFD+6   TEST ANY LIMIT ACCESS                   
         JZ    XIT                      NO                                      
*                                                                               
         XC    0(64,R4),0(R4)                                                   
         USING OFFICED,R4                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T217FFD+6                                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVOFFC                                                    
         MVC   OFCCLT,QCLT                                                      
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),T217FFD+6                                              
         MVC   OFCACCSC(3),SVCACCS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCACCSM(3),SVMACCS    ACCESS LIST FROM MKTREC                   
         MVC   OFCSECD,ASECBLK                                                  
*                                                                               
         L     RF,OFFICER                                                       
         GOTO1 (RF),DMCB,(C'N',(R4)),ACOMFACS                                   
         CLI   0(R1),0                                                          
         JE    XIT                                                              
*                                                                               
         MVI   ERROR,SECLOCK                                                    
         CLI   T217FFD+6,C'+'                                                   
         BNE   *+8                                                              
         MVI   ERROR,NOMKTACC                                                   
         J     TRAPERR                                                          
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
* NEW SCREEN MODE - SKIP SOME FIELDS IF STEREO                                  
*                                                                               
CHKSCRN  NTR1  BASE=*,LABEL=*                                                   
*     DEFAULTS CANADIAN CLIENTS TO "CAN" IN COUNTRY FIELD ...                   
*        ... FOR CLIENT RECORDS             MHC  06/10/02                       
*                                                                               
         CLI   TWASCR,X'70'        PRODUCT                                      
         JE    *+12                                                             
         CLI   TWASCR,X'76'        CLIENT                                       
         BNE   CHKSCR20                                                         
***                                                                             
* MAKE TRAFFIC OFFICE VISIBLE FOR AGENCY FM (AKAT 8/5/04)                       
***                                                                             
         CLC   AGENCY,=C'FM'       AGENCY FM?                                   
         BNE   CHKSCR10            NO                                           
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         NI    CLTTONLH+1,X'FF'-X'0C'   CHANGE TO NORMAL INTENSITY              
         NI    CLTTONH+1,X'FF'-X'2C'    NORMAL INTENSITY +UNPROTECT             
         DROP  RA                                                               
*                                                                               
CHKSCR10 XC    KEY,KEY             READ AGYHDR FOR CLIENT SCREEN                
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         L     R6,AIO3                                                          
         USING AGYKEY,R6                                                        
         MVC   SVAPROF,AGYPROF     SAVE AGENCY PROFILE                          
*                                                                               
         CLI   SVAPROF+7,C'C'      IF CANADIAN AGENCY                           
         BNE   CHKSCR20                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         CLI   TWASCR,X'76'        TEST CLIENT REC                              
         JNE   CHKSCR20                                                         
         MVC   CLTEX10,=C'CAN'                                                  
         OI    CLTEX10H+6,X'80'    TRANSMIT                                     
         MVC   CLTOP4,=C'BBM'                                                   
         OI    CLTOP4H+6,X'80'     TRANSMIT                                     
*                                                                               
CHKSCR20 CLI   TWASCR,X'B2'        TEST STATION ADDRESS SCREEN                  
         JE    CHKSCR22                                                         
         CLI   TWASCR,X'D7'        TEST REP SCREEN                              
         JE    CHKSCR22                                                         
         CLI   TWASCR,X'70'        TEST PRODUCT  SCREEN                         
         JE    CHKSCR22                                                         
         CLI   TWASCR,X'76'        TEST CLIENT SCREEN                           
         JNE   CHKSCR40                                                         
                                                                                
*==============================================================                 
* GET THE ACCESS RECORD TO SEE IF SAP AGY                                       
*==============================================================                 
                                                                                
CHKSCR22 XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING CT5REC,R4                                                        
*                                                                               
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGENCY                                                  
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',WORK,AIO                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SAPAGY,C'N'                                                      
         L     R4,AIO                                                           
         LA    R4,CT5DATA-CT5REC(R4)                                            
*                                                                               
CHKSCR30 CLI   0(R4),0                                                          
         BE    CHKSCR34                                                         
         CLI   0(R4),X'B4'                                                      
         BE    CHKSCR32                                                         
         LLC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     CHKSCR30                                                         
*                                                                               
         USING CTAGDD,R4                                                        
*                                                                               
CHKSCR32 TM    CTAGOPTS,CTAGSAP    TEST SAP AGY                                 
         BZ    CHKSCR34                                                         
         MVI   SAPAGY,C'Y'                                                      
         B     CHKSCR40                                                         
         DROP  R4                                                               
                                                                                
* NOW FIND THE SAP FIELD IN THE SCREEN!                                         
                                                                                
CHKSCR34 L     R4,ATWA                                                          
         LA    R4,64(R4)           POINT TO FIRST FIELD                         
*                                                                               
CHKSCR36 LLC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         JE    CHKSCR40                                                         
         TM    1(R4),X'20'         TEST PROTECTED                               
         JZ    CHKSCR36            NO - CAN'T BE A TITLE                        
         CLC   =C'SAP',8(R4)                                                    
         JNE   CHKSCR36                                                         
         LLC   RE,0(R4)                                                         
         AHI   RE,-8                                                            
         TM    1(R4),X'02'         TEST EXTENDED FLDHDR                         
         JZ    *+8                                                              
         AHI   RE,-8                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         XC    8(0,R4),8(R4)       CLEAR TITLE OF SAP FIELD                     
*                                                                               
         IC    R0,0(R4)            POINT TO UNP FIELD                           
         AR    R4,R0                                                            
         OI    1(R4),X'20'         SET SAP INPUT TO PROTECTED                   
*                                                                               
CHKSCR40 LA    R2,SECFLDS          R2=A(FIELD SECURITY DISPLACEMENTS)           
         LHI   R0,SECFLDSN         R0=N'SECURITY FIELDS                         
         L     R9,ASYSD            A(SYSD)                                      
*                                                                               
CHKSCR45 XR    R4,R4               CLEAR R4                                     
         ICM   R4,3,0(R2)          R4=DISPLACEMENT TO SECURITY VALUE            
         AR    R4,R9               A(SECURITY FIELD)                            
         MVI   0(R4),0             INIT TO FULL ACCESS                          
         OC    TWASAGN,TWASAGN     ON NEW SECURITY?                             
         BNZ   *+14                YES                                          
         OC    TWAACCS(2),TWAACCS  LIMIT ACCESS?                                
         BZ    CHKSCR50            NO - USER HAS FULL ACCESS                    
*                                                                               
         GOTO1 SECRET,DMCB,('SECPFLDP',ASECBLK),2(R2)                           
         BE    CHKSCR50            CC EQU IF FULL READ/WRITE ACCESS             
         LA    RF,C'Y'             C'Y'=READ ONLY                               
         BH    *+8                 CC HIGH IF READ ONLY                         
         LA    RF,C'N'             C'N'=NO ACCESS                               
         STC   RF,0(R4)            SET LIMIT FCONTROL ACCESS                    
CHKSCR50 AHI   R2,L'SECFLDS        BUMP TO NEXT DISPLACEMENT                    
         BCT   R0,CHKSCR45         PROCESS NEXT SECURITY FIELD                  
*                                                                               
CHKSCRX  J     XIT                                                              
*                                                                               
SECFLDS  DS    0XL3                ** DISPS. TO SECURITY VALUES **              
         DC    AL2(SECFLDPO-SYSD),AL1(01)  PO#               <EST>              
         DC    AL2(SCFLDEBF-SYSD),AL1(02)  EST BILL FORMULA  <EST>              
SECFLDSN EQU   (*-SECFLDS)/L'SECFLDS                                            
*                                                                               
         DROP  R6,RA                                                            
         LTORG                                                                  
*                                                                               
*==================================================================             
* PROVIDE ENTRY POINT FOR SPGOL39 TO CALL FOR PROCESSING                        
*==================================================================             
                                                                                
CALLBASE NTR1  BASE=*,LABEL=*                                                   
         OI    VCALLBAS,X'80'      SET FLAG THAT CALLBASE ACTIVE                
*                                                                               
         CLC   0(8,R1),=C'*T21767*'                                             
         JNE   *+8                                                              
         ST    RD,SFM67RD          SET TO RETURN TO CALLER                      
*                                                                               
         L     RA,ATWA                                                          
         L     RB,SYSRB                                                         
         LA    R7,4095(RB)         INITIALIZE MANUALLY!                         
         LA    R7,1(R7)                                                         
*                                                                               
         LARL  RF,FM32                                                          
         BASR  RE,RF                                                            
* NOTE NOTE NOTE!!! NO RETURN HERE. EXXMOD TEST VCALLBAS!                       
* NOTE NOTE NOTE!!! NO RETURN HERE. EXXMOD TEST VCALLBAS!                       
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE+1024                                                    
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
         ORG                                                                    
       ++INCLUDE SPSTABLK                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG CONTAGH                                                            
       ++INCLUDE SPSFMD8D                                                       
         EJECT                                                                  
         ORG CONTAGH                                                            
       ++INCLUDE SCSFM76D                                                       
         EJECT                                                                  
         ORG CONHEADH                                                           
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMSAVED                                                     
         EJECT                                                                  
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*SPDEMUPD                                                                       
*ESBRECD                                                                        
*CLTHDRD                                                                        
*PRDHDRD                                                                        
*ESTHDRD                                                                        
*AGYHDRD                                                                        
*MKTRECD                                                                        
*STARECD                                                                        
*MKTRECD                                                                        
*CTGENFILE                                                                      
*DDOFFICED                                                                      
*DDCOMFACS                                                                      
*DDMASTD                                                                        
*DDCOREQUS                                                                      
*DDFASECRETD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
SPDEMUPD DSECT                                                                  
       ++INCLUDE SPDEMUPD                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
RMKTRECD DSECT                                                                  
       ++INCLUDE REGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDMASTD                                                        
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLPFMD                                                       
         EJECT                                                                  
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE SPSYSFAC                                                       
         EJECT                                                                  
       ++INCLUDE GEGENTOK                                                       
         PRINT ON                                                               
*              DSECT TO COVER THE EXTRA TEMPORARY STORAGE                       
*                                                                               
TMPSTORD DSECT                                                                  
TMPKEYH  DS    XL8                 DUMMY KEY FIELD HEADER FOR GENCON            
TMPKEY   DS    CL50                DUMMY KEY FIELD                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'194SPSFM00   06/15/20'                                      
         END                                                                    
