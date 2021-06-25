*          DATA SET TAREP00    AT LEVEL 025 AS OF 03/20/15                      
*PHASE T70300C,*                                                                
*INCLUDE TINVCON                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T70300 - TALENT REPORT CONTROLLER'                              
T70300   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WKEND-WKST,T70300,R7,R6,RR=R2,CLEAR=YES                          
         ST    R2,RELO                                                          
         LR    R9,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AHI   R9,L'FAIO           GRABBING STORAGE FOR AIO1                    
         AHI   R9,L'SAIO           GRABBING STORAGE FOR AIO2                    
         AHI   R9,L'TAIO           GRABBING STORAGE FOR AIO3                    
         LA    R9,24(R9)           NEED SPACE FOR 3 8 BYTE LITERALS             
         ST    R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R1,SYSPARMS                                                      
         L     RA,4(R1)                                                         
         USING T703FFD,RA                                                       
         SPACE 1                                                                
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         NI    CONKEYH+1,X'FF'-X'20'  UNPROTECT ACTION FOR GENCON               
         OI    CONSERVH+1,X'01'     SERVICE FIELD IS ALWAYS MODIFIED            
         OI    CONSERVH+6,X'80'                                                 
         XC    CONHED2,CONHED2     CLEAR MESSAGE AREA                           
         OI    CONHED2H+6,X'80'                                                 
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         BAS   RE,SETRD            SET RD SO GET CONTROL AFTER GENCON           
         SPACE 1                                                                
         GOTO1 GENCON,DMCB,(R8)    OFF TO GENCON                                
         SPACE 1                                                                
         BAS   RE,ENTSCRN          SETUP ENTRY SCREEN                           
         SPACE 1                                                                
         GOTO1 CONCLUDE            FINISHING UP ROUTINES                        
         SPACE 1                                                                
         B     XIT                 THEN WE'RE THROUGH                           
         SPACE 3                                                                
SETRD    NTR1                                                                   
         ST    RD,SYSRD                                                         
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZE SYSTEM ADDRESSES                                      
         SPACE 1                                                                
SYSINIT  NTR1                                                                   
*                                  GET TERMINAL VALUES                          
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
         MVC   TERM,TWATRM                                                      
         MVC   WRIAUTH,TWAAUTH                                                  
         MVC   USERID,TWAORIG                                                   
         MVC   AGYALPHA,TWAAGY                                                  
         MVC   AGENCY,TWAAGY                                                    
         MVI   FILTIDNO,10         PROGRAM FILTER FIELD ID 10                   
         MVI   TWANSAVE,0          OUTSMART GENCON - DON'T RESTORE              
*                                                                               
*                                  SET UP CERTAIN ADDRESSES                     
         L     R1,SYSPARMS                                                      
         MVC   ATIOB+1(3),1(R1)    P1 1-3 HAS A(TIOB)                           
         LM    R3,R4,12(R1)        A(TIA) A(COMFACS)                            
         ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         ST    R3,ATIA                                                          
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   GETMSG,CGETMSG                                                   
         MVC   HELLO,CHELLO                                                     
         MVC   SCANNER,CSCANNER                                                 
         MVC   SOFTDATE,CSOFDAT                                                 
         MVC   VMQRPT,CMQRPT       WHEN AVAILABLE IN COMFACS                    
         MVC   SWITCH,CSWITCH                                                   
*                                                                               
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R5,NVTYPES                                                       
SYS1     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R5,SYS1                                                          
         SPACE 1                                                                
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
         SPACE 1                                                                
SYS2     MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R3),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,SYS2                                                          
         SPACE 1                                                                
         SR    R3,R3               SET UP ENTRIES TO TASYSVAL ROUTINES          
         LA    R4,SYSCOMM                                                       
         LA    R5,NSYSCOMM                                                      
         SPACE 1                                                                
SYS4     MVC   0(4,R4),TASYSVAL    ALL GO TO TASYSVAL                           
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS4                                                          
*                                  HAVE ANOTHER SET OF TASYSVAL RTNS            
         LA    R4,SYSCOMM2                                                      
         LA    R5,NSYSCOM2                                                      
         SPACE 1                                                                
SYS5     MVC   0(4,R4),TASYSVAL    ALL GO TO TASYSVAL                           
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS5                                                          
*                                                                               
         SR    R3,R3               SET UP ENTRIES TO TAREPGEN ROUTINES          
         LA    R4,WRICOMM                                                       
         LA    R5,NWRICOMM                                                      
*                                                                               
SYS6     MVC   0(4,R4),TAREPGEN    ALL GO TO TAREPGEN                           
         STC   R3,0(R4)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS6                                                          
         EJECT                                                                  
*              OTHER INITIALIZATION                                             
         SPACE 3                                                                
*                                  SEED SYSD WITH DUMP COMMENTS                 
         SPACE 1                                                                
         MVC   DUMPSYSD,=C'**SYSD**'                                            
         MVC   DUMPTLVL,=C'*TALVAL*'                                            
         MVC   DUMPRPGN,=C'*TARPGN*'                                            
         MVC   DUMPASRT,=C'*ASSORT*'                                            
         MVC   DUMPTLIO,=C'*SYSIOD*'                                            
*                                                                               
         LA    R1,BUFF                                                          
         MVC   0(8,R1),=C'**DPG***'                                             
         LA    R1,8(R1)                                                         
         ST    R1,ADPGPROG                                                      
         ST    R1,DRSTBUF                                                       
         LA    R1,3000(R1)                                                      
         ST    R1,DRENDBUF                                                      
         LA    R1,8(R1)                                                         
         ST    R1,DRTALIO                                                       
         LA    R1,1000(R1)                                                      
         LA    R1,8(R1)                                                         
         ST    R1,ACOLFILT                                                      
*                                                                               
*                                  SET SYSTEM DEPENDENT VALUES                  
         L     RF,=V(DUMMY)        END OF SYSTEM BASE                           
         A     RF,RELO                                                          
         ST    RF,SYSDUMMY                                                      
         MVI   SYSTEM,C'T'         TALENT                                       
         MVI   MAXIOS,3            USES 3 I/O AREAS                             
         MVC   SIZEIO,=F'4000'     EACH I/O IS 4000 BYTES                       
         MVC   GETUSER,WRIUSER     ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'32'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'2'                                                    
         MVC   DATADISP,=H'40'                                                  
         MVC   SYSDIR,=C'TALDIR  '                                              
         MVC   SYSFIL,=C'TALFILE '                                              
         MVC   REQFILE,=C'TALREQ '                                              
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENTS                         
         MVI   GETMSYS,70          USES GETMSG FOR SYSTEM 70                    
         MVC   LWORK,=AL4(WKEND-WKST) WE TOOK XXXXX BYTES IN NMOD               
         MVC   RCPROG(2),=C'TA'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9070300'    PRESET FOR SYSTEM CALLOVS               
*                                                                               
         CLI   TGTALNUM,0                                                       
         BNE   SYS20                                                            
         L     RF,SWITCH           SEE IF CALLED OFFLINE, SWITCH=0              
         LTR   RF,RF                                                            
         BNZ   SYS10                                                            
                                                                                
         L     R1,TWAMASTC         ADDRESS OF MASTER                            
         USING MASTD,R1                                                         
         L     R1,MCUTL            A(UTL)                                       
         B     SYS15                                                            
                                                                                
SYS10    GOTO1 SWITCH,DMCB,(X'FF',X'FFFFFFFF'),0                                
         L     R1,0(R1)                                                         
SYS15    XR    RF,RF                                                            
         IC    RF,TSYS-UTLD(R1)                                                 
         SRA   RF,4                                                             
         STC   RF,TGTALNUM         SAVE TALENT SYSTEM NUMBER (TAL?)             
                                                                                
SYS20    L     R2,TASYSTAB         A(SYSTEM TABLES)                             
         LH    R1,=Y(TGARRACT-TGTABLES)                                         
                                                                                
         AR    R1,R2                                                            
         A     R2,0(R1)            R2=A(RECACT TABLE HEADER)                    
         USING RACTD,R2                                                         
         LA    R1,RACTTBL          R1=A(RECACT TABLE)                           
         ST    R1,ARECACT                                                       
         MVC   LRECACT,RACTLTBL    LENGTH OF TABLE ENTRY                        
*                                                                               
         LA    R1,TGD                                                           
         ST    R1,ASTARTSV         BEGINNING OF SAVED STORAGE                   
         OC    TWAVPRNT,TWAVPRNT   IF NOT OFFLINE                               
         BNZ   *+10                                                             
         MVC   LSVTWA0,=AL2(TGEND-TGD)  SET AMOUNT TO SAVE                      
*                                                                               
         OI    GENSTAT1,RDUPAPPL+NOSETEFH  DON'T READ FOR UPDATE                
         OI    GENSTAT3,DIEONERR   DIE WHEN ERROR OFFLINE (TO GET DUMP)         
* CONFIRMATION OF DELETE REQUIRED                                               
         OI    GENSTAT4,CONFDEL                                                 
* DISALLOW 'D' FROM LIST(USE DE/DEL)                                            
         OI    GENSTAT5,NODLST                                                  
*                                                                               
         LA    R1,CONRECH          SET EFH TAGS OURSELVES                       
         ST    R1,EFHREC                                                        
         LA    R1,CONACTH                                                       
         ST    R1,EFHACT                                                        
         LA    R1,CONKEYH                                                       
         ST    R1,EFHKEY                                                        
         LA    R1,CONWHENH                                                      
         ST    R1,EFHWHEN                                                       
         LA    R1,CONOUTH                                                       
         ST    R1,EFHOUT                                                        
         LA    R1,CONDESTH                                                      
         ST    R1,EFHDEST                                                       
         LA    R1,CONOTHH                                                       
         ST    R1,EFHOTH                                                        
         LA    R1,CONTAGH                                                       
         ST    R1,EFHTAG                                                        
SYSINTX  B     XIT                                                              
         EJECT                                                                  
*              SETUP SYSTEM'S ENTRY SCREEN                                      
         SPACE 1                                                                
ENTSCRN  NTR1                                                                   
         CLI   OFFLINE,C'Y'        IF ONLINE                                    
         BE    XIT                                                              
         CLI   TWASCR,00           AND ON MENU SCREEN                           
         BNE   XIT                                                              
         CLI   TWAAUTH+1,X'0F'     AND CONNECTING VIA NEW SECURITY              
         BE    ESCR20                                                           
         CLI   TWAAUTH+1,X'FF'     HIDE STAFF AND PASSWORD FIELDS               
         BE    ESCR20                                                           
         LA    R2,CONTAGH                                                       
         LHI   R3,4                                                             
ESCR10   OI    1(R2),X'2C'                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R3,ESCR10                                                        
         SPACE 1                                                                
*                                                                               
ESCR20   CLI   CONRECH+5,0                                                      
         BNE   XIT                                                              
         CLI   CONACTH+5,0                                                      
         BNE   XIT                                                              
         CLI   CONSTAFH+5,0                                                     
         BNE   XIT                                                              
         CLI   PFAID,0             IF RECORD/ACTION OR PFKEY                    
         BNE   XIT                 HAVE NOT BEEN ENTERED                        
         CLI   ERROR,ERINVPSW                                                   
         BE    XIT                                                              
         NI    CONSTAFH+6,X'BF'    GIVE MESSAGE                                 
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,CONRECH                                                       
         OI    6(R2),X'40'                                                      
         XC    GETTXTCB,GETTXTCB                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMTYP,GTMINF                                                    
         MVC   GTMSGNO,=H'272'                                                  
         MVC   GTMSYS,GETMSYS                                                   
         GOTO1 ERREX                                                            
         EJECT                                                                  
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS TABLES ETC                                             
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
CORETAB  DS    0C                  TABLE OF CORE-RESIDENT MODULES               
         DC    AL1(QGENCON)        GENCON                                       
         DC    AL1(QDRONE)         DRONE                                        
         DC    AL1(QTASYSIO)       TASYSIO                                      
         DC    AL1(QTASYSVL)       TASYSVAL                                     
         DC    AL1(QTASYSTB)       TASYSTAB                                     
         DC    AL1(QTAREPGN)       TAREPGEN                                     
         DC    AL1(QQSORT)         QSORT                                        
         DC    AL1(QTASYSCA)       TASYSCALC                                    
         DC    AL1(QTRPACK)        TRPACK                                       
CORES    EQU   (*-CORETAB)                                                      
         SPACE 1                                                                
SYSVCON  DS    0V                                                               
         DC    V(TINVCON)                                                       
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              PHASES USED UP BY SYSTEM SO FAR                                  
*              THIS IS NOW MAINTAINED IN TAREPRACT                              
         SPACE 1                                                                
       ++INCLUDE TAREPWKD                                                       
       ++INCLUDE TAREPWORKD                                                     
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*TAREPFFD                                                                       
*DDGENTWA                                                                       
*DDMASTD                                                                        
*TAGENFILE                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*CTGENFILE                                                                      
*FAFACTS                                                                        
*FATIOB                                                                         
*DDCOMFACS                                                                      
*DDCOREQUS                                                                      
*DRGLOBAL                                                                       
*DRDICFILE                                                                      
*DDBIGBOX                                                                       
*DDWIDED                                                                        
*FAGETTXTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAREPFFD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAUTL                                                          
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRDICD                                                         
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025TAREP00   03/20/15'                                      
         END                                                                    
