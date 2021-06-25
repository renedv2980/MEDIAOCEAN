*          DATA SET ACEXP00    AT LEVEL 006 AS OF 09/11/02                      
*PHASE T61500A,*                                                                
*INCLUDE CONVMOS                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T61500 - COKE EXPENDITURE CONTROLLER'                           
T61500   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,T61500,R7,RR=R2,CLEAR=YES                                
         ST    R2,RELO                                                          
         LR    R9,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AH    R9,=H'4000'         GRABBING 2 2000 BYTE I/O AREAS               
         LA    R9,16(R9)           NEED SPACE FOR 2 8 BYTE LABELS               
         USING SYSD,R9                                                          
         ST    R7,BASER7                                                        
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         USING T615FFD,RA                                                       
         L     R1,SYSPARMS                                                      
         L     RA,4(R1)                                                         
         OC    TWAVPRNT,TWAVPRNT                                                
         BNZ   *+12                BRANCH IF OFF-LINE                           
         TM    TWAAUTH,X'80'                                                    
         BZ    NOTAUTH             NOT AUTHORIZED                               
         NI    CONKEYH+1,X'FF'-X'20'  UNPROTECT ACTION FOR GENCON               
         OI    CONSERVH+1,X'01'     SERVICE FIELD IS ALWAYS MODIFIED            
         OI    CONSERVH+6,X'80'                                                 
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         L     R1,SYSPARMS                                                      
         L     R2,8(R1)            A(SYSLIST)                                   
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A30'                                           
         LA    R1,DMCB                                                          
         L     RF,4(R2)            A(CALLOV)                                    
         BASR  RE,RF               GO TO CALLOV                                 
         L     RF,DMCB             PICK UP A(GENERAL CONTROLLER)                
         GOTO1 (RF),DMCB,(R8)      GO THERE - PASS A(WORKING STORAGE)           
         B     XIT                 THEN WE'RE THROUGH                           
         EJECT                                                                  
*              INITIALIZE SYSTEM DEPENDENT VALUES                               
         SPACE 1                                                                
SYSINIT  NTR1                                                                   
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
SYS2     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS2                                                          
         SPACE 1                                                                
         LA    R2,ADTABLES                                                      
         LA    R3,TABLES                                                        
         LA    R4,NTABLS                                                        
SYS3     L     R1,0(R3)            RELOCATE TABLE ENTRIES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS3                                                          
         SPACE 1                                                                
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,VCOUNT                                                        
SYS4     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS4                                                          
         SPACE 1                                                                
         L     R1,SYSPARMS                                                      
         L     R5,16(R1)                                                        
         ST    R5,ACOMFACS                                                      
         USING COMFACSD,R5                                                      
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QADDTRN      GET A(ADDTRN)                                
         GOTO1 CCALLOV,DMCB                                                     
         MVC   ADDTRN,0(R1)                                                     
         SPACE 1                                                                
*                                  SET SYSTEM DEPENDENT VALUES                  
         SPACE 1                                                                
         MVC   SYSDUMMY,DUMMY      END OF SYSTEM BASE                           
         MVI   SYSTEM,C'A'         ACCOUNT                                      
         MVI   MAXIOS,2            USES 2 I/O AREAS                             
         MVC   SIZEIO,=F'2000'     EACH I/O IS 2000 BYTES                       
         MVC   GETUSER,VALUSER     ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'42'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'2'                                                    
         MVC   DATADISP,=H'49'     USUALLY PERFIL                               
         MVC   SYSDIR,=C'ACCOUNT '                                              
         MVC   SYSFIL,=C'ACCOUNT '                                              
         MVI   USEIO,C'Y'          ONLY ACCPAK DOES THIS                        
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENTS                         
         MVI   GETMSYS,5           USES GETMSG FOR SYSTEM 5                     
         MVC   LWORK,=AL4(LENWORK)                                              
         MVC   RCPROG(2),=C'AC'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9061500'    PRESET FOR SYSTEM CALLOVS               
         MVC   ARECACT,ADRECACT      RECORD/ACTION DIRECTORY                    
         LA    R1,STARTSV                                                       
         ST    R1,ASTARTSV                                                      
         SR    R2,R2                                                            
         LA    R3,SAVXTWA                                                       
         A     R3,=AL4(L'SAVXTWA)  END OF SAVE STORAGE                          
         ST    R3,AMYAREA          RESOLVED A(MYAREA)                           
         MVI   NTWA,NTWAEQU        NUMBER OF TWAS TO SAVE                       
         OI    NTWA,X'80'          INDICATE WANT 6K TWAS                        
SYSINTX  B     XIT                                                              
         EJECT                                                                  
*              SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                   
         SPACE 1                                                                
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         L     R9,ASYSD                                                         
         L     R7,BASER7                                                        
         L     R8,ASPOOLD                                                       
         ST    RD,COMMRD                                                        
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     VVALUSER            X'00'                                        
         B     VADDEL              X'04'                                        
         B     VDELEL              X'08                                         
         B     VGETEL              X'0C'                                        
         B     VEXIT               X'10'                                        
         B     VNAMOUT             X'14'                                        
         B     VADDROUT            X'18'                                        
         B     VNAMIN              X'1C'                                        
         B     VADDRIN             X'20'                                        
         B     VBALIN              X'24'                                        
         B     VSTATIN             X'28'                                        
         B     VNUMERIC            X'2C'                                        
         B     VPACK               X'30'                                        
         B     VAUTH               X'34'                                        
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
*              USER AGENCY                                                      
         SPACE 1                                                                
VVALUSER L     R1,SYSPARMS                                                      
         MVC   AGENCY,0(R1)                                                     
         MVC   AGYSIGN,SPACES                                                   
         XC    AGYALPHA,AGYALPHA                                                
         MVI   AGYNUM,X'FF'        ASSUME NOT NUMERIC                           
*                                  AGENCY NAME & ADDRESS FROM ID REC.           
         MVC   FILENAME,=CL8'CTFILE'                                            
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),10(RA)                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         LA    R6,CTIDATA                                                       
         SR    R3,R3                                                            
VUSER1   CLI   0(R6),0                                                          
         BE    VUSER3                                                           
         CLI   0(R6),X'30'                                                      
         BE    VUSER2A                                                          
         CLI   0(R6),X'02'                                                      
         BE    VUSER2B                                                          
         CLI   0(R6),X'06'                                                      
         BE    VUSER2C                                                          
VUSER1A  IC    R3,1(R6)                                                         
         AR    R6,R3                                                            
         B     VUSER1                                                           
         USING CTDSTD,R6                                                        
VUSER2A  MVC   USERNAME,CTDSTNAM                                                
         MVC   USERADDR,CTDSTADD                                                
         B     VUSER1A                                                          
         USING CTDSCD,R6                                                        
VUSER2B  MVC   AGYSIGN,CTDSC                                                    
         MVC   WORK(3),AGYSIGN+2                                                
         NC    WORK(3),=X'F0F0F0'                                               
         CLC   WORK(3),=X'F0F0F0'                                               
         BNE   VUSER1A                                                          
         MVC   AGYNUM,AGYSIGN+2                                                 
         B     VUSER1A                                                          
         USING CTAGYD,R6                                                        
VUSER2C  MVC   AGYALPHA,CTAGYID                                                 
         B     VUSER1A                                                          
         SPACE 1                                                                
VUSER3   XC    FILENAME,FILENAME                                                
         CLC   AGENCY,COMPANY                                                   
         BE    VUSER4                                                           
         MVC   COMPANY,AGENCY      FIRST FOR COMPANY                            
         LA    R2,KEY              GET COMPANY RECORD                           
         USING ACKEYD,R2                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),COMPANY                                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 GETL,DMCB,(X'10',AIO),0                                          
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                NO COMPANY ELEMENT                           
         L     R2,ELADDR                                                        
         MVC   COMPEL,0(R2)        SAVE COMPANY ELEMENT                         
         SPACE 1                                                                
VUSER4   GOTO1 DATCON,DMCB,(5,0),(0,TODAY0)                                     
         GOTO1 DATCON,DMCB,(5,0),(1,TODAY1)                                     
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)                                     
         SPACE 1                                                                
         MVC   MOS(1),TODAY0+1                                                  
         MVC   MOS+1(1),TODAY0+3                                                
         CLI   TODAY0+2,C'1'                                                    
         BNE   VUSER9                                                           
         MVI   MOS+1,C'A'                                                       
         CLI   TODAY0+3,C'0'                                                    
         BE    VUSER9                                                           
         MVI   MOS+1,C'B'                                                       
         CLI   TODAY0+3,C'1'                                                    
         BE    VUSER9                                                           
         MVI   MOS+1,C'C'                                                       
VUSER9   DS    0H                                                               
VUSERX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD AN ELEMENT                                        
         SPACE 1                                                                
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
         SPACE 1                                                                
VADDEL   LM    R2,R3,0(R1)                                                      
         SPACE 1                                                                
         USING ACKEYD,R2                                                        
         MVC   HALF,ACLENGTH                                                    
         ZIC   R4,1(R3)                                                         
         AH    R4,HALF                                                          
         CH    R4,=H'999'                                                       
         BNH   *+16                                                             
         MVI   ERROR,57            RECORD TOO LONG (SYST 5)                     
         LA    R2,CONRECH                                                       
         B     THEEND                                                           
         SPACE 1                                                                
         GOTO1 HELLO,ELIST,(C'P',=C'ACCOUNT '),(R2),(R3)                        
         CLI   ELERR,0                                                          
         BE    XIT                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         EJECT                                                                  
*              ROUTINE TO DELETE AN ELEMENT                                     
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
VDELEL   LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,ELIST,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))          
         SPACE 1                                                                
         CLI   ELERR,0                                                          
         BNE   XIT                                                              
         GOTO1 HELLO,ELIST,(C'D',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
VGETEL   LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,ELIST,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET NAME OUT OF A RECORD                              
*              P1   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 1-3  A(OUTPUT AREA)                                    
         SPACE 1                                                                
VNAMOUT  LM    R2,R3,0(R1)                                                      
         MVC   0(36,R3),SPACES                                                  
         GOTO1 GETL,DMCB,(X'20',(R2)),0                                         
         CLI   ELERR,0                                                          
         BNE   XIT                 NO NAME ELEMENT                              
         L     R4,ELADDR                                                        
         SPACE 1                                                                
         USING ACNAMED,R4                                                       
         ZIC   R5,ACNMLEN                                                       
         LTR   R5,R5                                                            
         BZ    XIT                                                              
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),ACNMNAME                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET AN ADDRESS OUT OF A RECORD                        
*              P1   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 1-3  A(OUTPUT AREA)                                    
         SPACE 1                                                                
VADDROUT LM    R2,R3,0(R1)                                                      
         MVC   0(26*4,R3),SPACES                                                
         GOTO1 GETL,DMCB,(X'22',(R2)),0                                         
         CLI   ELERR,0                                                          
         BNE   XIT                 NO ADDRESS ELEMENT                           
         L     R4,ELADDR                                                        
         SPACE 1                                                                
         USING ACADDD,R4                                                        
         LA    R6,ACADADD                                                       
         ZIC   R5,ACADLNES                                                      
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   0(26,R3),0(R6)                                                   
         LA    R6,26(R6)                                                        
         LA    R3,26(R3)                                                        
         BCT   R5,*-14                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD A NAME ELEMENT                                  
*              P1   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 1-3  A(FIELD HEADER)                                   
         SPACE 1                                                                
VNAMIN   LM    R2,R3,0(R1)                                                      
         GOTO1 DELL,DMCB,(X'20',(R2)),0                                         
         LA    R4,EXPELM                                                        
         XC    EXPELM,EXPELM                                                    
         USING ACNAMED,R4                                                       
         ZIC   R5,5(R3)                                                         
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R5,2(R5)                                                         
         MVI   ACNMEL,X'20'                                                     
         STC   R5,ACNMLEN                                                       
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   ACNMNAME(0),8(R3)                                                
         GOTO1 ADDL,DMCB,(R2),(R4)                                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO BUILD AN ADDRESS ELEMENT                              
*              P1   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 1-3  A(FIELD HEADER)                                   
         SPACE 1                                                                
VADDRIN  LM    R2,R3,0(R1)                                                      
         GOTO1 DELL,DMCB,(X'22',(R2)),0                                         
         LA    R4,EXPELM                                                        
         XC    EXPELM,EXPELM                                                    
         USING ACADDD,R4                                                        
         LA    R5,ACADADD                                                       
         SR    R6,R6                                                            
         LA    R0,4                                                             
         SPACE 1                                                                
VADDRIN2 CLI   5(R3),0                                                          
         BE    VADDRIN4                                                         
         MVC   0(26,R5),SPACES                                                  
         ZIC   R1,5(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),8(R3)                                                    
         LA    R6,1(R6)                                                         
         LA    R5,26(R5)                                                        
VADDRIN3 ZIC   R1,0(R3)            BUMP TO NEXT UNPROTECTED                     
         AR    R3,R1                                                            
         TM    1(R3),X'20'                                                      
         BO    VADDRIN3                                                         
         BCT   R0,VADDRIN2                                                      
         SPACE 1                                                                
VADDRIN4 LTR   R6,R6                                                            
         BZ    XIT                                                              
         MVI   ACADEL,X'22'                                                     
         STC   R6,ACADLNES                                                      
         MH    R6,=H'26'                                                        
         LA    R6,3(R6)                                                         
         STC   R6,ACADLEN                                                       
         GOTO1 ADDL,DMCB,(R2),(R4)                                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD A BALANCE ELEMENT                                 
*              P1   BYTE 1-3  A(RECORD)                                         
*                                                                               
         SPACE 1                                                                
VBALIN   L     R2,0(R1)                                                         
         GOTO1 GETL,DMCB,(X'32',(R2)),0                                         
         CLI   ELERR,0                                                          
         BE    BALIN1              ALREADY HAVE BALANCE ELEMENT                 
         LA    R4,EXPELM                                                        
         XC    EXPELM,EXPELM                                                    
         USING ACBALD,R4                                                        
         MVI   ACBLEL,X'32'                                                     
         MVI   ACBLLEN,ACBLLNQ                                                  
         ZAP   ACBLFRWD,=P'0'                                                   
         ZAP   ACBLDR,=P'0'                                                     
         ZAP   ACBLCR,=P'0'                                                     
         ZAP   ACBLURG,=P'0'                                                    
         GOTO1 ADDL,DMCB,(R2),(R4)                                              
         SPACE 1                                                                
BALIN1   GOTO1 GETL,DMCB,(X'33',(R2)),0                                         
         CLI   ELERR,0                                                          
         BE    BALINX              ALREADY HAVE PEEL ELEMENT                    
         LA    R4,EXPELM                                                        
         XC    EXPELM,EXPELM                                                    
         USING ACPEELD,R4                                                       
         MVC   ACPEEL(2),=X'3314'                                               
         XC    ACPEPLDT(6),ACPEPLDT                                             
         ZAP   ACPEDR,=P'0'                                                     
         ZAP   ACPECR,=P'0'                                                     
         GOTO1 ADDL,DMCB,(R2),(R4)                                              
BALINX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD A STATUS ELEMENT                                  
*              P1   BYTE 1-3  A(RECORD)                                         
         SPACE 1                                                                
         USING ACSTATD,R4                                                       
VSTATIN  L     R2,0(R1)                                                         
         XC    EXPELM,EXPELM                                                    
         LA    R4,EXPELM                                                        
         MVC   ACSTEL(2),=X'3016'                                               
         GOTO1 GETL,DMCB,(X'30',(R2)),0                                         
         CLI   ELERR,0                                                          
         BNE   VSTATIN1            NO STATUS ELEMENT                            
         L     R5,ELADDR                                                        
         ZIC   R1,1(R5)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACSTEL(0),0(R5)     OLD STATUS TO ELEMENT                        
         GOTO1 DELL,DMCB,(X'30',(R2)),0                                         
         B     VSTATIN2                                                         
         SPACE 1                                                                
VSTATIN1 GOTO1 DATCON,DMCB,(5,0),(1,ACSTLAST)                                   
         MVC   ACSTBFDT,ACSTLAST                                                
VSTATIN2 MVI   ACSTANAL,C' '                                                    
         MVC   ACSTFILT,SPACES                                                  
         MVI   ACSTCOST,C' '                                                    
         GOTO1 ADDL,DMCB,(R2),(R4)                                              
         B     XIT                                                              
         EJECT                                                                  
*              OTHER DATA HANDLING ROUTINES                                     
         SPACE 1                                                                
VNUMERIC TM    4(R2),X'08'                                                      
         BO    VPACK                                                            
         MVI   ERROR,NOTNUMRC                                                   
         B     THEEND                                                           
         SPACE 1                                                                
VPACK    SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         SR    R1,R1                                                            
         ZAP   DUB,=P'0'                                                        
         LTR   R3,R3                                                            
         BZ    XITR1                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
XITR1    XIT1  REGS=(R1)                                                        
         EJECT                                                                  
*              VALIDATE AUTHORIZATION                                           
         SPACE 1                                                                
VAUTH    CLC   LSTRECN,RECNUM                                                   
         BNE   VAUTH2          IF RECORD CHANGES CLEAR SAVED STORAGE            
         CLC   LSTACTN,ACTNUM                                                   
         BE    VAUTH3              IF SAME ACTION DON'T CLEAR                   
         CLI   ACTNUM,8            IS ACTION TRANSFER                           
         BNE   VAUTH3              IF NOT DON'T CLEAR                           
VAUTH2   LA    RE,SAVXTWA          IF TRANFER CLEAR SAVED STORAGE               
         L     RF,=AL4(L'SAVXTWA)                                               
         XCEF                                                                   
VAUTH3   MVC   LSTRECN,RECNUM                                                   
         MVC   LSTACTN,ACTNUM                                                   
         MVC   WORK(1),RECNUM                                                   
         MVC   WORK+1(1),ACTEQU                                                 
         LA    R3,AUTHLST                                                       
         CLI   0(R3),X'FF'                                                      
         BE    NOTVLD                                                           
         CLC   0(2,R3),WORK        CHECK RECORD/ACTION COMBINATION              
         BE    *+12                                                             
         LA    R3,3(R3)                                                         
         B     *-22                                                             
         ZIC   R1,2(R3)      AUTHORIZATION BIT REQUIRED FOR REC/ACT             
         EX    R1,TMAUTH                                                        
         BNO   NOTVLD              NOT AUTHORIZED                               
         B     XIT                                                              
TMAUTH   TM    TWAAUTH,0                                                        
         EJECT                                                                  
*              COMMON EXIT ROUTINE                                              
VEXIT    OI    6(R2),X'40'         POSITION CURSOE                              
         CLI   ERROR,X'FE'                                                      
         BNE   EXT5                                                             
         GOTO1 ERREX2                                                           
EXT5     GOTO1 ERREX               PUT OUT SYSTEM MESSAGE                       
         EJECT                                                                  
FINV     MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
         SPACE 1                                                                
NOTAUTH  MVC   CONHEAD(L'MSG3),MSG3                                             
         OI    CONHEADH+6,X'80'                                                 
         OI    CONSERVH+6,X'40'                                                 
         LA    R2,CONSERVH                                                      
         B     XIT                                                              
         SPACE 1                                                                
NOTVLD   MVC   CONHEAD(L'MSG4),MSG4                                             
         LA    R2,CONRECH                                                       
MYEND    MVI   ERROR,X'FE'                                                      
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 EXIT                                                             
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS TABLES ETC                                             
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
TABLES   DS    0A                                                               
         DC    A(RECACT)                                                        
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         SPACE 1                                                                
SYSVCON  DS    0A                                                               
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    V(DUMMY)                                                         
         SPACE 1                                                                
MSG3     DC    C'** ERROR ** PROGRAM NOT AUTHORIZED'                            
MSG4     DC    C'** ERROR ** RECORD/ACTION NOT AUTHORIZED'                      
         SPACE 1                                                                
AUTHLST  DC    AL1(01,01),X'40'    ACCOUNT MAINT X'40' MUST BE ON               
         DC    AL1(01,10),X'40'    ACCOUNT LIST X'40' MUST BE ON                
         DC    AL1(02,01),X'20'    INVOICE MAINT X'20' MUST BE ON               
         DC    AL1(02,10),X'20'    INVOICE LIST X'20' MUST BE ON                
         DC    AL1(02,07),X'10'    INVOICE APPROVE X'10' MUST BE ON             
         DC    AL1(02,08),X'10'    INVOICE TRANSFER X'10' MUST BE ON            
*MN                                                                             
         DC    AL1(01,10),X'80'    ACCOUNT LIST X'40' MUST BE ON                
         DC    AL1(02,10),X'80'    INVOICE LIST X'20' MUST BE ON                
*MN                                                                             
         DC    X'00'                                                            
         SPACE 1                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*               TABLES                                                          
         SPACE 1                                                                
*              DIRECTORY OF RECORDS AND ACTIONS                                 
         SPACE 1                                                                
         ENTRY RECACT                                                           
RECACT   DS    0D                                                               
         SPACE 1                                                                
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
         SPACE 1                                                                
         DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
         DC    X'01',C'ACCOUNT ',AL1(01),X'00CF'                                
         DC    X'01',C'INVOICE ',AL1(02),X'00CF'                                
         EJECT                                                                  
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
         SPACE 1                                                                
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'APPROVE ',AL1(07,07,00)                                  
         DC    X'02',C'TRANSFER',AL1(08,08,00)                                  
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
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
         SPACE 1                                                                
         DC    X'03',AL1(01,01),X'FE01000080',C'    '  ACCOUNT MAINT            
         DC    X'03',AL1(01,10),X'FD02000080',C'    '  ACCOUNT LIST             
         DC    X'03',AL1(02,01),X'FC03000080',C'    '  INVOICE MAINT            
         DC    X'03',AL1(02,10),X'FB04000080',C'    '  INVOICE LIST             
         DC    X'03',AL1(02,07),X'FA05000080',C'    '  INVOICE APPROVE          
         DC    X'03',AL1(02,08),X'F906000080',C'    '  INV. TRANSFER            
         DC    X'FF'                                                            
         EJECT                                                                  
LENSPOOL EQU   SPOOLEND-SPOOLD                                                  
LENGEND  EQU   GENDEND-GEND                                                     
LENSYSD  EQU   SYSEND-SYSD                                                      
LENIOAS  EQU   NIOS*(LIOS+8)                                                    
LENWORK  EQU   LENSPOOL+LENGEND+LENSYSD+LENIOAS                                 
*                                                                               
NIOS     EQU   2                   2 IO AREAS                                   
LIOS     EQU   2000                2000 BYTE IO AREA                            
         EJECT                                                                  
       ++INCLUDE ACEXPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE ACEXPFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 1                                                                
*ACGENBOTH                                                                      
*CTGENFILE                                                                      
*DDCOMFACS                                                                      
*DDCOREQUS                                                                      
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACEXP00   09/11/02'                                      
         END                                                                    
