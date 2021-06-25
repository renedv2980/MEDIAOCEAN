*          DATA SET DDVXREGS   AT LEVEL 080 AS OF 11/28/17                      
*PHASE DDREGSA                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'EQUATE AND PRINT REG VALS - TEST'                               
DDREGS   CSECT                                                                  
         PRINT NOGEN                                                            
R0       EQU   0                   GENERAL                                      
R1       EQU   1                   GENERAL                                      
R2       EQU   2                   MESSAGE POINTER                              
R3       EQU   3                   DUMP STORAGE POINTER                         
R4       EQU   4                   GENERAL                                      
R5       EQU   5                   GENERAL                                      
R6       EQU   6                                                                
R7       EQU   7                                                                
R8       EQU   8                                                                
R9       EQU   9                                                                
RA       EQU   10                                                               
RB       EQU   11                  ABDPLPTR                                     
RC       EQU   12                  BASE                                         
RD       EQU   13                  SAVE AREA                                    
RE       EQU   14                  GENERAL                                      
RF       EQU   15                  GENERAL                                      
         SPACE 1                                                                
         B     14(RF)              SAVE CALLERS REGS                            
         DC    AL1(8)                                                           
         DC    CL8'*DDREGS*'                                                    
         DC    X'00'                                                            
         STM   RE,RC,12(RD)                                                     
         LR    RC,RF               SET R12 AS BASE REG                          
         USING DDREGS,RC                                                        
         LR    RF,RD                                                            
         LA    RD,SAVEAREA                                                      
         ST    RD,8(RF)                                                         
         ST    RF,4(RD)                                                         
         LR    R9,RC                                                            
         AHI   R9,4096                                                          
         USING DDREGS+4096,R9                                                   
         EJECT                                                                  
REGS010  LR    RB,R1                                                            
         USING ABDPL,RB                                                         
         XC    FREEAREA,FREEAREA                                                
         MVC   MYASID,ADPLASID     SAVE ASID                                    
         LA    R2,1                SET MSG001                                   
         MVC   ADPLPAAD,=F'0'      ACCESS LOCATION ZERO                         
         MVC   ADPLDLEN,=H'255'                                                 
         BAS   RE,ACCESS1                                                       
*                                                                               
         MVC   ESSYSYM(31),=CL31'ASCB00000'                                     
         LA    R1,ESSYSYM+4                                                     
         EDIT  (B2,MYASID),(5,0(R1)),FILL=0,ZERO=NOBLANK                        
         BAS   RE,GETSYM1                                                       
         ICM   RF,15,ESSYLAD       GET A(ASCB)                                  
         ST    RF,AASCB                                                         
         GOTO1 EQUATE,DMCB,=CL8'MYASCB00',(RF)                                  
*                                                                               
         MVC   ADPLPAAD,AASCB      GET MY ASCB                                  
         MVC   ADPLDLEN,=H'248'                                                 
         MVC   ADPLASID,=H'0001'                                                
         BAS   RE,ACCESS1                                                       
         LR    R3,R1                                                            
*NOP     LR    RF,R3                                                            
*NOP     GOTO1 EQUATE,DMCB,=CL8'MYASCB',(RF)                                    
         USING ASCB,R3                                                          
*                                                                               
         MVC   ADPLPAAD,ASCBASXB   FIND ASXB                                    
         L     RF,ADPLPAAD                                                      
         GOTO1 EQUATE,DMCB,=CL8'MYASXB',(RF)                                    
         MVC   ADPLDLEN,=H'255'                                                 
         BAS   RE,ACCESS                                                        
         LR    R3,R1                                                            
*NOP     LR    RF,R3                                                            
*NOP     GOTO1 EQUATE,DMCB,=CL8'MYASXB',(RF)                                    
*                                                                               
         MVC   ADPLPAAD,8(R3)      FIND TCB FOR ASCB                            
         L     RF,ADPLPAAD                                                      
         GOTO1 EQUATE,DMCB,=CL8'MYTCB',(RF)                                     
         MVC   ATCB,8(R3)                                                       
         MVC   ADPLDLEN,=H'360'                                                 
         BAS   RE,ACCESS                                                        
         LR    R3,R1                                                            
         USING TCB,R3                                                           
*NOP     LR    RF,R3                                                            
*NOP     GOTO1 EQUATE,DMCB,=CL8'MYTCB',(RF)                                     
*                                                                               
REGS014  ICM   RF,15,TCBRTWA       FIND RTM2WA                                  
         JNZ   REGS015                                                          
         ICM   RF,15,TCBBACK       OR BACK ONE TCB                              
         ST    RF,ADPLPAAD                                                      
         ST    RF,ATCB                                                          
         MVC   ADPLDLEN,=H'360'                                                 
         BAS   RE,ACCESS                                                        
         LR    R3,R1                                                            
         J     REGS014                                                          
*                                                                               
REGS015  GOTO1 EQUATE,DMCB,=CL8'RTM2WA',(RF)                                    
         MVC   ADPLPAAD,TCBRTWA                                                 
         MVC   ADPLDLEN,=H'872'                                                 
         BAS   RE,ACCESS                                                        
*                                                                               
         EJECT                                                                  
********************************************************                        
*        EQUATE REGISTER VALUES AND PSWADR             *                        
********************************************************                        
         SPACE 1                                                                
         LR    R3,R1                                                            
*        USING RTM2WA,R3                                                        
         TM    124(R3),X'80'       TEST FOR EXTENDED ADDRESSING                 
         BNO   *+8                                                              
         MVI   XADR,C'Y'                                                        
*                                                                               
         LA    R0,16               16 REGS                                      
         MVC   WORK,=CL8'R0'                                                    
         LA    R4,60(R3)           RTM2EREG                                     
REGS020  L     RF,0(R4)                                                         
         CLI   XADR,C'Y'                                                        
         BE    *+8                                                              
         LA    RF,0(RF)            CLEAR MSBS                                   
         LPR   RF,RF               ALWAYS CLEAR 80 BIT                          
         GOTO1 EQUATE,DMCB,WORK,(RF)                                            
         LA    R4,4(R4)                                                         
         CLI   WORK+1,X'F9'        REG A FOLLOWS REG 9                          
         BNE   *+8                                                              
         MVI   WORK+1,X'C0'                                                     
         IC    R1,WORK+1                                                        
         LA    R1,1(R1)            BUMP REGISTER                                
         STC   R1,WORK+1                                                        
         BCT   R0,REGS020                                                       
*                                  R4=PSW                                       
         L     RF,4(R4)            LOAD RF WITH PSWADR                          
         CLI   XADR,C'Y'                                                        
         BE    *+8                                                              
         LA    RF,0(RF)            CLEAR MSBS                                   
         LPR   RF,RF               ALWAYS CLEAR 80 BIT                          
         ST    RF,PSWADR                                                        
         GOTO1 EQUATE,DMCB,=CL8'PSWADR',(RF)                                    
*                                                                               
         EJECT                                                                  
********************************************************                        
*        HEXOUT REGISTER VALUES PSW AND PROG @ RB      *                        
********************************************************                        
         SPACE 1                                                                
         MVC   MESSAGE+1(L'MESSAGE-1),SPACES                                    
         MVI   MESSAGE,132                                                      
*                                                                               
         MVI   PASS,0                                                           
         LA    R4,60(R3)           RTM2EREG                                     
         MVC   REGB,104(R3)        RTMER11                                      
         MVC   REGD,112(R3)        RTMER13                                      
REGS030  SR    RF,RF                                                            
         IC    RF,PASS                                                          
         LA    R1,1(RF)            BUMP PASS                                    
         STC   R1,PASS                                                          
         MH    RF,=H'6'                                                         
         EX    0,REGSMSG(RF)       GET MESSAGE FOR THIS PASS                    
         LA    R5,MESSAGE+17                                                    
         LA    R0,4                4 REGS                                       
REGS031  GOTO1 =V(HEXOUT),DMCB,(R4),(R5),4                                      
         LA    R4,4(R4)                                                         
         LA    R5,10(R5)                                                        
         BCT   R0,REGS031                                                       
         LA    R1,MESSAGE                                                       
         BAS   RE,PRINT                                                         
         CLI   PASS,4                                                           
         BNE   REGS030                                                          
*                                                                               
REGS040  MVC   BYTE,126(R3)        TEST FOR AR MODE                             
         NI    BYTE,X'C0'                                                       
         CLI   BYTE,X'40'                                                       
         BNE   REGS080                                                          
*                                                                               
         MVC   MESSAGE+1(L'MESSAGE-1),SPACES                                    
         LA    R1,MESSAGE                                                       
         BAS   RE,PRINT                                                         
         MVC   MESSAGE+1(8),=C'Access  '                                        
         LA    R1,MESSAGE                                                       
         BAS   RE,PRINT                                                         
*                                                                               
         MVI   PASS,0                                                           
         LA    R4,708(R3)          RTM2ARE0                                     
REGS041  SR    RF,RF                                                            
         IC    RF,PASS                                                          
         LA    R1,1(RF)            BUMP PASS                                    
         STC   R1,PASS                                                          
         MH    RF,=H'6'                                                         
         EX    0,REGSMSG(RF)       GET MESSAGE FOR THIS PASS                    
         LA    R5,MESSAGE+17                                                    
         LA    R0,4                4 REGS                                       
REGS042  GOTO1 =V(HEXOUT),DMCB,(R4),(R5),4                                      
         LA    R4,4(R4)                                                         
         LA    R5,10(R5)                                                        
         BCT   R0,REGS042                                                       
         LA    R1,MESSAGE                                                       
         BAS   RE,PRINT                                                         
         CLI   PASS,4                                                           
         BNE   REGS041                                                          
*                                                                               
REGS080  LA    R4,124(R3)                                                       
         MVC   MESSAGE+1(L'MESSAGE-1),SPACES                                    
         LA    R1,MESSAGE                                                       
         BAS   RE,PRINT                                                         
         MVC   MESSAGE+1(16),=C'           PSW= '                               
         LA    R5,MESSAGE+17                                                    
         LA    R0,4                                                             
REGS082  GOTO1 =V(HEXOUT),DMCB,(R4),(R5),4                                      
         LA    R4,4(R4)                                                         
         LA    R5,10(R5)                                                        
         BCT   R0,REGS082                                                       
*                                                                               
         LA    R1,MESSAGE                                                       
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   MESSAGE+1(L'MESSAGE-1),SPACES                                    
         LA    R1,MESSAGE                                                       
         BAS   RE,PRINT                                                         
*                                                                               
         L     R1,PSWADR           CALC PSW-RB                                  
         S     R1,REGB                                                          
         ST    R1,FULL                                                          
         GOTO1 =V(HEXOUT),DMCB,FULL+1,DMPMSG1+24,3                              
*                                                                               
         MVC   ADPLPAAD,REGB       ACCESS REGB LOCATION                         
         MVC   ADPLDLEN,=H'255'                                                 
         BAS   RE,ACCESS                                                        
         LR    R3,R1                                                            
*                                                                               
         MVC   DMPMSG1+12(8),22(R3)                                             
*                                                                               
         CLC   16(4,R3),=X'A7F40006'                                            
         JNE   *+10                                                             
         MVC   DMPMSG1+12(8),20(R3)                                             
*                                                                               
         MVI   MESSAGE,40                                                       
         MVC   MESSAGE+1(L'DMPMSG1),DMPMSG1                                     
         LA    R1,MESSAGE                                                       
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   MESSAGE+1(L'MESSAGE-1),SPACES                                    
         LA    R1,MESSAGE                                                       
         BAS   RE,PRINT                                                         
*                                                                               
         EJECT                                                                  
********************************************************                        
*        LOCATE AND HEXOUT CALLERS REGS                *                        
********************************************************                        
         SPACE 1                                                                
         MVC   MESSAGE+1(L'MESSAGE-1),SPACES                                    
         MVI   MESSAGE,132                                                      
*                                                                               
         MVC   ADPLPAAD,REGD       ACCESS REGD LOCATION                         
         MVC   OLDRD,REGD                                                       
         MVC   ADPLDLEN,=H'255'                                                 
         BAS   RE,ACCESS                                                        
         LR    R3,R1                                                            
         MVC   REGD,4(R3)          SAVE NEXT RD                                 
*                                                                               
SAVE040  MVC   ADPLPAAD,REGD       ACCESS REGD LOCATION                         
         MVC   ADPLDLEN,=H'255'                                                 
         BAS   RE,ACCESS                                                        
         LR    R3,R1                                                            
         MVC   REGD,4(R3)          SAVE NEXT RD                                 
         CLC   OLDRD,8(R3)         TEST FORWARD POINTER                         
         JNE   EXIT                NO MATCH THEN WE'RE DONE                     
         MVC   OLDRD,ADPLPAAD                                                   
*                                                                               
         MVI   PASS,0                                                           
         LR    R4,R3               SAVE REGISTER AREA                           
         MVC   SAVEREGS(52),20(R4)                                              
         MVC   SAVEREGS+52(4),4(R4)                                             
         MVC   SAVEREGS+56(8),12(R4)                                            
*                                                                               
         MVC   MESSAGE+1(L'MESSAGE-1),SPACES                                    
         MVI   MESSAGE,132                                                      
         LA    R1,MESSAGE                                                       
         BAS   RE,PRINT                                                         
*                                                                               
         L     R1,SAVEREGS+(4*RE)    CALC RE-RB                                 
         S     R1,SAVEREGS+(4*RB)    RB                                         
         ST    R1,FULL                                                          
         GOTO1 =V(HEXOUT),DMCB,FULL+1,DMPMSG2+24,3                              
*                                                                               
         MVC   ADPLPAAD,SAVEREGS+(4*RB)   REGB LOCATION                         
         MVC   ADPLDLEN,=H'255'                                                 
         BAS   RE,ACCESS                                                        
         LR    R3,R1                                                            
*                                                                               
         MVC   DMPMSG2+12(8),22(R3)                                             
*                                                                               
         CLC   16(4,R3),=X'A7F40006'                                            
         JNE   *+10                                                             
         MVC   DMPMSG2+12(8),20(R3)                                             
*                                                                               
         MVI   MESSAGE,40                                                       
         MVC   MESSAGE+1(L'DMPMSG2),DMPMSG2                                     
         LA    R1,MESSAGE                                                       
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   MESSAGE+1(L'MESSAGE-1),SPACES                                    
         MVI   MESSAGE,132                                                      
         LA    R1,MESSAGE                                                       
         BAS   RE,PRINT                                                         
*                                                                               
         LA    R4,SAVEREGS                                                      
*                                                                               
SAVE041  SR    RF,RF                                                            
         IC    RF,PASS                                                          
         LA    R1,1(RF)            BUMP PASS                                    
         STC   R1,PASS                                                          
         MH    RF,=H'6'                                                         
         EX    0,REGSMSG(RF)      GET MESSAGE FOR THIS PASS                     
         LA    R5,MESSAGE+17                                                    
         LA    R0,4                4 REGS                                       
SAVE042  GOTO1 =V(HEXOUT),DMCB,(R4),(R5),4                                      
         LA    R4,4(R4)                                                         
         LA    R5,10(R5)                                                        
         BCT   R0,SAVE042                                                       
         LA    R1,MESSAGE                                                       
         BAS   RE,PRINT                                                         
         CLI   PASS,4                                                           
         BNE   SAVE041                                                          
*                                                                               
         MVC   ADPLPAAD,SAVEREGS+(4*R1)   REG1 LOCATION                         
         MVC   ADPLDLEN,=H'255'                                                 
         BAS   RE,ACCESS                                                        
         LR    R3,R1                                                            
         MVC   SAVPARMS(6*4),0(R3)                                              
*                                                                               
         MVC   MESSAGE+1(L'MESSAGE-1),SPACES                                    
         MVI   MESSAGE,132                                                      
         LA    R1,MESSAGE                                                       
         BAS   RE,PRINT                                                         
*                                                                               
         MVI   PASS,0                                                           
*                                                                               
SAVE141  SR    RF,RF                                                            
         IC    RF,PASS                                                          
         LR    R1,RF                                                            
         SLL   R1,2                                                             
         L     R4,SAVPARMS(R1)                                                  
         ST    R4,FULL                                                          
         LA    R1,1(RF)            BUMP PASS                                    
         STC   R1,PASS                                                          
         MH    RF,=H'6'                                                         
         EX    0,PARMMSG(RF)      GET MESSAGE FOR THIS PASS                     
         LA    R5,MESSAGE+17                                                    
         GOTO1 =V(HEXOUT),DMCB,FULL,(R5),4                                      
*                                                                               
         MVC   MESSAGE+60(16),=C'                '                              
         MVC   MESSAGE+27(32),SPACES                                            
         MVC   ADPLPAAD,FULL                                                    
         MVI   ADPLPAAD,0                                                       
         MVC   ADPLDLEN,=H'16'                                                  
         BAS   RE,ACCESS                                                        
         LTR   R3,R1                                                            
         JZ    SAVE144                                                          
         OC    FULL,FULL                                                        
         JZ    SAVE144                                                          
         MVC   MESSAGE+60(16),0(R3)                                             
         LA    R5,MESSAGE+27                                                    
         GOTO1 =V(HEXOUT),DMCB,(R3),(R5),16                                     
         J     SAVE145                                                          
*                                                                               
SAVE144  MVC   MESSAGE+27(6),=C'(....)'                                         
         MVC   MESSAGE+28(4),FULL                                               
*                                                                               
SAVE145  LA    R1,MESSAGE                                                       
         BAS   RE,PRINT                                                         
         CLI   PASS,6                                                           
         BNE   SAVE141                                                          
*                                                                               
SAVE150  CLC   REGD,=X'00000000'                                                
         JNE   SAVE040                                                          
*                                                                               
EXIT     XIT1  REGS=(RF)                                                        
*                                                                               
REGSMSG  MVC   MESSAGE+1(15),=C'Registers  0-3  '                               
         MVC   MESSAGE+1(15),=C'           4-7  '                               
         MVC   MESSAGE+1(15),=C'           8-B  '                               
         MVC   MESSAGE+1(15),=C'           C-F  '                               
*                                                                               
PARMMSG  MVC   MESSAGE+1(15),=C'Parms      P-1  '                               
         MVC   MESSAGE+1(15),=C'           P-2  '                               
         MVC   MESSAGE+1(15),=C'           P-3  '                               
         MVC   MESSAGE+1(15),=C'           P-4  '                               
         MVC   MESSAGE+1(15),=C'           P-5  '                               
         MVC   MESSAGE+1(15),=C'           P-6  '                               
*                                                                               
         EJECT                                                                  
********************************************************                        
*              GET SYMBOL                              *                        
********************************************************                        
         SPACE 1                                                                
GETSYM   MVC   ESSYSYM(31),=CL31'ASCB00001'                                     
GETSYM1  ST    RE,SAVERE                                                        
         MVI   ESSYDTY,ZZZDTYM                                                  
         MVC   ESSYDTD(31),=CL31'ASCB'                                          
         GOTO1 ADPLSERV,DMCB,ABDPL,A(CODEGTS),ESSY                              
         CH    RF,=H'12'                                                        
         BNL   ERROR                                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
********************************************************                        
*      EQUATE SYMBOL TO ADDRESS                        *                        
*      P1=A(CL8'SYMBOL'),P2=A(ADDRESS)                 *                        
********************************************************                        
         SPACE 1                                                                
EQUATE   ST    RE,SAVERE                                                        
         MVC   ESSYAST,=C'CV'                                                   
         LH    RE,MYASID                                                        
         ST    RE,ESSYAS2                                                       
         MVC   ESSYDOF,=F'0'                                                    
         MVC   ESSYDLE,=F'4'                                                    
         MVC   ESSYLAD,4(R1)                                                    
         MVI   ESSYSYM,C' '                                                     
         MVC   ESSYSYM+1(L'ESSYSYM-1),ESSYSYM                                   
         L     R1,0(R1)                                                         
         MVC   ESSYSYM(8),0(R1)                                                 
         MVI   ESSYDTY,ZZZDTYU                                                  
         MVC   ESSYDTD,ESSYSYM                                                  
         GOTO1 ADPLSERV,DMCB,ABDPL,A(CODEEQS),ESSY                              
         CH    RF,=H'12'                                                        
         BNL   EXIT                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
********************************************************                        
*      PRINT ERROR OR INFO MESSAGE                     *                        
********************************************************                        
         SPACE 1                                                                
ERROR    SR    RE,RE               BRANCH TO ERROR (ERROR WILL EXIT)            
INFO     ST    RE,SAVERE           BAL RE TO INFO  (INFO WILL RETURN)           
         SR    R0,R0                                                            
         LR    R1,R2                                                            
         L     RF,AMSGTAB                                                       
         LTR   R1,R1                                                            
         BZ    ERRMSG                                                           
MSGO010  ICM   R0,1,0(RF)          BUMP DOWN MSG TABLE                          
         BZ    ERRMSG                                                           
         AR    RF,R0                                                            
         BCT   R1,MSGO010          UNTIL I GET THE MESSAGE                      
         B     MSGOUT                                                           
*                                                                               
ERRMSG   L     RF,AMSGTAB                                                       
*                                                                               
MSGOUT   L     R1,ADPLBUF          CLEAR ADPLBUF TO BLANKS                      
         MVI   0(R1),C' '                                                       
         MVC   1(132,R1),0(R1)                                                  
         SR    RE,RE                                                            
         IC    RE,0(RF)                                                         
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8              MOVE IN MESSAGE                              
         B     *+10                                                             
         MVC   1(0,R1),1(RF)                                                    
         GOTO1 ADPLSERV,DMCB,ABDPL,A(CODEPRT)                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                GIVE UP                                      
         ICM   RE,15,SAVERE                                                     
         BNZR  RE                                                               
         B     EXIT                EXIT IF SAVERE=0                             
         EJECT                                                                  
********************************************************                        
*      GET STORAGE FROM DUMP A(ADPLPAAD) L'ADPLDLEN    *                        
********************************************************                        
         SPACE 1                                                                
ACCESS   ICM   R1,15,FREEAREA                                                   
         JZ    ACCESS0                                                          
         L     R0,FREELV                                                        
         FREEMAIN R,LV=(0),A=(1)                                                
ACCESS0  MVC   ADPLASID,MYASID                                                  
ACCESS1  ST    RE,SAVERE                                                        
         GOTO1 ADPLSERV,DMCB,ABDPL,A(CODEACC),ADPLPACC                          
         LTR   RF,RF                                                            
         BNZ   ACCESSE                                                          
         L     R1,ADPLPSOL                                                      
         ST    R1,FREEAREA                                                      
         LH    R0,ADPLOSSZ-ADPLOSEL(,R1)                                        
         ICM   R0,8,ADPLSBPL                                                    
         ST    R0,FREELV                                                        
*                                                                               
         L     R1,ADPLPART                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
ACCESSE  XC    FREEAREA,FREEAREA                                                
         XC    FREELV,FREELV                                                    
         XR    R1,R1                                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
********************************************************                        
*      GET ASID FOR CURRENTLY ACTIVE ASCB              *                        
********************************************************                        
         SPACE 1                                                                
GETASID  ST    RE,SAVERE                                                        
         MVI   ADPLPSF1,ADPLPSCR                                                
         GOTO1 ADPLSERV,DMCB,ABDPL,A(CODESEL),ADPLPSEL                          
         CH    RF,=H'8'                                                         
         BNL   ERROR               CAN'T FIND CURRENT ACTIVE ASCB               
         L     R1,ADPLPSOL                                                      
         LH    R0,ADPLOSSZ-ADPLOSEL(,R1)                                        
         ICM   R0,8,ADPLSBPL                                                    
         LH    RF,ADPLOSCT-ADPLOSEL(,R1)                                        
         CH    RF,=H'1'                                                         
         BNE   ERROR               MORE THAN 1 ACTIVE CPU                       
*                                                                               
         LA    RF,ADPLOSLL(,R1)                                                 
         MVC   MYASID,ADPLOSAI-ADPLOSNT(RF)                                     
         MVC   AASCB,ADPLOSAP-ADPLOSNT(RF)                                      
         FREEMAIN R,LV=(0),A=(1)                                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
********************************************************                        
*     XPRINT AND PRINT SERVICE CALLS                   *                        
********************************************************                        
         SPACE 1                                                                
XPRINT   L     R1,ADPLBUF                                                       
         ST    R1,PPR2BUF                                                       
         MVC   PPR2BUFL,=A(132)                                                 
         MVC   0(132,R1),MESSAGE                                                
         MVI   PPR2PFL1,PPR2MSG                                                 
         L     RF,ADPLSERV                                                      
         LA    R1,DMCB                                                          
         LA    RE,ABDPL                                                         
         ST    RE,0(R1)                                                         
         LA    RE,CODEPR2                                                       
         ST    RE,4(R1)                                                         
         LA    RE,PPR2                                                          
         ST    RE,8(R1)                                                         
         BASR  RE,RF                                                            
         LTR   RF,RF                                                            
         B     EXIT                                                             
*                                                                               
PRINT    ST    RE,SAVERE           PRINT TEXT AT R1+1 FOR LEN OF 0(R1)          
         L     RF,ADPLBUF                                                       
         MVI   0(RF),C' '                                                       
         MVC   1(132,RF),0(RF)                                                  
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),1(R1)                                                    
         L     RF,ADPLSERV                                                      
         LA    R1,DMCB                                                          
         LA    RE,ABDPL                                                         
         ST    RE,0(R1)                                                         
         LA    RE,CODEPRT                                                       
         ST    RE,4(R1)                                                         
         BASR  RE,RF                                                            
         LTR   RF,RF                                                            
         BNZ   ERR2                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
         XIT1                                                                   
         EJECT                                                                  
********************************************************                        
*           CONSTANTS AND SAVE AREAS                   *                        
********************************************************                        
         SPACE 1                                                                
SAVEAREA DS    100D                                                             
SAVEREGS DS    16F                                                              
SAVPARMS DS    6F                                                               
FREEAREA DS    F                                                                
FREELV   DS    F                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
SAVERE   DS    AL4                                                              
DMCB     DS    8F                                                               
PARMS    DS    8F                                                               
PSWADR   DS    F                                                                
REGB     DS    F                                                                
REGD     DS    F                                                                
OLDRD    DS    F                                                                
WORK     DS    CL32                                                             
FLAG     DS    X                                                                
XADR     DS    X                                                                
PASS     DS    X                                                                
MESSAGE  DS    CL133                                                            
MESSAGE1 DS    CL133                                                            
*                                                                               
MYASID   DC    H'0'                                                             
AASCB    DS    A(0)                                                             
ATCB     DS    A(0)                                                             
AMSGTAB  DC    A(MSGTAB)                                                        
ASYMPTR  DC    A(0)                                                             
SPACES   DC    132C' '                                                          
         LTORG                                                                  
         SPACE 2                                                                
CODEEQS  DC    A(ADPLSEQS)                                                      
CODEPRT  DC    A(ADPLSPRT)                                                      
CODEGTS  DC    A(ADPLSGTS)                                                      
CODEPR2  DC    A(ADPLSPR2)                                                      
CODEACC  DC    A(ADPLSACC)                                                      
CODESEL  DC    A(ADPLSSEL)                                                      
*                                                                               
ERR1     EQU   *                                                                
ERR2     EQU   *                                                                
ERRX     LA    RF,4                ONLY POSSIBLE ERROR                          
         XIT1  REGS=(RF)                                                        
*                                                                               
ESSY     BLSRESSY DSECT=NO                                                      
*                                                                               
PPR2     BLSUPPR2 DSECT=NO                                                      
*                                                                               
         BLSABDPL DSECT=NO,AMDEXIT=NO,AMDOSEL=NO,                      X        
               AMDPACC=YES,AMDPFMT=NO,AMDPECT=NO,AMDPSEL=YES                    
         EJECT                                                                  
********************************************************                        
*                   MESSAGES                           *                        
********************************************************                        
         SPACE 1                                                                
DMPMSG1  DC    CL40'YOU DIED IN ******** AT XXXXXX'                             
DMPMSG2  DC    CL40'CALLED BY   ******** AT XXXXXX'                             
         SPACE 1                                                                
MSGTAB   DS    0CL1                                                             
MSG000   DC    AL1(MSG001-*),C'UNSPECIFIED ERROR MESSAGE'                       
MSG001   DC    AL1(MSG002-*),C'UNABLE TO LOCATE ACTIVE ASCB'                    
MSG002   DC    AL1(MSG003-*),C'UNABLE TO LOCATE STORAGE'                        
MSG003   DC    AL1(MSG00X-*),C'UNABLE TO LOCATE SYSFACS'                        
MSG00X   DC    AL1(0)                                                           
         EJECT                                                                  
         BLSABDPL AMDPACC=NO,AMDPFMT=NO,AMDPECT=NO,AMDPSEL=NO                   
         EJECT                                                                  
         IHAASCB                                                                
*        IHARTM2A                                                               
         IKJTCB LIST=YES                                                        
         IEFZB4D0                                                               
         IEFZB4D2                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'080DDVXREGS  11/28/17'                                      
         END                                                                    
