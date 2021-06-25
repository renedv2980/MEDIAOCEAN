*          DATA SET SRLCK00A   AT LEVEL 049 AS OF 05/01/02                      
*PHASE T15C00A                                                                  
         SPACE 1                                                                
***********************************************************************         
* THIS SERVICE REQUEST DISPLAYS DATA ABOUT WHICH TASKS ARE WAITING    *         
* FOR LOCKS TO BE RELEASED, AND WHAT FILE/RECORD THOSE TASKS ARE      *         
* WAITING ON, AND IDENTIFIES THE OWNER OF THE LOCK.                   *         
*                                                                     *         
* P1 = X                   DISPLAY ALL TCBS (LOCKED OR NOT)           *         
*                                                                     *         
* REGISTER USAGE:                                                     *         
*      R1:  WORK                                                      *         
*      R2:  POINTER TO SRLLSTD                                        *         
*      R3:  POINTER TO SRLCKFFD                                       *         
*      R4:  WORK                                                      *         
*      R7:  WORK                                                      *         
*      R6:  WORK                                                      *         
*      R5:  POINTER TO TCB                                            *         
*      R8:  POINTER TO TIA                                            *         
*      R9:  SECOND BASE                                               *         
*      RA:  POINTER TO SYSFACD                                        *         
*      RB:  BASE                                                      *         
*      RC:  POINTER TO WORKING STORAGE                                *         
***********************************************************************         
         TITLE '$LOCK - DISPLAY LOCK DATA'                                      
         PRINT NOGEN                                                            
LOCK     CSECT                                                                  
         NMOD1 WORKL,**$LCK**,R9,CLEAR=YES,RR=R4                                
         USING WORKD,RC                                                         
         ST    R4,RELO                                                          
         ST    RD,BASERD                                                        
*                                                                               
         USING SRPARMD,R1          R8=A(S/R PARAM LIST)                         
*                                                                               
         L     RA,SRPARM1                                                       
         USING SYSFACD,RA          RA=A(SYS FAC LIST)                           
*                                                                               
         XC    ALET,ALET                                                        
         L     RF,VSSB                                                          
         USING SSBD,RF                                                          
         TM    SSBSTAT4,SSBDSLCK   LOCKET IN DSPACE?                            
         BZ    *+10                NO                                           
         MVC   ALET,SSBALET                                                     
         DROP  RF                                                               
*                                                                               
         L     R3,SRPARM6                                                       
         USING T15CFFD,R3          R3=A(TWA)                                    
*                                                                               
         L     R4,SRPARM4                                                       
         ST    R4,VCOMFACS                                                      
         DROP  R1                                                               
*                                                                               
MAIN     DS    0H                                                               
         LA    R2,SRLDAT1H                                                      
         USING SRLLSTD,R2                                                       
         XC    SRLMSG,SRLMSG                                                    
         USING TCBD,R5                                                          
         L     R5,VTCB             TABLE BEGINNING                              
         LH    R6,0(R5)            ENTRY LENGTH                                 
         L     R7,2(R5)            END OF TABLE                                 
         LA    R5,6(R5)            FIRST ENTRY IN TABLE                         
M10      DS    0H                                                               
         CLI   COUNT,8             ONLY 8 LINES PER SCREEN                      
         BH    MX                  EXIT                                         
         BAS   RE,LIST                                                          
         BNE   M20                                                              
*                                                                               
         OI    6(R2),X'80'         XMIT FIELD                                   
         ZIC   R1,0(R2)            PT @NXT FIELD                                
         AR    R2,R1                                                            
         OI    6(R2),X'80'         XMIT KEY/DA LINE                             
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
*                                                                               
         IC    R1,COUNT            BUMP COUNT                                   
         LA    R1,1(R1)                                                         
         STC   R1,COUNT                                                         
*                                                                               
M20      BXLE  R5,R6,M10                                                        
MX       DS    0H                                                               
         OI    SRLP1H+6,X'40'      POSN CURSOR ON P1 FIELD                      
         XMOD1                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* LIST TASK AND TASK WHICH IT IS LOCKED ON.  EXIT CC NEQ IF NOTHING             
* TO DISPLAY                                                                    
***********************************************************************         
         SPACE 1                                                                
LIST     NTR1  ,                                                                
         CLI   TCBSYS,0            IS TASK ACTIVE?                              
         BE    NEQXIT                                                           
         CLI   SRLP1,C'X'          PARM 1 - SHOW ALL TASKS                      
         BE    L10                                                              
         L     R1,TCBLOCK                                                       
         C     R1,VTCB             LT 1ST TABLE ENTRY?                          
         BL    NEQXIT                                                           
         CR    R1,R7               GT LAST TABLE ENTRY?                         
         BH    NEQXIT                                                           
*         LTR   R1,R1                                                           
*         BZ    NEQXIT                                                          
* DISP TASK                                                                     
L10      DS    0H                                                               
         MVI   LSTTASK,C'T'                                                     
         MVC   LSTTASK+1(1),TCBID+6 DISP TASK #                                 
         LA    R1,LSTSYS1          PASS DISPSYS OUTPUT ADDR                     
         BAS   RE,DISPSYS                                                       
* DISP DURATION                                                                 
         GOTO1 VTICTOC,DMCB,C'TGET' GET TIME OF DAY IN TU'S                     
         MVC   TIMENOW,DMCB                                                     
         L     R1,TIMENOW                                                       
         S     R1,TCBSTTM                                                       
         AH    R1,=H'5'                                                         
         XR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(5,LSTDURTN),1                                              
* DISP TASK WAITING ON                                                          
         LR    R8,R5               SAVE R5                                      
         L     R5,TCBLOCK          TASK THAT IS LOCKING RECORD                  
         MVI   LSTON,C'T'                                                       
*         LTR   R5,R5                                                           
*         BNZ   L20                                                             
         C     R5,VTCB             LT 1ST TASK?                                 
         BL    L20                                                              
         CR    R5,R7               GT LAST TASK?                                
         BNH   L30                                                              
L20      DS    0H                                                               
         LR    R5,R8               RESTORE R5                                   
         MVI   LSTON+1,C'*'                                                     
         MVC   LSTSYS2,=C'***/***'                                              
         BAS   RE,GETPARM          *** NEW ***                                  
*         MVC   LSTCMD,=C'******'                                               
*         MVC   LSTFILE,=C'******'                                              
         B     L40                                                              
*                                                                               
L30      DS    0H                                                               
         MVC   LSTON+1(1),TCBID+6  SAVE LOCKED-ON TASK                          
         LA    R1,LSTSYS2          SET OUTPUT ADDR FOR DISPSYS                  
         BAS   RE,DISPSYS                                                       
         LR    R5,R8               RESTORE R5 (IF NOT ABOVE)                    
         BAS   RE,GETPARM                                                       
* DISP A(TWA)                                                                   
L40      DS    0H                                                               
         L     RF,VCOMFACS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         LA    R0,TCBTWA+1                                                      
         GOTO1 (RF),DMCB,(R0),LSTTWA,3,=C'TOG'                                  
*                                                                               
* DISP DATAMGR KEY/DA                                                           
         ZIC   R1,0(R2)            PT @NEXT FLD                                 
         AR    R2,R1                                                            
         OC    KEY,KEY                                                          
         BNZ   L50                                                              
         MVI   8(R2),C'.'                                                       
         MVC   9(SRLLSTX-9,R2),8(R2)                                            
         B     LX                                                               
L50      DS    0H                                                               
         LA    R3,8(R2)                                                         
         GOTO1 (RF),DMCB,KEY,(R3),L'KEY,=C'TOG'                                 
LX       DS    0H                                                               
         B     EQXIT                                                            
         EJECT                                                                  
*                                                                               
GETPARM  NTR1                                                                   
* GO FIND DATAMGR CALL PARAMS                                                   
         L     R4,TCBSVRD          A(NEXT WORK AREA)                            
         LTR   R4,R4                                                            
         BNZ   DMLP                                                             
         MVC   LSTCMD,=C'*RD=0*'                                                
         MVC   LSTFILE,=C'*RD=0*'                                               
         XC    KEY,KEY                                                          
         B     EXIT                                                             
DMLP     DS    0H                                                               
         CLC   =C'MNTR',0(R4)      0(R4)=NAME                                   
         BNE   DL10                                                             
         MVC   LSTCMD,=C'NOCALL'                                                
         MVC   LSTFILE,=C'NOCALL'                                               
         XC    KEY,KEY                                                          
         B     EXIT                                                             
DL10     DS    0H                                                               
         CLC   VDATAMGR,16(R4)                                                  
         BE    GOTDMGR                                                          
         L     R4,4(R4)            4(R4)=BACK PTR                               
         B     DMLP                                                             
*                                                                               
GOTDMGR  DS    0H                                                               
         L     R1,24(R4)           PT R1@DMCB TO DATAMGR {24(R4)=R1}            
* DISP DATAMGR COMMAND (THIS TASK)                                              
         L     R4,0(R1)                                                         
         MVC   LSTCMD,0(R4)                                                     
* DISP DATAMGR FILE (THIS TASK)                                                 
         L     R4,4(R1)                                                         
         MVC   LSTFILE,0(R4)                                                    
* SAVE KEY PASSED TO DATAMGR                                                    
         XC    KEY,KEY                                                          
         L     R4,8(R1)                                                         
         CLC   =C'REC',LSTCMD+3    GETREC, PUTREC, OR ADDREC?                   
         BE    DL20                                                             
         MVC   KEY,0(R4)           DISP FULL KEY                                
         B     EXIT                                                             
DL20     DS    0H                  ONLY DISPLAY ADDR                            
         MVC   KEY(6),0(R4)                                                     
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
* DISP SYS/PROG FOR TASK                                                        
DISPSYS  NTR1                                                                   
         LR    R2,R1                                                            
         L     R4,VSELIST          SYSTEM NAME TBL                              
         LH    R6,0(R4)            ENTRY LENGTH                                 
         L     R7,2(R4)            LAST ENTRY ADDR                              
         LA    R4,6(R4)            FIRST ENTRY                                  
         USING SELISTD,R4                                                       
SYSSCAN  CLC   SESYS,TCBOVSYS                                                   
         BE    GOTSYS                                                           
         BXLE  R4,R6,SYSSCAN                                                    
         MVC   0(7,R2),=C'.../...' BAD SYSTEM (NO PROG)                         
         B     EXIT                                                             
*                                                                               
GOTSYS   DS    0H                                                               
         MVC   0(3,R2),SENAME                                                   
         MVI   3(R2),C'/'                                                       
*                                                                               
         L     R1,SEPGMS           PROGRAM NAME TBL                             
         LH    R6,0(R1)            ENTRY LENGTH                                 
         L     R7,2(R1)            LAST ENTRY                                   
         LA    R1,6(R1)            FIRST ENTRY                                  
         USING PGMLSTD,R1                                                       
PRGSCAN  CLC   PGMNUM,TCBPRG                                                    
         BE    GOTPRG                                                           
         BXLE  R1,R6,PRGSCAN                                                    
         MVC   4(3,R2),=C'...'     BAD PROGRAM                                  
         B     EXIT                                                             
*                                                                               
GOTPRG   DS    0H                                                               
         MVC   4(3,R2),PGMNAME                                                  
         DROP  R1,R4                                                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
SETEXIT  DS    0H                                                               
         OI    SRLMSGH+6,X'80'                                                  
         OI    6(R2),X'40'         POSN CURSOR ON REQUIRED FIELD                
         B     EXIT                                                             
         SPACE 2                                                                
ERR1     MVC   SRLMSG+12(11),=C'+/-/L/D/P/X'                                    
         B     ERRX                                                             
         SPACE 2                                                                
ERRX     MVC   SRLMSG(12),=C'***ERROR*** '                                      
         OI    SRLMSGH+6,X'80'                                                  
         L     RD,BASERD                                                        
         B     EXIT                                                             
         SPACE 2                                                                
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
ARZERO   DC    16F'0'                                                           
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
BASERD   DS    A                                                                
DMCB     DS    6F                                                               
*                                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
*                                                                               
RELO     DS    A                                                                
VCOMFACS DS    A                                                                
ALET     DS    A                                                                
TIMENOW  DS    F                                                                
KEY      DS    XL20                                                             
COUNT    DC    X'00'                                                            
WORK     DS    CL128                                                            
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT FOR LIST DISPLAY LINE AREA                                    *         
***********************************************************************         
         SPACE 1                                                                
SRLLSTD  DSECT                                                                  
         DS    CL8                 HEADER                                       
LSTTASK  DS    CL2                 TASK                                         
         DS    CL1                                                              
LSTSYS1  DS    CL7                 TASK SYS/PRG                                 
         DS    CL1                                                              
LSTDURTN DS    CL5                 TASK DURATION                                
         DS    CL1                                                              
LSTON    DS    CL2                 TASK WAITING ON                              
         DS    CL1                                                              
LSTSYS2  DS    CL7                 SYS/PRG OF ON                                
         DS    CL1                                                              
LSTCMD   DS    CL6                 TASK DATAMGR COMMAND                         
         DS    CL1                                                              
LSTFILE  DS    CL6                 TASK DATAMGR FILE                            
         DS    CL1                                                              
LSTTWA   DS    CL6                 TASK OWNER TWA ADDRESS                       
*                                                                               
SRLLSTX  EQU   *-SRLLSTD                                                        
         EJECT                                                                  
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
         SPACE 2                                                                
*FADSECTS                                                                       
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
*SRLCKFFD                                                                       
       ++INCLUDE SRLCKFFD                                                       
         EJECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049SRLCK00A  05/01/02'                                      
         END                                                                    
