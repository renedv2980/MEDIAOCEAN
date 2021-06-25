*          DATA SET SRDMP01    AT LEVEL 004 AS OF 05/01/02                      
*PHASE T15D01A                                                                  
*                                                                               
**********************************************************************          
*                                                                    *          
*  TITLE:        SRDMP01 -- $DUMP TICTRACE TABLE DISPLAY             *          
*                                                                    *          
*  COMMENTS:     DISPLAYS THE TICTRACE TABLE IN EASY TO              *          
*                READ FORMAT.                                        *          
*                                                                    *          
*  CALLED FROM:  $DUMP (T15D00)                                      *          
*                                                                    *          
*  INPUTS:       SCREEN SRDMPFD                                      *          
*                                                                    *          
*  REGISTERS:    R0 -- WORK                                          *          
*                R1 -- WORK                                          *          
*                R2 -- WORK                                          *          
*                R3 -- WORK                                          *          
*                R4 -- WORK                                          *          
*                R5 -- WORK                                          *          
*                R6 -- WORK                                          *          
*                R7 -- RE-ORGED SORT BLOCK                           *          
*                R8 -- WORK                                          *          
*                R9 -- SYSFACS                                       *          
*                RA -- TWA                                           *          
*                RB -- BASE                                          *          
*                RC -- WORKING STORAGE                               *          
*                RD -- SYSTEM                                        *          
*                RE -- WORK                                          *          
*                RF -- WORK                                          *          
*                                                                    *          
**********************************************************************          
         TITLE 'T15D01 - $DUMP TICTRACE TABLE DISPLAY'                          
T15D01   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**$TRA**,R8,RR=R3                                              
         L     RC,0(R1)                                                         
         USING WRKD,RC                                                          
         L     R9,4(R1)                                                         
         USING SYSFACD,R9                                                       
         L     RA,8(R1)                                                         
         USING T15DFFD,RA                                                       
         LH    R7,=Y(SORTBLCK-WRKD)                                             
         AR    R7,RC                                                            
         USING SORTBLCK,R7                                                      
         EJECT                                                                  
**********************************************************************          
*        GET ADDRESS OF FIRST AND MOST RECENT ENTRY IN TABLE         *          
*               AND ADDRESS OF FIRST BYTE BEYOND TABLE               *          
**********************************************************************          
         L     R6,12(R1)           ADDRESS OF TICTRACE TABLE                    
         CLC   0(8,R6),=C'TICTRACE'                                             
         BE    T15D010                                                          
         CLC   0(8,R6),=C'*TICPOP*'                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
T15D010  CLC   =C'ND,L',SRVID+1    'FE' SCREEN USED?                            
         BNE   T15D01A             NOT 'FE' SCREEN                              
         MVC   REGHOLD(L'SRVP1),SRVP4   COPY THE PARAMETERS                     
         MVC   REGHOLD+16(L'SRVP1),SRVP1                                        
         MVC   REGHOLD+32(L'SRVP1),SRVP2                                        
         MVC   REGHOLD+48(L'SRVP1),SRVP3                                        
         GOTO1 VCALLOV,DMCB,(X'FF',64(RA)),0  GO BEYOND 64 BYTE HDR             
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
         MVC   SRVID(5),=C'$ND'    INDICATE REGULAR OPTION FOR FACPAK           
         OI    SRVIDH+6,X'80'      TRANSMIT IT FOR LATER                        
         MVI   SRVIDH+7,5          FOR LENGTH OF 3                              
         MVC   SRVP4,REGHOLD       COPY BACK PARAMETERS                         
         MVC   SRVP1,REGHOLD+16                                                 
         MVC   SRVP2,REGHOLD+32                                                 
         MVC   SRVP3,REGHOLD+48                                                 
         OI    SRVP4H+6,X'80'                                                   
         OI    SRVP1H+6,X'80'                                                   
         OI    SRVP2H+6,X'80'                                                   
         OI    SRVP3H+6,X'80'                                                   
*                                                                               
T15D01A  MVC   SRVMSG(15),=CL15'TICTRACE TABLE'                                 
         LA    R2,SRVTAGH          POINT TO WHERE SCREEN WILL GO                
         GOTO1 VCALLOV,DMCB,(X'FD',(R2)),0                                      
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
         OI    SRVP3H+6,X'40'      SET CURSOR TO LAST OPTION                    
*                                                                               
         L     R2,8(R6)            OFFSET INTO TABLE OF                         
         AR    R2,R6                   MOST RECENT ENTRY                        
         ST    R2,ABEGIN           SAVE                                         
         LA    R6,12(R6)           BUMP TO FIRST ENTRY                          
         ST    R6,FSTENT                                                        
         LA    R5,60               60 ENTRIES IN TABLE                          
LOOP     LA    R6,72(R6)           BUMP AN ENTRY                                
         BCT   R5,LOOP                                                          
         ST    R6,ENDTBL           ADDR BEYOND TABLE                            
         EJECT                                                                  
**********************************************************************          
*        PARAM 2 = NUMBER OF ENTRIES TO GO BACK INTO THE TABLE.      *          
**********************************************************************          
******************   VALIDATE AND CONVERT TO BINARY   ****************          
         CLI   SRVP2H+5,0          IS THERE A # OF ENTRIES TO GO BACK?          
         BNE   PARAM2                                                           
*                                                                               
         MVC   SRVP2H+8(2),=C'15'  DEFAULT PARAMETER                            
         OI    SRVP2H+6,X'80'                                                   
         B     TABLE                                                            
*                                                                               
PARAM2   ZIC   R3,SRVP2H+5         LOAD LENGTH                                  
         LA    R1,SRVP2H+8         POINT TO THE DATA                            
CKNUM    CLI   0(R1),C'0'          MAKE SURE VALID NUMERIC                      
         BL    ERR2                                                             
         CLI   0(R1),C'9'                                                       
         BH    ERR2                                                             
         LA    R1,1(R1)            CHECK NEXT CHARACTER                         
         BCT   R3,CKNUM            UNTIL NO MORE CHARACTERS                     
         LA    R1,SRVP2H+8         POINT TO THE DATA                            
         IC    R3,SRVP2H+5         INPUT LENGTH                                 
         BCTR  R3,R0               -1 FOR PACK                                  
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1)         LOAD NUMERIC                                 
         CVB   R1,DUB              AND CONVERT TO BINARY                        
         STH   R1,ENTNUM           NUMBER OF ENTRIES TO GO BACK                 
         C     R1,=F'60'                                                        
         BNL   ERR1                ONLY 60 ENTRIES IN TABLE                     
*                                                                               
         A     R1,=F'15'                                                        
         C     R1,=F'60'                                                        
         BL    EDITP2                                                           
         MVC   SRVP2(2),=C'  '                                                  
         OI    SRVP2H+6,X'80'                                                   
         B     CALCADDR                                                         
EDITP2   EDIT  (R1),(2,SRVP2)                                                   
**********   CALCULATE NEW BEGINNING ADDRESS (FROM PARAM 2)   ********          
CALCADDR SR    R1,R1                                                            
         LH    R1,ENTNUM           NUMBER OF ENTRIES TO TAB                     
         LA    R4,72                                                            
         L     R3,ABEGIN           BEGINNING ADDRESS                            
TAB      SR    R3,R4               BACK UP AN ENTRY                             
         C     R3,FSTENT           BEGINNING?                                   
         BNE   *+8                                                              
         L     R3,ENDTBL                                                        
         BCT   R1,TAB                                                           
         ST    R3,ABEGIN           START HERE                                   
         EJECT                                                                  
*********************************************************************           
*    MOVE ALL IMPORTANT INFO FROM TICTRACE TABLE INTO A NEW TABLE   *           
*********************************************************************           
TABLE    L     R2,ABEGIN           WHERE TO START IN TICTRACE TABLE             
         LA    R4,MYTABLE          BEGINNING OF MY TABLE                        
         USING TBDSECT,R4                                                       
         LA    R1,17               COUNTER- ONLY 17 LINES IN TABLE              
         LA    R3,72               TO BACK UP ONE ENTRY                         
         B     *+8                                                              
FILLTBL  LA    R4,L'MYTABLE(R4)    BUMP MY TABLE                                
         MVC   TBDTYPE,0(R2)       TYPE                                         
         MVC   TBDTIME,8(R2)       TIME                                         
         CLI   0(R2),C'*'          POP?                                         
         BNE   TBL10               NO                                           
         MVC   TBDTYPE,0(R2)                                                    
         MVC   TBDTIME,4(R2)       MOVE AGAIN BECAUSE MEL SCREWED UP            
         MVC   TBDPSW,8(R2)        SAVE PSW ADDRESS (THANKS MEL)                
         MVC   TBDBREG,64(R2)      SAVE RB                                      
         B     TBL20                                                            
TBL10    MVC   TBDATCB,16(R2)      ADDRESS TCB ENTRY                            
         MVC   TBDSTATS,20(R2)     TCB INFO                                     
         MVC   TBDUSER,32(R2)      USER ID                                      
TBL20    SR    R2,R3               POINT TO PREVIOUS ENTRY                      
         C     R2,FSTENT           OVERSHOOT?                                   
         BNE   TBL30               NO                                           
         L     R2,ENDTBL           YES, LOAD A(BEYOND TABLE)                    
         SR    R2,R3               BACK UP TO BEGINNING OF LAST ENTRY           
TBL30    BCT   R1,FILLTBL          NO                                           
         MVI   0(R4),X'FF'                                                      
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*                    GET INFO AND FILL SCREEN                        *          
**********************************************************************          
         LA    R4,MYTABLE          BEGINNING OF MY TABLE                        
         USING TBDSECT,R4                                                       
         LA    R6,DMPL1            FIRST SCREEN LINE                            
         USING SCDSECT,R6                                                       
**********************    COLUMN HEADINGS   **************************          
         MVC   SCDAWRK,=C'TCBWRK'                                               
         MVC   SCDATWA,=C'TCBTWA'                                               
         MVC   SCDAPGM,=C'TCBPGM'                                               
         LA    R6,87(R6)           NEXT LINE                                    
******************   CONVERT TIME AND DISPLAY   **********************          
         LA    R4,L'MYTABLE(R4)    SKIP FIRST ENTRY                             
         B     SCLOOP1                                                          
SCLOOP   DS    0H                                                               
*                                                                               
         LA    R6,87(R6)           NEXT LINE ON SCREEN                          
         LA    R4,L'MYTABLE(R4)    NEXT ENTRY IN TABLE                          
         CLI   0(R4),X'FF'         END?                                         
         BE    XIT                                                              
SCLOOP1  SR    R2,R2                                                            
         SR    R3,R3                                                            
         L     R3,TBDTIME          TIME IN TU'S                                 
         L     RE,VSSB                                                          
         TM    SSBSTAT3-SSBD(RE),SSBMVSTU                                       
         BZ    *+12                                                             
         D     R2,=F'38400'        MVS TU'S TO SECS (1/38000)                   
         B     *+8                                                              
         D     R2,=F'100'          OLD TU'S TO SECS (1/100)                     
SCLOOP2  SR    R2,R2                                                            
         D     R2,=F'60'           REMAINDER TO GET MIN                         
         MVC   SCDTIME,DOTS                                                     
         EDIT  (R2),(2,SCDSEC),ZERO=NOBLANK                                     
         CLI   SCDSEC,X'40'                                                     
         BNE   *+8                                                              
         MVI   SCDSEC,C'0'                                                      
         SR    R2,R2                                                            
         D     R2,=F'60'           TO GET HRS                                   
         EDIT  (R2),(2,SCDMIN),ZERO=NOBLANK                                     
         CLI   SCDMIN,X'40'                                                     
         BNE   *+8                                                              
         MVI   SCDMIN,C'0'                                                      
         EDIT  (R3),(2,SCDHRS),ZERO=BLANK                                       
************************   CHECK TYPE   ******************************          
         MVC   SCDTYPE(1),TBDTYPE  SET TYPE                                     
         CLI   TBDTYPE,C'1'                                                     
         BNE   *+8                                                              
         B     TYPE1S              TYPE 1 AND S INFO                            
         CLI   TBDTYPE,C'S'                                                     
         BNE   *+8                                                              
         B     TYPE1S                                                           
         CLI   TBDTYPE,C'*'        TYPE * INFO                                  
         BNE   *+8                                                              
         B     TYPEPOP                                                          
         B     TYPENOT             NO INFO                                      
         EJECT                                                                  
************************  TYPE 1 AND S  ******************************          
TYPE1S   DS    0H                                                               
         MVC   SCDTYPE+1(3),=C'SET'                                             
         L     R5,TBDATCB          A(TCB)                                       
         USING TCBD,R5                                                          
         L     R2,VTCB             TCB BEGIN ADDRESS                            
         L     R3,2(R2)            TCB END ADDRESS                              
         CR    R5,R2               IN RANGE?                                    
         BL    SCLOOP              NO                                           
         CR    R5,R3               IN RANGE?                                    
         BH    SCLOOP              NO                                           
*                                                                               
         MVI   SCDTID,C'T'         TASK ID                                      
         MVC   SCDTID+1(1),TBDTASK                                              
*                                                                               
         LA    R2,TCBWRKA+1        EDIT A(WRK)                                  
         LA    R3,SCDAWRK                                                       
         BAS   RE,ADDRCONV                                                      
*                                                                               
         LA    R2,TCBTWA+1         EDIT A(TWA)                                  
         LA    R3,SCDATWA                                                       
         BAS   RE,ADDRCONV                                                      
*                                                                               
         LA    R2,TCBPGMA+1        EDIT A(PGM)                                  
         LA    R3,SCDAPGM                                                       
         BAS   RE,ADDRCONV                                                      
*                                                                               
         MVC   SCDLINE,DOTS        LINE                                         
         MVC   SCDTRM,DOTS         TERM                                         
         CLI   TBDTSYM,X'40'       IS THERE A LINE ADDRESS?                     
         BNH   *+10                NO, SKIP                                     
         MVC   SCDLINE(4),TBDTSYM                                               
         CLI   TBDTSYM+4,X'40'     IS THERE A TERM                              
         BNH   *+10                NO, SKIP                                     
         MVC   SCDTRM,TBDTSYM+4                                                 
*                                                                               
         MVC   SPWORK+0(1),TBDTSYS                                              
         MVC   SPWORK+1(1),TBDTPRG                                              
         BAS   RE,PGMCHASE         GO EXPAND IT                                 
         LA    R0,10               MOVE FIRST TEN BYTES                         
         CR    R1,R0                                                            
         BNH   *+6                                                              
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SCDSYSPR(0),WORK                                                 
*                                                                               
         MVC   SCDUSER,DOTS                                                     
         XC    USERKEY,USERKEY                                                  
         MVI   USERKEY,C'I'                                                     
         OC    TBDUSER,TBDUSER                                                  
         BZ    TYPE1SX                                                          
         MVC   USERKEY+23(2),TBDUSER                                            
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',USERKEY,RIO                  
         CLI   DMCB+8,X'10'        RECORD NOT FOUND?                            
         BE    TYPE1SX                                                          
         LA    R5,RIO                                                           
         MVI   ELCODE,X'02'        USER NAME ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SCDUSER,2(R5)       USER NAME                                    
*                                                                               
TYPE1SX  B     SCLOOP                                                           
         DROP  R5                                                               
         EJECT                                                                  
*************************   TYPE *POP  ******************************           
TYPEPOP  DS    0H                                                               
         MVC   SCDTYPE,TBDTYPE                                                  
         L     R5,TBDATCB          BASE REG                                     
         L     R3,TBDPSW           PSW ADDRESS AT POP                           
         MVC   SCDSYSPR(8),22(R5)  NAME OF MODULE                               
         MVI   SCDSYSPR+9,C'+'                                                  
         SR    R3,R5               OFFSET INTO MODULE                           
         ST    R3,FULL                                                          
         LA    R2,FULL+1                                                        
         LA    R3,SCDSYSPR+11                                                   
         BAS   RE,ADDRCONV                                                      
*                                                                               
         B     SCLOOP                                                           
************************  OTHER TYPES  *******************************          
TYPENOT  DS    0H                                                               
         MVC   SCDTYPE+1(3),=C'SET'                                             
         B     SCLOOP                                                           
         EJECT                                                                  
*************************  TIDY UP  **********************************          
**********************************************************************          
XIT      XMOD1                                                                  
ERR1     DS    0H                                                               
         MVC   SRVMSG+12(L'MSG1),MSG1                                           
         B     ERRX                                                             
ERR2     DS    0H                                                               
         MVC   SRVMSG+12(L'MSG2),MSG2                                           
         B     ERRX                                                             
ERRX     MVC   SRVMSG(12),=C'** ERROR ** '                                      
         B     XIT                                                              
         EJECT                                                                  
***********************  ADDRESS CONVERSION  *************************          
*      EDITS F'WORD INTO 6 BYTES, R2=A(F'WORD+1), R3=A(OUTPUT)       *          
**********************************************************************          
ADDRCONV DS    0H                                                               
         MVC   0(6,R3),DOTS        PRIME WITH DOTS                              
         NC    0(3,R2),0(R2)       IS ADDRESS ZERO?                             
         BZR   RE                  YES - RETURN                                 
         ST    RE,SAVERE                                                        
         GOTO1 VHEXOUT,DMCB,(R2),(R3),3                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
*********************************************************************           
*        SUBROUTINE PGMCHASE - BUILDS SYSTEM/PHASE FIELD IN WORK    *           
*                                                                   *           
*        ENTRY-                                                     *           
*              SPWORK=SYS/PRG BYTES                                 *           
*        EXIT-                                                      *           
*              RETURNS SQUASHED 'SSSSSSS/PPPPPPP' IN WORK           *           
*              R1=LENGTH OUTPUT FIELD                               *           
*********************************************************************           
PGMCHASE NTR1                                                                   
         CLI   SPWORK,0            IS ANY SYSTEM ACTIVE ?                       
         BNE   NOTIDLE             YES - FIND WHICH                             
         MVC   WORK(10),DOTS       ELSE RETURN DOTS                             
         LA    R1,10                                                            
         B     PGMX                                                             
NOTIDLE  GOTO1 VHEXOUT,DMCB,SPWORK,FULL,2  EXPAND SYS/PROG                      
         MVI   WORK,C' '           PRIME WORK AREA                              
         MVC   WORK+1(14),WORK                                                  
         MVI   WORK+7,C'/'                                                      
         L     R5,VSELIST          POINT TO START OF SELIST                     
         LH    R2,0(R5)            PRIME INDICES                                
         L     R3,2(R5)                                                         
         LA    R5,6(,R5)                                                        
         USING SELISTD,R5                                                       
SESCAN   CLC   SESYS,SPWORK        IS THIS THE SYSTEM ?                         
         BE    GOTSE               YES - SKIP                                   
         BXLE  R5,R2,SESCAN        LOOP TO END OF SELIST                        
*                                                                               
BADSE    MVC   WORK(2),FULL        MOVE X'SS' TO SSSSSSS AREA                   
         B     BADPGM                                                           
*                                                                               
GOTSE    LA    R2,SENAME+6         POINT TO END OF SENAME                       
GOTSEL1  CLI   0(R2),X'40'         IS THIS THE LAST NO BLANK ?                  
         BNE   GOTSEL2             YES - SKIP                                   
         BCT   R2,GOTSEL1          ELSE SCAN BACK                               
GOTSEL2  CLI   0(R2),X'F0'         IS LAST CHAR NUMERIC ?                       
         BNL   GOTSEL3             YES - SKIP                                   
         MVC   WORK(4),SENAME      MOVE ONLY FIRST FOUR CHARS                   
         B     GOTSEL4                                                          
GOTSEL3  MVC   WORK(7),SENAME      MOVE WHOLE SENAME                            
*                                                                               
GOTSEL4  L     R4,SEPGMS           PROGRAM NAME LIST FOR THIS SYSTEM            
         USING PGMLSTD,R4                                                       
         LH    R2,0(,R4)           PRIME INDEXES                                
         L     R3,2(,R4)                                                        
         LA    R4,6(,R4)                                                        
PGMSCAN  CLC   PGMNUM,SPWORK+1     IS THIS THE PROGRAM ?                        
         BE    GOTPGM              YES - SKIP                                   
         BXLE  R4,R2,PGMSCAN       LOOP TO END PGMLIST                          
*                                                                               
BADPGM   MVC   WORK+8(2),FULL+2    MOVE X'PP' TO PPPPPPP AREA                   
         B     CHASEND                                                          
*                                                                               
GOTPGM   MVC   WORK+8(7),PGMNAME                                                
         DROP  R5                                                               
         DROP  R4                                                               
CHASEND  LA    R4,WORK             COMPRESS ALL BLANKS FROM WORK                
         LR    R5,R4               SAVE WORK ADDRESS                            
         LR    R1,R4                                                            
         LA    R2,15               BYTE COUNT TO COMPRESS                       
SQUASH1  CLI   0(R4),C' '          BLANK ?                                      
         BE    SQUASH2             YES - IGNORE IT                              
         MVC   0(1,R1),0(R4)       NO MOVE IT                                   
         LA    R1,1(,R1)           POINT TO NEXT TARGET BYTE                    
SQUASH2  LA    R4,1(,R4)           POINT TO NEXT SOURCE BYTE                    
         BCT   R2,SQUASH1          LOOP R2 TIMES                                
         SR    R1,R5               GET LENGTH IN BYTES AFTER SQUASH             
*                                                                               
PGMX     DS    0H                                                               
         XIT1  REGS=(R1)                                                        
*                                                                               
         GETEL R5,28,ELCODE                                                     
         EJECT                                                                  
************************  LITERALS  **********************************          
         LTORG                                                                  
*                                                                               
MSG1     DC    C'ONLY 60 ENTRIES IN TABLE'                                      
MSG2     DC    C'NOT VALID NUMERIC'                                             
DOTS     DC    12C'.'                                                           
         EJECT                                                                  
       ++INCLUDE SRDMPWORKD                                                     
***********************  WRKING STORAGE  *****************************          
         ORG   SORTBLCK                                                         
ABEGIN   DS    F                   A(MOST RECENT ENTRY IN TICTRACE)             
FSTENT   DS    F                   A(FIRST ENTRY IN TICTRACE)                   
ENDTBL   DS    F                   A(AFTER THE LAST ENTRY IN TICTRACE)          
SAVERE   DS    F                   SAVE REG. E                                  
ENTNUM   DS    H                   HOW MANY ENTRIES TO GO BACK                  
USERKEY  DS    XL25                                                             
RIO      DS    CL1000                                                           
ELCODE   DS    XL1                                                              
SPWORK   DS    CL2                                                              
MYTABLE  DS    17XL32              TABLE                                        
         DS    XL1                 END OF TABLE MARKER                          
**********************************************************************          
*                          DSECTS                                    *          
**********************************************************************          
TBDSECT  DSECT                     DSECT FOR TABLE ENTRY                        
TBDTYPE  DS    CL4                 TYPE OF ENTRY (1SET, SSET, ETC)              
TBDPSW   DS    CL4                 PSW ADDR WHEN POP OCCURED                    
TBDTIME  DS    CL4                 TIME IN TU'S                                 
TBDATCB  DS    CL4                 ADDRESS OF TCB ENTRY                         
         ORG   *-4                                                              
TBDBREG  DS    CL4                 BASE REG FOR POPS                            
TBDSTATS DS    0CL12                                                            
TBDTSYM  DS    CL8                 LINE,TERMINAL                                
TBDTOSYS DS    CL1                                                              
TBDTSYS  DS    CL1                 SYSTEM NUMBER                                
TBDTPRG  DS    CL1                 PROGRAM NUMBER                               
TBDTASK  DS    CL1                 TASK ID                                      
TBDUSER  DS    CL2                 USER ID IN BINARY                            
         DS    CL1                                                              
TBDLENQ  EQU   *-TBDSECT           32 BYTES                                     
*                                                                               
**********************************************************************          
SCDSECT  DSECT                     DSECT FOR SCREEN LINE                        
SCDTID   DS    CL2                 TASK ID 'TN'                                 
         DS    CL1                                                              
SCDTIME  DS    0CL8                TIME (HH.MM.SS)                              
SCDHRS   DS    CL2                                                              
         DS    CL1                                                              
SCDMIN   DS    CL2                                                              
         DS    CL1                                                              
SCDSEC   DS    CL2                                                              
         DS    CL1                                                              
SCDTYPE  DS    CL4                 TYPE OF ENTRY                                
         DS    CL1                                                              
SCDUSER  DS    CL10                USER NAME                                    
         DS    CL1                                                              
SCDSYSPR DS    CL17                SYS/PROG  OR MODULE AND DISP                 
         DS    CL1                                                              
SCDLINE  DS    CL4                 TCB LINE ADDR                                
SCDTRM   DS    CL4                 TCB TERM ADDR                                
         DS    CL1                                                              
SCDAWRK  DS    CL6                 A(WRK)                                       
         DS    CL1                                                              
SCDATWA  DS    CL6                 A(TWA)                                       
         DS    CL1                                                              
SCDAPGM  DS    CL6                 A(PGM)                                       
SCDLENQ  EQU   *-SCDSECT           RIGHT NOW  75 BYTES                          
**********************************************************************          
         EJECT                                                                  
* SRDMPDSECT (INCLUDES DDCOMFACS, FADSECTS, DDFLDHEADER, SCREENS)               
         PRINT OFF                                                              
       ++INCLUDE SRDMPDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SRDMP01   05/01/02'                                      
         END                                                                    
