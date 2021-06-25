*          DATA SET REGAV17S   AT LEVEL 073 AS OF 05/01/02                      
*PHASE T81317B,+0                                                               
*INCLUDE INVDAY                                                                 
*INCLUDE REBKLST                                                                
         TITLE 'T81317 - REPPAK FILE MAINT - AVAIL CDE ADD/CHA/DIS/DEL'         
T81317   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81317,RR=R5                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T81317+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASYSD                                                         
         USING SYSD,R8                                                          
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
         ST    R5,RELO                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
         OI    GENSTAT5,NODLST     CAN'T DELETE FROM LIST                       
*                                                                               
         CLI   ACTNUM,ACTDEL       DELETE                                       
         BE    *+12                                                             
         CLI   ACTNUM,ACTREST      AND RESTORE ARE INVALID                      
         BNE   MAIN10                                                           
*                                                                               
         MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     ERREND                                                           
*                                                                               
MAIN10   DS    0H                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
VKEY     DS    0H                                                               
*              INIT WORK AREA                                                   
         XC    KEY,KEY                                                          
         SPACE 2                                                                
*              VALIDATE THE CODE                                                
         XC    CODEHLD,CODEHLD                                                  
         LA    R2,MINCODEH                                                      
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0             NOT REQUIRED IF LIST                         
         BNE   VK50                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK100                                                            
VK50     CLI   5(R2),2             REQUIRED                                     
         BL    ERREND                                                           
         MVC   CODEHLD,8(R2)                                                    
         OC    CODEHLD,=8X'40'                                                  
         SPACE 2                                                                
*                                                                               
*                                                                               
VK100    MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RARTREC,R6                                                       
*                                                                               
         MVI   RARTKTYP,X'3E'                                                   
         MVC   RARTKREP,AGENCY                                                  
         MVC   RARTKCOD,CODEHLD                                                 
         MVC   SAVEKEY,KEY                                                      
*                                                                               
VKXIT    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY KEY ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
DKEY     DS    0H                                                               
         L     R6,AIO                                                           
         USING RARTREC,R6                                                       
*                                                                               
         LA    R2,MINCODEH         AVAIL CODE                                   
         MVC   8(L'RARTKCOD,R2),RARTKCOD                                        
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
DKXIT    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY RECORD ROUTINE                          *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
DREC     DS    0H                                                               
*                                                                               
         L     R6,AIO                                                           
         USING RARTREC,R6                                                       
*                                                                               
         BAS   RE,CLRSCRN                                                       
*                                                                               
*  COMMENT                                                                      
         MVC   MINCOMM,RARTCOMM                                                 
         OI    MINCOMMH+6,X'80'                                                 
*                                                                               
*  LENGTH/QUARTER LOOP                                                          
         LA    R2,MINLEN1H                                                      
         LA    R3,14                                                            
         USING RALQELEM,R4                                                      
         GOTO1 HELLO,DMCB,(C'G',REPFILE),(X'02',(R6)),0                         
         CLI   12(R1),0                                                         
         BNE   DRXIT                                                            
         L     R4,12(R1)                                                        
*                                                                               
         LA    R2,MINYR1H          YEAR                                         
*                                                                               
* !!!!  DR100    EDIT  (B1,RALQYEAR),(2,8(R2))                                  
*                                                                               
DR100    DS    0H                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(1),RALQYEAR                                                  
         MVC   DUB+1(2),=X'0101'   YY0101                                       
         GOTO1 DATCON,DMCB,(3,DUB),(10,WORK2)                                   
         MVC   8(2,R2),WORK2+6     MOVE OUT YEAR TO SCREEN                      
*                                                                               
         MVI   5(R2),X'02'         2 BYTE YEAR                                  
         OI    1(R2),X'20'         PROTECT THIS                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         BAS   RE,NEXTFLD          LENGTH                                       
         EDIT  (B1,RALQLEN),(3,8(R2)),ALIGN=LEFT                                
         OI    1(R2),X'20'         PROTECT                                      
         MVI   5(R2),X'02'         AT LEAST A 2 BYTE LENGTH                     
*                                                                               
         TM    RALQSTAT,X'80'      CHECK LENGTH IN MINUTES                      
         BZ    DR200                                                            
         OC    8(4,R2),SPACES                                                   
         LA    RE,8(R2)                                                         
         LA    RF,4                                                             
DR120    CLI   0(RE),X'40'                                                      
         BE    DR140                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,DR120                                                         
         DC    H'0'                                                             
DR140    MVI   0(RE),C'M'                                                       
         LA    RE,4                                                             
         SR    RE,RF                                                            
         STC   RE,5(R2)            LENGTH                                       
*                                                                               
DR200    OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         BAS   RE,NEXTFLD                                                       
         MVI   8(R2),C'N'                                                       
         OI    6(R2),X'80'         TRANSMIT                                     
         TM    RALQQTR,X'80'       CHECK QUARTER 1                              
         BZ    DR220                                                            
         MVI   8(R2),C'Y'                                                       
*                                                                               
DR220    BAS   RE,NEXTFLD                                                       
         MVI   8(R2),C'N'                                                       
         OI    6(R2),X'80'         TRANSMIT                                     
         TM    RALQQTR,X'40'       CHECK QUARTER 2                              
         BZ    DR240                                                            
         MVI   8(R2),C'Y'                                                       
*                                                                               
DR240    BAS   RE,NEXTFLD                                                       
         MVI   8(R2),C'N'                                                       
         OI    6(R2),X'80'         TRANSMIT                                     
         TM    RALQQTR,X'20'       CHECK QUARTER 3                              
         BZ    DR260                                                            
         MVI   8(R2),C'Y'                                                       
*                                                                               
DR260    BAS   RE,NEXTFLD                                                       
         MVI   8(R2),C'N'                                                       
         OI    6(R2),X'80'         TRANSMIT                                     
         TM    RALQQTR,X'10'       CHECK QUARTER 4                              
         BZ    DR300                                                            
         MVI   8(R2),C'Y'                                                       
*                                                                               
*  GET NEXT ELEMENT                                                             
DR300    ZIC   RE,RALQLN                                                        
         AR    R4,RE                                                            
         CLI   0(R4),X'02'                                                      
         BNE   DRXIT                                                            
*                                                                               
         BAS   RE,NEXTFLD                                                       
         BCT   R3,DR100                                                         
*                                                                               
DRXIT    B     EXIT                                                             
*                                                                               
         DROP  R6,R4                                                            
         SPACE 5                                                                
*                                                                               
* CLEAR THE SCREEN                                                              
*                                                                               
CLRSCRN  NTR1                                                                   
         XC    MINCOMM,MINCOMM                                                  
         OI    MINCOMMH+6,X'80'                                                 
*                                                                               
         LA    R2,MINYR1H          FIRST FIELD                                  
         LA    R3,MINTAGH          END OF SCREEN                                
CLRSC20  CR    R2,R3                                                            
         BNL   CLRSCEX                                                          
*                                                                               
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
         XC    8(L'MINYR1,R2),8(R2)                                             
         OI    6(R2),X'80'         TRANSMIT                                     
         BAS   RE,NEXTFLD                                                       
*                                                                               
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
         XC    8(L'MINLEN1,R2),8(R2)                                            
         OI    6(R2),X'80'         TRANSMIT                                     
         BAS   RE,NEXTFLD                                                       
*                                                                               
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
         XC    8(L'MINQTR1,R2),8(R2)                                            
         OI    6(R2),X'80'         TRANSMIT                                     
         BAS   RE,NEXTFLD                                                       
*                                                                               
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
         XC    8(L'MINQTR1,R2),8(R2)                                            
         OI    6(R2),X'80'         TRANSMIT                                     
         BAS   RE,NEXTFLD                                                       
*                                                                               
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
         XC    8(L'MINQTR1,R2),8(R2)                                            
         OI    6(R2),X'80'         TRANSMIT                                     
         BAS   RE,NEXTFLD                                                       
*                                                                               
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
         XC    8(L'MINQTR1,R2),8(R2)                                            
         OI    6(R2),X'80'         TRANSMIT                                     
         BAS   RE,NEXTFLD                                                       
*                                                                               
         B     CLRSC20                                                          
*                                                                               
CLRSCEX  B     EXIT                                                             
*                                                                               
MVESPACE MVC   8(0,R2),SPACES                                                   
         SPACE 5                                                                
*                                                                               
* POINT TO NEXT SCREEN FIELD                                                    
*                                                                               
NEXTFLD  ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              LIST RECORD ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
LIST     DS    0H                                                               
         OC    KEY(17),KEY                                                      
         BNZ   LR100                                                            
*                                                                               
         SPACE                                                                  
         LA    R6,KEY                                                           
         USING RARTKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(27),SAVEKEY                                                  
LR100    GOTO1 HIGH                                                             
         B     LR220                                                            
*                                                                               
LR200    GOTO1 SEQ                                                              
LR220    LA    R6,KEY                                                           
         CLC   SAVEKEY(12),KEY                                                  
         BNE   LREXT                                                            
         OC    CODEHLD,CODEHLD                                                  
         BZ    LR230                                                            
         CLC   SAVEKEY+12(8),KEY+12  CHECK CODE MATCH                           
         BH    LR200                                                            
*                                                                               
LR230    MVC   SAVEKEY,KEY                                                      
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         LA    R5,LISTAR                                                        
         USING LLINED,R5                                                        
         XC    LISTAR,LISTAR                                                    
*                                                                               
         MVC   LRTCODE,RARTKCOD    AVAIL CODE                                   
*                                                                               
         MVC   LRTCOMM,RARTCOMM    COMMENT                                      
*                                                                               
         GOTO1 LISTMON                                                          
         B     LR200               GOTO READ SEQ                                
*                                                                               
LREXT    DS    0H                                                               
         B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE                                                                  
VREC     DS    0H                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   ACTNUM,ACTCHA                                                    
         BNE   VR5                                                              
         MVC   AIO,AIO2                                                         
         B     VR10                                                             
*                                                                               
VR5      DS    0H                                                               
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
VR10     DS    0H                                                               
         L     R6,AIO                                                           
         USING REINVREC,R6                                                      
*                                                                               
         MVI   RARTKTYP,X'3E'                                                   
         MVC   RARTKREP,AGENCY                                                  
         MVC   RARTKCOD,MINCODE                                                 
         OC    RARTKCOD,=8X'40'                                                 
         SPACE 1                                                                
         MVC   RARTPCOD(2),=XL2'0164'                                           
*                                                                               
*  COMMENT                                                                      
*                                                                               
         MVC   RARTCOMM,MINCOMM                                                 
         OC    RARTCOMM,SPACES                                                  
*                                                                               
*  YEAR/LENGTH/QUARTER LOOP                                                     
         MVI   ERROR,INVALID                                                    
         XC    WORK,WORK                                                        
         LA    R2,MINYR1H                                                       
         LA    R3,14                                                            
         LA    R4,WORK                                                          
         USING RALQELEM,R4                                                      
         MVI   RALQCODE,X'02'                                                   
         MVI   RALQLN,RALQLENQ     ELEM LENGTH                                  
*                                                                               
*  VALIDATE THE YEAR                                                            
         LA    R2,MINYR1H                                                       
VR100    MVI   ERROR,INVALID                                                    
         MVI   INPSW,C'N'          SET DEFAULT TO NO INPUT                      
         CLI   5(R2),0                                                          
         BE    VR150                                                            
         CLI   5(R2),2             MINIMUM OF 2 POSITIONS                       
         BL    ERREND                                                           
*                                                                               
         MVC   HALF,8(R2)                                                       
         LA    RE,HALF                                                          
         LA    RF,2                                                             
*                                                                               
*--MUST BE NUMERIC                                                              
VR120    CLI   0(RE),C'0'                                                       
         BL    ERREND                                                           
         CLI   0(RE),C'9'                                                       
         BH    ERREND                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,VR120                                                         
*                                                                               
*--CONVERT YEAR TO BINARY                                                       
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(2),8(R2)        MOVE IN YEAR                                 
         MVC   DUB+2(4),=C'0101'   YY/01/01                                     
         GOTO1 DATCON,DMCB,(0,DUB),(3,WORK2)                                    
         MVC   RALQYEAR,WORK2      SAVE AWAY YEAR                               
*                                                                               
*&&DO                                                                           
         PACK  DUB(8),8(2,R2)                                                   
         CVB   R0,DUB                                                           
         STC   R0,RALQYEAR                                                      
*&&                                                                             
         MVI   INPSW,C'Y'                                                       
*                                                                               
*  VALIDATE THE LENGTH                                                          
VR150    BAS   RE,NEXTFLD                                                       
         CLI   INPSW,C'Y'          WAS YEAR INPUTTED                            
         BE    VR160                                                            
         CLI   5(R2),0                                                          
         BNE   ERREND              NO YEAR, NO LENGTH ERROR                     
         B     VR170                                                            
*                                                                               
VR160    CLI   5(R2),0                                                          
         BE    ERREND              YEAR INPUTTED, LENGTH MUST BE THERE          
         B     VR200                                                            
*                                                                               
*  IF LENGTH NOT INPUTTED QUARTERS CANNOT BE INPUTTED                           
*                                                                               
VR170    LA    R1,4                                                             
*                                                                               
VR180    BAS   RE,NEXTFLD                                                       
         CLI   5(R2),0                                                          
         BNE   ERREND                                                           
         BCT   R1,VR180                                                         
         B     VR300                                                            
         SPACE 1                                                                
*                                                                               
*  VALIDATE LENGTH                                                              
*                                                                               
VR200    LR    RE,R2                                                            
         LA    RE,8(RE)            POINT TO FIELD                               
         ZIC   RF,5(R2)                                                         
         ZIC   R1,5(R2)                                                         
*--MUST BE NUMERIC                                                              
VR210    CLI   0(RE),C'0'                                                       
         BL    VR220                                                            
         CLI   0(RE),C'9'                                                       
         BH    VR220                                                            
         LA    RE,1(RE)                                                         
         BCT   RF,VR210                                                         
         B     VR230                                                            
*                                                                               
VR220    C     RF,=F'1'            IF NOT IN LAST POSITION ERROR                
         BNE   ERREND                                                           
*  CHECK FOR MINUTES/SECONDS INDICATOR                                          
         BCTR  R1,0                                                             
         CLI   0(RE),C'M'                                                       
         BNE   *+12                                                             
         OI    RALQSTAT,X'80'                                                   
         B     VR230                                                            
         CLI   0(RE),C'S'                                                       
         BNE   ERREND                                                           
*  CONVERT FIELD TO BINARY                                                      
VR230    BCTR  R1,0                                                             
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         STCM  R0,1,RALQLEN                                                     
         SPACE 1                                                                
*                                                                               
         MVC   TMPYEAR,RALQYEAR                                                 
         MVC   TMPLEN,RALQLEN                                                   
         BAS   RE,CHK02                                                         
*                                                                               
*  VALIDATE QUARTERS                                                            
*                                                                               
VR235    DS    0H                                                               
         LA    R1,QTRTAB                                                        
         LA    RF,4                                                             
         XC    RALQQTR,RALQQTR                                                  
*                                                                               
VR240    ZIC   RE,0(R2)            GET NEXT FIELD                               
         AR    R2,RE                                                            
*                                                                               
         CLI   5(R2),0             IF BLANK, DEFAULT TO YES                     
         BNE   VR245                                                            
         MVI   5(R2),1                                                          
         MVI   8(R2),C'Y'                                                       
*                                                                               
VR245    DS    0H                                                               
         CLI   8(R2),C'N'          NO?                                          
         BE    VR260                                                            
         CLI   8(R2),C'Y'          YES?                                         
         BNE   ERREND                                                           
*                                                                               
         OC    RALQQTR,0(R1)                                                    
*                                                                               
VR260    LA    R1,1(R1)                                                         
         BCT   RF,VR240                                                         
         BAS   RE,NEXTFLD          POINT TO NEXT LENGTH                         
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO),WORK,0                         
*                                                                               
VR300    XC    RALQYEAR(3),RALQYEAR                                             
         BCT   R3,VR100                                                         
*                                                                               
VREX     DS    0H                                                               
         CLI   ACTNUM,ACTCHA                                                    
         BNE   *+8                                                              
         BAS   RE,REST03                                                        
*                                                                               
         B     DREC                                                             
*                                                                               
         DROP  R4                                                               
********************************************************************            
*     CHECK IF DUPLICATE 02 EL FOR SAME YR/LENGTH                               
********************************************************************            
CHK02    NTR1                                                                   
         L     R5,AIO              ORIGINAL RECORD                              
         MVI   ELCODE,X'02'                                                     
         MVC   DATADISP,=H'34'                                                  
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         USING RALQELEM,R5                                                      
*                                                                               
CHK0210  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   CHK02X                                                           
*                                                                               
         CLC   RALQYEAR,TMPYEAR    SAME YEAR ALREADY ENTERED?                   
         BNE   CHK0210             NO                                           
         CLC   RALQLEN,TMPLEN      SAME LENGTH?                                 
         BNE   CHK0210                                                          
*                                                                               
         MVC   RERROR(2),=AL2(DUPYRLEN)                                         
         GOTO1 MYERROR                                                          
*                                                                               
CHK02X   B     EXIT                                                             
*                                                                               
********************************************************************            
*     RESTORE 03 STATION ELEMENTS                                               
********************************************************************            
REST03   NTR1                                                                   
         L     R5,AIO1             ORIGINAL RECORD                              
         MVI   ELCODE,X'03'                                                     
         MVC   DATADISP,=H'34'                                                  
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
REST0310 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   REST03X                                                          
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',REPFILE),(0,AIO2),(R5),0                        
         B     REST0310                                                         
*                                                                               
REST03X  B     EXIT                                                             
*                                                                               
         GETELN R5,DATADISP,ELCODE                                              
********************************************************************            
QTRTAB   DC    XL4'80402010'                                                    
VARPACK  PACK  DUB,8(0,R2)                                                      
         DROP  R4,R6                                                            
         EJECT                                                                  
ERREND   GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
REPFILE  DC    CL8'REPFILE'                                                     
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
LLINED   DSECT                                                                  
LRTCODE  DS    CL8                                                              
         DS    CL3                                                              
LRTYEAR  DS    CL2                                                              
         DS    CL3                                                              
LRTCOMM  DS    CL46                                                             
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* REGAVFFD                                                                      
* DDGENTWA                                                                      
* REGAVWTWA                                                                     
* REGAVD7D                                                                      
* REGENMKT                                                                      
* REGENREP(A)                                                                   
* REGAVWORKD                                                                    
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REGAVFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE REGAVE4D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE REGAVE5D                                                       
         EJECT                                                                  
       ++INCLUDE REGAVWTWA                                                      
         EJECT                                                                  
REINVREC DSECT                                                                  
       ++INCLUDE REGENARTE                                                      
         EJECT                                                                  
       ++INCLUDE REGAVWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
*                                                                               
*              WORK AREA                                                        
*                                                                               
*                                                                               
INVLIST  DS    F                   POINTER TO INVENTORY INFO                    
INVDYTIM DS    CL60                EXTENDED DAY TIME DEMO TABLE                 
*                                                                               
INVMED   DS    CL1                 MEDIA                                        
INVSTAT  DS    CL5                 STATION                                      
INVMKT   DS    CL2                 MARKET                                       
INVSRC   DS    CL1                 SOURCE                                       
INVFBK   DS    CL2                 FROM BOOK                                    
INVTYP   DS    CL1                 I OR P                                       
INVEFDT  DS    CL2                 EFFECTIVE DATE - COMPRESSED                  
INVNO    DS    CL1                 NUMBER IN INVENTORY LIST                     
INVBAD   DS    CL1                 0=NO ERROR, N=NUMBER OF BAD ITEM             
TOTWGHT  DS    CL1                 TOTAL NUMBER QTR HOURS                       
INVTOBK  DS    CL15                TO BOOK CODES                                
*                                                                               
INVIND   DS    CL1                 INVENTORY TYPE INDICATOR                     
INVDAYS  DS    CL1                 1=MON, 7=SUN                                 
INVTIM   DS    CL4                 MILITARY TIME                                
INVCODE  DS    CL2                 PROGRAM CODE                                 
INVCDCTL DS    B                   CONTROL BITS FOR PROGRAM CODE                
INVBTYPE DS    C                   BOOK TYPE (USER INPUT, APPLIES TO            
*                                  DEMO FILE TRANSFERS)                         
INVFRBT  DS    C                   BOOK TYPE (ON INV TO INV TRANSFER            
*                                                                               
TRBKLIST DS    CL60                BOOK ENTRIES BUILT BY REBKLST                
         SPACE                                                                  
TRBKCNT  DS    X                   COUNT OF BOOK ENTRIES                        
TRMODE   DS    C                   COMMUNICATION TO BUFFER ROUTINE              
TRWTOV   DS    C                   USER WEIGHTING OVERRIDE (Y/N)                
TRHOOKSW DS    C                   HOOK ENTERED FOR DEMAND CALL (Y/N)           
TRSVKEY  DS    CL27                                                             
TRFNOVER DS    C                   Y=SUPPRESS TIME PERIOD FOOTNOTING            
TRAPAGE  DS    A                   A(2304 BYTE PAGE)                            
TRPAGE   DS    X                   PAGES WRITTEN TO TWA                         
TRRECS   DS    X                   RECORDS GENERATED DURING LINE EDIT           
         SPACE 1                                                                
DEMEDIA  DS    CL1                 FROM MEDIA                                   
DEMSTA   DS    CL5                      STATION                                 
DEMRKT   DS    CL2                      MARKET FOR DEMOS                        
*                                                                               
HALF2    DS    H                                                                
BYTE2    DS    CL1                                                              
BYTE3    DS    CL1                                                              
BYTE4    DS    CL1                                                              
*****************************************************                           
*****************************************************                           
CODEHLD  DS    CL8                 AVAIL CODE HOLD AREA                         
YEARHLD  DS    CL1                 YEAR HOLD AREA                               
INPSW    DS    CL1                 DATA INPUTTED SWITCH                         
*                                                                               
*  PRINT ELEMENT ADDRESS STORAGE LOCATIONS                                      
DYTMPTR  DS    F                   DAY/TIME ELEMENT                             
PROGPTR  DS    F                   PROGRAM ELEMENT                              
AVPRPTR  DS    F                   AVAIL PROGRAM ELEMENT                        
OVFLSW   DS    CL1                 TOO MANY LINES TO PRINT                      
*                                                                               
WORK2    DS    CL200               EXTRA WORK AREA                              
SAVEKEY  DS    CL27                                                             
CHNGLEN  DS    CL1                                                              
*                                                                               
TMPYEAR  DS    CL1                                                              
TMPLEN   DS    CL1                                                              
*                                                                               
DUPYRLEN EQU   840                 DUPLICATE YEAR/LEN COMBO                     
*                                                                               
FIRSTSW  DS    CL1                                                              
DAYINP   DS    CL1                                                              
RMPPROFS DS    CL8                 PROFILE SETTINGS                             
BSVDA    DS    CL4                 SAVED DISK ADDRESS                           
         EJECT                                                                  
* INVENTORY LIST ENTRY DSECT                                                    
*                                                                               
INVLD    DSECT                                                                  
INVLREC  DS    0CL10                                                            
INVLFLE  DS    CL1                 P=PAV, I=INVENTORY                           
INVLTYP  DS    CL1                 X'80'  INVENTORY NUMBER                      
*                                  X'40'  FIRST IN DAY/TIME EXP.                
*                                  X'20'  LAST IN DAY/TIME EXP.                 
*                                  X'08'  ADD EXPRESSION                        
INVLWT   DS    CL1                 WEIGHT (BINARY)                              
INVLDATA DS    0CL6                                                             
INVLSTIM DS    CL2                 START TIME                                   
INVLETIM DS    CL2                 END TIME                                     
INVLDAY  DS    CL1                 DAY                                          
         DS    CL1                 SPARE                                        
         ORG   INVLDATA                                                         
INVLNUMB DS    CL3                 NUMBER                                       
INVLDATE DS    CL3                 START DATE (Y/M/D BINARY)                    
         DS    CL1                 SPARE                                        
         SPACE 2                                                                
         EJECT                                                                  
RINVD    DSECT                                                                  
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE DDCOMFACS                                                      
         SPACE 5                                                                
* SAVED STORAGE IN TWA0 FOR NESFM00 STARTS HERE                                 
T813FFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T813FFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
SVLIST   DS    CL268               CALL ROUTINE STACK POINTER                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073REGAV17S  05/01/02'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
