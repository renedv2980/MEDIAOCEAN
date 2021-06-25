*          DATA SET SPLFM2A    AT LEVEL 030 AS OF 05/01/02                      
*PHASE T2192AA,+0                                                               
         TITLE 'T2192A - DEMO OVERRIDE COPY'                                    
T2192A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2192A                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
*                                                                               
         LA    R0,REC2             CLEAR WORK AREA IN REC2                      
         LA    R1,2000                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
* INITIALIZE DBLOCK                                                             
         LA    RE,MYDBLOCK                                                      
         USING DBLOCKD,RE                                                       
         MVC   DBCOMFCS,VCOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'C'       CANADIAN                                     
         DROP  RE                                                               
*                                                                               
         MVC   DMCB+4(4),=X'D9000AE0'   DEMOCON                                 
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDEMOCON,0(R1)                                                   
*                                                                               
         MVC   DMCB+4(4),=X'D9000AD9'   DEMOVAL                                 
         GOTO1 VCALLOV,DMCB,0                                                   
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDEMOVAL,0(R1)                                                   
*                                                                               
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING DOVRECD,R8                                                       
         CLI   SVFMTSW,0           TEST FORMAT OR EDIT                          
*        BE    FMT                                                              
*        B     EDT                                                              
         B     FMT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
FMT      DS    0H                                                               
         NI    STATUS,X'FF'-(STXISTS)                                           
         CLC   TOKEY,SVKEY         CHANGE IN TO KEY?                            
         BE    *+8                                                              
         OI    STATUS,STREDISP     SET TO REDISPLAY                             
         MVC   TOKEY,SVKEY         SAVE 'TO' KEY                                
         MVC   FROMKEY,SVKEY       GET FROM RECORD                              
*                                                                               
FMT05    LA    R2,DMCFRSHH         VALIDATE FROM SHOW                           
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),0                                                          
         BE    LFMERR                                                           
         TM    DMCFRSHH+4,X'20'    PREVIOUSLY VALIDATED                         
         BO    FMT10                                                            
         OI    STATUS,STREDISP     SET TO REDISPLAY                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D12'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(4),SVNTWK                                                  
         MVC   KEY+8(4),8(R2)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   LFMERR                                                           
         GOTO1 GETREC                                                           
         USING NPGMRECD,R8                                                      
         L     R8,AREC                                                          
         MVC   DMCSHNM,NPGMPGM     FROM SHOW NAME                               
         FOUT  DMCSHNMH                                                         
         OI    DMCFRSHH+4,X'20'    VALIDATED                                    
         DROP  R8                                                               
*                                                                               
FMT10    MVC   TOKEY,SVKEY         SAVE 'TO' KEY                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FMT15                                                            
         OI    STATUS,STXISTS      TO  REC EXISTS                               
         CLI   DMCOWRT,C'Y'                                                     
         BE    FMT15                                                            
         LA    R2,DMCOWRTH                                                      
         MVI   ERRCD,INVERR                                                     
         B     LFMERR                                                           
*                                                                               
FMT15    MVC   FROMKEY,SVKEY       GET FROM RECORD                              
         MVC   FROMKEY+7(4),DMCFRSH                                             
         OC    FROMKEY+7(4),=C'    '                                            
         MVC   KEY,FROMKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   LFMERR                                                           
         GOTO1 GETREC                                                           
         LA    R2,REC+24                                                        
         CLI   0(R2),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                MUST FIND 01 ELEM                            
*                                                                               
FMT20    DS    0H                  FORMAT DEMO LIST                             
         TM    STATUS,STREDISP                                                  
         BNO   FMT50               COPY SELECTED DEMOS                          
         BAS   RE,CLRSCR           CLEAR DEMOS                                  
*                                                                               
         LA    R2,REC+24                                                        
         SR    R4,R4                                                            
         ZIC   R5,1(R2)                                                         
         SH    R5,=H'12'                                                        
         BP    *+6                                                              
         DC    H'0'                INVALID RECORD                               
         D     R4,=F'3'            LEAVES R5 SET FOR BCT                        
         LA    R4,12(R2)           POINT TO FIRST DEMO                          
*                                                                               
         LA    R2,DMCDEM1H         POINT TO FIRST DEMO LINE                     
         BAS   RE,BUMPFLD          BUMP TO FIRST DEMO                           
FMT22    DS    0H                                                               
         GOTO1 VDEMOCON,DMCB,(1,(R4)),(2,F),(C'S',MYDBLOCK),0                   
         XC    8(10,R2),8(R2)                                                   
         MVC   8(8,R2),F                                                        
         FOUT  (R2)                                                             
*                                                                               
         CLC   =C'USER',F          CHK FOR USER DEMO                            
         BNE   FMT24                                                            
         XC    8(10,R2),8(R2)                                                   
         MVC   8(3,R2),=C'U /'     FORMAT IS LIKE U1/                           
         MVC   9(1,R2),F+4         MOVE NUMBER                                  
         MVC   11(5,R2),F          MOVE NAME                                    
*                                                                               
FMT24    BAS   RE,BUMPFLD          NEXT SELECT FIELD                            
         BAS   RE,BUMPFLD          NEXT DEMO FIELD                              
         LA    R4,3(R4)            NEXT DEMO IN ELEM                            
         OC    0(3,R4),0(R4)       TEST ANY MORE                                
         BZ    FMT40                                                            
         BCT   R5,FMT22                                                         
FMT40    LA    R2,DMCDALLH         POINT TO SELECT ALL                          
         NI    STATUS,X'FF'-STREDISP                                            
         MVI   ERRCD,DATERR                                                     
         B     LFMERR                                                           
*                                                                               
FMT50    DS    0H                  COPY SELECTED DEMOS                          
         LA    R2,DMCDALLH         POINT TO SELECT ALL                          
*                                                                               
         MVC   KEY,TOKEY           REREAD TO REC                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FMT120              NEED TO ADD                                  
         LA    R1,REC5                                                          
         ST    R1,AREC             READ OLD REC INTO REC5                       
         GOTO1 GETREC                                                           
         LA    R1,REC                                                           
         ST    R1,AREC             PUT NEW REC FROM REC                         
         GOTO1 PUTREC                                                           
         B     FMTX                                                             
*                                                                               
FMT120   GOTO1 ADDREC                                                           
FMTX     B     EXIT                                                             
         EJECT                                                                  
*================================================================*              
* BUMP FIELD AT R2                                               *              
*================================================================*              
         SPACE 1                                                                
BUMPFLD  NTR1                                                                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*================================================================*              
* CLEAR FIELDS WITH DATA                                         *              
*================================================================*              
         SPACE 1                                                                
CLRSCR   NTR1                                                                   
         LA    R2,DMCDEM1H                                                      
*                                                                               
CLRSCR2  ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R1,DMCDEMXH                                                      
         BNL   EXIT                                                             
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,FMTOC                                                         
         BE    CLRSCR2                                                          
         EX    RE,FMTCLC                                                        
         BE    CLRSCR2                                                          
         EX    RE,FMTXC                                                         
         OI    6(R2),X'80'         SET XMT                                      
         B     CLRSCR2                                                          
*                                                                               
FMTOC    OC    8(0,R2),8(R2)                                                    
FMTXC    XC    8(0,R2),8(R2)                                                    
FMTCLC   CLC   8(0,R2),SPACES                                                   
         EJECT                                                                  
*=================================================================*             
* SET END OF RECORD TO ZEROS                                                    
*=================================================================*             
SETEOR   DS    0H                                                               
         L     R8,AREC                                                          
         USING DOVRECD,R8                                                       
         SR    RF,RF                                                            
         ICM   RF,3,DOVLEN                                                      
         LA    RF,REC(RF)                                                       
         XC    0(2,RF),0(RF)       ZERO END OF RECORD                           
         BR    RE                                                               
         DROP  R8                                                               
         EJECT                                                                  
*=================================================================*             
* IF NO 05 ELEMS, ALLOW CHANGE OF DEMOS                                         
*=================================================================*             
         SPACE 1                                                                
         LA    R2,REC+24                        RESET R2                        
         MVC   ELEM+2(3),2(R2)                  MOVE CREATION DATE              
         GOTO1 VRECUP,DMCB,(0,REC),(R2),0       DELETE OLD 01                   
         GOTO1 (RF),(R1),(0,REC),ELEM,(R2)      ADD NEW 01 ELEM                 
         BAS   RE,SETEOR                                                        
         SPACE 1                                                                
         EJECT                                                                  
*                                                                               
EDTOVE1  MVC   NERRCD,=AL2(NOSHOW)                                              
         B     EDTOVEX                                                          
*                                                                               
EDTOVE2  MVC   NERRCD,=AL2(NOSHWDTA)                                            
*                                                                               
EDTOVEX  DS    0H                                                               
         MVI   ERRCD,NEWERR                                                     
         OI    6(R2),X'40'         POSITION CURSOR                              
         B     LFMERR                                                           
*                                                                               
CHGERR   MVI   ERRCD,NOCHGERR                                                   
         B     LFMERR                                                           
*                                                                               
DEMERR   MVI   ERRCD,DEMINV                                                     
         B     LFMERR                                                           
*                                                                               
IMPERR   MVI   ERRCD,INVERR                                                     
         B     LFMERR                                                           
         EJECT                                                                  
LFMERR   GOTO1 ERROR                                                            
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPLFMWRK                                                       
GENOLD   DSECT                                                                  
         ORG   REC2                                                             
         DS    CL2                                                              
FH       DS    XL8                                                              
F        DS    XL32                                                             
TODAYB   DS    XL3                                                              
         DS    XL5                                                              
*                                                                               
         DS    0D                                                               
MYDBLOCK DS    XL256                                                            
SVEL01   DS    XL256                                                            
SVEL02   DS    XL256                                                            
BLK      DS    XL256                                                            
         EJECT                                                                  
T219FFD  DSECT                                                                  
         ORG   LFMTABH                                                          
*SPLFMEA                                                                        
       ++INCLUDE SPLFMEAD                                                       
         EJECT                                                                  
VDEMOCON DS    V                                                                
VDEMOVAL DS    V                                                                
STATUS   DS    XL1                                                              
STXISTS  EQU   X'80'                                                            
STREDISP EQU   X'40'                                                            
TOKEY    DS    CL13                                                             
FROMKEY  DS    CL13                                                             
REC5     DS    CL2000                                                           
         EJECT                                                                  
SCAND    DSECT                                                                  
*         DSECT TO COVER SCANNER LINES                                          
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENNDOV                                                      
       ++INCLUDE SPGENNPGM                                                      
         PRINT OFF                                                              
         EJECT                                                                  
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030SPLFM2A   05/01/02'                                      
         END                                                                    
