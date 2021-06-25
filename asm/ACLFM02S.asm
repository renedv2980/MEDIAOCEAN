*          DATA SET ACLFM02S   AT LEVEL 028 AS OF 05/01/02                      
*PHASE T60302B,+0                                                               
*INCLUDE SCANNER                                                                
*INCLUDE UNSCAN                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'LFM MODULE TO HANDLE LEDGER RECORDS'                            
T60302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFM2**,R8                                                    
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         EJECT                                                                  
*              BUILD KEY                                                        
         SPACE 3                                                                
         MVI   ERROR,X'FF'                                                      
         CLI   MODE,BUILDKEY                                                    
         BNE   LE2                                                              
         MVC   KEY,SPACES                                                       
         LA    R2,LOGCOMPH                                                      
         GOTO1 ANY                                                              
         MVC   KEY(1),LOGCOMP                                                   
         CLI   5(R2),1                                                          
         BE    LE00                                                             
         GOTO1 =V(HEXIN),DMCB,LOGCOMP,KEY,2,RR=RB                               
         OC    DMCB+12(4),DMCB+12                                               
         BNZ   LE00                                                             
         MVI   ERROR,2                                                          
         B     XIT                                                              
         SPACE 1                                                                
LE00     EQU   *                                                                
         TM    4(R2),X'20'                                                      
         BO    LE0                                                              
         FOUT  LOGCNAMH,SPACES,36                                               
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         OI    4(R2),X'20'                                                      
         MVI   ANYKEY,C'Y'                                                      
         SPACE 2                                                                
LE0      DS    0H                                                               
         LA    R2,LOGUNITH                                                      
         TM    4(R2),X'20'                                                      
         BO    *+12                                                             
         OI    4(R2),X'20'                                                      
         MVI   ANYKEY,C'Y'                                                      
         GOTO1 ANY                                                              
         MVC   KEY+1(1),LOGUNIT                                                 
         FOUT  LOGUNAMH,SPACES,36                                               
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         SPACE 2                                                                
LE1      MVC   KEY+2(1),LOGLEDG                                                 
         LA    R2,LOGLEDGH                                                      
*&&US                                                                           
         CLI   LOGUNIT,C'T'                                                     
         BE    LE1B                                                             
*&&                                                                             
         CLI   5(R2),2                                                          
         BNL   LE1C                                                             
         B     LE1E                                                             
*&&US                                                                           
LE1B     GOTO1 =V(HEXIN),DMCB,LOGLEDG,KEY+2,2,RR=RB                             
         OC    12(4,R1),12(R1)                                                  
         BZ    LE1C                                                             
         CLI   KEY+2,X'40'                                                      
         BH    LE1E                                                             
*&&                                                                             
LE1C     MVI   ERROR,2                                                          
         B     XIT                                                              
LE1E     GOTO1 ANY                                                              
         TM    4(R2),X'20'                                                      
         BO    XIT                                                              
         OI    4(R2),X'20'                                                      
         MVI   ANYKEY,C'Y'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY LEDGER RECORD                                            
         SPACE 3                                                                
LE2      CLI   MODE,DSPLYREC                                                    
         BNE   LE20                                                             
         FOUT  LOGLNAMH,SPACES,36                                               
         LA    R2,LOGNMDSH                                                      
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         FOUT  LOGNUMH,SPACES,6                                                 
         FOUT  LOGRSETH,SPACES,4                                                
         FOUT  LOGCDACH,SPACES,14                                               
         MVC   LOGCDNM,SPACES                                                   
         OI    LOGCDNMH+6,X'80'                                                 
         MVC   LOGXANL,SPACES                                                   
         OI    LOGXANLH+6,X'80'                                                 
         MVC   LOGXCST,SPACES                                                   
         OI    LOGXCSTH+6,X'80'                                                 
         LA    R4,IO2                                                           
         SR    R5,R5                                                            
         AH    R4,DATADISP                                                      
         SPACE 2                                                                
LE4      CLI   0(R4),0                                                          
         BE    LE12                                                             
         CLI   0(R4),X'14'                                                      
         BE    LE8                                                              
         CLI   0(R4),X'16'                                                      
         BE    LE10                                                             
         CLI   0(R4),X'30'                                                      
         BE    LE16                                                             
         CLI   0(R4),X'11'                                                      
         BE    LE19                                                             
         SPACE 2                                                                
LE6      ZIC   R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     LE4                                                              
         SPACE 2                                                                
LE8      DS    0H                  LEDGER ELEMENT                               
         USING ACLEDGD,R4                                                       
         XC    LOGTYPE,LOGTYPE                                                  
         LA    R6,LOGTYPE                                                       
         MVC   0(5,R6),=C'TYPE='                                                
         MVC   5(1,R6),ACLTTYPE                                                 
         LA    R6,6(R6)                                                         
         MVI   0(R6),C','                                                       
         CLI   ACLTLIKE,C' '                                                    
         BNH   LE8C                SPACE OR X'00'                               
         MVC   1(5,R6),=C'LIKE='                                                
         MVC   6(1,R6),ACLTLIKE                                                 
         LA    R6,7(R6)                                                         
         MVI   0(R6),C','                                                       
*                                                                               
LE8C     CLI   ACLTPRNT,C' '                                                    
         BNH   LE8E                                                             
         MVC   1(6,R6),=C'PRINT='                                               
         MVC   7(1,R6),ACLTPRNT                                                 
         LA    R6,8(R6)                                                         
         MVI   0(R6),C','                                                       
*                                                                               
LE8E     DS    0H                                                               
*&&UK                                                                           
         MVC   1(7,R6),=C'PEELOD='                                              
         MVI   8(R6),C'L'                                                       
         TM    ACLTSTAT,X'80'                                                   
         BZ    *+8                                                              
         MVI   8(R6),C'T'                                                       
         LA    R6,9(R6)                                                         
         MVI   0(R6),C','                                                       
*&&                                                                             
*&&US                                                                           
         TM    ACLTSTAT,X'40'                                                   
         BZ    LE8F                                                             
         MVC   1(6,R6),=C'OFFCK='                                               
         MVI   7(R6),C'O'                                                       
         LA    R6,8(R6)                                                         
         MVI   0(R6),C','                                                       
         SPACE 1                                                                
LE8F     TM    ACLTSTAT,X'20'                                                   
         BZ    LE8FA                                                            
         MVC   1(5,R6),=C'CANAD'                                                
         LA    R6,6(R6)                                                         
         MVI   0(R6),C','                                                       
LE8FA    TM    ACLTSTAT,X'10'                                                   
         BZ    LE8FB                                                            
         MVC   1(7,R6),=C'VEHICLE'                                              
         LA    R6,8(R6)                                                         
         MVI   0(R6),C','                                                       
         SPACE 1                                                                
LE8FB    TM    ACLTSTAT,X'08'                                                   
         BZ    LE8FC                                                            
         MVC   1(6,R6),=C'EXPEND'                                               
         LA    R6,7(R6)                                                         
         MVI   0(R6),C','                                                       
         SPACE 1                                                                
*                                                                               
LE8FC    DS    0H                                                               
*                                                                               
*LE8FC    TM    ACLTSTAT,X'04'                                                  
*         BZ    LE8FD                                                           
*         MVC   1(4,R6),=C'NTAL'                                                
*         LA    R6,5(R6)                                                        
*         MVI   0(R6),C','                                                      
*         SPACE 1                                                               
*LE8FD    TM    ACLTSTAT,X'02'                                                  
*         BZ    LE8FE                                                           
*         MVC   1(6,R6),=C'PAYEST'                                              
*         LA    R6,7(R6)                                                        
*         MVI   0(R6),C','                                                      
          SPACE 1                                                               
LE8FE     TM    ACLTSTAT,X'01'                                                  
          BZ    LE8G                                                            
          MVC   1(4,R6),=C'100%'                                                
          LA    R6,5(R6)                                                        
          MVI   0(R6),C','                                                      
*&&                                                                             
LE8G     CLI   ACLTOFF,0                                                        
         BE    LE8L                                                             
         MVC   1(7,R6),=C'OFFPOS='                                              
         MVC   8(1,R6),ACLTOFF                                                  
         CLI   ACLTOFF,C' '                                                     
         BL    LE8I                                                             
         CLI   ACLTOFF,X'4C'       TEST NEW OFFICE IN KEY LEDGER                
         BNH   LE8I                YES                                          
         CLI   ACLTOFF,X'F0'                                                    
         BH    LE8K                                                             
         LA    R6,9(R6)                                                         
         MVI   0(R6),C','                                                       
         B     LE8L                                                             
*                                                                               
LE8I     ZIC   R0,ACLTOFF                                                       
         N     R0,=F'15'           ISOLATE DISPLACEMENT INTO KEY                
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         LA    R6,8(R6)                                                         
         TM    ACLTOFF,X'40'       TEST NEW OFFICE IN KEY                       
         BZ    *+12                NO                                           
         MVI   0(R6),C'+'          YES-PUT A PLUS SIGN BEFORE NUMBER            
         LA    R6,1(R6)                                                         
*                                                                               
         UNPK  0(2,R6),DUB                                                      
         LA    R6,2(R6)                                                         
         MVI   0(R6),C','                                                       
         B     LE8L                                                             
*                                                                               
LE8K     DS    0H                                                               
         GOTO1 =V(HEXOUT),DMCB,ACLTOFF,8(R6),1,RR=RB                            
         LA    R6,10(R6)                                                        
         MVI   0(R6),C','                                                       
         B     LE8L                                                             
*                                                                               
LE8L     CLI   ACLTLEN,X'20'                                                    
         BL    LE8X                OLD ELEMENT                                  
         LA    RF,IO2                                                           
         CLC   =C'SN',1(RF)                                                     
         BE    LE8M                IGNORE FOR TALENT PERFORMERS                 
         CLI   ACLTDPOS,0                                                       
         BE    LE8M                NO DEPT INFO                                 
         ZIC   R1,ACLTDPOS                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   1(5,R6),=C'DEPT='                                                
         LA    R5,6(R6)                                                         
         LA    R6,8(R6)                                                         
         UNPK  DUB(3),DUB+6(2)                                                  
         MVC   0(2,R5),DUB+1                                                    
         CLI   0(R5),C'0'                                                       
         BNE   *+12                                                             
         BCTR  R6,0                                                             
         MVC   0(1,R5),1(R5)                                                    
         MVI   0(R6),C'/'                                                       
         ZIC   R1,ACLTDLEN                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         LA    R5,1(R6)                                                         
         LA    R6,3(R6)                                                         
         UNPK  DUB(3),DUB+6(2)                                                  
         MVC   0(2,R5),DUB+1                                                    
         CLI   0(R5),C'0'                                                       
         BNE   *+16                                                             
         BCTR  R6,0                                                             
         MVC   0(1,R5),1(R5)                                                    
         MVI   1(R5),C' '                                                       
         MVI   0(R6),C','                                                       
         SPACE 1                                                                
LE8M     OC    ACLTCDAC,ACLTCDAC  ACCOUNT FOR C.D.                              
         BZ    LE8N                                                             
         MVC   LOGCDAC,ACLTCDAC                                                 
         OI    LOGCDACH+6,X'80'                                                 
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(L'ACLTCDAC),ACLTCDAC                                       
         LA    R2,LOGCDACH                                                      
         GOTO1 READ                READ THE ACCOUNT                             
         GOTO1 NAMOUT              AND DISPLAY THE NAME                         
         SPACE 1                                                                
LE8N     CLI   ACLTCLI,0                                                        
         BE    LE8P                                                             
         MVC   1(4,R6),=C'CLI='                                                 
         MVC   5(1,R6),ACLTCLI                                                  
         OI    5(R6),X'F0'                                                      
         LA    R6,6(R6)                                                         
         MVI   0(R6),C','                                                       
         SPACE 1                                                                
LE8P     CLI   ACLTBUD,0                                                        
         BE    LE8Q                                                             
         MVC   1(7,R6),=C'BUDPOS='                                              
         MVC   8(1,R6),ACLTBUD                                                  
         CLI   ACLTBUD,C' '                                                     
         BL    LE8P1                                                            
         CLI   ACLTBUD,X'F0'                                                    
         BH    LE8P2                                                            
         LA    R6,9(R6)                                                         
         MVI   0(R6),C','                                                       
         B     LE8Q                                                             
*                                                                               
LE8P1    ZIC   R0,ACLTBUD                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(2,R6),DUB                                                      
         LA    R6,10(R6)                                                        
         MVI   0(R6),C','                                                       
         B     LE8Q                                                             
*                                                                               
LE8P2    DS    0H                                                               
         GOTO1 =V(HEXOUT),DMCB,ACLTBUD,8(R6),1,RR=RB                            
         LA    R6,10(R6)                                                        
         MVI   0(R6),C','                                                       
*                                                                               
LE8Q     CLI   ACLTOFFC,X'40'      OFFICE FOR G/L UPDATE                        
         BNH   LE8X                                                             
         MVC   1(6,R6),=C'GLOFF='                                               
         MVC   7(2,R6),ACLTOFFC    GLOFF=X(X)                                   
         LA    R6,9(R6)                                                         
         CLI   ACLTOFFC+1,X'40'                                                 
         BH    *+6                                                              
         BCTR  R6,0                ONE BYTE OFFICE                              
         MVI   0(R6),C','                                                       
*                                                                               
LE8X     MVI   0(R6),C' '          BLANK LAST COMMA                             
         ST    R6,ADLAST           SAVE ADDRESS OF LAST BYTE                    
         FOUT  LOGTYPEH                                                         
         LA    RF,IO2                                                           
         CLC   1(2,RF),=C'SE'                                                   
         BNE   *+8                                                              
         BAS   RE,DSPXNL                                                        
         B     LE6                                                              
         SPACE 2                                                                
LE10     DS    0H                  HEIRARCHY                                    
         USING ACHEIRD,R4                                                       
         FOUT  LOGDESAH,ACHRDESA,15                                             
         FOUT  LOGDESBH,ACHRDESB,15                                             
         FOUT  LOGDESCH,ACHRDESC,15                                             
         FOUT  LOGDESDH,ACHRDESD,15                                             
         OI    LOGLEVAH+4,X'20'    FIELD HAS BEEN VALIDATED                     
         OI    LOGLEVBH+4,X'20'                                                 
         OI    LOGLEVCH+4,X'20'                                                 
         OI    LOGLEVDH+4,X'20'                                                 
         FOUT  LOGLEVAH,SPACES,2                                                
         FOUT  LOGLEVBH,SPACES,2                                                
         FOUT  LOGLEVCH,SPACES,2                                                
         FOUT  LOGLEVDH,SPACES,2                                                
         EDIT  (1,ACHRLEVA),(2,LOGLEVA),ALIGN=LEFT                              
         SR    R6,R6                                                            
         SR    R7,R7                                                            
         IC    R7,ACHRLEVB                                                      
         IC    R6,ACHRLEVA                                                      
         LTR   R7,R7                                                            
         BZ    LE6                                                              
         LR    R3,R7                                                            
         SR    R7,R6                                                            
         EDIT  (R7),(2,LOGLEVB),ALIGN=LEFT                                      
         IC    R6,ACHRLEVC                                                      
         LR    R7,R6                                                            
         SR    R7,R3                                                            
         LTR   R6,R6                                                            
         BZ    LE6                                                              
         EDIT  (R7),(2,LOGLEVC),ALIGN=LEFT                                      
         IC    R7,ACHRLEVD                                                      
         LTR   R7,R7                                                            
         BZ    LE6                                                              
         SR    R7,R6                                                            
         EDIT  (R7),(2,LOGLEVD),ALIGN=LEFT                                      
         B     LE6                                                              
         SPACE 2                                                                
LE12     BAS   RE,GENDISP                                                       
         BAS   RE,SRCHDISP                                                      
         LA    R2,LOGLNAMH                                                      
         B     XIT                                                              
*                                                                               
         SPACE 2                                                                
         USING ACSTATD,R4                                                       
LE16     FOUT  LOGSECH,SPACES,3                                                 
         CLC   ACSTSECY,SPACES                                                  
         BE    LE17                                                             
         EDIT  (2,ACSTSECY),(3,LOGSEC),ALIGN=LEFT                               
LE17     TM    ACSTSTX,X'06'       ANY P/L OR BAL SHEET SETTING                 
         BZ    LE6                                                              
         L     R6,ADLAST           GET ADDRS OF LAST BYTE OF TYPE LINE          
         MVC   0(4,R6),=C',P/L'                                                 
         TM    ACSTSTX,X'04'                                                    
         BO    LE6                                                              
         MVC   1(3,R6),=C'BAL'                                                  
         B     LE6                                                              
         SPACE 1                                                                
         USING ACMEDIAD,R4                                                      
LE19     MVC   LOGNUM,ACMDLBIL                                                  
         MVC   LOGRSET,ACMDRSET                                                 
         B     LE6                                                              
         EJECT                                                                  
*              BUILD LEDGER RECORD                                              
         SPACE 2                                                                
         USING ACLEDGD,R4                                                       
LE20     LA    R2,LOGLNAMH                                                      
         GOTO1 ANY                                                              
         GOTO1 NAMIN                                                            
         XC    HALF,HALF                                                        
         XC    SVWO,SVWO                                                        
         MVC   LOGCDNM,SPACES                                                   
         OI    LOGCDNMH+6,X'80'                                                 
*&&US                                                                           
         LA    R4,IO                                                            
         MVI   ELCODE,ACLTELQ      LOOK FOR EXISTING LEDGER EL.                 
         BAS   RE,GETEL                                                         
         BNE   LE20D                                                            
         CLI   ACLTLEN,7           TEST SHORT ELEMENT                           
         BE    LE20D                                                            
         CLC   =C'SN',1(R4)        TALENT PERFORMERS                            
         BNE   *+10                                                             
         MVC   HALF,ACLTAGNT       SAVE LAST AGENT CODE USED                    
         MVC   SVWO,ACLTWO         SAVE WRITE-OFF NUMBER                        
*&&                                                                             
LE20D    LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   PLBAL,0                                                          
         MVC   ACLTEL(2),=X'1420'                                               
         MVC   ACLTAGNT,HALF                                                    
         MVC   ACLTWO,SVWO                                                      
         LA    R2,LOGTYPEH                                                      
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     XIT                                                              
*                                                                               
         GOTO1 =V(SCANNER),DMCB,(R2),(8,BLOCK),RR=RB                            
         CLI   DMCB+4,0                                                         
         BNE   *+12                                                             
LEERR    MVI   ERROR,NOTVLCDE                                                   
         B     XIT                                                              
         MVI   BYTE,0                                                           
         ZIC   R3,DMCB+4                                                        
         LA    R5,BLOCK                                                         
         USING SCAND,R5                                                         
*                                                                               
LE21     CLC   0(2,R5),=X'0000'    END OF TABLE                                 
         BE    LE21X                                                            
         CLC   FLD1(3),=C'P/L'     FIND IF ANY P/LOSS                           
         BNE   *+12                OR BAL. SHEET STATUS SET                     
         MVI   PLBAL,X'04'                                                      
         B     LE21Z                                                            
         CLC   FLD1(3),=C'BAL'                                                  
         BNE   *+12                OR BAL. SHEET STATUS SET                     
         MVI   PLBAL,X'02'                                                      
         B     LE21Z                                                            
*&&US                                                                           
         CLC   FLD1(5),=C'CANAD'                                                
         BNE   *+12                                                             
         OI    ACLTSTAT,X'20'                                                   
         B     LE21Z                                                            
         CLC   FLD1(7),=C'VEHICLE'                                              
         BNE   *+12                                                             
         OI    ACLTSTAT,X'10'                                                   
         B     LE21Z                                                            
         CLC   FLD1(6),=C'EXPEND'                                               
         BNE   *+12                                                             
         OI    ACLTSTAT,X'08'                                                   
         B     LE21Z                                                            
*                                                                               
*        CLC   FLD1(4),=C'NTAL'                                                 
*        BNE   *+12                                                             
*        OI    ACLTSTAT,X'04'                                                   
*        B     LE21Z                                                            
*        CLC   FLD1(6),=C'PAYEST'                                               
*        BNE   *+12                                                             
*        OI    ACLTSTAT,X'02'                                                   
*        B     LE21Z                                                            
         CLC   FLD1(4),=C'100%'                                                 
         BNE   *+12                                                             
         OI    ACLTSTAT,X'01'                                                   
         B     LE21Z                                                            
*&&                                                                             
         CLI   FLD2LEN,0                                                        
         BE    LEERR               2ND FLD ALWAYS REQUIRED                      
         CLC   FLD1(5),=C'GLOFF'                                                
         BE    LE21OFC                                                          
         CLC   FLD1(6),=C'OFFPOS'                                               
         BE    LE21A                                                            
         CLC   FLD1(6),=C'BUDPOS'                                               
         BE    LE21A                                                            
         CLC   FLD1(4),=C'DEPT'                                                 
         BE    LE21A                                                            
         CLC   FLD1(3),=C'CLI'                                                  
         BE    LE21CLI                                                          
         CLI   FLD2LEN,1                                                        
         BNE   LEERR               ONLY OFFPOS CAN HAVE 2 CHARS                 
*                                                                               
LE21A    CLI   FLD1LEN,4                                                        
         BNE   LE21B                                                            
         CLC   FLD1(4),=C'TYPE'        KEYWORDS OF LENGTH 4                     
         BE    LE21TY                                                           
         CLC   FLD1(4),=C'LIKE'                                                 
         BE    LE21LK                                                           
         CLC   FLD1(4),=C'DEPT'                                                 
         BE    LE21DP                                                           
         B     LEERR                                                            
*                                                                               
LE21B    CLI   FLD1LEN,5                                                        
         BNE   LE21C                                                            
         CLC   FLD1(5),=C'PRINT'       KEYWORDS OF LENGTH 5                     
         BE    LE21PR                                                           
         CLC   FLD1(5),=C'OFFCK'                                                
         BE    LE21OC                                                           
         B     LEERR                                                            
*                                                                               
LE21C    CLI   FLD1LEN,6                                                        
         BNE   LEERR                                                            
         CLC   FLD1(6),=C'PEELOD'      KEYWORDS OF LENGTH 6                     
         BE    LE21PL                                                           
         CLC   FLD1(6),=C'OFFPOS'                                               
         BE    LE21OP                                                           
         CLC   FLD1(6),=C'BUDPOS'                                               
         BE    LE21BP                                                           
         B     LEERR                                                            
*                                                                               
LE21TY   DS    0H                  TYPE                                         
         TM    BYTE,X'01'                                                       
         BNZ   LEERR               WAS INPUT ALREADY                            
         CLI   FLD2,C'A'           ANALYSIS                                     
         BE    LE21TYX                                                          
         CLI   FLD2,C'G'           GENERAL                                      
         BE    LE21TYX                                                          
         CLI   FLD2,C'S'           SUBSID                                       
         BNE   LEERR                                                            
*                                                                               
LE21TYX  MVC   ACLTTYPE,FLD2                                                    
         OI    BYTE,X'01'                                                       
         B     LE21Z                                                            
         SPACE 2                                                                
LE21LK   DS    0H                  LIKE                                         
         TM    BYTE,X'02'                                                       
         BNZ   LEERR               WAS INPUT ALREADY                            
         CLI   FLD2,C'R'                                                        
         BE    LE21LKX                                                          
         CLI   FLD2,C'J'                                                        
         BE    LE21LKX                                                          
         CLI   FLD2,C'G'                                                        
         BNE   LEERR                                                            
*                                                                               
LE21LKX  MVC   ACLTLIKE,FLD2                                                    
         OI    BYTE,X'02'                                                       
         B     LE21Z                                                            
         SPACE 2                                                                
LE21PR   DS    0H                  STATEMENT PRINTING CONTROL                   
         TM    BYTE,X'04'                                                       
         BNZ   LEERR               WAS INPUT ALREADY                            
         CLI   FLD2,C'N'                                                        
         BE    LE21PRX                                                          
         CLI   FLD2,C'+'                                                        
         BE    LE21PRX                                                          
         CLI   FLD2,C'-'                                                        
         BNE   LEERR                                                            
*                                                                               
LE21PRX  MVC   ACLTPRNT,FLD2                                                    
         OI    BYTE,X'04'                                                       
         B     LE21Z                                                            
         SPACE 2                                                                
LE21PL   DS    0H            BROUGHT FORWARD ON PEELS                           
*&&US                                                                           
         B     LEERR                                                            
*&&                                                                             
*&&UK                                                                           
         TM    BYTE,X'08'                                                       
         BNZ   LEERR               WAS INPUT ALREADY                            
         CLI   FLD2,C'T'                                                        
         BNE   LE21PL2                                                          
         OI    ACLTSTAT,X'80'                                                   
         B     LE21PLX                                                          
*                                                                               
LE21PL2  CLI   FLD2,C'L'                                                        
         BNE   LEERR                                                            
*                                                                               
LE21PLX  OI    BYTE,X'08'                                                       
         B     LE21Z                                                            
*&&                                                                             
         SPACE 2                                                                
LE21OC   DS    0H            OFFICE CHK PRINTING                                
*&&UK                                                                           
         B     LEERR               INVALID KEYWORD FOR UK                       
*&&                                                                             
*&&US                                                                           
         TM    BYTE,X'10'                                                       
         BNZ   LEERR               WAS INPUT ALREADY                            
         CLI   FLD2,C'O'                                                        
         BNE   LEERR                                                            
         OI    ACLTSTAT,X'40'                                                   
         OI    BYTE,X'10'                                                       
         B     LE21Z                                                            
*&&                                                                             
         SPACE 2                                                                
LE21OP   DS    0H                  POSTION OF OFFICE CODE IN LEDGER             
         TM    BYTE,X'20'                                                       
         BNZ   LEERR               WAS INPUT ALREADY                            
         OC    FLD2B,FLD2B         SEE IF NUMERIC                               
         BZ    LE21OP2                                                          
         L     R0,FLD2B                                                         
         C     R0,=F'12'                                                        
         BH    LEERR                                                            
         C     R0,=F'0'                                                         
         BL    LEERR                                                            
         STC   R0,ACLTOFF                                                       
         B     LE21OPXX                                                         
*                                                                               
LE21OP2  CLI   FLD2LEN,1                                                        
         BNE   LE21OP4                                                          
         CLI   FLD2,C'C'                                                        
         BE    LE21OPX                                                          
         CLI   FLD2,C'P'                                                        
         BE    LE21OPX                                                          
         CLI   FLD2,C'T'                                                        
         BE    LE21OPX                                                          
         CLI   FLD2,C'0'           LEAVE AS X'00' IN ELEM                       
         BE    LE21OPXX                                                         
         B     LEERR                                                            
*                                                                               
LE21OP4  CLI   FLD2,C'+'           TEST PARAMETER STARTS WITH PLUS              
         BNE   LE21OP8                                                          
         TM    COMPSTA4,X'01'      TEST NEW OFFICES COMPANY                     
         BZ    LEERR               NO-DON'T ALLOW IT                            
*                                                                               
         CLI   FLD2LEN,2                                                        
         BL    LEERR                                                            
         BE    *+12                                                             
         CLI   FLD2LEN,3                                                        
         BH    LEERR                                                            
         ZIC   R1,FLD2LEN                                                       
         BCTR  R1,0                CHECK THAT NUMBER FOLLOWS PLUS SIGN          
         LA    RE,FLD2+1                                                        
LE21OP5  CLI   0(RE),C'0'                                                       
         BL    LEERR                                                            
         CLI   0(RE),C'9'                                                       
         BH    LEERR                                                            
         LA    RE,1(RE)                                                         
         BCT   R1,LE21OP5                                                       
*                                                                               
LE21OP6  ZIC   R1,FLD2LEN                                                       
         SH    R1,=H'2'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD2+1(0)                                                    
         CVB   R0,DUB                                                           
         LTR   R0,R0               TEST NUMBER BETWEEN 1-12                     
         BZ    LEERR                                                            
         CH    R0,=H'12'                                                        
         BH    LEERR                                                            
         STC   R0,ACLTOFF                                                       
         OI    ACLTOFF,X'40'       TURN ON SPECIAL BIT                          
         B     LE21OPXX                                                         
*                                                                               
LE21OP8  CLC   FLD2(2),=C'F4'                                                   
         BH    LEERR                                                            
         CLC   FLD2(2),=C'F1'                                                   
         BL    LEERR                                                            
         GOTO1 =V(HEXIN),DMCB,FLD2,ACLTOFF,2,RR=RB                              
         OC    DMCB+12(4),DMCB+12                                               
         BZ    LEERR                                                            
         B     LE21OPXX                                                         
*                                                                               
LE21OPX  MVC   ACLTOFF,FLD2                                                     
LE21OPXX OI    BYTE,X'20'                                                       
         B     LE21Z                                                            
         SPACE 2                                                                
LE21BP   DS    0H                  POSTION OF OFFICE CODE IN LEDGER             
*                                  FOR BUDGETS                                  
         CLI   ACLTBUD,0                                                        
         BNE   LEERR               WAS INPUT ALREADY                            
         OC    FLD2B,FLD2B         SEE IF NUMERIC                               
         BZ    LE21BP2                                                          
         L     R0,FLD2B                                                         
         C     R0,=F'12'                                                        
         BH    LEERR                                                            
         C     R0,=F'0'                                                         
         BL    LEERR                                                            
         STC   R0,ACLTBUD                                                       
         B     LE21BPXX                                                         
*                                                                               
LE21BP2  CLI   FLD2LEN,1                                                        
         BNE   LE21BP4                                                          
         CLI   FLD2,C'C'                                                        
         BE    LE21BPX                                                          
         CLI   FLD2,C'T'                                                        
         BE    LE21BPX                                                          
         CLI   FLD2,C'0'           LEAVE AS X'00' IN ELEM                       
         BE    LE21BPXX                                                         
         B     LEERR                                                            
*                                                                               
LE21BP4  CLC   FLD2(2),=C'F4'                                                   
         BH    LEERR                                                            
         CLC   FLD2(2),=C'F1'                                                   
         BL    LEERR                                                            
         GOTO1 =V(HEXIN),DMCB,FLD2,ACLTBUD,2,RR=RB                              
         OC    DMCB+12(4),DMCB+12                                               
         BZ    LEERR                                                            
         B     LE21BPXX                                                         
*                                                                               
LE21BPX  MVC   ACLTBUD,FLD2                                                     
LE21BPXX DS    0H                                                               
         B     LE21Z                                                            
         SPACE 1                                                                
LE21DP   OC    ACLTDPOS(2),ACLTDPOS                                             
         BNZ   LEERR               ALREADY INPUT                                
         CLI   FLD2LEN,3                                                        
         BL    LEERR               MUST BE 3 OR 4                               
         CLI   FLD2LEN,4                                                        
         BH    LEERR               FORMAT IS N/N OR NN/N                        
         LA    R1,1                                                             
         CLI   FLD2+1,C'/'                                                      
         BE    *+16                                                             
         LA    R1,2                                                             
         CLI   FLD2+2,C'/'                                                      
         BNE   LEERR                                                            
         LR    RF,R1                                                            
         LA    R6,FLD2                                                          
         CLI   0(R6),C'1'                                                       
         BL    LEERR                                                            
         CLI   0(R6),C'9'                                                       
         BH    LEERR                                                            
         LA    R6,1(R6)                                                         
         BCT   RF,*-20                                                          
         LA    R6,FLD2                                                          
         LR    RF,R1                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R6)                                                      
         CVB   R6,DUB                                                           
         STC   R6,ACLTDPOS                                                      
         CH    R6,=H'12'                                                        
         BH    LEERR                                                            
         ZIC   RF,FLD2LEN                                                       
         BCTR  RF,0                                                             
         SR    RF,R1               LENGTH OF FIELD 2 IN RF                      
         CH    RF,=H'1'                                                         
         BNE   LEERR                                                            
         LA    R6,FLD2+1(R1)       R6 TO START OF FIELD 2                       
         LR    R1,RF                                                            
         LR    R7,R6                                                            
         CLI   0(R6),C'1'                                                       
         BL    LEERR                                                            
         CLI   0(R6),C'2'                                                       
         BH    LEERR                                                            
         LR    R6,R7                                                            
         LR    RF,R1                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R6)                                                      
         CVB   R6,DUB                                                           
         STC   R6,ACLTDLEN                                                      
         ZIC   RF,ACLTDPOS                                                      
         AR    RF,R6                                                            
         CH    RF,=H'13'                                                        
         BH    LEERR                                                            
         B     LE21Z                                                            
         SPACE 1                                                                
LE21CLI  OC    FLD2B,FLD2B                                                      
         BZ    LEERR                                                            
         CLI   ACLTCLI,0                                                        
         BNE   LEERR               ALREADY INPUT                                
         CLC   FLD2B,=F'1'                                                      
         BL    LEERR                                                            
         CLC   FLD2B,=F'9'                                                      
         BH    LEERR                                                            
         MVC   ACLTCLI,FLD2B+3                                                  
         B     LE21Z                                                            
*                                                                               
LE21OFC  CLI   ACLTOFFC,X'40'                                                   
         BH    LEERR               ALREADY HAVE OFFICE                          
         MVC   ACLTOFFC,FLD2                                                    
         CLI   FLD2LEN,1                                                        
         BL    LEERR               MUST BE A LEAST 1                            
         BE    LE21Z                                                            
         CLI   FLD2LEN,2                                                        
         BH    LEERR               NEVER MORE THAN 2                            
         TM    COMPSTA4,X'01'      NEW OFFICES                                  
         BNO   LEERR               IF NOT ON 2 OFFICES 2 IS ERROR               
         SPACE 2                                                                
LE21Z    LA    R5,32(R5)                                                        
         BCT   R3,LE21                                                          
*                                                                               
LE21X    TM    BYTE,X'01'           TYPE MUST BE INPUT                          
         BZ    LEERR                                                            
*                                                                               
         LA    R2,LOGCDACH                                                      
         CLI   5(R2),0                                                          
         BE    LE21X5                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         ZIC   R1,LOGCDACH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),LOGCDAC                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         LA    R6,IO                                                            
         AH    R6,DATADISP                                                      
LE21X3   CLI   0(R6),0                                                          
         BE    LEERR                                                            
         CLI   0(R6),X'32'                                                      
         BE    LE21X4                                                           
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     LE21X3                                                           
LE21X4   MVC   ACLTCDAC,KEY+1                                                   
         OC    ACLTCDAC,SPACES                                                  
LE21X5   DS    0H                                                               
         LA    R2,LOGXANLH                                                      
         MVI   ERROR,X'FF'                                                      
         LA    RF,IO2                                                           
         CLC   1(2,RF),=C'SE'                                                   
         BNE   *+8                                                              
         BAS   RE,BLDXNL                                                        
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                 ERROR                                        
         GOTO1 ADDANEL                                                          
         LA    R2,LOGSECH          SECURITY NUMBER                              
         GOTO1 STATIN                                                           
         LA    R4,ELEMENT                                                       
         USING ACSTATD,R4                                                       
         CLI   5(R2),0                                                          
         BE    LE21X7                                                           
         GOTO1 NUMERIC                                                          
         GOTO1 PACK                                                             
         STH   R1,ACSTSECY                                                      
LE21X7   MVC   ACSTSTX,PLBAL                                                    
         GOTO1 REMANEL,DMCB,(X'30',0)                                           
         GOTO1 ADDANEL                                                          
         SPACE 2                                                                
LE22     MVC   ELEMENT,SPACES                                                   
         LA    R2,LOGLEVAH         HEIRARCHIES                                  
         TM    4(R2),X'20'         HAS FIELD BEEN CHANGED                       
         BO    *+12                                                             
         BAS   RE,OKIT                                                          
         BNE   XIT                                                              
         GOTO1 ANY                                                              
         GOTO1 NUMERIC                                                          
         GOTO1 PACK                                                             
         MVI   ERROR,NOTVLCDE                                                   
         CH    R1,=H'12'                                                        
         BH    XIT                                                              
         USING ACHEIRD,R4                                                       
         MVC   ACHREL(2),=X'1642'                                               
         MVI   ACHRLEVA,0                                                       
         MVI   ACHRLEVB,0                                                       
         MVI   ACHRLEVC,0                                                       
         MVI   ACHRLEVD,0                                                       
         STC   R1,ACHRLEVA                                                      
         LR    R3,R1                                                            
         LA    R2,LOGDESAH                                                      
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   ACHRDESA,WORK                                                    
         LA    R2,LOGLEVBH                                                      
         TM    4(R2),X'20'         HAS FIELD BEEN CHANGED                       
         BO    *+12                                                             
         BAS   RE,OKIT                                                          
         BNE   XIT                                                              
         CLI   5(R2),0                                                          
         BE    LE24                                                             
         GOTO1 NUMERIC                                                          
         GOTO1 PACK                                                             
         AR    R3,R1                                                            
         MVI   ERROR,NOTVLCDE                                                   
         CH    R3,=H'12'                                                        
         BH    XIT                                                              
         LTR   R1,R1                                                            
         BZ    LE24                                                             
         STC   R3,ACHRLEVB                                                      
         LA    R2,LOGDESBH                                                      
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   ACHRDESB,WORK                                                    
         SPACE 2                                                                
         LA    R2,LOGLEVCH                                                      
         TM    4(R2),X'20'         HAS FIELD BEEN CHANGED                       
         BO    *+12                                                             
         BAS   RE,OKIT                                                          
         BNE   XIT                                                              
         CLI   5(R2),0                                                          
         BE    LE24                                                             
         GOTO1 NUMERIC                                                          
         LTR   R1,R1                                                            
         BZ    LE24                                                             
         AR    R3,R1                                                            
         MVI   ERROR,NOTVLCDE                                                   
         CH    R3,=H'12'                                                        
         BH    XIT                                                              
         STC   R3,ACHRLEVC                                                      
         LA    R2,LOGDESCH                                                      
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   ACHRDESC,WORK                                                    
         SPACE 2                                                                
         LA    R2,LOGLEVDH                                                      
         TM    4(R2),X'20'         HAS FIELD BEEN CHANGED                       
         BO    *+12                                                             
         BAS   RE,OKIT                                                          
         BNE   XIT                                                              
         CLI   5(R2),0                                                          
         BE    LE24                                                             
         GOTO1 NUMERIC                                                          
         LTR   R1,R1                                                            
         BZ    LE24                                                             
         AR    R3,R1                                                            
         MVI   ERROR,NOTVLCDE                                                   
         CH    R3,=H'12'                                                        
         BH    XIT                                                              
         STC   R3,ACHRLEVD                                                      
         LA    R2,LOGDESDH                                                      
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   ACHRDESD,WORK                                                    
         SPACE 2                                                                
LE24     CH    R3,=H'12'                                                        
         BNE   XIT                                                              
         GOTO1 ADDANEL                                                          
         GOTO1 REMANEL,DMCB,(X'15',0)                                           
         LA    R2,LOGGENH                                                       
         CLI   5(R2),0                                                          
         BNE   LE26                                                             
         CLI   LOGTYPE,C'S'                                                     
         BNE   LE29A                                                            
         SPACE 2                                                                
LE26     GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         BAS   RE,GENVAL                                                        
         LA    R2,LOGGEN2H                                                      
         BAS   RE,GENVAL                                                        
         LA    R2,LOGGEN3H                                                      
         BAS   RE,GENVAL                                                        
         LA    R2,LOGGEN4H                                                      
         BAS   RE,GENVAL                                                        
         LA    R2,LOGGEN5H                                                      
         BAS   RE,GENVAL                                                        
         SPACE 2                                                                
LE29A    GOTO1 REMANEL,DMCB,(X'11',0)                                           
         LA    R2,LOGNUMH                                                       
         CLI   5(R2),0             ADD BILL AND RESET NUMBER FOR                
         BE    LE32                PRODUCTION                                   
         CLI   5(R2),6                                                          
         BNE   LE30                                                             
         GOTO1 NUMERIC                                                          
         SPACE 1                                                                
         USING ACMEDIAD,R4                                                      
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   ACMDEL,X'11'                                                     
         MVI   ACMDLEN,X'46'                                                    
         MVC   ACMDCODE(46),SPACES                                              
         MVI   ACMDANAL,C' '                                                    
         MVC   ACMDFBIL,LOGNUM                                                  
         MVC   ACMDLBIL,LOGNUM                                                  
         LA    R2,LOGRSETH                                                      
         GOTO1 ANY                                                              
         CLI   5(R2),4                                                          
         BNE   LE32                                                             
         GOTO1 NUMERIC                                                          
         MVC   ACMDRSET,LOGRSET                                                 
         GOTO1 ADDANEL                                                          
         B     LE32                                                             
         SPACE 2                                                                
LE30     MVI   ERROR,INVALID                                                    
         B     XIT                                                              
         SPACE 2                                                                
LE32     DS    0H                                                               
*                                                                               
* CHECK FOR SEARCH                                                              
*                                                                               
         CLI   LOGXSCH,C'Y'        NAME SEARCH PTRS?                            
         BNE   LE40                                                             
*&&DO                                                                           
         LA    RF,SRCHLEDG         SEE IF LEDGER IS ALLOWED                     
LE33     CLI   0(RF),X'FF'         END OF LEDGER SEARCH TABLE                   
         BNE   LE34                                                             
         MVC   LOGHEAD(L'SRCH_NA),SRCH_NA                                       
         OI    LOGHEADH+6,X'80'                                                 
         LA    R2,LOGXSCHH         SEARCH FIELD ON TWA                          
         MVI   ERROR,X'FE'         NOT IN TABLE                                 
         B     XIT                                                              
*                                                                               
LE34     CLC   LOGUNIT(1),0(RF)                                                 
         BNE   LE35                                                             
         CLC   LOGLEDG(1),1(RF)                                                 
         BE    LE39                                                             
LE35     LA    RF,2(RF)                                                         
         B     LE33                                                             
*&&                                                                             
LE39     LA    R1,IO2                                                           
         AH    R1,DATADISP                                                      
         USING LDGELD,R1                                                        
         XR    RF,RF                                                            
         CLI   LDGEL,LDGELQ                                                     
         BE    *+12                                                             
         IC    RF,LDGLN                                                         
         BXH   R1,RF,*-12                                                       
         OI    LDGSTAT2,LDGSSRCH                                                
LE40     MVI   ERROR,X'FF'                                                      
         SPACE 2                                                                
*                                                                               
XIT      XIT1  REGS=(R2)                                                        
         SPACE 3                                                                
OKIT     DS    0H                                                               
         CLI   LOGACT,C'N'                                                      
         BER   RE                  OK TO CHANGE IF NEW                          
         CLC   LOGPASS,=C'%%%'                                                  
         BER   RE                  OR IF CORRECT PASSWORD IS ENTERED            
         MVI   ERROR,87            INVALID CHANGE                               
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY GENERAL LEDGER INSTRUCTIONS                   
         SPACE 3                                                                
GENDISP  NTR1                                                                   
         FOUT  LOGGENH,SPACES,56                                                
         FOUT  LOGGEN2H,SPACES,56                                               
         FOUT  LOGGEN3H,SPACES,56                                               
         FOUT  LOGGEN4H,SPACES,56                                               
         FOUT  LOGGEN5H,SPACES,56                                               
         MVI   IO,C' '                                                          
         MOVE  (IO+1,999),IO                                                    
         LA    R2,IO                                                            
         LA    R3,40                                                            
         LA    R4,IO2                                                           
         SR    R5,R5                                                            
         AH    R4,DATADISP                                                      
         SR    R6,R6                                                            
         SPACE 2                                                                
GD2      CLI   0(R4),0             LOOK FOR INSTRUCTION ELEMENTS                
         BE    GD8                                                              
         CLI   0(R4),X'15'                                                      
         BE    GD4                                                              
GD3      IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     GD2                                                              
         SPACE 2                                                                
         USING ACGENLD,R4                                                       
GD4      MVC   0(10,R2),ACGLSUB    FORMAT TO BLOCK                              
         MVC   10(10,R2),ACGLACC                                                
         CLI   ACGLLEN,26                                                       
         BL    *+10                                                             
         MVC   10(14,R2),ACGLACC                                                
         CLC   ACGLSUB,SPACES                                                   
         BNE   GD6                                                              
         MVC   0(7,R2),=C'DEFAULT'                                              
         SPACE 2                                                                
GD6      LA    R6,1(R6)                                                         
         LA    R2,24(R2)                                                        
         BCT   R3,GD3                                                           
         SPACE 2                                                                
GD8      LTR   R6,R6                                                            
         BZ    XIT                                                              
         GOTO1 =V(UNSCAN),DMCB,((R6),IO),(14,LOGGENH),0,RR=RB                   
         CLI   DMCB,0                                                           
         BE    XIT                                                              
         GOTO1 (RF),(R1),,(14,LOGGEN2H)                                         
         CLI   DMCB,0                                                           
         BE    XIT                                                              
         GOTO1 (RF),(R1),,(14,LOGGEN3H)                                         
         CLI   DMCB,0                                                           
         BE    XIT                                                              
         GOTO1 (RF),(R1),,(14,LOGGEN4H)                                         
         CLI   DMCB,0                                                           
         BE    XIT                                                              
         GOTO1 (RF),(R1),,(14,LOGGEN5H)                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE GENERAL LEDGER DETAILS                       
         SPACE 3                                                                
GENVAL   LR    R9,RE                                                            
         CLI   5(R2),0                                                          
         BCR   8,R9                                                             
         GOTO1 =V(SCANNER),DMCB,(14,(R2)),(8,BLOCK),RR=RB                       
         SR    R3,R3                                                            
         IC    R3,DMCB+4                                                        
         LTR   R3,R3                                                            
         BZ    LE30                                                             
         LA    R4,ELEMENT                                                       
         USING ACGENLD,R4                                                       
         LA    R5,BLOCK                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),LOGCOMP                                                   
         CLI   LOGCOMPH+5,1                                                     
         BE    GVAA                                                             
         GOTO1 =V(HEXIN),DMCB,LOGCOMP,KEY,2,RR=RB                               
GVAA     DS    0H                                                               
         GOTO1 READ                                                             
         LA    R6,IO                                                            
         AH    R6,DATADISP                                                      
         USING ACCOMPD,R6                                                       
         SPACE 2                                                                
GVA      CLI   0(R6),X'10'                                                      
         BE    GVB                                                              
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     GVA                                                              
         SPACE 2                                                                
GVB      DS    0H                                                               
         MVC   ACGLEL(2),=X'151A'                                               
         SPACE 1                                                                
GV2      CLI   1(R5),0             MUST HAVE 'DEFAULT='                         
         BE    LE30                                                             
*                                                                               
         CLI   KEY,X'AB'           DON'T EDIT GL ACCOUNT FOR H AND K            
         BE    GV2G                                                             
*                                                                               
         MVC   KEY+3(12),SPACES                                                 
         MVC   KEY+1(14),22(R5)                                                 
         CLI   KEY+1,C'G'                                                       
         BNE   LE30                                                             
         LA    RE,KEY+3            LOOK FOR AN ASTERISK                         
         LA    RF,12                                                            
GV2A     CLI   0(RE),C'*'                                                       
         BE    GV2C                                                             
         LA    RE,1(RE)                                                         
         BCT   RF,GV2A                                                          
         B     GV2E                                                             
         SPACE 1                                                                
GV2C     MVI   0(RE),C' '          BLANK IT OUT                                 
         LA    RF,KEY                                                           
         SR    RE,RF                                                            
         STC   RE,BYTE                                                          
         GOTO1 HIGH                                                             
         ZIC   RE,BYTE                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   LE30                                                             
         B     GV2G                                                             
GV2E     GOTO1 READ                                                             
GV2G     MVC   ACGLSUB(10),12(R5)                                               
         MVC   ACGLACC(14),22(R5)                                               
         CLC   ACGLSUB(7),=C'DEFAULT'                                           
         BNE   GV6                                                              
         MVC   ACGLSUB,SPACES                                                   
         SPACE 2                                                                
GV6      GOTO1 ADDANEL                                                          
         LA    R5,36(R5)                                                        
         BCT   R3,GV2                                                           
         BR    R9                                                               
         DROP  R1                                                               
         EJECT                                                                  
SRCHDISP NTR1                                                                   
         MVI   LOGXSCH,C' '        DEFAULT BLANK                                
         OI    LOGXSCHH+6,X'80'                                                 
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
         USING LDGELD,R4                                                        
SRCHLP   SR    R5,R5                                                            
         CLI   LDGEL,0             END OF RECORD                                
         BE    XIT                                                              
         CLI   LDGEL,LDGELQ                                                     
         BNE   SRCH5                                                            
         CLI   LDGLN,LDGLNQ                                                     
         BL    SRCH5                                                            
         TM    LDGSTAT2,LDGSSRCH                                                
         BZ    SRCH5                                                            
         MVI   LOGXSCH,C'Y'        LEDGER FOR SEARCH                            
         B     XIT                                                              
*                                                                               
SRCH5    IC    R5,LDGLN                                                         
         AR    R4,R5                                                            
         B     SRCHLP                                                           
         EJECT                                                                  
*              ROUTINES FOR EXPENSE ANALYSIS                                    
*                                                                               
*                                                                               
         USING ACLEDGD,R4                                                       
DSPXNL   NTR1  ,             DISPLAY EXPSENSE ANALYSIS CATEGORY FIELD           
         MVI   LOGXANL,C'N'                                                     
         MVC   LOGXANL+1(L'LOGXANL-1),LOGXANL                                   
         LA    R0,12                                                            
         LA    RF,LOGXANL                                                       
         SR    R1,R1                                                            
         ICM   R1,3,ACLTMSK        12 BIT MASK                                  
         SLL   R1,15                                                            
*                                                                               
         SLL   R1,1                SHIFT TO HIGH ORDER OF R1                    
         LTR   R1,R1               IS IT ON                                     
         BNM   *+8                                                              
         MVI   0(RF),C'Y'          POSITION IS PART OF CATEGORY                 
         LA    RF,1(RF)            RF = NEXT SCREEN POSITION                    
         BCT   R0,*-18                                                          
         MVI   LOGXCST,C'N'        SET POSTING INDICATOR                        
         TM    ACLTSTAT,X'04'                                                   
         BNO   *+8                                                              
         MVI   LOGXCST,C'Y'                                                     
         B     EXIT                                                             
*                                                                               
*                                                                               
BLDXNL   NTR1  ,                   BUILD EXPENSE CATEGORY MASK                  
         CLI   LOGXANLH+5,0        TEST NO INPUT IN EITHER FIELD                
         BNE   *+12                                                             
         CLI   LOGXCSTH+5,0                                                     
         BE    BLDXNL7                                                          
         MVI   ERROR,2                                                          
         LA    R0,12                                                            
         SR    R5,R5                                                            
         SR    R3,R3                                                            
         LA    RF,LOGXANL                                                       
*                                                                               
BLDXNL3  CLI   0(RF),C'N'           MUST BE N OR Y                              
         BE    BLDXNL5                                                          
         CLI   0(RF),C'Y'                                                       
         BNE   EXIT                ERROR                                        
         O     R3,=X'00000001'                                                  
         AH    R5,=H'1'            COUNT NUMBER OF POSITIONS                    
         CH    R5,=H'5'            NOT MORE THAN 5                              
         BH    EXIT                                                             
*                                                                               
BLDXNL5  SLL   R3,1                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,BLDXNL3                                                       
         SLL   R3,3                                                             
         STCM  R3,3,ACLTMSK                                                     
         LA    R2,LOGXCSTH                                                      
         CLI   LOGXCST,C'N'        NO COST POSTINGS                             
         BE    BLDXNL7                                                          
         CLI   LOGXCST,C'Y'                                                     
         BNE   XIT                 ERROR                                        
         OI    ACLTSTAT,X'04'                                                   
BLDXNL7  MVI   ERROR,X'FF'                                                      
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*   ALLOWABLE LEDGERS TO HAVE SEARCH OPTION ON                                  
*---------------------------------------------------------------------          
SRCHLEDG DC    C'SP'               ONLY PAYABLES FOR NOW                        
         DC    C'SQ'                                                            
         DC    C'SS'                                                            
         DC    C'ST'                                                            
         DC    C'SU'                                                            
         DC    C'SV'                                                            
         DC    C'SW'                                                            
         DC    C'SX'                                                            
         DC    C'SY'                                                            
         DC    X'FF'                                                            
*                                                                               
SRCH_NA  DC    CL50'**ERROR  SEARCH NOT AVAILABLE FOR THIS UNIT/LEDGER'         
         EJECT                                                                  
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMFDD                                                       
       ++INCLUDE ACLFMWORK                                                      
         SPACE 1                                                                
ADLAST   DS    A                                                                
BYTE     DS    CL1                                                              
PLBAL    DS    CL1                                                              
ELCODE   DS    CL1                                                              
SVWO     DS    XL2                                                              
         EJECT                                                                  
* ACGENBOTH                                                                     
* ACLFMEQU                                                                      
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028ACLFM02S  05/01/02'                                      
         END                                                                    
