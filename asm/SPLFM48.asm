*          DATA SET SPLFM48    AT LEVEL 020 AS OF 07/29/02                      
         TITLE 'T21948 - PROCTOR AND GAMBLE ESTIMATE RECORD'                    
*PHASE T21948A                                                                  
         PRINT NOGEN                                                            
T21948   CSECT                                                                  
         NMOD1 0,*T21948*                                                       
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     R9,VCOMFACS                                                      
         USING COMFACSD,R9                                                      
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING PGESTD,R8                                                        
         CLI   SVFMTSW,0           TEST FORMAT OR EDIT                          
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
FMT      DS    0H                  DISPLAY DATA                                 
         CLI   SVACT,C'A'          TEST FORMAT BEFORE ADD                       
         BE    EXXMOD              YES - JUST EXIT                              
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
*                                                                               
         LR    R6,R8               A(REC)                                       
         MVI   ELCODE,PGSTEIDQ     X'10'                                        
         MVI   ERRCD,MSSNGERR                                                   
*                                                                               
         USING PGSTELMD,R6                                                      
*                                                                               
         CLC   =C'PG',LFMREC       PROCTOR AND GAMBLE                           
         BE    DISPG                                                            
         CLC   =C'AT',LFMREC       AT&T                                         
         BE    DISAT                                                            
         CLC   =C'GF',LFMREC                                                    
         BE    DISGF                                                            
         MVI   ERRCD,NOESTERR                                                   
         B     LFMERR                                                           
*                                                                               
DISPG    DS    0H                                                               
*                                                                               
         BRAS  RE,PGDISP                                                        
*                                                                               
         B     FMTX                                                             
         EJECT                                                                  
*                                                                               
DISAT    DS    0H                                                               
*                                                                               
         BRAS  RE,ATDISP                                                        
*                                                                               
         B     FMTX                                                             
         EJECT                                                                  
*                                                                               
DISGF    DS    0H                                                               
*                                                                               
         BRAS  RE,GFDISP                                                        
*                                                                               
*                                                                               
*                                                                               
*                                                                               
FMTX     B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*********************                                                           
*     EDIT MODE     *                                                           
*********************                                                           
*                                                                               
EDT      DS    0H                  VALIDATE DATA                                
         CLI   SVACT,C'A'          ACTION ADD                                   
         BE    EDT10                                                            
         MVC   KEY,SVKEY           ELSE ACTION CHANGE                           
         CLI   SVACT,C'R'                                                       
         BNE   EDT01                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
         OI    DMINBTS,X'08'                                                    
*                                                                               
EDT01    DS    0H                                                               
         GOTO1 READ                                                             
         CLI   SVACT,C'R'                                                       
         BNE   EDT02                                                            
         NI    KEY+13,X'7F'                                                     
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'SPTDIR',KEY,KEY                       
*                                                                               
EDT02    DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R1,AREC                                                          
         CLI   SVACT,C'R'                                                       
         BNE   *+12                                                             
         NI    KEY+15,X'7F'                                                     
         B     WRTREC                                                           
*                                                                               
* DELETE ALL X'10' ELEMENTS                                                     
*                                                                               
         MVI   ERRCD,INVERR                                                     
*                                                                               
         GOTO1 CHELLO,DMCB,(C'D',=C'SPTFIL'),('PGSTEIDQ',AREC),0                
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
         CLI   SVACT,C'X'          TEST ACTION DELETE                           
         BE    EDTDEL                                                           
         B     EDT20                                                            
*                                                                               
EDT10    DS    0H                  BUILD THE KEY                                
         MVC   PGKEY,SVKEY                                                      
         MVI   PGKRLEN+1,X'19'     L'KEY + 1                                    
         XC    PGSTAT,PGSTAT                                                    
         XC    PGKLINK,PGKLINK                                                  
         MVC   PGKALAG,AGYALPHA                                                 
         MVI   PGKEDQ(R8),0        END MARKER                                   
         SPACE 1                                                                
*===============================================================*               
* VALIDATE DATA                                                 *               
*===============================================================*               
         SPACE 1                                                                
EDT20    DS    0H                                                               
         CLC   =C'PG',LFMREC                                                    
         BE    VALPG                                                            
         CLC   =C'AT',LFMREC                                                    
         BE    VALAT                                                            
         B     VALGF                                                            
         EJECT                                                                  
VALPG    LA    R6,ELEM                                                          
         USING PGSTELMD,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   PGSTEID,PGSTEIDQ                                                 
         MVI   PGSTELN,PGSTELNQ                                                 
         MVI   PGSTESEQ,X'01'      INITIALIZE SEQUENCE NUMBER                   
*                                                                               
         LA    R2,PGECHRH          CHARGE PERIOD HEADER                         
         BAS   RE,TESTPG0                                                       
         BE    VPG2                                                             
*                                                                               
         MVI   ERRCD,INVERR                                                     
         TM    4(R2),X'08'         NUMERIC?                                     
         BZ    LFMERR              NO                                           
         CLI   5(R2),L'PGECHR      EXACT LENGTH?                                
         BNE   LFMERR              NO                                           
         MVC   PGSTNAME,=C'CHRGPER '                                            
         MVC   PGSTDATA(3),PGECHR                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
VPG2     LA    R2,PGEACCH          ACCOUNT                                      
         BAS   RE,TESTPG0                                                       
         BE    VPG4                                                             
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),L'PGEACC                                                   
         BNE   LFMERR                                                           
         MVI   PGSTESEQ,X'02'      SECOND ELEMENT                               
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'ACCOUNT '                                            
         MVC   PGSTDATA(6),PGEACC                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
* BRAND ALLOWED REGARDLESS OF ESTIMATE                                          
*                                                                               
VPG4     LA    R2,PGEBRNH          BRAND                                        
         GOTO1 ANY                                                              
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),L'PGEBRN                                                   
         BNE   LFMERR                                                           
         MVI   PGSTESEQ,X'03'      THIRD ELEMENT                                
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'BRAND   '                                            
         MVC   PGSTDATA(4),PGEBRN                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
         LA    R2,PGEESTH          ESTIMATE #                                   
         BAS   RE,TESTPG0                                                       
         BE    VPG6                                                             
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),L'PGEEST                                                   
         BNE   LFMERR                                                           
         MVI   PGSTESEQ,X'04'      FOURTH ELEMENT                               
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'ESTIMATE'                                            
         MVC   PGSTDATA(4),PGEEST                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
VPG6     LA    R2,PGEEVTH          EVENT CODE                                   
         BAS   RE,TESTPG0                                                       
         BE    VPG8                                                             
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),L'PGEEVT                                                   
         BNE   LFMERR                                                           
         MVI   PGSTESEQ,X'05'      FIFTH ELEMENT                                
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'EVENTCD '                                            
         MVC   PGSTDATA(6),PGEEVT                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                  MULTI-BRAND FLAG (Y/N)                       
VPG8     LA    R2,PGEMBFH                                                       
         BAS   RE,TESTPG0                                                       
         BE    VPG22                                                            
*                                                                               
         MVI   PGSTESEQ,X'06'      SIXTH ELEMENT                                
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'MLTBRND '                                            
         MVI   PGSTDATA,C'N'       DEFAULT=N                                    
         CLI   PGEMBFH+5,0                                                      
         BE    VPG20                                                            
         CLI   PGEMBF,C'Y'                                                      
         BE    *+12                                                             
         CLI   PGEMBF,C'N'                                                      
         BNE   LFMERR                                                           
         MVC   PGSTDATA(1),PGEMBF                                               
*                                                                               
VPG20    DS    0H                                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                  NO BRAND FLAG (Y/N)                          
VPG22    LA    R2,PGENOBH                                                       
         BAS   RE,TESTPG0                                                       
         BE    VPG32                                                            
*                                  NO BRAND FLAG (Y/N)                          
         MVI   ERRCD,INVERR                                                     
         MVI   PGSTESEQ,X'07'      7TH ELEMENT                                  
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'NOBRAND '                                            
         MVI   PGSTDATA,C'Y'       DEFAULT=Y                                    
         CLI   PGENOBH+5,0                                                      
         BE    VPG30                                                            
         CLI   PGENOB,C'Y'                                                      
         BE    VPG30                                                            
         MVI   PGSTDATA,C'N'                                                    
         CLI   PGENOB,C'N'                                                      
         BE    VPG30                                                            
         B     LFMERR                                                           
*                                                                               
VPG30    DS    0H                                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                  FISCAL YEAR END DATE                         
VPG32    LA    R2,PGEENDH                                                       
         CLI   5(R2),0                                                          
         BE    VPG40               FIELD NOT REQUIRED                           
         BAS   RE,TESTPG0                                                       
         BE    VPG40                                                            
*                                                                               
         MVI   ERRCD,INVERR                                                     
         MVI   PGSTESEQ,X'08'      8TH ELEMENT                                  
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'FISYREND'                                            
         GOTO1 VDATVAL,DMCB,(0,PGEEND),PGSTDATA                                 
         OC    0(4,R1),0(R1)                                                    
         BZ    LFMERR                                                           
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
VPG40    LA    R2,PGEXFRH          TRANSFER ESTIMATE NUMBER                     
         CLI   5(R2),0             TEST INPUT                                   
         BE    VPG50               NO, SKIP TO NEXT FIELD                       
*                                                                               
         BAS   RE,TESTPG0                                                       
         MVI   PGSTESEQ,X'09'      9TH ELEMENT                                  
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'ACCEST  '                                            
         MVI   ERRCD,INVERR                                                     
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    LFMERR                                                           
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2) *EXECUTED*                                           
*                                                                               
         CVB   R0,DUB                                                           
         STC   R0,PGSTDATA         SAVE ESTIMATE NUMBER                         
         LTR   R0,R0                                                            
         BZ    LFMERR                                                           
         CH    R0,=H'255'                                                       
         BH    LFMERR                                                           
         CLC   PGSTDATA,SVKEY+8    CANNOT REF ITSELF                            
         BE    LFMERR                                                           
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
* TEST ACC PG EST RECORD EXISTS                                                 
*                                                                               
         MVI   ERRCD,NOFNDERR                                                   
         XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
         MVC   KEY+8(1),PGSTDATA                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   LFMERR                                                           
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
*                                                                               
VPG50    DS    0H                                                               
         LA    R2,PGESUFH          BRAND SUFFIX                                 
         GOTO1 ANY                                                              
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   5(R2),L'PGESUF      TEST INPUT                                   
         BL    LFMERR              NO, EXIT                                     
*                                                                               
         BAS   RE,TESTPG0                                                       
         MVI   ERRCD,INVERR                                                     
         TM    4(R2),X'04'                                                      
         BNO   LFMERR                                                           
*                                                                               
         MVI   PGSTESEQ,X'0A'      10TH ELEMENT                                 
         MVC   PGSTNAME,=CL8'BRDSUFF'                                           
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTDATA(2),PGESUF                                               
*                                                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
         B     WRTREC                                                           
         EJECT                                                                  
*========================================================*                      
* IF PGEST = 0, INPUT NOT PERMITTED AND EXIT WITH CC EQ  *                      
* IF PGEST > 0, INPUT IS REQUIRED   AND EXIT WITH CC NEQ *                      
*========================================================*                      
         SPACE 1                                                                
TESTPG0  CLI   SVKEY+8,0                                                        
         BE    TESTPG02                                                         
* ESTIMATE NOT 0 - FIELD SHOULD BE PRESENT                                      
         MVI   ERRCD,MSSNGERR                                                   
         CLI   5(R2),0                                                          
         BE    LFMERR                                                           
         BR    RE                  EXIT WITH CC NEQ                             
*                                                                               
TESTPG02 MVI   ERRCD,INVFLD                                                     
         CLI   5(R2),0                                                          
         BNE   LFMERR                                                           
         BR    RE                  EXIT WITH CC EQ                              
         EJECT                                                                  
*======================================================*                        
* VALIDATE DATA FOR ATT                                *                        
*======================================================*                        
         SPACE 1                                                                
VALAT    DS    0H                                                               
         LA    R6,ELEM                                                          
         USING PGSTELMD,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   PGSTEID,PGSTEIDQ                                                 
         MVI   PGSTELN,PGSTELNQ                                                 
         MVI   PGSTESEQ,X'01'      FIRST ELEMENT                                
*                                                                               
         LA    R2,ATTETYH          ESTIMATE TYPE                                
         GOTO1 ANY                                                              
         MVI   ERRCD,INVERR                                                     
         MVC   PGSTNAME,=C'ESTTYPE '                                            
         MVC   PGSTDATA(8),ATTETY                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
         LA    R2,ATTDATH          DATE                                         
         GOTO1 ANY                                                              
         MVI   PGSTESEQ,X'02'                                                   
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'DATE    '                                            
         MVC   PGSTDATA(9),ATTDAT                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
         LA    R2,ATTBUSH          BUSINESS UNIT HEADER                         
         GOTO1 ANY                                                              
         MVI   PGSTESEQ,X'03'                                                   
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'BUS UNIT'                                            
         MVC   PGSTDATA(2),ATTBUS                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
         LA    R2,ATTDIVH          DIVISION                                     
         GOTO1 ANY                                                              
         MVI   PGSTESEQ,X'04'                                                   
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'DIVISION'                                            
         MVC   PGSTDATA(2),ATTDIV                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
         LA    R2,ATTRESH          RESP CODE                                    
         GOTO1 ANY                                                              
         MVI   PGSTESEQ,X'05'                                                   
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'RESPCD  '                                            
         MVC   PGSTDATA(9),ATTRES                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
         LA    R2,ATTEXTH          EXTC                                         
         GOTO1 ANY                                                              
         MVI   PGSTESEQ,X'06'                                                   
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'EXTC    '                                            
         MVC   PGSTDATA(5),ATTEXT                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
         LA    R2,ATTESTH          ESTIMATE ID                                  
         GOTO1 ANY                                                              
         MVI   PGSTESEQ,X'07'                                                   
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'ESTID   '                                            
         MVC   PGSTDATA(5),ATTEST                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
         LA    R2,ATTBUDH          BUDGET NO                                    
         GOTO1 ANY                                                              
         MVI   PGSTESEQ,X'08'                                                   
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'BUDGTNO '                                            
         MVC   PGSTDATA(3),ATTBUD                                               
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
         LA    R2,ATTDESH          DESCRIPTION                                  
         GOTO1 ANY                                                              
         MVI   PGSTESEQ,X'09'                                                   
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'DESCRPTN'                                            
         MVC   PGSTDATA(50),ATTDES                                              
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
         B     WRTREC                                                           
         EJECT                                                                  
*======================================================*                        
* VALIDATE DATA FOR GENERAL FOODS                      *                        
*======================================================*                        
         SPACE 1                                                                
VALGF    DS    0H                                                               
         LA    R6,ELEM                                                          
         USING PGSTELMD,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   PGSTEID,PGSTEIDQ                                                 
         MVI   PGSTELN,PGSTELNQ                                                 
         MVI   ERRCD,INVERR                                                     
*                                                                               
         CLI   GFEDBCH+5,0                                                      
         BE    VLGF02                                                           
*                                                                               
VLGF01   MVI   PGSTESEQ,X'01'      FIRST ELEMENT                                
         MVC   PGSTNAME,=C'DIVBRNCD'                                            
         MVC   PGSTDATA(L'GFEDBC),GFEDBC                                        
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
VLGF02   CLI   GFEPRCH+5,0                                                      
         BE    VLGF03                                                           
         MVI   PGSTESEQ,X'02'      SECOND ELEMENT                               
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'PROD CD '                                            
         MVC   PGSTDATA(L'GFEPRC),GFEPRC                                        
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
VLGF03   CLI   GFEGFNH+5,0                                                      
         BE    VLGF04                                                           
         MVI   PGSTESEQ,X'03'      THIRD ELEMENT                                
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'GF NTRL '                                            
         MVC   PGSTDATA(L'GFEGFN),GFEGFN                                        
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
VLGF04   CLI   GFEGFSH+5,0                                                      
         BE    VLGF11                                                           
         MVI   PGSTESEQ,X'04'      FOURTH ELEMENT                               
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'GFSBNTRL'                                            
         MVC   PGSTDATA(L'GFEGFS),GFEGFS                                        
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
VLGF11   DS    0H                                                               
*                                                                               
         CLI   GFEBAGYH+5,0                                                     
         BE    VLGF12                                                           
         MVI   PGSTESEQ,X'11'                                                   
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=CL8'GFBILAGY'                                          
         MVC   PGSTDATA(L'GFEBAGY),GFEBAGY                                      
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
VLGF12   DS    0H                                                               
*                                                                               
         LA    R2,GFEEXPTH                                                      
         CLI   GFEEXPTH+5,0                                                     
         BE    VLGF13                                                           
         MVI   ERRCD,INVERR                                                     
         CLI   GFEEXPTH+5,6                                                     
         BNE   LFMERR                                                           
         MVI   PGSTESEQ,X'12'                                                   
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=CL8'GFEXPTYP'                                          
         MVC   PGSTDATA(L'GFEEXPT),GFEEXPT                                      
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
VLGF13   DS    0H                                                               
*                                                                               
         CLI   GFEPRIDH+5,0                                                     
         BE    VLGF14                                                           
         MVI   PGSTESEQ,X'13'                                                   
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=CL8'GFPRODID'                                          
         MVC   PGSTDATA(L'GFEPRID),GFEPRID                                      
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
VLGF14   DS    0H                                                               
*                                                                               
         CLI   GFECAGYH+5,0                                                     
         BE    VLGF15                                                           
         MVI   PGSTESEQ,X'14'                                                   
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=CL8'GFCRTVAG'                                          
         MVC   PGSTDATA(L'GFECAGY),GFECAGY                                      
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
VLGF15   DS    0H                                                               
*                                                                               
         CLI   GFESAGYH+5,0                                                     
         BE    VLGF16                                                           
         MVI   PGSTESEQ,X'15'                                                   
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=CL8'GFSRCAGY'                                          
         MVC   PGSTDATA(L'GFESAGY),GFESAGY                                      
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
VLGF16   DS    0H                                                               
*                                                                               
         CLI   GFEREQNH+5,0                                                     
         BE    VLGF17                                                           
         MVI   PGSTESEQ,X'16'                                                   
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=CL8'GFREQNUM'                                          
         MVC   PGSTDATA(L'GFEREQN),GFEREQN                                      
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
VLGF17   DS    0H                                                               
*                                                                               
         CLI   GFETMKTH+5,0                                                     
         BE    VLGF18                                                           
         MVI   PGSTESEQ,X'17'                                                   
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=CL8'GFTGRMKT'                                          
         MVC   PGSTDATA(L'GFETMKT),GFETMKT                                      
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
VLGF18   DS    0H                                                               
*                                                                               
         CLI   GFEDEALH+5,0                                                     
         BE    VLGFX                                                            
         MVI   PGSTESEQ,X'18'                                                   
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=CL8'GFDEALNO'                                          
         MVC   PGSTDATA(L'GFEDEAL),GFEDEAL                                      
         GOTO1 CHELLO,DMCB,(C'P',=C'SPTFIL'),(0,AREC),ELEM                      
         CLI   DMCB+12,0                                                        
         BNZ   LFMERR                                                           
*                                                                               
VLGFX    DS    0H                                                               
         B     WRTREC                                                           
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*==============================================================*                
* WRITE REC TO THE SPOT FILE                                   *                
*==============================================================*                
         SPACE 1                                                                
WRTREC   CLI   SVACT,C'A'                                                       
         BE    EDTADD                                                           
         GOTO1 PUTREC                                                           
         B     EDTX                                                             
*                                                                               
EDTADD   GOTO1 ADDREC                                                           
*                                                                               
EDTX     B     EXXMOD                                                           
*                                                                               
LFMERR   GOTO1 ERROR                                                            
         EJECT                                                                  
EDTDEL   LA    R2,LFMTABH                                                       
         SR    R0,R0                                                            
EDTDEL2  ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         TM    1(R2),X'20'         TEST PROT                                    
         BO    EDTDEL2                                                          
*        CLC   8(3,R2),=C'DEL'                                                  
         CLC   =C'DEL',LFMACT                                                   
         BNE   EDTDEL10                                                         
         OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         B     EXXMOD                                                           
*                                                                               
EDTDEL10 LA    R2,LFMMSGH                                                       
         MVC   8(29,R2),=C'* ERROR * INVALID DELETE CODE'                       
         MVI   ERRAREA,X'FF'                                                    
         LA    R2,LFMRECH                                                       
         OI    6(R2),X'40'                                                      
         B     EXXMOD                                                           
*                                                                               
         GETEL R6,24,ELCODE                                                     
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
GFDISP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TWAXC GFEBAGYH,GFEDEALH                                                
*                                                                               
         USING PGSTELMD,R6                                                      
*                                                                               
         BRAS  RE,GETEL            ESTIMATE <> 0                                
         B     GFDISLP1                                                         
*                                                                               
GFDISLP  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
GFDISLP1 BNE   GFDISX                                                           
*                                                                               
         CLC   PGSTNAME,=CL8'DIVBRNCD'                                          
         BE    GFDIS01                                                          
         CLC   PGSTNAME,=CL8'PROD CD '                                          
         BE    GFDIS02                                                          
         CLC   PGSTNAME,=CL8'GF NTRL '                                          
         BE    GFDIS03                                                          
         CLC   PGSTNAME,=CL8'GFSBNTRL'                                          
         BE    GFDIS04                                                          
         CLC   PGSTNAME,=CL8'GFBILAGY'                                          
         BE    GFDIS11                                                          
         CLC   PGSTNAME,=CL8'GFEXPTYP'                                          
         BE    GFDIS12                                                          
         CLC   PGSTNAME,=CL8'GFPRODID'                                          
         BE    GFDIS13                                                          
         CLC   PGSTNAME,=CL8'GFCRTVAG'                                          
         BE    GFDIS14                                                          
         CLC   PGSTNAME,=CL8'GFSRCAGY'                                          
         BE    GFDIS15                                                          
         CLC   PGSTNAME,=CL8'GFREQNUM'                                          
         BE    GFDIS16                                                          
         CLC   PGSTNAME,=CL8'GFTGRMKT'                                          
         BE    GFDIS17                                                          
         CLC   PGSTNAME,=CL8'GFDEALNO'                                          
         BE    GFDIS18                                                          
*                                                                               
         B     GFDISLP                                                          
*                                                                               
GFDIS01  DS    0H                                                               
*                                                                               
         FOUT  GFEDBCH,PGSTDATA,2                                               
         B     GFDISLP                                                          
*                                                                               
GFDIS02  DS    0H                                                               
*                                                                               
         FOUT  GFEPRCH,PGSTDATA,4                                               
         B     GFDISLP                                                          
*                                                                               
GFDIS03  DS    0H                                                               
*                                                                               
         FOUT  GFEGFNH,PGSTDATA,3                                               
         B     GFDISLP                                                          
*                                                                               
GFDIS04  DS    0H                                                               
*                                                                               
         FOUT  GFEGFSH,PGSTDATA,3                                               
         B     GFDISLP                                                          
*                                                                               
GFDIS11  DS    0H                                                               
*                                                                               
         FOUT  GFEBAGYH,PGSTDATA,8                                              
         B     GFDISLP                                                          
*                                                                               
GFDIS12  DS    0H                                                               
*                                                                               
         FOUT  GFEEXPTH,PGSTDATA,6                                              
         B     GFDISLP                                                          
*                                                                               
GFDIS13  DS    0H                                                               
*                                                                               
         FOUT  GFEPRIDH,PGSTDATA,10                                             
         B     GFDISLP                                                          
*                                                                               
GFDIS14  DS    0H                                                               
*                                                                               
         FOUT  GFECAGYH,PGSTDATA,8                                              
         B     GFDISLP                                                          
*                                                                               
GFDIS15  DS    0H                                                               
*                                                                               
         FOUT  GFESAGYH,PGSTDATA,8                                              
         B     GFDISLP                                                          
*                                                                               
GFDIS16  DS    0H                                                               
*                                                                               
         FOUT  GFEREQNH,PGSTDATA,20                                             
         B     GFDISLP                                                          
*                                                                               
GFDIS17  DS    0H                                                               
*                                                                               
         FOUT  GFETMKTH,PGSTDATA,1                                              
         B     GFDISLP                                                          
*                                                                               
GFDIS18  DS    0H                                                               
*                                                                               
         FOUT  GFEDEALH,PGSTDATA,10                                             
         B     GFDISLP                                                          
*                                                                               
GFDISX   DS    0H                                                               
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
ATDISP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING PGSTELMD,R6                                                      
*                                                                               
         BRAS  RE,GETEL                                                         
         JNE   LFMERR                                                           
         FOUT  ATTETYH,PGSTDATA,8  ESTIMATE TYPE                                
         BRAS  RE,NEXTEL                                                        
         JNE   LFMERR                                                           
         FOUT  ATTDATH,PGSTDATA,9  DATE                                         
         BRAS  RE,NEXTEL                                                        
         JNE   LFMERR                                                           
         FOUT  ATTBUSH,PGSTDATA,2  BUSINESS UNIT                                
         BRAS  RE,NEXTEL                                                        
         JNE   LFMERR                                                           
         FOUT  ATTDIVH,PGSTDATA,2  DIVISION                                     
         BRAS  RE,NEXTEL                                                        
         JNE   LFMERR                                                           
         FOUT  ATTRESH,PGSTDATA,9  RESP CODE                                    
         BRAS  RE,NEXTEL                                                        
         JNE   LFMERR                                                           
         FOUT  ATTEXTH,PGSTDATA,5  EXTC                                         
         BRAS  RE,NEXTEL                                                        
         JNE   LFMERR                                                           
         FOUT  ATTESTH,PGSTDATA,5  ESTIMATE ID                                  
         BRAS  RE,NEXTEL                                                        
         JNE   LFMERR                                                           
         FOUT  ATTBUDH,PGSTDATA,3  BUDGET NO                                    
         BRAS  RE,NEXTEL                                                        
         JNE   LFMERR                                                           
         FOUT  ATTDESH,PGSTDATA,50 DESCRIPTION                                  
*                                                                               
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
PGDISP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING PGSTELMD,R6                                                      
*                                                                               
         CLI   SVKEY+8,0                                                        
         BE    PGDIS00                                                          
*                                                                               
         BRAS  RE,GETEL                                                         
         JNE   LFMERR                                                           
         FOUT  PGECHRH,PGSTDATA,3  CHARGE PERIOD                                
         BRAS  RE,NEXTEL                                                        
         JNE   LFMERR                                                           
         FOUT  PGEACCH,PGSTDATA,6  ACCOUNT                                      
         BRAS  RE,NEXTEL                                                        
         JNE   LFMERR                                                           
         FOUT  PGEBRNH,PGSTDATA,4  BRAND                                        
         BRAS  RE,NEXTEL                                                        
         JNE   LFMERR                                                           
         FOUT  PGEESTH,PGSTDATA,4  ESTIMATE #                                   
         BRAS  RE,NEXTEL                                                        
         JNE   LFMERR                                                           
         FOUT  PGEEVTH,PGSTDATA,6  EVENT CODE                                   
         BRAS  RE,NEXTEL                                                        
         JNE   LFMERR                                                           
         FOUT  PGEMBFH,PGSTDATA,1  MULTI-BRAND FLAG                             
         BRAS  RE,NEXTEL                                                        
         JNE   LFMERR                                                           
         FOUT  PGENOBH,PGSTDATA,1  NO BRAND                                     
* CLEAR NEW FIELDS IN CASE NO ELEMENTS                                          
         XC    PGEEND,PGEEND                                                    
         LA    R2,PGEENDH                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    PGEXFR,PGEXFR                                                    
         LA    R2,PGEXFRH                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    PGESUF,PGESUF                                                    
         LA    R2,PGESUFH                                                       
         OI    6(R2),X'80'                                                      
*****                                                                           
PGDISLP  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   PGDISX                                                           
*                                                                               
         CLI   PGSTESEQ,X'08'                                                   
         BE    PGDIS08                                                          
         CLI   PGSTESEQ,X'09'                                                   
         BE    PGDIS09                                                          
         CLI   PGSTESEQ,X'0A'                                                   
         BE    PGDIS0A                                                          
*                                                                               
         B     PGDISLP                                                          
*                                                                               
PGDIS08  DS    0H                                                               
         GOTO1 VDATCON,DMCB,PGSTDATA,(8,DUB)                                    
         FOUT  PGEENDH,DUB,8       FISCAL YEAR END                              
         B     PGDISLP                                                          
*                                                                               
PGDIS09  DS    0H                                                               
         ZIC   R0,PGSTDATA                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         FOUT  PGEXFRH,WORK,3      ACC XFR ESTIMATE                             
         B     PGDISLP                                                          
*                                                                               
PGDIS0A  DS    0H                                                               
         FOUT  PGESUFH,PGSTDATA,2  BRAND SUFFIX                                 
         B     PGDISLP                                                          
*                                                                               
         EJECT                                                                  
*=======================================================*                       
* DISPLAY LIMITED DATA FOR ESTIMATE 0 RECORD            *                       
*=======================================================*                       
         SPACE 1                                                                
PGDIS00  BRAS  RE,GETEL            ONLY ELEMENT IS PGBRAND                      
         JNE   LFMERR                                                           
         FOUT  PGEBRNH,PGSTDATA,3                                               
* CLEAR ALL REMAINING FIELDS                                                    
         LA    R2,PGECHRH                                                       
         OI    6(R2),X'80'                                                      
         XC    PGECHR,PGECHR       CHARGE PERIOD                                
         LA    R2,PGECHRH                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    PGEACC,PGEACC       ACCOUNT                                      
         LA    R2,PGEACCH                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    PGEEST,PGEEST       ESTIMATE                                     
         LA    R2,PGEESTH                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    PGEEVT,PGEEVT       EVENT CODE                                   
         LA    R2,PGEEVTH                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    PGEMBF,PGEMBF       MULTI-BRAND FLAG                             
         LA    R2,PGEMBFH                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    PGENOB,PGENOB       NO BRAND                                     
         LA    R2,PGENOBH                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    PGEEND,PGEEND       FISCAL YEAR END                              
         LA    R2,PGEENDH                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    PGEXFR,PGEXFR       ACC XFR EST                                  
         LA    R2,PGEXFRH                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    PGESUF,PGESUF       BRAND SUFFIX                                 
         LA    R2,PGESUFH                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
PGDISX   DS    0H                                                               
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
       ++INCLUDE SPLFMWRK                                                       
         ORG   LFMTABH                                                          
       ++INCLUDE SPLFMC8D                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
       ++INCLUDE SPLFMC9D                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
       ++INCLUDE SPLFMCAD                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE SPGENPGEST                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPLFM48   07/29/02'                                      
         END                                                                    
