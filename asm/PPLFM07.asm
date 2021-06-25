*          DATA SET PPLFM07    AT LEVEL 067 AS OF 07/12/02                      
*PHASE T40407A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T40407 - MAINT ESTIMATE RECORD FOR PROCTOR-GAMBLE'              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 07/08/02 ALLOW DELETE AND RESTORE ACTION FOR GFEST                       
*                                                                               
* SMYE 01/02    ADD 7 NEW FIELDS FOR GFEST RECORD                               
*                                                                               
* SMYE 12/10/01 IN VALPG ALLOW Y OR N IN MULTI-BRAND AND NO BRAND               
*               FIELDS FOR AGENCY 'H9', CLIENTS PGB AND PG1                     
*                                                                               
* SMYE 07/20/01 "FORCE" 'NO BRAND' FLD TO Y AND 'MULTI-BRAND' FLD TO N          
*                                                                               
* BPLA 10/94    PGREC - EXPAND ACCOUNT AND BRAND FIELDS                         
*               ALSO - CLEAR IOAREA FIRST                                       
*                                                                               
* BPLA 05/06/91 FIX CHECK TO SEE IF PROPER SCREEN IS ALREADY THERE              
*                                                                               
* ROSA 11/01/90 ADD NEW FIELD 'NO BRAND' TO RECORD                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T40407   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T40407*,R9,RR=R8                                              
         ST    R8,RELO                                                          
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T404FFD,RA                                                       
         LA    R8,IOAREA                                                        
         USING PGSTRECD,R8                                                      
*                                                                               
         LA    R4,IOAREA           CLEAR I/O AREA FIRST THING                   
         LA    R5,1000                                                          
         BAS   RE,CLEARWRK                                                      
*                                                                               
         CLI   BREC,X'08'          SEE PG PGEST REC                             
         BNE   INIT10                                                           
         CLI   SCRNUM,X'F7'        TEST IF THERE'S PG-SCREEN                    
         BE    MAIN                                                             
         B     INIT15                                                           
*                                                                               
INIT10   CLI   BREC,X'09'          SEE IF GF REC                                
         BE    *+6                                                              
         DC    H'0'                INVALID RECORD TYPE                          
*                                                                               
         CLI   SCRNUM,X'F8'        TEST IF THERE'S GF-SCREEN                    
         BE    MAIN                                                             
*                                                                               
INIT15   CLI   BREC,X'08'          IS IT PROCTER & GAMBLE EST?                  
         BNE   INIT20              NO                                           
         GOTO1 VCALLOV,DMCB,HDRLAST,X'D90404F7'                                 
         B     INIT25                                                           
*                                                                               
INIT20   CLI   BREC,X'09'          IS IT GENERAL FOODS EST?                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VCALLOV,DMCB,HDRLAST,X'D90404F8'                                 
*                                                                               
INIT25   CLI   4(R1),X'FF'         ERROR IN READING PHASES                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BREC,X'08'                                                       
         BNE   *+12                                                             
         MVI   SCRNUM,X'F7'                                                     
         B     MAIN                                                             
         CLI   BREC,X'09'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   SCRNUM,X'F8'                                                     
         B     MAIN                                                             
*                                                                               
EXMOD    XMOD1                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MAIN     DS    0H                                                               
         MVC   KEY+27(4),ESTADDR   USED FOR A(PGEST RECORD)                     
         CLI   DSPSW,1             FIRST TIME CHANGE?                           
         BNE   EDT                 NO                                           
*                                                                               
FMT      DS    0H                  DISPLAY DATA                                 
         BAS   RE,GETREC                                                        
         MVI   BYTE,PGSTEIDQ       X'10'                                        
         LA    R3,MISSERR                                                       
         LR    R6,R8               A(IOAREA)                                    
         USING PGSTELEM,R6                                                      
*                                                                               
         CLC   =C'PG',HDRREC                                                    
         BE    DISPG                                                            
         CLC   =C'GF',HDRREC                                                    
         BE    DISGF                                                            
         LA    R3,NOESTERR                                                      
         B     LFMERR                                                           
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DISPG    BAS   RE,GETEL                                                         
         BNE   LFMERR                                                           
         FOUT  PGECHRH,PGSTDATA,3  CHARGE PERIOD                                
         BAS   RE,NEXTEL                                                        
         BNE   LFMERR                                                           
         FOUT  PGEACCH,PGSTDATA,6  ACCOUNT                                      
         BAS   RE,NEXTEL                                                        
         BNE   LFMERR                                                           
         FOUT  PGEBRNH,PGSTDATA,4  PG BRAND                                     
         BAS   RE,NEXTEL                                                        
         BNE   LFMERR                                                           
         FOUT  PGEESTH,PGSTDATA,4  ESTIMATE #                                   
         BAS   RE,NEXTEL                                                        
         BNE   LFMERR                                                           
         FOUT  PGEEVTH,PGSTDATA,6  EVENT CODE                                   
         BAS   RE,NEXTEL                                                        
         BNE   LFMERR                                                           
         FOUT  PGEMBFH,PGSTDATA,1  MULTI-BRAND FLAG                             
         BAS   RE,NEXTEL                                                        
         BE    *+8                                                              
         MVI   PGSTDATA,C'Y'       DEFAULT TO Y                                 
         FOUT  PGENOBH,PGSTDATA,1  NO BRAND FLAG                                
         BAS   RE,NEXTEL                                                        
         BNE   LFMERR                                                           
         FOUT  PGEBRSH,PGSTDATA,2  BRAND SUFFIX                                 
         B     FMT10                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DISGF    DS    0H                                                               
         CLC   KEY+10(2),=X'0000'                                               
         BE    DISGF00                                                          
         BAS   RE,GETEL            IF ESTIMATE <> 0                             
         FOUT  GFEDBCH,PGSTDATA,2  CLEAR FIRST TWO FIELDS                       
         BAS   RE,NEXTEL                                                        
         FOUT  GFEPRCH,PGSTDATA,4                                               
         BAS   RE,NEXTEL                                                        
         BNE   LFMERR                                                           
         FOUT  GFEGFNH,PGSTDATA,3                                               
         BAS   RE,NEXTEL                                                        
         BNE   LFMERR                                                           
         FOUT  GFEGFSH,PGSTDATA,3                                               
         BAS   RE,NEXTEL                                                        
         B     DISGF20                                                          
*                                                                               
DISGF00  BAS   RE,GETEL            DISPLAY DATA FOR ESTIMATE=0                  
         BNE   LFMERR                                                           
         FOUT  GFEDBCH,PGSTDATA,2                                               
         BAS   RE,NEXTEL                                                        
         BNE   LFMERR                                                           
         FOUT  GFEPRCH,PGSTDATA,4                                               
         BAS   RE,NEXTEL           CLEAR NEXT TWO FIELDS                        
         FOUT  GFEGFNH,PGSTDATA,3                                               
         BAS   RE,NEXTEL                                                        
         FOUT  GFEGFSH,PGSTDATA,3                                               
         BAS   RE,NEXTEL                                                        
*                                                                               
DISGF20  DS    0H                                                               
         LA    R2,GFEBAGH                                                       
         XC    8(L'GFEBAG,R2),8(R2)                                             
         BAS   RE,EXOUT            DO EXECUTED MOVE FOR OUTPUT                  
         BAS   RE,NEXTEL                                                        
         LA    R2,GFEXPTH                                                       
         XC    8(L'GFEXPT,R2),8(R2)                                             
         BAS   RE,EXOUT            DO EXECUTED MOVE FOR OUTPUT                  
         BAS   RE,NEXTEL                                                        
         LA    R2,GFEPIDH                                                       
         XC    8(L'GFEPID,R2),8(R2)                                             
         BAS   RE,EXOUT            DO EXECUTED MOVE FOR OUTPUT                  
*                                                                               
         EDIT  (C10,GFEPID),(10,8(R2)),ALIGN=LEFT                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         LA    R2,GFECAGH                                                       
         XC    8(L'GFECAG,R2),8(R2)                                             
         BAS   RE,EXOUT            DO EXECUTED MOVE FOR OUTPUT                  
         BAS   RE,NEXTEL                                                        
         LA    R2,GFESAGH                                                       
         XC    8(L'GFESAG,R2),8(R2)                                             
         BAS   RE,EXOUT            DO EXECUTED MOVE FOR OUTPUT                  
         BAS   RE,NEXTEL                                                        
         LA    R2,GFETMKH                                                       
         XC    8(L'GFETMK,R2),8(R2)                                             
         BAS   RE,EXOUT            DO EXECUTED MOVE FOR OUTPUT                  
         BAS   RE,NEXTEL                                                        
         LA    R2,GFEDGVH                                                       
         XC    8(L'GFEDGV,R2),8(R2)                                             
         BAS   RE,EXOUT            DO EXECUTED MOVE FOR OUTPUT                  
*                                                                               
FMT10    LA    R2,HDRACTH          CURSOR TO ACTION                             
         TM    KEY+25,X'80'        RECORD IS DELETED?                           
         BZ    FMT15                                                            
         CLI   BACT,X'0D'          RESTORING DELETED RECORD?                    
         BE    WRTREC                                                           
         LA    R3,56               MSG: RECORD IS DELETED                       
         B     LFMERR                                                           
*                                                                               
FMT15    CLI   BACT,X'0C'          DELETE?                                      
         BE    WRTREC                                                           
         CLI   BACT,X'0D'          RESTORE?                                     
         BNE   *+12                                                             
         LA    R3,239              CANNOT RESTORE, REC IS NOT DELETED           
         B     LFMERR                                                           
*                                                                               
         CLI   BACT,2              ACTION CHANGE?                               
         BE    FMTX                                                             
         MVI   DONESW,1            ACTION COMPLETE                              
         B     EXMOD                                                            
*                                                                               
FMTX     CLI   BREC,X'08'          P&G?                                         
         BNE   *+12                NO                                           
         LA    R2,PGECHRH          POSITION CURSOR FOR INPUT                    
         B     EXIT                                                             
         CLI   BREC,X'09'          GF?                                          
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         LA    R2,GFEDBCH                                                       
         B     EXIT                                                             
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDT      DS    0H                  VALIDATE DATA                                
         SR    R4,R4                                                            
         LA    R4,PGSTREC                                                       
         LA    R4,PGSTKEDQ(R4)     A(FIRST ELEMENT)                             
*                                                                               
         CLI   BACT,1              ACTION ADD?                                  
         BE    EDT10                                                            
*                                                                               
         TM    KEY+25,X'80'        RECORD IS DELETED?                           
         BZ    *+16                                                             
         LA    R2,HDRACTH          CURSOR TO ACTION                             
         LA    R3,240              INVALID ACTION FOR DELETED REC               
         B     LFMERR                                                           
*                                                                               
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
*                                                                               
         LA    R3,INVERR           DELETE ALL X'10' ELEMENTS                    
*                                                                               
EDT05    CLI   0(R4),0             END OF REC?                                  
         BE    EDT20               YES                                          
         GOTO1 VRECUP,DMCB,(X'01',IOAREA),(R4),0,0                              
         CLI   DMCB+12,0                                                        
         BE    EDT05                                                            
         DC    H'0'                                                             
*                                                                               
EDT10    DS    0H                                                               
         MVC   PGSTKEY,KEY         BUILD KEY                                    
         MVI   PGSTLEN+1,X'22'     L'KEY + 1                                    
         XC    PGSTCNTL,PGSTCNTL                                                
         XC    PGSTKDA,PGSTKDA                                                  
         MVI   PGSTKEDQ(R8),0      END OF REC MARKER                            
*                                                                               
EDT20    DS    0H                  VALIDATE DATA                                
         CLC   =C'PG',HDRREC                                                    
         BE    VALPG                                                            
         CLC   =C'GF',HDRREC                                                    
         BE    VALGF                                                            
         LA    R3,NOESTERR                                                      
         B     LFMERR                                                           
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VALPG    DS    0H                  VALIDATE DATA FOR PROCTER AND GAMBLE         
         LA    R6,WORK                                                          
         USING PGSTELEM,R6                                                      
         XC    WORK,WORK                                                        
*                                                                               
         LA    R2,PGECHRH          CHARGE PERIOD HEADER                         
         GOTO1 ANY                                                              
         LA    R3,INVERR                                                        
         TM    4(R2),X'08'         NUMERIC?                                     
         BZ    LFMERR              NO                                           
         CLI   5(R2),L'PGECHR      EXACT LENGTH?                                
         BNE   LFMERR                                                           
         MVI   PGSTESEQ,X'01'                                                   
         MVI   PGSTEID,PGSTEIDQ                                                 
         MVI   PGSTELN,PGSTELNQ                                                 
         MVC   PGSTNAME,=C'CHRG PER'                                            
         MVC   PGSTDATA(L'PGECHR),PGECHR                                        
         GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,PGEACCH          ACCOUNT                                      
         GOTO1 ANY                                                              
         CLI   5(R2),L'PGEACC                                                   
         BNE   LFMERR                                                           
         MVI   PGSTESEQ,X'02'                                                   
         LA    R4,PGSTELNQ(R4)     A("NEXT ELEMENT")                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'ACCOUNT '                                            
         MVC   PGSTDATA(L'PGEACC),PGEACC                                        
         GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,PGEBRNH          BRAND                                        
         GOTO1 ANY                                                              
         CLI   5(R2),L'PGEBRN                                                   
         BNE   LFMERR                                                           
         MVI   PGSTESEQ,X'03'                                                   
         LA    R4,PGSTELNQ(R4)     A("NEXT ELEMENT")                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'BRAND   '                                            
         MVC   PGSTDATA(L'PGEBRN),PGEBRN                                        
         GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,PGEESTH          ESTIMATE #                                   
         GOTO1 ANY                                                              
         CLI   5(R2),L'PGEEST                                                   
         BNE   LFMERR                                                           
         MVI   PGSTESEQ,X'04'                                                   
         LA    R4,PGSTELNQ(R4)     A("NEXT ELEMENT")                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'ESTIMATE'                                            
         MVC   PGSTDATA(L'PGEEST),PGEEST                                        
         GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,PGEEVTH          EVENT CODE                                   
         GOTO1 ANY                                                              
         CLI   5(R2),L'PGEEVT                                                   
         BNE   LFMERR                                                           
         MVI   PGSTESEQ,X'05'                                                   
         LA    R4,PGSTELNQ(R4)     A("NEXT ELEMENT")                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'EVENT CD'                                            
         MVC   PGSTDATA(L'PGEEVT),PGEEVT                                        
         GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,PGEMBFH     MULTI-BRAND FLAG (Y/N)  (7/20/01-N ONLY)          
         CLC   PGSTKAGY,=C'H9'     "STARCOM" AGENCY ?                           
         BNE   VALPG62             NO                                           
         CLC   PGSTKCLT,=C'PGB'    CLIENT PGB ?                                 
         BE    VALPG60             YES                                          
         CLC   PGSTKCLT,=C'PG1'    CLIENT PG1 ?                                 
         BNE   VALPG62             NOT PGB OR PG1                               
VALPG60  CLI   PGEMBF,C'Y'                                                      
         BE    VALPG64             OK                                           
VALPG62  CLI   PGEMBF,C'N'                                                      
         BNE   LFMERR                                                           
VALPG64  MVI   PGSTESEQ,X'06'                                                   
         LA    R4,PGSTELNQ(R4)     A("NEXT ELEMENT")                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'MLT-BRND'                                            
         MVC   PGSTDATA(L'PGEMBF),PGEMBF                                        
         GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
*NOP*    BNZ   LFMERR                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* VALIDATE NO BRAND OPTN                                                        
*                                                                               
         LA    R2,PGENOBH          NO BRAND FLAG (Y/N)                          
         CLC   PGSTKAGY,=C'H9'     "STARCOM" AGENCY ?                           
         BNE   VALPG72             NO                                           
         CLC   PGSTKCLT,=C'PGB'    CLIENT PGB ?                                 
         BE    VALPG70             YES                                          
         CLC   PGSTKCLT,=C'PG1'    CLIENT PG1 ?                                 
         BNE   VALPG72             NOT PGB OR PG1                               
VALPG70  CLI   PGENOB,C'N'                                                      
         BE    VALPG74             OK                                           
VALPG72  CLI   PGENOB,C'Y'                                                      
         BNE   LFMERR                                                           
VALPG74  MVI   PGSTESEQ,X'07'                                                   
         LA    R4,PGSTELNQ(R4)     A("NEXT ELEMENT")                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'NOBRAND '                                            
         MVC   PGSTDATA(L'PGENOB),PGENOB                                        
         GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* VALIDATE BRAND SUFFIX                                                         
*                                                                               
         LA    R2,PGEBRSH          PGEBRSH                                      
         TM    4(R2),X'04'         ALPHABETIC?                                  
         BZ    LFMERR              NO                                           
         CLI   5(R2),L'PGEBRS      EXACT LENGTH?                                
         BNE   LFMERR                                                           
         MVI   PGSTESEQ,X'08'                                                   
         LA    R4,PGSTELNQ(R4)     A("NEXT ELEMENT")                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'BRND SUF'                                            
         MVC   PGSTDATA(L'PGEBRS),PGEBRS                                        
         GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    WRTREC                                                           
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VALGF    DS    0H                  VALIDATE DATA FOR GENERAL FOODS              
         LA    R6,WORK                                                          
         USING PGSTELEM,R6                                                      
         XC    WORK,WORK                                                        
         MVI   PGSTEID,PGSTEIDQ                                                 
         MVI   PGSTELN,PGSTELNQ                                                 
         LA    R3,INVERR                                                        
*                                                                               
         LA    R2,GFEDBCH          FIRST ELEMENT                                
         GOTO1 ANY                                                              
VLGF01   MVI   PGSTESEQ,X'01'                                                   
         MVC   PGSTNAME,=C'DIVBRNCD'                                            
         MVC   PGSTDATA(L'GFEDBC),GFEDBC                                        
         GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,GFEPRCH          SECOND ELEMENT                               
         GOTO1 ANY                                                              
VLGF02   MVI   PGSTESEQ,X'02'                                                   
         LA    R4,PGSTELNQ(R4)     A("NEXT ELEMENT")                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'PROD CD '                                            
         MVC   PGSTDATA(L'GFEPRC),GFEPRC                                        
         GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,GFEGFNH          THIRD ELEMENT                                
         CLI   5(R2),0                                                          
         BE    CHK3                                                             
         CLC   KEY+10(2),=X'0000'                                               
         BNE   VLGF03                                                           
         LA    R3,INVFLD                                                        
         B     LFMERR                                                           
CHK3     CLC   KEY+10(2),=X'0000'                                               
         BE    VLGF03                                                           
         LA    R3,MISSERR                                                       
         B     LFMERR                                                           
VLGF03   MVI   PGSTESEQ,X'03'                                                   
         LA    R4,PGSTELNQ(R4)     A("NEXT ELEMENT")                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'GF NTRL '                                            
         MVC   PGSTDATA(L'GFEGFN),GFEGFN                                        
         GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,GFEGFSH          FOURTH ELEMENT                               
         CLI   5(R2),0                                                          
         BE    CHK4                                                             
         CLC   KEY+10(2),=X'0000'                                               
         BNE   VLGF04                                                           
         LA    R3,INVFLD                                                        
         B     LFMERR                                                           
CHK4     CLC   KEY+10(2),=X'0000'                                               
         BE    VLGF04                                                           
         LA    R3,MISSERR                                                       
         B     LFMERR                                                           
VLGF04   MVI   PGSTESEQ,X'04'                                                   
         LA    R4,PGSTELNQ(R4)     A("NEXT ELEMENT")                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'GFSBNTRL'                                            
         MVC   PGSTDATA(L'GFEGFS),GFEGFS                                        
         GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,GFEBAGH          FIFTH ELEMENT                                
         B     VLGF05              BYPASS, UNTIL REQUIREMENTS DEFINIED          
*                                                                               
         CLI   5(R2),0                                                          
         BE    CHK5                                                             
         CLC   KEY+10(2),=X'0000'                                               
         BNE   VLGF05                                                           
         LA    R3,INVFLD                                                        
         B     LFMERR                                                           
CHK5     CLC   KEY+10(2),=X'0000'                                               
         BE    VLGF05                                                           
         LA    R3,MISSERR                                                       
         B     LFMERR                                                           
VLGF05   MVI   PGSTESEQ,X'05'                                                   
         LA    R4,PGSTELNQ(R4)     A("NEXT ELEMENT")                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'BILL AGY'                                            
         CLI   5(R2),0             ANY INPUT ?                                  
         BE    VLGF05M             NO - LEAVE PGSTDATA AS NULLS                 
         MVC   PGSTDATA(L'GFEBAG),SPACES                                        
         LA    R7,PGSTDATA         POINT R7 TO DATA FIELD FOR ELEM              
         BAS   RE,EXMOVL           DO EX MOVE TO LEFT-JUSTIFY DATA              
VLGF05M  GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,GFEXPTH          SIXTH ELEMENT                                
         B     VLGF06              BYPASS, UNTIL REQUIREMENTS DEFINIED          
*                                                                               
         CLI   5(R2),0                                                          
         BE    CHK6                                                             
         CLC   KEY+10(2),=X'0000'                                               
         BNE   VLGF06                                                           
         LA    R3,INVFLD                                                        
         B     LFMERR                                                           
CHK6     CLC   KEY+10(2),=X'0000'                                               
         BE    VLGF06                                                           
         LA    R3,MISSERR                                                       
         B     LFMERR                                                           
VLGF06   MVI   PGSTESEQ,X'06'                                                   
         LA    R4,PGSTELNQ(R4)     A("NEXT ELEMENT")                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'EXP TYPE'                                            
         CLI   5(R2),0             ANY INPUT ?                                  
         BE    VLGF06M             NO - LEAVE PGSTDATA AS NULLS                 
         MVC   PGSTDATA(L'GFEXPT),SPACES                                        
         LA    R7,PGSTDATA         POINT R7 TO DATA FIELD FOR ELEM              
         BAS   RE,EXMOVL           DO EX MOVE TO LEFT-JUSTIFY DATA              
VLGF06M  GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,GFEPIDH          SEVENTH ELEMENT                              
         B     VLGF07              BYPASS, UNTIL REQUIREMENTS DEFINIED          
*                                                                               
         CLI   5(R2),0                                                          
         BE    CHK7                                                             
         CLC   KEY+10(2),=X'0000'                                               
         BNE   VLGF07                                                           
         LA    R3,INVFLD                                                        
         B     LFMERR                                                           
CHK7     CLC   KEY+10(2),=X'0000'                                               
         BE    VLGF07                                                           
         LA    R3,MISSERR                                                       
         B     LFMERR                                                           
*                                                                               
VLGF07   DS    0H                                                               
         MVI   PGSTESEQ,X'07'                                                   
         LA    R4,PGSTELNQ(R4)     A("NEXT ELEMENT")                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'PROD ID '                                            
         CLI   5(R2),0             ANY INPUT ?                                  
         BE    VLGF07M             NO - LEAVE PGSTDATA AS NULLS                 
         LA    R3,NOTNUM                                                        
         TM    4(R2),X'08'         NUMERIC ?                                    
         BNO   LFMERR              NO                                           
         MVC   PGSTDATA(L'GFEPID),ZEROS                                         
         LA    R7,PGSTDATA         POINT R7 TO DATA FIELD FOR ELEM              
         LA    R7,L'GFEPID(R7)     BUMP R7 TO LAST BYTE FOR THIS FIELD          
         BAS   RE,EXMOVR           DO EX MOVE TO RIGHT-JUSTIFY DATA             
*                                                                               
VLGF07M  GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,GFECAGH          EIGHTH ELEMENT                               
         B     VLGF08              BYPASS, UNTIL REQUIREMENTS DEFINIED          
*                                                                               
         CLI   5(R2),0                                                          
         BE    CHK8                                                             
         CLC   KEY+10(2),=X'0000'                                               
         BNE   VLGF08                                                           
         LA    R3,INVFLD                                                        
         B     LFMERR                                                           
CHK8     CLC   KEY+10(2),=X'0000'                                               
         BE    VLGF08                                                           
         LA    R3,MISSERR                                                       
         B     LFMERR                                                           
*                                                                               
VLGF08   DS    0H                                                               
         MVI   PGSTESEQ,X'08'                                                   
         LA    R4,PGSTELNQ(R4)     A("NEXT ELEMENT")                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'CRTV AGY'                                            
         CLI   5(R2),0             ANY INPUT ?                                  
         BE    VLGF08M             NO - LEAVE PGSTDATA AS NULLS                 
         MVC   PGSTDATA(L'GFECAG),SPACES                                        
         LA    R7,PGSTDATA         POINT R7 TO DATA FIELD FOR ELEM              
         BAS   RE,EXMOVL           DO EX MOVE TO LEFT-JUSTIFY DATA              
VLGF08M  GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,GFESAGH          NINTH ELEMENT                                
         B     VLGF09              BYPASS, UNTIL REQUIREMENTS DEFINIED          
*                                                                               
         CLI   5(R2),0                                                          
         BE    CHK9                                                             
         CLC   KEY+10(2),=X'0000'                                               
         BNE   VLGF09                                                           
         LA    R3,INVFLD                                                        
         B     LFMERR                                                           
CHK9     CLC   KEY+10(2),=X'0000'                                               
         BE    VLGF09                                                           
         LA    R3,MISSERR                                                       
         B     LFMERR                                                           
*                                                                               
VLGF09   DS    0H                                                               
         MVI   PGSTESEQ,X'09'                                                   
         LA    R4,PGSTELNQ(R4)     A("NEXT ELEMENT")                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'SRCE AGY'                                            
         CLI   5(R2),0             ANY INPUT ?                                  
         BE    VLGF09M             NO - LEAVE PGSTDATA AS NULLS                 
         MVC   PGSTDATA(L'GFESAG),SPACES                                        
         LA    R7,PGSTDATA         POINT R7 TO DATA FIELD FOR ELEM              
         BAS   RE,EXMOVL           DO EX MOVE TO LEFT-JUSTIFY DATA              
VLGF09M  GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,GFETMKH          TENTH ELEMENT                                
         B     VLGF10              BYPASS, UNTIL REQUIREMENTS DEFINIED          
*                                                                               
         CLI   5(R2),0                                                          
         BE    CHK10                                                            
         CLC   KEY+10(2),=X'0000'                                               
         BNE   VLGF10                                                           
         LA    R3,INVFLD                                                        
         B     LFMERR                                                           
CHK10    CLC   KEY+10(2),=X'0000'                                               
         BE    VLGF10                                                           
         LA    R3,MISSERR                                                       
         B     LFMERR                                                           
*                                                                               
VLGF10   DS    0H                                                               
         MVI   PGSTESEQ,X'0A'                                                   
         LA    R4,PGSTELNQ(R4)     A("NEXT ELEMENT")                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'TRGT MKT'                                            
         CLI   5(R2),0             ANY INPUT ?                                  
         BE    VLGF10M             NO - LEAVE PGSTDATA AS NULLS                 
         MVC   PGSTDATA(L'GFETMK),SPACES                                        
         LA    R7,PGSTDATA         POINT R7 TO DATA FIELD FOR ELEM              
         BAS   RE,EXMOVL           DO EX MOVE TO LEFT-JUSTIFY DATA              
VLGF10M  GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,GFEDGVH          ELEVENTH ELEMENT                             
         B     VLGF11              BYPASS, UNTIL REQUIREMENTS DEFINIED          
*                                                                               
         CLI   5(R2),0                                                          
         BE    CHK11                                                            
         CLC   KEY+10(2),=X'0000'                                               
         BNE   VLGF11                                                           
         LA    R3,INVFLD                                                        
         B     LFMERR                                                           
CHK11    CLC   KEY+10(2),=X'0000'                                               
         BE    VLGF11                                                           
         LA    R3,MISSERR                                                       
         B     LFMERR                                                           
*                                                                               
VLGF11   DS    0H                                                               
         MVI   PGSTESEQ,X'0B'                                                   
         LA    R4,PGSTELNQ(R4)     A("NEXT ELEMENT")                            
         XC    PGSTDATA,PGSTDATA                                                
         MVC   PGSTNAME,=C'DEAL# GV'                                            
         CLI   5(R2),0             ANY INPUT ?                                  
         BE    VLGF11M             NO - LEAVE PGSTDATA AS NULLS                 
         MVC   PGSTDATA(L'GFEDGV),SPACES                                        
         LA    R7,PGSTDATA         POINT R7 TO DATA FIELD FOR ELEM              
         BAS   RE,EXMOVL           DO EX MOVE TO LEFT-JUSTIFY DATA              
VLGF11M  GOTO1 VRECUP,DMCB,(X'01',IOAREA),WORK,(0,(R4)),0                       
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* PUT REC TO THE PRINT FILE                                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WRTREC   LA    R2,HDRACTH          CURSOR TO ACTION                             
         CLI   BACT,1              ACTION ADD?                                  
         BE    WRTR50                                                           
*                                                                               
         CLI   BACT,X'0C'          ACTION DELETE?                               
         BNE   WRTR20                                                           
         OI    KEY+25,X'80'                                                     
         GOTO1 WRITE                                                            
         LA    R3,56               RECORD IS DELETED                            
         B     LFMERR                                                           
*                                                                               
WRTR20   CLI   BACT,X'0D'          ACTION RESTORE?                              
         BNE   WRTR30                                                           
         NI    KEY+25,X'FF'-X'80'                                               
         GOTO1 WRITE                                                            
         LA    R3,238              RECORD IS RESTORED                           
         B     LFMERR                                                           
*                                                                               
WRTR30   GOTO1 PUTREC                                                           
         B     WRTR95                                                           
*                                                                               
WRTR50   GOTO1 ADDREC                                                           
*                                                                               
WRTR95   MVI   DONESW,1                                                         
         B     EXMOD                                                            
*                                                                               
LFMERR   GOTO1 ERROR                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* DO AN EXECUTED MOVE TO RIGHT-JUSTIFY ELEMENT DATA (IN PGSTDATA)               
* R2 POINTING TO FIELD HEADER                                                   
* R7 POINTING TO LAST BYTE OF TARGET FIELD                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EXMOVR   NTR1                                                                   
         ZIC   R1,5(R2)                                                         
         SR    R7,R1               POINT R7 TO START OF TARGET FIELD            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),8(R2)                                                    
JUMPXIT1 XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* DO AN EXECUTED MOVE TO LEFT-JUSTIFY ELEMENT DATA (IN PGSTDATA)                
* LEAVING TARGET FIELD BLANK-FILLED                                             
* R2 POINTING TO FIELD HEADER                                                   
* R7 POINTING TO PGSTDATA                                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EXMOVL   NTR1                                                                   
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),8(R2)                                                    
         J     JUMPXIT1                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*  DO AN EXECUTED MOVE TO OUTPUT DATA FROM PGSTDATA                             
*  R2 POINTING TO FIELD HEADER (FIELD DATA HAS BEEN CLEARED)                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EXOUT    NTR1                                                                   
         LA    R4,L'PGSTDATA       MAX LUP COUNTER                              
         LA    R5,PGSTDATA                FIRST BYTE OF ELEMENT DATA            
         LA    R3,PGSTDATA+L'PGSTDATA-1   LAST BYTE OF ELEMENT DATA             
EXOLUP   CLI   0(R3),C' '          ANY DATA ?                                   
         BH    EXOMOV              YES - GO DO OUTPUT                           
         AHI   R3,-1               MOVE TO LEFT IN PGSTDATA                     
         BCT   R4,EXOLUP                                                        
         B     EXOTMT              NOTHING FOUND - OUTPUT CLEARED FIELD         
EXOMOV   DS    0H                                                               
         SR    R3,R5               SET R3 FOR EXECUTED MOVE                     
*                                  R5 AT START OF PGSTDATA IN ELEMENT           
EXOMLUP  CLI   0(R5),C' '          ANYTHING HERE                                
         BH    EXOMEX              YES - GO MOVE                                
         LA    R5,1(R5)            BUMP OVER                                    
         B     EXOMLUP                                                          
*                                                                               
EXOMEX   EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R5)                                                    
EXOTMT   OI    6(R2),X'80'         TRANSMIT                                     
         J     JUMPXIT1                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DROP  R6,R8                                                            
*                                                                               
         GETEL R6,33,BYTE                                                       
*                                                                               
ZEROS    DC    40C'0'                                                           
SPACES   DC    CL50' '                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
MISSERR  EQU   1                                                                
INVERR   EQU   2                                                                
NOTNUM   EQU   3                   ONLY NUMERIC INPUT ALLOWED                   
INVDCB   EQU   5                                                                
INVFLD   EQU   6                   INPUT NOT PERMITTED IN THIS FIELD            
NOESTERR EQU   42                  EST NOT FOUND                                
*                                                                               
       ++INCLUDE PLFMWRK                                                        
         EJECT                                                                  
*                                                                               
         ORG   HDRLAST                                                          
       ++INCLUDE PPLFMF7D          NOTE CHANGE TO PPLFMF7D AFTER TSTL01         
         EJECT                                                                  
*                                                                               
         ORG   HDRLAST                                                          
       ++INCLUDE PPLFMF8D                                                       
         EJECT                                                                  
*                                                                               
PGSTRECD DSECT                                                                  
       ++INCLUDE PGESTREC                                                       
         EJECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067PPLFM07   07/12/02'                                      
         END                                                                    
