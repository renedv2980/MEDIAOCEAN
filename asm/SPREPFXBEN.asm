*          DATA SET SPREPFXBEN AT LEVEL 130 AS OF 02/20/99                      
*PHASE SPFX02W                                                                  
*==============================================================*                
         TITLE 'SPFX02W - STATION FILE CABLE CONVERSION'                        
         SPACE 1                                                                
SPFX02   CSECT                                                                  
         DS    6000C                                                            
         ORG   SPFX02                                                           
         SPACE 1                                                                
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         LA    R7,2048(RC)                                                      
         LA    R7,2048(R7)                                                      
         USING SPFX02,RB,RC,R7                                                  
         CLI   MODE,REQFRST                                                     
         BE    FX2                                                              
EXIT     XIT1                                                                   
*                                                                               
**********************************************************                      
*  FIRST BUILD AGY TABLE WITH THE AGY/MED T,N,C                                 
*  FOR ALL CANADIAN AGENCIES                                                    
*                                                                               
*                                                                               
FX2      XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         LA    R2,KEY                                                           
         LA    R3,AGYTABLE                                                      
         XC    AGYTABLE,AGYTABLE                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    CMP06                                                            
         DC    H'0'                                                             
*                                                                               
*                                                                               
AGYSEQ   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'SPTDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CMP06    CLI   KEY,X'06'                                                        
         BNE   DONEAGY                                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',                     +        
               KEY+14,IO,DMWORK                                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IO                                                            
         USING AGYEL,R6                                                         
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    CHKCAN                  CHECK FOR CANADIAN AGENCY                
         DC    H'0'                                                             
*                                                                               
CHKCAN   DS    0H                                                               
*                                                                               
         CLI   AGYPROF+7,C'C'          CANADIAN AGENCY                          
         BNE   AGYSEQ                  IF NOT GET NEXT AGENCY RECORD            
         MVC   P(3),AGYID                                                       
         GOTO1 REPORT                                                           
*                                                                               
*  GET THE AGY/MED CODESFOR MEDIA T,N,C IN THAT ORDER                           
         LA    R6,IO                                                            
         USING AGYMEDEL,R6                                                      
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    CHKMEDT                                                          
         DC    H'0'                                                             
*                                                                               
NEXTMEDT BAS   RE,NEXTEL                                                        
         BNE   AGYSEQ                                                           
CHKMEDT  DS    0H                                                               
         USING AGYMEDEL,R6                                                      
         CLI   AGYMEDCD,C'T'                                                    
         BNE   NEXTMEDT                                                         
         MVC   0(1,R3),AGYMEDBT          MOVE 1 BYTE MEDIA T CODE               
* NOW CHECK MEDIA N                                                             
         LA    R6,IO                                                            
         USING AGYMEDEL,R6                                                      
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    CHKMEDN                                                          
         DC    H'0'                                                             
*                                                                               
NEXTMEDN BAS   RE,NEXTEL                                                        
         BNE   AGYSEQ                                                           
CHKMEDN  DS    0H                                                               
         CLI   AGYMEDCD,C'N'                                                    
         BNE   NEXTMEDN                                                         
         MVC   1(1,R3),AGYMEDBT          MOVE 1 BYTE MEDIA T CODE               
*                                                                               
* NOW CHECK MEDIA C                                                             
         LA    R6,IO                                                            
         USING AGYMEDEL,R6                                                      
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    CHKMEDC                                                          
         DC    H'0'                                                             
*                                                                               
NEXTMEDC BAS   RE,NEXTEL                                                        
         BNE   AGYSEQ                                                           
CHKMEDC  DS    0H                                                               
         CLI   AGYMEDCD,C'C'                                                    
         BNE   NEXTMEDC                                                         
         MVC   2(1,R3),AGYMEDBT          MOVE 1 BYTE MEDIA T CODE               
*                                                                               
         MVC   P(10),=C'AGENCY MED'                                             
         GOTO1 HEXOUT,DMCB,0(R3),P+20,3,=C'TOG'                                 
         GOTO1 REPORT                                                           
*                                                                               
         LA    R3,L'AGYTABLE(R3)                                                
         B     AGYSEQ                                                           
*                                                                               
DONEAGY  MVC   0(3,R3),=X'FFFFFF'                                               
         LA    RE,AGYTABLE                                                      
         STCM  RE,15,VAGYTAB                                                    
*********************************************************                       
* NOW WE MUST CHECK THE 0D03 MARKET ASSIGNMENT RECORDS                          
* FOR THE AGEN/MED WE HAVE IN THE AGENCY TABLE                                  
* ALL T MED MUST HAVE THE SAME N AND C MEDIAS 0D03 RECS                         
* IF NO T MED FOR THAT 0D03 WE MUST MAKE SURE THERE ARE NO                      
* N AND C MED 0D03 RECORDS AS WELL                                              
*********************************************************                       
MKT10    LA    R3,AGYTABLE                                                      
MKT20    XC    MKTKEYSV,MKTKEYSV                                                
         LA    R2,MKTKEYSV                                                      
         CLC   =X'FFFFFF',0(R3)                                                 
         BE    DONEMKT                                                          
         USING MKARECD,R2                                                       
         MVC   MKTKEYSV(2),=X'0D03'     MARKET ASSIGNMENT RECORD                
         MVC   MKAKAGMD,0(R3)     MEDI T AGY/MED                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',MKTKEYSV,KEY                  
         CLI   DMCB+8,0                                                         
         BE    MKT30                                                            
         DC    H'0'                                                             
*                                                                               
MKTSEQ   GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'SPTDIR',MKTKEYSV,KEY                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MKT30    CLC   KEY(MKAKCLT-MKARECD),MKTKEYSV     SAME AGY/MED 0D03 ?            
         BE    MKT35                                                            
*                                                                               
         LA    R2,KEY              IF NEW KEY READ IS STILL UP TO               
         USING MKARECD,R2          THE SAME MEDIA IN AGY/MED TABLE              
         CLC   MKAKAGMD,0(R3)      SAME MEDIA T UP TO IN TABLE?                 
         BNE   MKT200              IF NOT,MUST BE 1ST OF NEXT AGY/MED           
*  IF SAME MEDIA AS TABLE AND NOT FOUND CHECK FOR MED N AND C                   
         MVC   P(11),=C'T NOT FOUND'                                            
         GOTO1 HEXOUT,DMCB,MKTKEYSV,P+15,13,=C'TOG'                             
         GOTO1 REPORT                                                           
         BAS   RE,CHKNC            NO MED T CHECK MED N AND C                   
         B     MKT200                                                           
MKT35    MVC   MKTKEYSV,KEY                      SAVE NEW KEY                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',                     +        
               KEY+14,IO,DMWORK                                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZICM  R0,IO+13,2                                                       
         ST    R0,DMCB+8                                                        
         GOTO1 HEXOUT,DMCB,IO,P+2,,=C'TOG'                                      
         GOTO1 REPORT                                                           
*                                                                               
***********     MEDIA N *************************************                   
* READ AGY/MED N TO SEE IF IT EXIST AND IT MUST MATCH MEDIA T                   
* IF NOT COPY T TO N                                                            
*************************************************************                   
                                                                                
         LA    RE,KEYSAVE                                                       
MEDNKEY  USING MKARECD,RE                                                       
         MVC   KEYSAVE,MKTKEYSV                                                 
         MVC   MEDNKEY.MKAKAGMD,1(R3)     MEDI N AGY/MED                        
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*        CLC   KEY(MKAKCLT-MKARECD),KEYSAVE      SAME AGY/MED 0D03 ?            
         CLC   KEY(L'MKAKEY),KEYSAVE      SAME AGY/MED 0D03 ?                   
         BE    MKT50                                                            
         MVC   P(28),=C'THIS 0D03 DOESNT HAVE MED N '                           
         GOTO1 REPORT                                                           
* ADD THE MEDIA N RECORD SAME AS T                                              
*        ZICM  RF,IO+14,2   LENGTH OF RECORD OF MEDIA N                         
         LA    RF,L'IO                                                          
         LR    R1,RF                                                            
         LA    R0,IO                   MEDIA T 0D03                             
         LA    RE,IO2                  MEDIA N 0D03                             
         MVCL  RE,R0                                                            
         OI    IO2+8,X'03'             TURN ON MEDIA N                          
*                                                                               
*        ADDREC CALL HERE                                                       
         CLI   RCWRITE,C'N'                                                     
         BE    MKT45                                                            
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'SPTFILE',KEY+14,IO2,DMWORK            
*                                                                               
MKT45    DS    0H                                                               
         MVC   P(11),=C'ADDED N REC'                                            
         GOTO1 REPORT                                                           
         ZICM  R0,IO2+13,2                                                      
         ST    R0,DMCB+8                                                        
         GOTO1 HEXOUT,DMCB,IO2,P,,=C'TOG'                                       
         GOTO1 REPORT                                                           
         B     READC                                                            
MKT50    DS    0H              WE HAVE MEDIA N FOR THIS 0D03                    
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',                     +        
               KEY+14,IO2,DMWORK                                                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZICM  RF,IO+13,2   LENGTH OF RECORD OF MEDIA N                         
         LA    R0,L'MKAKEY   SUBTRACT LEN OF KEY                                
         SR    RF,R0                                                            
         LR    R1,RF                                                            
         LA    R0,IO+L'MKAKEY          MEDIA T 0D03                             
*        LA    R1,IO+L'MKAKEY          MEDIA T 0D03                             
         LA    RE,IO2+L'MKAKEY         MEDIA N 0D03                             
         CLCL  R0,RE                                                            
         BE    MKT100                                                           
*                                                                               
*  COPY MED T TO MED N                                                          
*                                                                               
*                                                                               
         MVC   P(38),=C'THIS 0D03 MEDIA T DOESNT MATCH MEDIA N'                 
         GOTO1 REPORT                                                           
         MVC   P(5),=C'KEY 1'                                                   
         GOTO1 HEXOUT,DMCB,MKTKEYSV,P+7,13,=C'TOG'                              
         GOTO1 REPORT                                                           
         ZICM  R0,IO+13,2                                                       
         ST    R0,DMCB+8                                                        
         GOTO1 HEXOUT,DMCB,IO,P,,=C'TOG'                                        
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(5),=C'KEY 2'                                                   
         GOTO1 HEXOUT,DMCB,KEY,P+7,13,=C'TOG'                                   
         GOTO1 REPORT                                                           
*                                                                               
         ZICM  R0,IO2+13,2                                                      
         ST    R0,DMCB+8                                                        
         GOTO1 HEXOUT,DMCB,IO2,P,,=C'TOG'                                       
         GOTO1 REPORT                                                           
*                                                                               
         LA    RF,L'IO                                                          
         LR    R1,RF                                                            
         LA    R0,IO2                                                           
         LA    RE,IO                                                            
         MVCL  R0,RE                                                            
*                                                                               
         OI    IO2+8,X'03'             MEDIA N                                  
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    MKT90                                                            
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFIL',                     +        
               KEY+14,IO2,DMWORK                                                
*        GOTO1 DATAMGR,DMCB,PUTREC,=C'SPTFILE',KEY+14,IO2                       
*                                                                               
*  PRINT OUT FIXED RECORD  MED N                                                
MKT90    ZICM  R0,IO2+13,2                                                      
         ST    R0,DMCB+8                                                        
         GOTO1 HEXOUT,DMCB,IO2,P+8,,=C'TOG'                                     
         MVC   P(6),=C'FIXED '                                                  
         GOTO1 REPORT                                                           
*                                                                               
         B     READC     READ MEDIA C                                           
*                                                                               
MKT100   ZICM  R0,IO2+13,2                                                      
         ST    R0,DMCB+8                                                        
         GOTO1 HEXOUT,DMCB,IO2,P+2,,=C'TOG'                                     
         GOTO1 REPORT                                                           
*                                                                               
READC    DS    0H                                                               
*                                                                               
***********     MEDIA C *************************************                   
* READ AGY/MED N TO SEE IF IT EXIST AND IT MUST MATCH MEDIA T                   
* IF NOT COPY T TO N                                                            
*************************************************************                   
         LA    RE,KEYSAVE                                                       
MEDCKEY  USING MKARECD,RE                                                       
         MVC   KEYSAVE,MKTKEYSV                                                 
         MVC   MEDCKEY.MKAKAGMD,2(R3)     MEDI C AGY/MED                        
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*        CLC   KEY(MKAKCLT-MKARECD),KEYSAVE      SAME AGY/MED 0D03 ?            
         CLC   KEY(L'MKAKEY),KEYSAVE      SAME AGY/MED 0D03 ?                   
         BE    MKT120                                                           
         MVC   P(27),=C'THIS 0D03 DOESNT HAVE MED C'                            
         GOTO1 REPORT                                                           
* ADD THE MEDIA N RECORD SAME AS T                                              
*        ZICM  RF,IO+14,2   LENGTH OF RECORD OF MEDIA C                         
         LA    RF,L'IO                                                          
         LR    R1,RF                                                            
         LA    R0,IO                   MEDIA T 0D03                             
         LA    RE,IO3                  MEDIA C 0D03                             
         MVCL  RE,R0                                                            
         NI    IO3+8,X'F0'                                                      
         OI    IO3+8,X'08'             TURN ON MEDIA C                          
*                                                                               
*        ADDREC CALL HERE                                                       
         CLI   RCWRITE,C'N'                                                     
         BE    MKT115                                                           
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'SPTFILE',KEY+14,IO3,DMWORK            
*                                                                               
MKT115   MVC   P(11),=C'ADDED C REC'                                            
         GOTO1 REPORT                                                           
         ZICM  R0,IO3+13,2                                                      
         ST    R0,DMCB+8                                                        
         GOTO1 HEXOUT,DMCB,IO3,P,,=C'TOG'                                       
         GOTO1 REPORT                                                           
*                                                                               
         B     MKT180                                                           
*                                                                               
MKT120   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',                     +        
               KEY+14,IO3,DMWORK                                                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZICM  RF,IO+13,2                                                       
         LA    R0,L'MKAKEY                                                      
         SR    RF,R0                                                            
         LR    R1,RF                                                            
         LA    R0,IO3+L'MKAKEY                                                  
         LA    RE,IO+L'MKAKEY                                                   
         CLCL  R0,RE                                                            
         BE    MKT160                                                           
*                                                                               
*  COPY MED T TO MED C                                                          
*                                                                               
*                                                                               
         MVC   P(38),=C'THIS 0D03 MEDIA T DOESNT MATCH MEDIA C'                 
         GOTO1 REPORT                                                           
         MVC   P(5),=C'KEY 1'                                                   
         GOTO1 HEXOUT,DMCB,MKTKEYSV,P+7,13,=C'TOG'                              
         GOTO1 REPORT                                                           
         ZICM  R0,IO+13,2                                                       
         ST    R0,DMCB+8                                                        
*        GOTO1 HEXOUT,DMCB,IO,P,100,=C'TOG'                                     
         GOTO1 HEXOUT,DMCB,IO,P,,=C'TOG'                                        
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(5),=C'KEY 3'                                                   
         GOTO1 HEXOUT,DMCB,KEY,P+7,13,=C'TOG'                                   
         GOTO1 REPORT                                                           
*                                                                               
         ZICM  R0,IO3+13,2                                                      
         ST    R0,DMCB+8                                                        
         GOTO1 HEXOUT,DMCB,IO3,P,100,=C'TOG'                                    
         GOTO1 REPORT                                                           
* NOW MOVE MEDIA T TO MEDIA C REC                                               
         LA    RF,L'IO                                                          
         LR    R1,RF                                                            
         LA    R0,IO3                                                           
         LA    RE,IO                                                            
         MVCL  R0,RE                                                            
*                                                                               
         NI    IO3+8,X'F0'                                                      
         OI    IO3+8,X'08'                                                      
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    MKT140                                                           
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFIL',                     +        
               KEY+14,IO3,DMWORK                                                
*        GOTO1 DATAMGR,DMCB,PUTREC,=C'SPTFILE',KEY+14,IO3                       
*                                                                               
MKT140   ZICM  R0,IO3+13,2                                                      
         ST    R0,DMCB+8                                                        
         GOTO1 HEXOUT,DMCB,IO3,P+8,,=C'TOG'                                     
         MVC   P(6),=C'FIXED '                                                  
         GOTO1 REPORT                                                           
*                                                                               
         B     MKT180                                                           
*                                                                               
*                                                                               
MKT160   ZICM  R0,IO3+13,2                                                      
         ST    R0,DMCB+8                                                        
         GOTO1 HEXOUT,DMCB,IO3,P+2,,=C'TOG'                                     
         GOTO1 REPORT                                                           
*                                                                               
*    RESTORE SEQ READ                                                           
MKT180   XC    KEY,KEY         RESTORE MKT KEY AND READ MORE RECS WITH          
         MVC   KEY,MKTKEYSV    THAT AGY/MED                                     
*        GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',MKTKEYSV,KEY                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*        CLC   KEY(MKAKCLT-MKARECD),MKTKEYSV                                    
         CLC   KEY(L'MKAKEY),MKTKEYSV     SAME RECORD READ                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MKTKEYSV,KEY                                                     
         B     MKTSEQ                                                           
*                                                                               
MKT200   LA    R3,3(R3)        READ NEXT MEDIA T IN AGY TABLE                   
         B     MKT20                                                            
*                                                                               
DONEMKT  DS    0H                                                               
********************************************************                        
*  CHECK MEDIA N RECORDS.  IF NO MEDIA T THEN DELETE MED N                      
********************************************************                        
MKT300   LA    R3,AGYTABLE                                                      
         MVC   P(25),=C'****** CHECK N FIRST ***'                               
         GOTO1 REPORT                                                           
MKT310   XC    MKTKEYSV,MKTKEYSV                                                
         LA    R2,MKTKEYSV                                                      
         CLC   =X'FFFFFF',0(R3)                                                 
         BE    DONEMKTN                                                         
         USING MKARECD,R2                                                       
         MVC   MKTKEYSV(2),=X'0D03'     MARKET ASSIGNMENT RECORD                
         MVC   MKAKAGMD,1(R3)     MEDI N AGY/MED                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',MKTKEYSV,KEY                  
         CLI   DMCB+8,0                                                         
         BE    MKT330                                                           
         DC    H'0'                                                             
*                                                                               
MKTNSEQ  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'SPTDIR',MKTKEYSV,KEY                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MKT330   CLC   KEY(MKAKCLT-MKARECD),MKTKEYSV     SAME AGY/MED 0D03 ?            
         BE    MKT350                                                           
*                                                                               
MKT340   LA    R2,KEY              IF NEW KEY READ IS STILL UP TO               
         USING MKARECD,R2          THE SAME MEDIA IN AGY/MED TABLE              
         CLC   MKAKAGMD,1(R3)      SAME MEDIA N UP TO IN TABLE?                 
         BNE   MKT440              IF NOT,MUST BE 1ST OF NEXT AGY/MED           
* CHECK FOR AGY/MED T                                                           
MKT350   DS    0H                                                               
         GOTO1 HEXOUT,DMCB,KEY,P,13,=C'TOG'                                     
         GOTO1 REPORT                                                           
         MVC   MKTKEYSV,KEY                                                     
         LA    RE,KEYSAVE                                                       
MEDTKEY  USING MKARECD,RE                                                       
         MVC   KEYSAVE,MKTKEYSV                                                 
         MVC   MEDTKEY.MKAKAGMD,0(R3)     MEDI T AGY/MED                        
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
****     CLC   KEY(MKAKCLT-MKARECD),KEYSAVE                                     
         CLC   KEY(L'MKAKEY),KEYSAVE            MEDIA T REC?                    
*        BE    *+8                                                              
*        B     MKT360                            DONT HAVE T                    
*        CLC   MKTKEYSV+9(4),KEY+9  SAME CLT/MKT AS MED N?                      
         BE    MKT400                                                           
MKT360   MVC   P(30),=C'THIS 0D03 N DOESNT HAVE MED T '                         
         GOTO1 REPORT                                                           
         MVC   P(10),=C'MED T READ'                                             
         GOTO1 HEXOUT,DMCB,KEY,P+13,13,=C'TOG'                                  
         GOTO1 REPORT                                                           
*********** NOW WE MUST DEL N AND C MEDIA 0D03 RECORDS                          
*  REREAD MED N TO DEL IT                                                       
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',MKTKEYSV,KEY                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',                     +        
               KEY+14,IO,DMWORK                                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SAVEKEY2,KEY                                                     
         OI    KEY+13,X'80'    TURN ON DELETION                                 
         CLI   RCWRITE,C'Y'                                                     
         BNE   MKT380                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR',SAVEKEY2,KEY                   
         OI    IO+15,X'80'        MARK REC FOR DEL                              
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFIL',                     +        
               SAVEKEY2+14,IO,DMWORK                                            
*                                                                               
MKT380   MVC   P(9),=C'DELETED N'                                               
         GOTO1 HEXOUT,DMCB,KEY,P+13,14,=C'TOG'                                  
         GOTO1 REPORT                                                           
*&&DO                                                                           
* ===== MEDIA C MUST BE DELETED                                                 
MKT385   DS    0H                                                               
         MVC   KEYSAVE,MKTKEYSV                                                 
         MVC   KEYSAVE+8(1),2(R3)       ,MEDIA N                                
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(L'MKAKEY),KEYSAVE      DO WE HAVE MEDIA C?                   
         BNE   MKT390                                                           
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',                     +        
               KEY+14,IO2,DMWORK                                                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SAVEKEY2,KEY                                                     
         OI    KEY+13,X'80'    TURN ON DELETION                                 
         CLI   RCWRITE,C'Y'                                                     
         BNE   MKT388                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR',SAVEKEY2,KEY                   
         OI    IO2+15,X'80'        MARK REC FOR DEL                             
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFIL',                     +        
               SAVEKEY2+14,IO2,DMWORK                                           
*                                                                               
MKT388   MVC   P(9),=C'DELETED C'                                               
         GOTO1 HEXOUT,DMCB,KEY,P+13,14,=C'TOG'                                  
         GOTO1 REPORT                                                           
*&&                                                                             
MKT390   B     MKT420      GO RESTORE READ SEQ AND GET NEXT MED N               
*****************************************************                           
MKT400   DS    0H                                                               
         GOTO1 HEXOUT,DMCB,KEY,P,13,=C'TOG'                                     
         GOTO1 REPORT                                                           
         MVC   KEY,MKTKEYSV    THAT AGY/MED                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',MKTKEYSV,KEY                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(MKAKCLT-MKARECD),MKTKEYSV     SAME AGY/MED 0D03 ?            
         BE    *+6                                                              
         DC    H'0'                           DIE IF DONT HAVE T RECORD         
         MVC   P(11),=C'KEY RESTORE'                                            
         GOTO1 HEXOUT,DMCB,MKTKEYSV,P+14,13,=C'TOG'                             
         GOTO1 REPORT                                                           
         B     MKTNSEQ                                                          
* RESTORE READ SEQ  FOR DELETED RECORD  (NEXT REC)                              
MKT420   XC    KEY,KEY         RESTORE MKT KEY AND READ MORE RECS WITH          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',MKTKEYSV,KEY                  
*        CLI   DMCB+8,X'02'                                                     
*        BE    MKT425                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MKTKEYSV,KEY                                                     
         B     MKT340                                                           
*                                                                               
MKT440   LA    R3,3(R3)                                                         
         B     MKT310                                                           
*                                                                               
*                                                                               
DONEMKTN DS    0H                                                               
********************************************************                        
*  ALL C MUST HAVE T IF NOT DELETE N AND C                                      
********************************************************                        
*                                                                               
MKT500   LA    R3,AGYTABLE                                                      
         MVC   P(25),=C'****** CHECK C FIRST ***'                               
         GOTO1 REPORT                                                           
MKT510   XC    MKTKEYSV,MKTKEYSV                                                
         LA    R2,MKTKEYSV                                                      
         CLC   =X'FFFFFF',0(R3)                                                 
         BE    DONEMKTC                                                         
         USING MKARECD,R2                                                       
         MVC   MKTKEYSV(2),=X'0D03'     MARKET ASSIGNMENT RECORD                
         MVC   MKAKAGMD,2(R3)     MEDI N AGY/MED                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',MKTKEYSV,KEY                  
         CLI   DMCB+8,0                                                         
         BE    MKT530                                                           
         DC    H'0'                                                             
*                                                                               
MKTCSEQ  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'SPTDIR',MKTKEYSV,KEY                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MKT530   CLC   KEY(MKAKCLT-MKARECD),MKTKEYSV     SAME AGY/MED 0D03 ?            
         BE    MKT550                                                           
*                                                                               
MKT540   LA    R2,KEY              IF NEW KEY READ IS STILL UP TO               
         USING MKARECD,R2          THE SAME MEDIA IN AGY/MED TABLE              
         CLC   MKAKAGMD,2(R3)      SAME MEDIA N UP TO IN TABLE?                 
         BNE   MKT640              IF NOT,MUST BE 1ST OF NEXT AGY/MED           
* CHECK FOR AGY/MED T                                                           
MKT550   DS    0H                                                               
         GOTO1 HEXOUT,DMCB,KEY,P,13,=C'TOG'                                     
         GOTO1 REPORT                                                           
         MVC   MKTKEYSV,KEY                                                     
         LA    RE,KEYSAVE                                                       
MEDCKEY  USING MKARECD,RE                                                       
         MVC   KEYSAVE,MKTKEYSV                                                 
         MVC   MEDCKEY.MKAKAGMD,0(R3)     MEDI T AGY/MED                        
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*****    CLC   KEY(MKAKCLT-MKARECD),KEYSAVE                                     
         CLC   KEY(L'MKAKEY),KEYSAVE               MEDIA T REC?                 
*        BE    *+8                                                              
*        B     MKT560                            DONT HAVE T                    
*        CLC   MKTKEYSV+9(4),KEY+9  SAME CLT/MKT AS MED N?                      
         BE    MKT600                                                           
MKT560   MVC   P(30),=C'THIS 0D03 C DOESNT HAVE MED T '                         
         GOTO1 REPORT                                                           
         MVC   P(10),=C'MED T READ'                                             
         GOTO1 HEXOUT,DMCB,KEY,P+13,13,=C'TOG'                                  
         GOTO1 REPORT                                                           
*********** NOW WE MUST DEL C AND N MEDIA 0D03 RECORDS                          
*  REREAD MED C TO DEL IT                                                       
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',MKTKEYSV,KEY                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',                     +        
               KEY+14,IO,DMWORK                                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SAVEKEY2,KEY                                                     
         OI    KEY+13,X'80'    TURN ON DELETION                                 
         CLI   RCWRITE,C'Y'                                                     
         BNE   MKT580                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR',SAVEKEY2,KEY                   
         OI    IO+15,X'80'        MARK REC FOR DEL                              
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFIL',                     +        
               SAVEKEY2+14,IO,DMWORK                                            
*                                                                               
MKT580   MVC   P(9),=C'DELETED C'                                               
         GOTO1 HEXOUT,DMCB,KEY,P+13,14,=C'TOG'                                  
         GOTO1 REPORT                                                           
*&&DO                                                                           
* ===== MEDIA N MUST BE DELETED                                                 
MKT585   DS    0H                                                               
         MVC   KEYSAVE,MKTKEYSV                                                 
         MVC   KEYSAVE+8(1),1(R3)       ,MEDIA N                                
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(L'MKAKEY),KEYSAVE      DO WE HAVE MEDIA N?                   
         BNE   MKT590                                                           
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',                     +        
               KEY+14,IO2,DMWORK                                                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SAVEKEY2,KEY                                                     
         OI    KEY+13,X'80'    TURN ON DELETION                                 
         CLI   RCWRITE,C'Y'                                                     
         BNE   MKT588                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR',SAVEKEY2,KEY                   
         OI    IO2+15,X'80'        MARK REC FOR DEL                             
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFIL',                     +        
               SAVEKEY2+14,IO2,DMWORK                                           
*                                                                               
MKT588   MVC   P(9),=C'DELETED N'                                               
         GOTO1 HEXOUT,DMCB,KEY,P+13,14,=C'TOG'                                  
         GOTO1 REPORT                                                           
*&&                                                                             
MKT590   B     MKT620      GO RESTORE READ SEQ AND GET NEXT MED N               
*****************************************************                           
MKT600   DS    0H                                                               
         GOTO1 HEXOUT,DMCB,KEY,P,13,=C'TOG'                                     
         GOTO1 REPORT                                                           
*                                                                               
         MVC   KEY,MKTKEYSV    THAT AGY/MED                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',MKTKEYSV,KEY                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(MKAKCLT-MKARECD),MKTKEYSV     SAME AGY/MED 0D03 ?            
         BE    *+6                                                              
         DC    H'0'                           DIE IF DONT HAVE T RECORD         
         MVC   MKTKEYSV,KEY                                                     
         MVC   P(11),=C'KEY RESTORE'                                            
         GOTO1 HEXOUT,DMCB,MKTKEYSV,P+14,13,=C'TOG'                             
         GOTO1 REPORT                                                           
         B     MKTCSEQ                                                          
*                                                                               
* RESTORE READ SEQ  NEXT RECORD WITHOUT DELETED                                 
MKT620   XC    KEY,KEY         RESTORE MKT KEY AND READ MORE RECS WITH          
         MVC   KEY,MKTKEYSV    THAT AGY/MED                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',MKTKEYSV,KEY                  
*        CLI   DMCB,X'08'                                                       
*        BE    MKT625                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*KT625   CLC   KEY(MKAKCLT-MKARECD),MKTKEYSV     SAME AGY/MED 0D03 ?            
*        BE    *+6                                                              
*        DC    H'0'                           DIE IF DONT HAVE T RECORD         
         MVC   MKTKEYSV,KEY                                                     
*        MVC   P(11),=C'KEY RESTORE'                                            
*        GOTO1 HEXOUT,DMCB,MKTKEYSV,P+14,13,=C'TOG'                             
*        GOTO1 REPORT                                                           
*        B     MKTCSEQ                                                          
         B     MKT540                                                           
*                                                                               
MKT640   LA    R3,3(R3)                                                         
         B     MKT510                                                           
*                                                                               
DONEMKTC DS    0H                                                               
*                                                                               
********************************************************                        
FX2XIT   GOTO1 AENDREQ                                                          
         EJECT                                                                  
         DC    H'0'                                                             
*                                                                               
*******************************************************                         
CHKNC    NTR1                                                                   
         LA    RE,KEYSAVE                                                       
TEMP     USING MKARECD,RE                                                       
         MVC   KEYSAVE,MKTKEYSV           ORIGINAL KEY                          
         MVC   TEMP.MKAKAGMD,1(R3)     MEDI N AGY/MED                           
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(MKAKCLT-MKARECD),KEYSAVE      SAME AGY/MED 0D03 ?            
         BNE   CHK60                                                            
*                                                                               
CHK50    DS    0H              WE HAVE MEDIA N FOR THIS 0D03                    
*                                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',                     +        
               KEY+14,IO2,DMWORK                                                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(22),=C'WE HAVE N REC BUT NO T'                                 
         GOTO1 REPORT                                                           
         MVC   P(100),IO2                                                       
         GOTO1 REPORT                                                           
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   CHK60                                                            
         MVC   SAVEKEY2,KEY                                                     
*                                                                               
         OI    KEY+13,X'80'    TURN ON DELETION                                 
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR',SAVEKEY2,KEY                   
         OI    IO2+15,X'80'        MARK REC FOR DEL                             
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFIL',                     +        
               SAVEKEY2+14,IO2,DMWORK                                           
CHK60    DS    0H                                                               
         DC    H'0'                                                             
CHKX     XIT1                                                                   
*******************************************************                         
         GETEL R6,24,ELCODE                                                     
*                                                                               
TEMPRECD DS    0CL80                                                            
TEMPHEAD DS    CL4                                                              
TEMPBAGY DS    CL1                                                              
TEMPAGY  DS    CL2                                                              
TEMPSEQ  DS    XL64                                                             
         DS    CL9                                                              
AGYTABLE DS    30CL3                                                            
VAGYTAB  DS    A                                                                
AIO      DS    A                                                                
IO       DS    CL1000                                                           
IO2      DS    CL1000                                                           
IO3      DS    CL1000                                                           
MKTKEYSV DS    CL(L'KEY)                                                        
SAVEKEY2 DS    CL(L'KEY)                                                        
ELCODE   DS    CL1                                                              
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPCBLLST                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
AGYRECD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
MKTASGD  DSECT                                                                  
       ++INCLUDE SPGENMKA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'130SPREPFXBEN02/20/99'                                      
         END                                                                    
