*          DATA SET SPBUY11    AT LEVEL 056 AS OF 08/08/14                      
*PHASE T21111B  <====                                                           
                                                                                
*===============================================================                
* 16SEP13 MHER FOR CABLE MG, INPUT ONLY NETWORK                                 
*                                                                               
* 02OCT03 MHER INSERT SEQ NUMBER IN SVNPGM DATA FOR DEMO DATA REF               
*                                                                               
* 23OCT01 MHER CHANGE STATION IN DOVREC FROM 1 BYTE SEQ TO 2-BYTE               
*              PACKED                                                           
*===============================================================                
                                                                                
         TITLE 'T21111 - SPOTPAK BUY - BUY DESC EDITS II'                       
T21111   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21111                                                         
*                                                                               
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21111+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
*                                                                               
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
*                                                                               
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         ZIC   RF,EDTVAL           GET BRANCH VALUE                             
         BASR  RE,0                SET RE SO HAVE A CLUE I WAS HERE             
         B     BUEDTTAB(RF)                                                     
         EJECT                                                                  
BUEDTTAB DC    5AL4(0)             X'00'-X'10'                                  
         B     PGM                 X'14'                                        
         B     ADJ                 X'18'                                        
         DC    3AL4(0)             X'1C'-X'24'                                  
         B     REF                 X'28'                                        
         DC    9AL4(0)             X'2C'-X'4C'                                  
         B     PGM                 X'50' (PGMCD)                                
         DC    20AL4(0)            X'54'-X'A0'                                  
         B     DSK                 X'A4'                                        
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
BUYERR   GOTO1 ERROR                                                            
         EJECT                                                                  
* EDIT PROGRAMMING                                                              
*                                                                               
PGM      MVI   UPNDX,SBUYPRGQ                                                   
         MVC   BUPROG,SPACES                                                    
         MVI   ERRCD,PGMERR                                                     
         GOTO1 FLDVAL                                                           
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BE    PGM1                                                             
         TM    SVCOPT2,X'02'        BUY PROG PROFILE ON?                        
         BZ    PGM1                                                             
*                                                                               
         L     RE,=A(SVBPPROF-BUYSAVE)                                          
         AR    RE,RA                                                            
         LA    RF,15                                                            
         CLI   0(RE),C'I'       INCLUDE?                                        
         LA    RE,1(RE)                                                         
         BNE   PGMB                                                             
PGMA     CLC   0(1,R4),0(RE)                                                    
         BE    PGMC                                                             
         LA    RE,1(RE)                                                         
         BCT   RF,PGMA                                                          
         B     BUYERR                                                           
*                                                                               
PGMB     CLC   0(1,R4),0(RE)       EXCLUDE                                      
         BE    BUYERR                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,PGMB                                                          
*                                                                               
PGMC     CLI   1(R4),C'/'                                                       
         BNE   BUYERR                                                           
*                                                                               
PGM1     CLI   BUTRCODE,C'B'       TEST NEW BUY                                 
         BNE   *+14                NO                                           
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BNZ   PGM10               YES                                          
*                                                                               
         LTR   R5,R5                                                            
         BNZ   PGM2                                                             
         LA    R4,1(R4)            UPDATE INPUT POINTER                         
         ST    R4,FADDR                                                         
         B     EXIT                                                             
PGM2     CLI   FLEN+1,17                                                        
         BH    BUYERR                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
PGM2MVC  MVC   BUPROG(0),0(R4) *EXECUTED*                                       
*                                                                               
         LA    RE,BUPROG-1(R5)     POINT TO NEXT TO LAST CHAR                   
         CLC   0(2,RE),=C'-S'                                                   
         BNE   EXIT                                                             
         MVI   BUPROG+17,0         SET SPECIAL PROGRAM IND                      
         CLI   SVAPROF+6,C'Y'      TEST -S AUTH REQUIRED                        
         BNE   EXIT                                                             
         TM    T211FFD+12,X'80'    TEST BIT ON                                  
         BO    EXIT                                                             
         MVI   ERRCD,NOFNACCS                                                   
         B     BUYERR                                                           
         EJECT                                                                  
* CANAD NTWK                                                                    
*                                                                               
PGM10    DS    0H                                                               
* CLEAR STATION LIST BUILD AREA                                                 
         L     R0,AREC3                                                         
         LHI   R1,REC2-REC                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   BUXSW,0             RESET STA EXCEPTION SWITCH                   
         LA    RE,0(R4,R5)         POINT PAST PROGRAM                           
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
         CLC   =C'-S',0(RE)                                                     
         BNE   PGM10A                                                           
         MVI   BUPROG+17,0         SET '-S' FLAG                                
         BCTR  R5,0                                                             
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
PGM10A   CLI   0(R4),C'='                                                       
         BE    PGM11                                                            
* NOT SIMULCAST                                                                 
         CLI   BUPROG+17,0                                                      
         BE    *+8                                                              
         LA    RE,2(RE)            POINT TO END AGAIN IF NOT -S                 
         SH    RE,=H'4'            NOW BACK UP TO SEARCH FOR /X                 
         CLC   =C'/XON',0(RE)      TEST USE STATION EXCEPTIONS                  
         BNE   PGM10B                                                           
         MVI   BUXSW,C'Y'          SET SWITCH                                   
         SH    R5,=H'4'            ADJUST INPUT LEN                             
         BZ    BUYERR              MAKE SURE MORE REMAINS                       
         B     PGM10C                                                           
*                                                                               
PGM10B   BCTR  RE,0                BACK UP ANOTHER                              
         CLC   =C'/XOFF',0(RE)     TEST SUPPRESS STA EXCEPTIONS                 
         BNE   PGM10C                                                           
         MVI   BUXSW,C'N'                                                       
         SH    R5,=H'5'                                                         
         BZ    BUYERR                                                           
*                                                                               
PGM10C   CH    R5,=H'4'                                                         
         BH    BUYERR                                                           
         MVC   BUORB,SPACES                                                     
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     PGM12                                                            
PGM10MVC MVC   BUORB(0),0(R4) *EXECUTED*                                        
         EJECT                                                                  
*=========================================================*                     
* SIMULCAST                                               *                     
*=========================================================*                     
         SPACE 1                                                                
PGM11    CLI   FLEN+1,17                                                        
         BH    BUYERR                                                           
         LH    R5,FLEN             RESTORE LENGTH                               
         BCTR  R5,0                                                             
         EX    R5,PGM2MVC          MOVE NAME TO PROGRAM                         
         CH    R5,=H'3'                                                         
         BNH   *+8                                                              
         LA    R5,3                                                             
         MVC   BUORB,SPACES        AND SET UP READ SHOW REC                     
         EX    R5,PGM10MVC                                                      
*                                                                               
PGM12    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D12'                                                  
         MVC   KEY+2(2),AGYALPHA                                                
         MVC   KEY+4(4),QSTA                                                    
         MVC   KEY+8(4),BUORB                                                   
         GOTO1 HIGH                                                             
         MVI   ERRCD,PGMNOTFD                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BE    PGM13                                                            
         CLI   BUORB,C'='          TEST SIMULCAST                               
         BNE   BUYERR              NO - TOPOR                                   
         EJECT                                                                  
* NO SHOW REC - BUILD STATION LIST NOW                                          
         LA    R7,SVNDEF                                                        
         USING SVNDEFD,R7                                                       
         L     RE,AREC3                                                         
         USING SVNPGMD,RE                                                       
*                                                                               
PGM12X   CLC   SVNDPCT,=F'-1'      TEST NOT BOUGHT                              
         BE    PGM12Y                                                           
         MVC   SVNPMKST,SVNDMKST   MKT-STA                                      
         MVC   SVNPTIM,SVNDOFF     TIME OFFSET                                  
         MVC   SVNPSHR,SVNDPCT     COST SHARE                                   
         MVC   SVNPTAX,SVNDTAX     TAX RATE                                     
         MVC   SVNPSEQ,SVNDSEQ     SEQNUM                                       
         LA    RE,SVNPLEN(RE)                                                   
*                                                                               
PGM12Y   LA    R7,L'SVNDEF(R7)                                                  
         OC    SVNDMKST,SVNDMKST                                                
         BNZ   PGM12X                                                           
         B     EXIT                                                             
         DROP  R7,RE                                                            
         EJECT                                                                  
*======================================================*                        
* PROCESS SHOW RECORD                                  *                        
*======================================================*                        
         SPACE 1                                                                
PGM13    LA    R5,REC                                                           
         ST    R5,AREC                                                          
         USING NPGMRECD,R5                                                      
         GOTO1 GETREC                                                           
* CHECK START DATE VS. KILL DATE                                                
         MVI   ERRCD,SHOWKLLD                                                   
         OC    NPGMKDAT,NPGMKDAT                                                
         BZ    *+14                                                             
         CLC   NPGMKDAT,BUSTARTP                                                
         BL    BUYERR                                                           
         MVC   BUPROG(17),NPGMPGM  MOVE TO BUPROG                               
*                                                                               
         CLI   BUPROG+17,0         TEST '-S'                                    
         BNE   PGM14                                                            
* FIND SPOT TO INSERT '-S'                                                      
         LA    RE,BUPROG+16                                                     
         LA    R0,17                                                            
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         LA    RE,1(RE)                                                         
         LA    R0,BUPROG+15        MAKE SURE HAVE AT LEAST 2 SPACES             
         CR    RE,R0                                                            
         BNH   *+6                                                              
         LR    RE,R0                                                            
         MVC   0(2,RE),=C'-S'                                                   
         EJECT                                                                  
*=================================================================*             
* BUXSW = 0 IF XON/XOFF IS NOT ENTERED                            *             
*       = Y IF XON ENTERED MEANING USE INPUT DAY/TIME BUT KEEP    *             
*           STATION EXCEPTIONS (LIKE +1)                          *             
*       = N IF XOFF ENTERED WHICH SUPPRESSES EXCEPTIONS           *             
*=================================================================*             
         SPACE 1                                                                
PGM14    CLI   BUORB,C'='          TEST SIMULCAST                               
         BE    PGM16                                                            
         CLI   BUXSW,0             TEST USE INPUT DAYS/TIMES                    
         BNE   PGM16               YES - IGNORE PROGRAM RECORD                  
*                                                                               
         MVI   ERRCD,DAYNOTEQ                                                   
         CLC   BUDAYS,NPGMDAY                                                   
         BNE   BUYERR                                                           
*                                                                               
         MVI   ERRCD,TIMNOTEQ                                                   
         CLC   BUTIME(4),NPGMSTR   SHOULD HAVE SAME START/END TIMES             
         BNE   BUYERR                                                           
*                                                                               
         MVI   ERRCD,DPTVSHOW                                                   
         LHI   RF,SVB0PROF-BUYSAVE                                              
         AR    RF,RA                                                            
         CLI   10(RF),C'Y'         TEST DPT MUST MATCH                          
         BNE   *+14                                                             
         CLC   BUDPT,NPGMDPT       YES- CHECK IT                                
         BNE   BUYERR                                                           
         MVC   BUDPT,NPGMDPT       NO - USE INPUT DAYPART                       
         EJECT                                                                  
*****************************************************************               
* BUILD SAVE AREA IN REC3                                       *               
* FORMAT IS STA(5)/REL DAY(2)/TIME(2)/DPT(1)/COST SHR(4)/TAX(2) *               
*****************************************************************               
         SPACE 1                                                                
PGM16    L     R8,AREC3                                                         
         USING SVNPGMD,R8                                                       
* PACK STATIONS IN PROGRAM REC                                                  
         LA    R6,NPGMEL                                                        
PGM18    CLI   0(R6),0                                                          
         BE    PGM20                                                            
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),5                                                          
         BNE   PGM18                                                            
         USING NPGMEL05,R6                                                      
         LA    R4,=C'0000'                                                      
         XC    WORK(8),WORK        CLEAR DEBRIS                                 
         MVC   WORK(4),NPGMSTA                                                  
         MVC   WORK+4(1),QSTA+4                                                 
         GOTO1 STAPACK,DMCB,(C'P',(R4)),WORK,DUB                                
         MVC   NPGMSTA(3),DUB+2    MOVE PACKED STATION OVER EBCDIC              
         NI    NPGMSTA+2,X'E0'     DROP MEDIA BITS                              
         MVC   BYTE,SVNDEF+4       MOVE ACTUAL BITS FROM FIRST STATION          
         NI    BYTE,X'1F'          DROP HOB'S                                   
         OC    NPGMSTA+2(1),BYTE   'OR' IN ACTUAL MEDIA BITS                    
         B     PGM18                                                            
         EJECT                                                                  
PGM20    DS    0H                                                               
         LA    R6,NPGMEL                                                        
         LA    R7,SVNDEF                                                        
         USING SVNDEFD,R7                                                       
         MVI   SVNDINDX,0          CLEAR SEQNUM                                 
*                                                                               
PGM22    CLC   SVNDPCT,=F'-1'      TEST NOT BOUGHT                              
         BE    PGM34               YES - SKIP                                   
*                                                                               
PGM23    CLI   0(R6),0                                                          
         BE    PGM24                                                            
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),5                                                          
         BNE   PGM23                                                            
*                                                                               
         CLC   SVNDSTA(2),NPGMSTA     SAME STATION                              
         BNE   PGM23                  NO                                        
         CLC   NPGMSTIM,=C'NO'        CHECK NOT SHOWN                           
         BE    PGM34                  YES - SKIP                                
         SPACE 1                                                                
* IF GET HERE, STATION IS IN SHOW RECORD *                                      
         SPACE 1                                                                
         CLI   BUXSW,C'N'          TEST XOFF (IGNORE EXCEPTIONS)                
         BE    PGM26               YES - SO IGNORE                              
         MVC   SVNPDAY(4),NPGMSDAY DAY/TIME                                     
         MVC   SVNPDPT,NPGMSDPT    DPT                                          
         B     PGM26                                                            
         SPACE 1                                                                
* IF GET HERE, STATION IS NOT IN SHOW RECORD *                                  
         SPACE 1                                                                
PGM24    DS    0H                                                               
         CLC   SVNDPCT,=F'-1'      TEST NOT BOUGHT                              
         BE    PGM34               YES - SKIP                                   
         CLI   BUXSW,0             TEST XON/XOFF ENTERED                        
         BNE   PGM26               YES - USE INPUT DAY/TIME/DPT                 
         MVC   SVNPTIM,NPGMSTR     ELSE USE TIME FROM SHOW RECORD               
         MVC   SVNPDPT,NPGMDPT                                                  
*                                                                               
PGM26    MVC   SVNPMKST,SVNDMKST   MKT/STA                                      
         MVC   SVNPSHR,SVNDPCT     COST SHARE                                   
         MVC   SVNPTAX,SVNDTAX     TAX RATE                                     
         MVC   SVNPSEQ,SVNDSEQ     SEQNUM                                       
*                                                                               
         CLI   BUORB,C'='          TEST SIMULCAST                               
         BNE   PGM28                                                            
*                                                                               
         XC    5(2,R8),5(R8)       CLEAR                                        
         MVC   7(2,R8),9(R7)       TIME OFFSET                                  
         MVI   9(R8),0             DAYPART                                      
         B     PGM32                                                            
         EJECT                                                                  
* MAKE SURE DAYPART IS IN MENU                                                  
         SPACE 1                                                                
PGM28    CLI   9(R8),0             TEST DAYPART PRESENT                         
         BNE   *+10                YES                                          
         MVC   9(1,R8),BUDPT       ELSE USE INPUT DAYPART                       
*                                                                               
         LA    R0,L'SVMENU-1                                                    
         LA    R1,SVMENU                                                        
*                                                                               
PGM30    CLC   9(1,R8),0(R1)                                                    
         BE    PGM32                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,PGM30                                                         
         MVI   ERRCD,DPTERR                                                     
         B     BUYERR                                                           
PGM32    DS    0H                                                               
         LA    R8,SVNPLEN(R8)                                                   
PGM34    LA    R7,L'SVNDEF(R7)                                                  
         LA    R6,NPGMEL           POINT TO FIRST ELEM AGAIN                    
         OC    SVNDMKST,SVNDMKST                                                
         BNZ   PGM22                                                            
         DROP  R7,R8                                                            
         EJECT                                                                  
*======================================================*                        
* CHECK FOR DEMO OVRD REC *                                                     
*======================================================*                        
         SPACE 1                                                                
PGM40    L     RE,AREC4                                                         
         XC    0(24,RE),0(RE)                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D17'                                                  
         MVC   KEY+2(1),SVKEY      A-M                                          
         MVC   KEY+3(2),SVKEY+6    PACKED NETWORK                               
****OLD  MVC   KEY+3(1),SVNETBTS   NTWK SEQUENCE NUMBER                         
         MVC   KEY+5(2),SVCLT      CLIENT OVERRIDE                              
         MVC   KEY+7(4),BUORB      PGM                                          
         MVC   KEY+11(1),SVCPROF+3  RTG SVC                                     
PGM42    DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    PGM43                                                            
         MVC   KEY,KEYSAVE         RESET KEY                                    
         XC    KEY+5(2),KEY+5      CLEAR CLIENT OVERRIDE                        
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   EXIT                                                             
*                                                                               
PGM43    L     R0,AREC                                                          
         L     R5,AREC4                                                         
         ST    R5,AREC                                                          
         USING DOVRECD,R5                                                       
         GOTO1 GETREC                                                           
         ST    R0,AREC                                                          
* TEST USE-TILL BOOK VS. EST BOOK                                               
         CLC   SVBOOK,DOVUTBK                                                   
         BNH   *+12                                                             
         MVI   0(R5),0                                                          
         B     EXIT                                                             
*                                                                               
         LA    RE,24(R5)           LOOK FOR SHOW OVERRIDE ELEMENT               
PGM44    ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BE    PGM50                                                            
         CLI   0(RE),99                                                         
         BNE   PGM44                                                            
* FOUND IT - NOW USE IT                                                         
         MVC   KEYSAVE,KEY                                                      
         MVC   KEY+7(4),2(RE)                                                   
         B     PGM42                                                            
         EJECT                                                                  
*======================================================*                        
* CHECK FOR SECOND DEMO OVRD REC                       *                        
*======================================================*                        
         SPACE 1                                                                
PGM50    L     R5,AREC4            CORRECT USER DEMO NUMBERS                    
*                                                                               
         L     RE,AREC5                                                         
         XC    0(13,RE),0(RE)      CLEAR IN CASE NOT FOUND                      
         GOTO1 SEQ                                                              
         CLC   KEY(12),KEYSAVE                                                  
         BNE   EXIT                                                             
         L     R0,AREC                                                          
         MVC   AREC,AREC5                                                       
         GOTO1 GETREC                                                           
         ST    R0,AREC                                                          
* NEED TO MAKE SURE THERE HAS BEEN SOME INPUT                                   
         L     RE,AREC5                                                         
         LA    R6,24(RE)                                                        
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             TEST FOR A SECOND ELEMENT                    
         BNE   PGM52               FOUND ONE - EXIT                             
         XC    0(13,RE),0(RE)      ELSE BEST TO PRETEND ITS NOT THERE           
         B     EXIT                                                             
*                                                                               
PGM52    L     R5,AREC5            CORRECT USER DEMO NUMBERS                    
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
PERADJ   NTR1                                                                   
*                                                                               
         CLI   BUPERIND,2          TEST INPUT METHOD                            
         BL    PERADJ20            WEEK                                         
         BE    PERADJ30            END DATE                                     
         BNE   PERADJ40            -E                                           
*                                                                               
PERADJ20 DS    0H                  NUMBER OF WEEKS                              
         CLC   BUDAY1IN,BUDAYXIN  TEST ROTATOR                                  
         BE    EXIT                NO - BUEND IS DAY IN LAST WEEK               
* SET END DATE TO LAST DAY OF ROTATOR                                           
         ZIC   R0,BUDAYXIN                                                      
         ZIC   RE,BUDAYX                                                        
PERADJ22 SR    R0,RE                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
PERADJ24 GOTO1 VADDAY,DMCB,BUEND,DUB,(R0)                                       
         MVC   BUEND,DUB                                                        
         GOTO1 VDATCON,DMCB,BUEND,(3,BUENDB)                                    
         GOTO1 (RF),(R1),BUEND,(2,BUENDP)                                       
         B     EXIT                                                             
         SPACE 2                                                                
* END DATE MUST BE FIRST OR  LAST DAY OF ROTATOR                                
*                                                                               
PERADJ30 DS    0H                  END DATE                                     
         MVI   ERRCD,ENDDTERR                                                   
         CLC   BUDAYXIN,BUDAYX     TEST END DAY = END DATE DAY                  
         BE    EXIT                                                             
*                                                                               
         CLC   BUDAYX,BUDAY1       TEST END DATE DAY = ST DATE DAY              
         BNE   BUYERR                                                           
* ADJUST END DATE TO LAST DAY OF ROTATOR (BUDAYXIN)                             
         ZIC   R0,BUDAYXIN                                                      
         ZIC   RE,BUDAYX                                                        
         B     PERADJ22                                                         
         SPACE 2                                                                
PERADJ40 ZIC   R0,BUDAYXIN         INPUT END DAY                                
         ZIC   RE,BUDAYX           END DATE DAY                                 
         SR    R0,RE                                                            
         BNP   PERADJ24                                                         
         LNR   R0,R0                                                            
         B     PERADJ24            GO MOVE END DATE BACK                        
         EJECT                                                                  
* EDIT ADJACENCY CODES                                                          
*                                                                               
ADJ      CLI   SVCPROF+9,C'0'                                                   
         BE    EXIT                                                             
         MVI   ERRCD,ADJERR                                                     
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
*                                                                               
ADJ1     CLI   SVCPROF+9,C'1'      TEST ALPHA REQ'D                             
         BE    ADJ2                YES                                          
         TM    FVAL,X'08'          TEST VALID NUM                               
         BZ    BUYERR                                                           
         PACK  BUADJ,0(1,R4)                                                    
         NI    BUADJ,X'F0'                                                      
         CLI   FLEN+1,2                                                         
         BH    BUYERR                                                           
         BL    EXIT                                                             
         MVC   BYTE,1(R4)                                                       
         NI    BYTE,X'0F'                                                       
         OC    BUADJ,BYTE                                                       
         B     EXIT                                                             
*                                                                               
* EDIT ALPHA ADJ CODES                                                          
*                                                                               
ADJ2     TM    FVAL,X'04'          TEST VALID ALPHA                             
         BZ    BUYERR                                                           
         CLI   FLEN+1,1                                                         
         BNE   BUYERR                                                           
         MVC   BUADJ,0(R4)                                                      
*                                                                               
         CLI   SVADJLST,0          TEST PROFILE IN STORAGE                      
         BNE   ADJ4                YES                                          
         SPACE 1                                                                
* READ AJ PROFILE FOR HIGH XFR EST NUMBER                                       
         SPACE 1                                                                
         MVI   ERRCD,NOADJPRF                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0AJ'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),BUYMD                                                  
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
         L     RF,VCOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,SVADJLST,VDATAMGR                                 
         CLI   SVADJLST,0                                                       
         BE    BUYERR                                                           
*                                                                               
ADJ4     MVI   ERRCD,ADJERR                                                     
         LA    R0,15                                                            
         LA    R1,SVADJLST+1                                                    
         CLI   SVADJLST,C'I'       TEST INCLUDE/EXCLUDE LIST                    
         BNE   ADJ10                                                            
*                                                                               
ADJ6     CLC   BUADJ,0(R1)         MATCH I FILTER IS GOOD                       
         BE    EXIT                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,ADJ6                                                          
         B     BUYERR                                                           
*                                                                               
ADJ10    CLC   BUADJ,0(R1)         MATCH X FILTER IS BAD                        
         BE    BUYERR                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,ADJ10                                                         
         B     EXIT                                                             
         EJECT                                                                  
* EDIT REFERENCE LINE NUMBER                                                    
*                                                                               
REF      XC    FSTOPS,FSTOPS                                                    
         TM    SVESTFL1,EF1NMG     TEST NEW MG                                  
         BZ    *+8                 NO                                           
         MVI   FSTOPS,C'/'         SET FOR OTHER STATION MG                     
         XC    BUREF,BUREF                                                      
         XC    BUREFMAS,BUREFMAS                                                
         MVI   ERRCD,REFERR                                                     
         MVI   BUREFTYP,X'12'                                                   
         CLC   =C'PKG',0(R4)                                                    
         BE    REF6                                                             
         MVI   BUREFTYP,X'16'                                                   
         CLC   =C'REV',0(R4)                                                    
         BE    REF6                                                             
         MVI   BUREFTYP,X'18'                                                   
         CLC   =C'MG',0(R4)                                                     
         BNE   BUYERR                                                           
         MVI   BUMGFLG,BUMGCLR                                                  
* FIND NON-NUMERIC DATE CHAR THAT FOLLOWS LINE NUM FOR MAKE-GOOD                
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8              TEST UNREF REQUEST                           
         B     *+10                                                             
         CLC   0(0,R4),=C'000' *EXECUTED*                                       
         BE    EXIT                YES - DONE                                   
         MVI   BUMGFLG,0                                                        
         LA    R5,1(R5)            RESTORE LEN                                  
*                                                                               
         MVC   BUMGLINE,SVLIN      SAVE MAKEGOOD LINE NUMBER                    
         CLI   0(R4),C'*'          ALLOW C,MG=*                                 
         BE    EXIT                                                             
         XC    BUMGDTA,BUMGDTA     CLEAR DATA                                   
         MVI   ERRCD,INVMGCD                                                    
         CLI   0(R4),C'-'          ALLOW C,MG=-4JAN01-02                        
         BNE   REF1                                                             
         MVI   BUMGFLG,BUMGNEG                                                  
         LA    R4,1(R4)            UPDATE FLDVAL PARAMS                         
         ST    R4,FADDR                                                         
         XC    FSTOPS,FSTOPS                                                    
         XC    FLEN,FLEN                                                        
         GOTO1 FLDVAL                                                           
         B     REF2                                                             
*                                                                               
REF1     CLI   0(R4),C'0'          TEST NUMERIC                                 
         BNL   REF2                IF >= 0, INPUT IS A LINE NUMBER              
*                                                                               
         MVC   BUMGCODE,0(R4)      MOVE CODE                                    
         CLI   QSTA,C'0'           TEST CABLE                                   
         BL    REF1A               NO                                           
         CLC   BUMGCODE,=C'AA'                                                  
         BL    BUYERR                                                           
         CLC   BUMGCODE,=C'Z9'                                                  
         BH    BUYERR                                                           
         B     REF40                                                            
*                                                                               
REF1A    CLI   BUMGCODE,C'A'       ALLOW C,MG=A0                                
         BL    BUYERR                                                           
         CLI   BUMGCODE,C'Z'                                                    
         BH    BUYERR                                                           
         CLI   BUMGCODE+1,C'0'                                                  
         BL    REF2                                                             
         CLI   BUMGCODE+1,C'9'                                                  
         BNH   REF1X                                                            
         CLI   BUMGCODE+1,C'A'                                                  
         BL    BUYERR                                                           
         CLI   BUMGCODE+1,C'Z'                                                  
         BH    BUYERR                                                           
*                                                                               
REF1X    B     REF11                                                            
*                                                                               
REF2     CLI   0(R4),C'0'                                                       
         BL    REF4                                                             
         CLI   0(R4),C'9'                                                       
         BH    REF4                                                             
         LA    R4,1(R4)                                                         
         BCT   R5,REF2                                                          
         B     BUYERR                                                           
*                                                                               
REF4     MVC   FSTOPS(1),0(R4)     SET TO STOP ON THAT CHAR                     
         XC    FLEN,FLEN           SET TO RE-EDIT                               
*                                                                               
REF6     GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         TM    FVAL,X'08'          NUMERIC                                      
         BZ    BUYERR                                                           
         CVB   R0,DUB                                                           
         CHI   R0,999                                                           
         BH    BUYERR                                                           
         STH   R0,BUREFMAS                                                      
         CLI   BUREFTYP,X'18'      TEST MG                                      
         BE    REF7                                                             
         CLC   BUREFMAS,SVLIN      TEST REF TO ITSELF                           
         BE    BUYERR                                                           
*                                                                               
REF7     CLI   BUREFTYP,X'18'      TEST MG SLAVE                                
         BNE   EXIT                NO-EXIT                                      
* EDIT MAKE-GOOD DATE                                                           
         MVI   ERRCD,MGDTERR                                                    
         AR    R4,R5               UPDATE FLDVAL PARAMS                         
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVC   FSTOPS(3),=C',-/'                                                
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         GOTO1 VDATVAL,DMCB,(1,(R4)),WORK                                       
         OC    0(4,R1),0(R1)                                                    
         BZ    BUYERR                                                           
REF9     MVC   WORK(2),SVSTART     DETERMINE MG YEAR                            
         CLC   SVSTART(2),SVEND                                                 
         BE    REF10                                                            
         CLC   WORK+2(4),SVSTART+2                                              
         BNL   *+10                                                             
         MVC   WORK(2),SVEND                                                    
*                                                                               
REF10    CLC   WORK(6),SVEND                                                    
         BH    BUYERR                                                           
         CLC   WORK(6),SVSTART                                                  
         BL    BUYERR                                                           
*                                                                               
         GOTO1 VDATCON,DMCB,WORK,(2,BUMGDATE)                                   
*                                                                               
         TM    SVESTFL1,EF1NMG     TEST NEW MAKEGOODS                           
         BO    *+8                 YES - DEFAULT TO SPOT NUMBER 0               
         MVI   BUMGSPOT,1          SET DEFAULT SPOT NUM                         
         CLI   FSTOP,C'-'                                                       
         BNE   REF11                                                            
* EDIT SPOT NUM                                                                 
         MVI   ERRCD,MGSPTNUM                                                   
         MVC   FSTOPS(2),=C',/'                                                 
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         TM    FVAL,X'08'                                                       
         BZ    BUYERR                                                           
         CLI   FLEN+1,2                                                         
         BH    BUYERR                                                           
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    BUYERR                                                           
         STC   R0,BUMGSPOT                                                      
*                                                                               
* SEE IF THERE IS A STATION                                                     
*                                                                               
REF11    CLI   FSTOP,C'/'                                                       
         BNE   REF20                                                            
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C','                                                      
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BZ    BUYERR                                                           
         MVI   ERRCD,INVERR                                                     
*                                                                               
         CLI   QSTA,C'0'           TEST CABLE                                   
         BL    REF12               NO                                           
         MVC   WORK+24(4),QSTA        CABLE INPUT IS NETWORK ONLY               
         MVI   WORK+28,C'/'                                                     
         MVC   WORK+29(4),0(R4)                                                 
         MVI   WORK+33,C' '                                                     
         LA    R4,WORK+24                                                       
*                                                                               
REF12    L     R6,AREC2            CALL STAVAL TO VALIDATE                      
         USING STABLKD,R6                                                       
         XC    0(STBLNQ,R6),0(R6)  CLEAR INTERFACE BLOCK                        
         MVC   STBMED,BUYMD        SET MEDIA                                    
         ST    R4,STBADDR          SET A(STATION STRING)                        
         OI    STBADDR,X'80'                                                    
         MVI   STBCTRY,C'U'                                                     
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+8                                                              
         MVI   STBCTRY,C'C'                                                     
         MVC   STBACOM,VCOMFACS                                                 
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A68'                                           
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(R6)                                                   
*                                                                               
         CLI   STBERR,0                                                         
         BNE   BUYERR                                                           
         MVC   WORK(L'STBSTA),STBSTA  SET OUTPUT STATION                        
         CLI   WORK+4,C' '                                                      
         BH    *+10                                                             
         MVC   WORK+4(1),BUYMD                                                  
         MVC   WORK+5(L'STBNET),STBNET    AND NETWORK                           
         GOTO1 STAPACK,DMCB,(C'P',=C'0000'),WORK,WORK+20                        
         MVC   BUMGSTA,WORK+22     SAVE STATION                                 
         BAS   RE,GETMKT           AND MARKET NUMBER                            
*                                                                               
* FIND REASON CODE FOR MG'S                                                     
*                                                                               
REF20    XC    BUMGCOM,BUMGCOM     CLEAR COMMENT                                
         CLI   FSTOP,C','                                                       
         BNE   REF40                                                            
         MVI   ERRCD,BADREAS       BAD REASON CODE                              
         GOTO1 FLDVAL                                                           
         CLI   FLEN+1,6                                                         
         BH    BUYERR                                                           
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING RSNRECD,R1                                                       
         MVC   RSNKTYPE,=X'0D77'     REASON CODE                                
         MVC   RSNKAGY,AGYALPHA                                                 
         MVC   RSNKMED,BUYMD                                                    
         MVC   RSNKCODE,SPACES                                                  
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   RSNKCODE(0),0(R4)                                                
         MVC   RSNKOFF(1),SVOFFC      TRY TO FIND REC WITH OFFICE               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSNKEY),KEYSAVE                                            
         BE    REF30                                                            
         LA    R1,KEY                                                           
         MVC   KEY,KEYSAVE                                                      
         XC    RSNKOFF,RSNKOFF                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSNKEY),KEYSAVE    ELSE LOOK FOR GLOBAL RECORD             
         BNE   BUYERR                                                           
         DROP  R1                                                               
*                                                                               
REF30    L     R0,AREC                                                          
         L     R5,AREC4                                                         
         ST    R5,AREC                                                          
         GOTO1 GETREC                                                           
         ST    R0,AREC                                                          
         USING RSNRECD,R5                                                       
         MVC   BUMGRSCD,RSNKCODE                                                
         MVC   BUMGRSOF,RSNKOFF                                                 
         LA    R5,RSNELEM                                                       
         MVC   BUMGCOM,2(R5)       SET REASON CODE COMMENT                      
         MVC   KEY,SVKEY                                                        
         DROP  R5                                                               
*                                                                               
REF40    XC    FSTOPS,FSTOPS                                                    
         GOTO1 FLDVAL                                                           
         CLI   FSTOP,X'FF'                                                      
         BNE   BUYERR              END OF DATA                                  
*                                                                               
REFX     B     EXIT                                                             
         EJECT                                                                  
GETMKT   NTR1                                                                   
         MVI   ERRCD,STAERR                                                     
         L     R4,AREC4                                                         
         ST    R4,AREC                                                          
         XC    0(200,R4),0(R4)                                                  
         MVC   WORK2+24(20),KEY    SAVE KEY                                     
*                                                                               
GM10     MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),BUYMD                                                   
         MVC   KEY+2(5),WORK       STATION                                      
         MVC   KEY+7(2),AGYALPHA                                                
         USING STARECD,R4                                                       
         GOTO1 RDSTA                                                            
*                                                                               
GM20     PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,BUMGMKT                                                     
         MVC   KEY(20),WORK2+24   RESTORE KEY                                   
         MVC   AREC,AREC1                                                       
         B     EXIT                                                             
         EJECT                                                                  
*=================================================================*             
* DAILY SKED DATA. SCHEDULE IS RETURNED IN WEEKLY SLOT IN BUDSKED *             
* WHEN USER IS ENTERING DATA ON INPUT LINES, DS= OR DSNN= IS      *             
*  PRESENT (CONSECUTIVE WEEKS FROM 1 OR NN REQUIRED).             *             
* WHEN DATA IS IN DISPLAY AREA, BUWKS CONTAINS THE WEEK NUMBER.   *             
*=================================================================*             
         SPACE 1                                                                
* FIRST, CALCULATE NUMBER OF DIGITS REQUIRED FOR EACH WEEK                      
* = NUMBER OF DAYS IN ROTATION                                                  
*                                                                               
DSK      BAS   RE,FIXWKS           FIX BDWKS FOR NWS SCREWUPS                   
         XC    FLEN,FLEN           SET TO RE-EDIT                               
         MVI   ERRCD,SKEDERR1      BUY NOT A ROTATOR                            
         SR    R0,R0                                                            
         IC    R0,BUDAYNUM                                                      
         CLI   BUTRCODE,C'B'       TEST NEW BUY                                 
         BE    *+8                                                              
         IC    R0,BDSEDAY                                                       
         SRDL  R0,4                                                             
         SRL   R1,28                                                            
         SR    R1,R0                                                            
         BZ    BUYERR                                                           
         BP    *+8                                                              
         LA    R1,7(R1)                                                         
         LA    R1,1(R1)            START - END + 1 GIVES NUM DAYS               
         STH   R1,HALF                                                          
*                                                                               
DSK2     MVI   ERRCD,SKEDERR2                                                   
         L     R4,FADDR            CHECK FOR 'SD' INPUT                         
         CLC   =C'SD',0(R4)                                                     
         BNE   DSK2B                                                            
         LA    R4,2(R4)            BUMP INPUT POINTER                           
         ST    R4,FADDR                                                         
         XC    FLEN,FLEN           AND CLEAR LENGTH                             
* NOW EDIT FOR WEEK NUMBER                                                      
         XC    FSTOPS,FSTOPS                                                    
         MVI   FSTOPS,C'='                                                      
         GOTO1 FLDVAL                                                           
         LTR   R5,R5                                                            
         BNZ   DSK2A                                                            
         LA    R4,1(R4)            BUMP PAST = SIGN                             
         ST    R4,FADDR                                                         
         MVI   BUWKS,1             SET STARTING WEEK NUMBER                     
         B     DSK2B                                                            
*                                                                               
DSK2A    TM    FVAL,X'08'          TEST VALID NUMERIC                           
         BZ    BUYERR                                                           
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    BUYERR                                                           
         CH    R0,=H'14'                                                        
         BH    BUYERR                                                           
         STC   R0,BUWKS                                                         
*                                                                               
DSK2B    MVI   ERRCD,NOSKEDWK                                                   
         ZIC   R0,BUWKS                                                         
         CLM   R0,1,BDWKS          TEST WEEK IN BUY RECORD                      
         BH    BUYERR                                                           
         BCTR  R0,0                                                             
         MH    R0,=H'7'                                                         
         LA    R6,BUDSKED                                                       
         AR    R6,R0               POINT TO CORRECT WEEK                        
*                                                                               
DSK2X    MVI   FSTOPS,C','                                                      
         MVI   FSTOPS+1,C'/'                                                    
         GOTO1 FLDVAL                                                           
         LTR   R5,R5               TEST NO INPUT                                
         BNZ   DSK4                                                             
         LA    R4,1(R4)            SKIP OVER '/'                                
         ST    R4,FADDR                                                         
         B     DSK6                                                             
*                                                                               
DSK4     MVI   ERRCD,SKEDERR2                                                   
         TM    FVAL,X'08'          TEST NUMERIC                                 
         BZ    BUYERR                                                           
         CH    R5,HALF             TEST MORE DIGITS THAN NEEDED                 
         BH    BUYERR                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(R4)                                                    
*                                                                               
DSK6     CLI   FSTOP,C'/'                                                       
         BNE   DSKX                                                             
*                                                                               
         MVI   ERRCD,SKEDERR3                                                   
         LA    R6,7(R6)            POINT TO NEXT WEEK                           
         LA    R0,BUDSKED+L'BUDSKED-1                                           
         CR    R6,R0                                                            
         BH    BUYERR                                                           
         B     DSK2X                                                            
*                                                                               
DSKX     B     EXIT                                                             
*                                                                               
FIXWKS   NTR1                                                                   
         GOTO1 VDATCON,DMCB,(3,BDSTART),WORK                                    
         GOTO1 (RF),(R1),(3,BDEND),WORK+12  SAVE YYMMDD END DATE                
*                                                                               
         SR    R0,R0               CLEAR COUNTER                                
FIXWK2   GOTO1 VADDAY,DMCB,WORK,WORK+6,F'7'                                     
         MVC   WORK(6),WORK+6                                                   
         CLC   WORK(6),WORK+12     TEST PAST END DATE                           
         BH    FIXWK4                                                           
         BCT   R0,FIXWK2                                                        
*                                                                               
FIXWK4   BCTR  R0,0                ADJUST NUMBER OF WEEKS BY 1                  
         LPR   R0,R0               MAKE COUNTER POSITIVE                        
         STC   R0,BDWKS                                                         
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
*                                                                               
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
STARECD  DSECT                                                                  
*SPGENSTA                                                                       
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
*SPGENEST                                                                       
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
* SPGENNDEF                                                                     
       ++INCLUDE SPGENNPGM                                                      
         EJECT                                                                  
* SPGENPROG                                                                     
       ++INCLUDE SPGENPROG                                                      
* SPGENDOV                                                                      
       ++INCLUDE SPGENDOV                                                       
         EJECT                                                                  
*SPGENREAS                                                                      
       ++INCLUDE SPGENREAS                                                      
         EJECT                                                                  
       ++INCLUDE SPSTABLK                                                       
         EJECT                                                                  
*DEDBLOCK                                                                       
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT OFF                                                              
       ++INCLUDE SPBUYWORK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056SPBUY11   08/08/14'                                      
         END                                                                    
