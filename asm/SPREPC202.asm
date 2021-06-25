*          DATA SET SPREPC202  AT LEVEL 038 AS OF 12/04/97                      
*PHASE SPC202A                                                                  
         TITLE 'SPC202 - CANADIAN NETWORK BUY TRANSFER'                         
SPC202   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPC202                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPC202,R8,RR=R2                                                
         ST    R2,RELO                                                          
                                                                                
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
                                                                                
         CLI   MODE,REQFRST                                                     
         BE    RQFRST                                                           
         CLI   MODE,STAFRST                                                     
         BE    SFRST                                                            
         CLI   MODE,ESTFRST                                                     
         BE    EFRST                                                            
         CLI   MODE,PROCBUY                                                     
         BE    PRBUY                                                            
         CLI   MODE,REQLAST                                                     
         BE    RQLAST                                                           
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
RELO     DC    A(0)                                                             
*                                                                               
RQFRST   DS    0H                                                               
         MVI   STATUS,0                                                         
         CLC   QPRD,=C'POL'        ONLY PRODUCT POL                             
         BE    RQFRST05                                                         
         MVC   P(L'NTPRDPOL),NTPRDPOL                                           
         B     ERRORX                                                           
*                                                                               
RQFRST05 DS    0H                                                               
         CLC   QEST,=C'ALL'        ESTIMATE CANNOT BE "ALL"                     
         BNE   RQFRST07                                                         
         MVC   P(L'ESTALL),ESTALL                                               
         B     ERRORX                                                           
*                                                                               
RQFRST07 DS    0H                                                               
         THMS  DDSTIME=YES                                                      
         STCM  R1,15,WORK1                                                      
         STCM  R0,15,WORK2                                                      
         AP    WORK1,WORK2            ADD DDS TIME OFFSET                       
         CP    WORK1,=P'240000'       PAST MIDNIGHT?                            
         BL    RQFRST10                                                         
         SP    WORK1,=P'240000'       YES, ADJUST TO NEXT DAY                   
*                                                                               
RQFRST10 DS    0H                                                               
         SRP   WORK1,1,0              SHIFT LEFT 1 DIGIT                        
         ZAP   DUB,=P'0'                                                        
         MVO   DUB,WORK1(1)           HOUR                                      
         CVB   R1,DUB                                                           
         STC   R1,SVTIME                                                        
         MVO   DUB,WORK1+1(1)         MINUTE                                    
         CVB   R1,DUB                                                           
         STC   R1,SVTIME+1                                                      
         MVO   DUB,WORK1+2(1)         SECOND                                    
         CVB   R1,DUB                                                           
         STC   R1,SVTIME+2                                                      
         B     EXIT                                                             
*                                                                               
EFRST    DS    0H                                                               
         GOTO1 CLPACK,DMCB,Q2USER,BCLT2                                         
         PACK  DUB,Q2USER+3(3)                                                  
         CVB   R1,DUB                                                           
         STC   R1,BEST2                                                         
         LA    R6,KEY                                                           
         USING ESTHDRD,R6                                                       
         XC    KEY,KEY                                                          
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT2                                                    
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,BEST2                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ESTNTFND                                                         
*                                                                               
* COPY 'TO' ESTIMATE DATES MUST BE WITHIN COPY 'FROM' ESTIMATE DATES            
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         L     R5,ADEST                                                         
*        CLC   ESTART,ESTART-ESTHDR(R5)                                         
*        BL    ESTNTEQL            TO EST STARTS BEFORE FROM EST                
*        CLC   EEND,EEND-ESTHDR(R5)                                             
*        BH    ESTNTEQL            TO EST ENDS AFTER FORM EST                   
*                                                                               
         GOTO1 DATCON,DMCB,(0,ESTART),(2,TOESTSTD)                              
         GOTO1 DATCON,DMCB,(0,EEND),(2,TOESTEND)                                
         CLC   ESTART(12),ESTART-ESTHDR(R5)                                     
         BNE   EXIT                                                             
         OI    STATUS,SAMEST       ESTIMATE DATES ARE THE SAME                  
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
SFRST    DS    0H                                                               
         MVC   NETBKEY,KEY                                                      
         BAS   RE,NETDFN           GET NETWORK DEFINITION RECORD                
         XC    KEY,KEY                                                          
         MVC   KEY(13),NETBKEY                                                  
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                          PRBUY                                      *         
*  BEFORE:  ADBUY = ADDRESS OF NETWORK BUY TO COPY                    *         
*           BCLT2 = BINARY CODE FOR TO CLIENT                         *         
*  AFTER:   NETWORK AND EXPLODED BUYS ARE COPIED                      *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PRBUY    DS    0H                                                               
         MVC   NETBKEY,KEY                                                      
         MVI   BUYTYP,C'N'                                                      
         MVI   BUYLINE,1                                                        
         BAS   RE,CPYBUY           COPY THE BUY TO THE "TO" CLIENT              
                                                                                
* MOVE NETWORK BUY TO IOAREA SO WE CAN USE ADBUY                                
         MVI   BUYTYP,C'X'                                                      
         LA    RE,IOAREA           CLEAR IOAREA                                 
         LH    RF,=H'2000'                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R0,IOAREA           SET 'TO' ADDRESS                             
         L     RE,ADBUY            SET 'FROM' ADDR                              
         SR    R1,R1                                                            
         ICM   R1,3,13(RE)         SET 'TO' LEN                                 
         LR    RF,R1               SET 'FROM' LENGTH                            
         MVCL  R0,RE                                                            
                                                                                
         USING NTWKELEM,R6                                                      
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'68'                                                     
         BAS   RE,GETEL                                                         
         BNE   PRBUY30                                                          
                                                                                
PRBUY10  XC    KEY,KEY             GET EXPLODED RECORDS                         
         MVC   KEY(13),NETBKEY                                                  
         MVC   KEY+4(5),NTWKMKST   MKT/STA                                      
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PRBUY20                                                          
         L     R5,ADBUY                                                         
         ST    R5,AREC                                                          
         GOTO1 GETBUY                                                           
         BAS   RE,CPYBUY           COPY EXPLODED RECORDS                        
                                                                                
PRBUY20  BAS   RE,NEXTEL                                                        
         BE    PRBUY10                                                          
                                                                                
PRBUY30  XC    KEY,KEY                                                          
         MVC   KEY(13),NETBKEY                                                  
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         DROP  R6                                                               
                                                                                
RQLAST   DS    0H                                                               
         MVC   P(20),=C'NUMBER OF BUYS ADDED'                                   
         EDIT  COUNT,(10,P+25),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                          CPYBUY                                     *         
*  COPY BUY FROM ONE CLIENT TO ANOTHER                                *         
*  BEFORE:  ADBUY = ADDRESS OF BUY TO COPY                            *         
*  AFTER:   CHANGE "TO" CLIENT                                        *         
*           CLEAR BDMASPRD                                            *         
*           CLEAR BDCOST- EXPLODED BUYS ONLY                          *         
*           CHANGE 0B0E -> 0B0A                                       *         
*           UPDATE X'68' ELEMENTS- NETWORK BUYS ONLY                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
CPYBUY   NTR1                                                                   
         USING BUYRECD,R6                                                       
         LA    R6,KEY                                                           
         XC    KEY+13(L'KEY-13),KEY+13                                          
         MVC   KEY+11(1),BUYLINE                                                
         MVC   BUYKCLT,BCLT2       CHANGE TO THE "TO" CLIENT                    
         MVC   BUYKEST,BEST2       CHANGE TO THE "TO" ESTIMATE                  
         GOTO1 HIGH                                                             
         CLI   BUYTYP,C'N'                                                      
         BE    CPYBY10                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CPYBY20                                                          
         DC    H'0'                                                             
                                                                                
CPYBY10  CLC   KEY(13),KEYSAVE                                                  
         BNE   CPYBY20                                                          
         ZIC   R1,BUYLINE                                                       
         LA    R1,1(R1)                                                         
         STC   R1,BUYLINE                                                       
         STC   R1,KEY+11                                                        
         GOTO1 HIGH                                                             
         B     CPYBY10                                                          
                                                                                
CPYBY20  MVC   KEY,KEYSAVE                                                      
*                                                                               
*        CLI   QOPT5,C'Y'          DEBUG INFO                                   
*        BNE   CPYBY21                                                          
*        MVC   P(10),=C'BEFORE BUY'                                             
*        GOTO1 REPORT                                                           
*        BAS   RE,PRTBUY                                                        
*                                                                               
CPYBY21  L     R6,ADBUY            ADDRESS OF RECORD TO COPY                    
         USING BUYRECD,R6                                                       
         MVC   BUYKEY+10(1),BUYLINE                                             
         MVC   BUYKCLT,BCLT2       CHANGE TO THE "TO" CLIENT                    
         MVC   BUYKEST,BEST2       CHANGE TO THE "TO" ESTIMATE                  
         XC    BDMASPRD,BDMASPRD   CLEAR PRODUCTS                               
         MVC   BDCHG,TODAYB        LAST CHANGE DATE                             
         MVI   BDWHY,X'80'         NEW BUY                                      
         XC    BDCOST,BDCOST       CLEAR COST                                   
                                                                                
         NI    STATUS,X'FF'-CLRWKS                                              
         NI    STATUS,X'FF'-ELEM0B                                              
         XC    BSTART,BSTART                                                    
         TM    STATUS,SAMEST       ESTIMATE DATES ARE THE SAME                  
         BO    CPYBY30             THEN SKIP                                    
         LA    R6,BDELEM           DELETE ELEMS NOT IN EST                      
CPYBY24  CLI   0(R6),0                                                          
         BE    CPYBY28                                                          
         CLI   0(R6),X'0C'                                                      
         BE    CPYBY26             DELETE X'0C' ELEMS                           
         CLI   0(R6),X'10'                                                      
         BE    CPYBY26             DELETE X'10' ELEMS                           
         CLI   0(R6),X'0B'                                                      
         BNE   CPYBY25                                                          
         CLC   TOESTSTD,2(R6)                                                   
         BH    CPYBY26             DELETE-EST START AFTER ELEM DATE             
         CLC   TOESTEND,2(R6)                                                   
         BL    CPYBY26             DELETE-EST END BEFORE ELEM DATE              
* KEEPING 0B ELEM SO FIX IT                                                     
         NI    6(R6),X'FF'-X'40'   TURN OFF ANY MINUS                           
         NI    6(R6),X'FF'-X'10'   TURN OFF ANY MAKEGOOD PENDING                
         XC    4(2,R6),4(R6)       CLEAR PAY DATE                               
         OC    BSTART,BSTART                                                    
         BNZ   *+10                                                             
         MVC   BSTART,2(R6)        SAVE BUY START DATE                          
         MVC   BEND,2(R6)          SAVE BUY END DATE                            
         OI    STATUS,ELEM0B       SET REC HAS 0B ELEM                          
CPYBY25  ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     CPYBY24                                                          
CPYBY26  CLI   0(R6),X'0B'         IF ANY 0B DELETES NEED TO CLEAR WKS          
         BNE   *+8                                                              
         OI    STATUS,CLRWKS                                                    
         GOTO1 RECUP,DMCB,(C'S',ADBUY),(R6),0     DELETE  ELEMENT               
         B     CPYBY24                                                          
*                                                                               
CPYBY28  TM    STATUS,CLRWKS       DO WE NEED TO CLEAR WKS                      
         BNO   CPYBY30                                                          
         L     R6,ADBUY                                                         
         XC    BDWKS,BDWKS                            CLEAR WKS                 
         MVI   BDINPUT,2           END DATE                                     
         GOTO1 DATCON,DMCB,(2,BSTART),(3,BDSTART)   AND ADD DATES               
         GOTO1 DATCON,DMCB,(2,BEND),(3,BDEND)                                   
*                                                                               
CPYBY30  L     R6,ADBUY                                                         
         LA    R6,BDELEM           CHANGE 0B0E -> 0B0A                          
CPYBY32  CLI   0(R6),0                                                          
         BE    CPYBY50                                                          
         CLI   0(R6),X'0B'                                                      
         BNE   CPYBY40                                                          
         CLI   1(R6),X'0A'                                                      
         BE    CPYBY40                                                          
         XC    ELEM,ELEM                                                        
         MVC   ELEM(10),0(R6)                                                   
         MVI   ELEM+1,X'0A'                                                     
         GOTO1 RECUP,DMCB,(C'S',ADBUY),(R6),0     DELETE  ELEMENT               
         GOTO1 RECUP,DMCB,(C'S',ADBUY),ELEM,(R6)  RE-ADD W/ LEN X'0A'           
         OI    STATUS,ELEM0B                                                    
CPYBY40  ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     CPYBY32                                                          
                                                                                
CPYBY50  DS    0H                                                               
         CLI   BUYTYP,C'N'                                                      
         BNE   CPYBY100                                                         
                                                                                
* COMPARE X'68' ELEMENTS WITH PSTATAB                                           
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         LA    R6,BDELEM                                                        
         USING NTWKELEM,R6                                                      
CPYBY60  CLI   0(R6),0                                                          
         BE    CPYBY100                                                         
         CLI   0(R6),X'68'                                                      
         BNE   CPYBY80                                                          
         LA    R5,PSTATAB                                                       
CPYBY70  CLI   0(R5),X'FF'                                                      
         BE    CPYBY90             DELETE THIS  X'68' ELEMENT                   
         CLC   NTWKMKST+2(2),0(R5)                                              
         BE    CPYBY80             KEEP THIS ELEMENT                            
         LA    R5,2(R5)                                                         
         B     CPYBY70                                                          
CPYBY80  ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     CPYBY60                                                          
CPYBY90  GOTO1 RECUP,DMCB,(C'S',ADBUY),(R6),0      DELETE  ELEMENT              
         B     CPYBY60                                                          
         DROP  R6                                                               
                                                                                
CPYBY100 DS    0H                                                               
         XC    ELEM,ELEM           ADD X'98' ELEMENT                            
         LA    R6,ELEM                                                          
         USING BTRCELEM,R6                                                      
         MVI   BTRCCODE,BTRCCODQ                                                
         MVI   BTRCLEN,BTRCLENQ                                                 
         MVC   BTRCDATE,TODAYB                                                  
         MVC   BTRCTIME,SVTIME                                                  
         MVC   BTRCLUID(4),=C'SPC2'                                             
         DROP  R6                                                               
                                                                                
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         LA    R6,24(R6)           FIND END OF RECORD OR 99 ELEMENT             
CPYBY110 CLI   0(R6),0                                                          
         BE    CPYBY120                                                         
         CLI   0(R6),X'99'                                                      
         BE    CPYBY115                                                         
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     CPYBY110                                                         
CPYBY115 GOTO1 RECUP,DMCB,(C'S',ADBUY),(R6),0      DELETE 99 ELEMENT            
CPYBY120 GOTO1 RECUP,DMCB,(C'S',ADBUY),ELEM,(R6)   ADD 98 ELEMENT               
*                                                                               
         TM    STATUS,ELEM0B       ANY 0B ELEMS IN REC                          
         BNO   CPYBYX                                                           
         L     R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
*                                                                               
         CLI   QOPT5,C'Y'          DEBUG INFO                                   
         BNE   CPYBY122                                                         
         MVC   P(10),=C'AFTER  BUY'                                             
         GOTO1 REPORT                                                           
*                                                                               
CPYBY122 BAS   RE,PRTBUY                                                        
         CLI   RCWRITE,C'Y'                                                     
         BNE   CPYBYX                                                           
         GOTO1 DATAMGR,DMCB,ADDREC,=C'SPTFILE',KEY+14,AREC                      
CPYBYX   B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                          NETDFN                                     *         
*  GET ESTIMATE OR CLIENT SPECIFIC NETWORD RECORD                     *         
*  AFTER:   PSTAB TABLE OF PACKED STATIONS FROM X'01' ELEMENTS        *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
NETDFN   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING NDEFRECD,R6                                                      
         MVC   NDEFKTYP,=X'0D11'                                                
         MVC   NDEFKAGY,QAGY                                                    
         MVC   NDEFKNET,STA                                                     
         MVC   NDEFKCLT,BCLT2                                                   
         MVC   NDEFKEST,BEST2                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    NETDFN10                                                         
                                                                                
         MVC   KEY,KEYSAVE                                                      
         MVI   NDEFKEST,0                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   NONDEFRC                                                         
                                                                                
NETDFN10 DS    0H                                                               
         LA    R6,IOAREA                                                        
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         MVI   ELCODE,1                                                         
         USING NDEFEL01,R6                                                      
*                                                                               
         LA    R5,PSTATAB                                                       
         XC    PSTATAB,PSTATAB                                                  
         BAS   RE,GETEL                                                         
         BNE   NOSTATNS                                                         
*                                                                               
NETDFN20 DS    0H                                                               
         CLC   NDEFSTA,=C'ZZZZ'    SKIP THIS STATION                            
         BE    NETDFN30                                                         
*                                                                               
         CLC   NDEFPCT,=X'FFFFFFFF'                                             
         BE    NETDFN30                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                TABLE TOO SMALL                              
*                                                                               
         MVC   STATN(4),NDEFSTA                                                 
         MVI   STATN+4,C' '                                                     
         GOTO1 MSPACK,DMCB,MARKT,STATN,PMKTST                                   
*                                                                               
         MVC   0(2,R5),PMKTST+2    SAVE 1ST 2 BYTES OF PACKED STATION           
         LA    R5,2(R5)                                                         
NETDFN30 BAS   RE,NEXTEL                                                        
         BE    NETDFN20                                                         
         MVI   0(R5),X'FF'                                                      
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                          PRINT BUY                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PRTBUY   NTR1                                                                   
         L     R5,ADBUY                                                         
         USING BUYRECD,R5                                                       
         LA    R4,P                                                             
         USING PLINED,R4                                                        
*                                                                               
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
         MVC   PPRD,=C'POL'                                                     
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
*                                                                               
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BE    PRTB3                                                            
         CLI   QOPT5,C'Y'          DEBUG INFO                                   
         BNE   PRTB3                                                            
         GOTO1 HEXOUT,DMCB,KEY,PKEY,18                                          
         GOTO1 REPORT                                                           
         BAS   RE,PRTELEMS         PRINT OUT 0B ELEMS                           
         B     EXIT                                                             
PRTB3    GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*        PRINT 0B ELEMS                                               *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRTELEMS NTR1                                                                   
         L     R6,ADBUY            ADDRESS OF RECORD                            
         LA    R6,BDELEM                                                        
PE10     CLI   0(R6),0                                                          
         BE    PEX                                                              
         CLI   0(R6),X'01'                                                      
         BE    PE15                                                             
         CLI   0(R6),X'0B'                                                      
         BE    PE15                                                             
         CLI   0(R6),X'0C'                                                      
         BNE   PE20                                                             
PE15     ZIC   R0,1(R6)                                                         
         GOTO1 HEXOUT,DMCB,(R6),P+5,(R0)                                        
         CLI   0(R6),X'0B'                                                      
         BNE   PE18                                                             
         GOTO1 DATCON,DMCB,(2,2(R6)),(11,P+40)                                  
PE18     GOTO1 REPORT                                                           
PE20     ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     PE10                                                             
PEX      B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                              ERRORS                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
ESTNTFND DS    0H                                                               
         LA    R4,P                                                             
         MVC   PCLT,Q2USER                                                      
         MVC   PPRD,=C'POL'                                                     
         MVC   PEST(3),Q2USER+3                                                 
         MVC   PKEY(L'ESNTFND),ESNTFND                                          
         B     ERRORX                                                           
*                                                                               
ESTNTEQL LA    R4,P                                                             
         MVC   PCLT,Q2USER                                                      
         MVC   PPRD,=C'POL'                                                     
         MVC   PEST(3),Q2USER+3                                                 
         MVC   PKEY(L'ESNTEQL),ESNTEQL                                          
         B     ERRORX                                                           
*                                                                               
NONDEFRC LA    R4,P                                                             
         MVC   PSTA,STA                                                         
         MVC   PCLT,Q2USER                                                      
         MVC   PEST(3),Q2USER+3                                                 
         MVC   PKEY(L'NTDNTFND),NTDNTFND                                        
         B     ERRORX                                                           
*                                                                               
NOSTATNS LA    R4,P                                                             
         MVC   PSTA,STA                                                         
         MVC   PCLT,Q2USER                                                      
         MVC   PEST(3),Q2USER+3                                                 
         MVC   PKEY(L'NOSTAS),NOSTAS                                            
         B     ERRORX                                                           
*                                                                               
ERRORX   GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
         DROP  R4                                                               
         GETEL R6,24,ELCODE                                                     
         LTORG                                                                  
         EJECT                                                                  
MARKT    DC    C'0000'                                                          
ESNTFND  DC    C'ESTIMATE RECORD NOT FOUND'                                     
ESNTEQL  DC    C'TO ESTIMATE DATES MUST BE WITHIN FROM ESTIMATE DATES'          
NTDNTFND DC    C'NETWORK DEF RECORD DOES NOT EXIST'                             
NOSTAS   DC    C'NO STATIONS IN NETWORK DEFINITION RECORD'                      
NTPRDPOL DC    C'PRODUCT MUST BE POL'                                           
ESTALL   DC    C'ESTIMATE CANNOT BE ALL'                                        
*                                                                               
COUNT    DS    F                                                                
ELCODE   DS    X                                                                
STATUS   DS    X                                                                
SAMEST   EQU   X'80'               ESTIMATE DATES ARE THE SAME                  
CLRWKS   EQU   X'40'               CLEAR WKS FROM BUY AND ADD DATES             
ELEM0B   EQU   X'20'               0B ELEMS EXIST ON REC                        
BUYTYP   DS    X                   N=NETWORK, X=EXPLODED                        
BUYLINE  DS    X                   BUYLINE NUMBER                               
BEST2    DS    X                                                                
TOESTSTD DS    XL2                 TO ESTIMATE START DATE                       
TOESTEND DS    XL2                 TO ESTIMATE END DATE                         
BSTART   DS    XL2                                                              
BEND     DS    XL2                                                              
BCLT2    DS    XL2                                                              
PMKTST   DS    XL5                                                              
STATN    DS    CL5                                                              
SVTIME   DS    XL3                                                              
WORK1    DS    XL4                                                              
WORK2    DS    XL4                                                              
NETBKEY  DS    XL13                SAVE NETWORK BUY KEY                         
STAWORK  DS    XL31                STAPACK DSECT                                
ELEM     DS    XL256                                                            
PSTATAB  DS    XL200                                                            
         DC    X'FF'                                                            
IOAREA   DS    4000C                                                            
*                                                                               
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PCLT     DS    CL3                                                              
         DS    CL5                                                              
PPRD     DS    CL3                                                              
         DS    CL5                                                              
PEST     DS    CL3                                                              
         DS    CL5                                                              
PMKT     DS    CL4                                                              
         DS    CL5                                                              
PSTA     DS    CL4                                                              
         DS    CL5                                                              
PLIN     DS    CL3                                                              
         DS    CL5                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENNDEF                                                      
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038SPREPC202 12/04/97'                                      
         END                                                                    
