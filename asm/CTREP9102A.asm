*          DATA SET CTREP9102A AT LEVEL 176 AS OF 05/01/02                      
*PHASE CT9102A,+0                                                               
*INCLUDE SORTER                                                                 
         TITLE 'DEMO SYSTEM CONTROL FILE REPORT'                                
CT9102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TEMPX-TEMPD,**CT9102                                             
         USING TEMPD,RC                                                         
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING CT9102+4096,R9                                                   
         CLI   MODE,REQFRST                                                     
         BL    EXIT                                                             
         XC    KEY,KEY                                                          
         LA    RE,RECTAB           FIND ROUTINE FOR THIS REQUEST                
RQF1     CLI   0(RE),0             DID NOT FIND RECORD TYPE                     
         BE    EXIT                                                             
         CLC   0(1,RE),QOPT1       TRY FOR MATCH ON RECORD TYPE                 
         BE    *+12                                                             
         LA    RE,2(RE)                                                         
         B     RQF1                                                             
         MVC   RCSUBPRG,1(RE)      SET HEADLINE CONTROL                         
         ZIC   R1,1(RE)            GET ROUTINE NUMBER                           
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         SPACE 2                                                                
         B     EXIT                LATEST BOOK                                  
         B     STARPT              STATION CALL LETTER EQUATES                  
         B     DNAMES              DEMO NAMES                                   
         B     EXIT                DEMO MODIFIERS                               
         B     ROPRPT              DEMO LOOKUP CONTROLS                         
         B     SVIRPT              SVI ADJUSTMENT CONTROLS                      
         EJECT                                                                  
         TITLE 'DEMO NAMES REPORT'                                              
***********************************************************************         
*DNAMES - PRINT DEMO NAMES FOR ALL FILES,AGY,MED,...                            
***********************************************************************         
DNAMES   CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         L     RF,ACOMFACS                                                      
*        MVC   SWITCH,CSWITCH-COMFACSD(RF)                                      
         LA    RE,GLOBAL                                                        
         L     RF,=F'800'                                                       
         XCEF                                                                   
         LA    R4,KEY                                                           
         USING CTDREC,R4           BUILD DEMO NAMES RECD KEY                    
         MVI   CTDKTYP,CTDKTEQU                                                 
         BAS   RE,HIGH                                                          
         MVC   PKEY,KEY                                                         
         B     *+8                                                              
*                                                                               
DNAME5   BAS   RE,SEQ              READ ALL DEMO NAMES RECDS                    
         LA    R4,KEY                                                           
         CLI   CTDKTYP,CTDKTEQU                                                 
         BNE   EXIT                NO MORE, EXIT                                
         CLI   CTDKTYP+1,0                                                      
         BNE   EXIT                NO MORE, EXIT                                
         CLC   PKEY(CTDKDEMO-CTDKEY),KEY   IF A KEY FLD CHGS--NEW PAGE          
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   PKEY,KEY                                                         
*                                                                               
         MVC   PRTFIL,SPACES       GET FILE TYPE EXPANSION                      
         LA    R1,FILES                                                         
         LA    RE,FILTAB                                                        
*                                                                               
DNAME7   CLC   CTDKFILE,0(RE)                                                   
         BNE   *+14                                                             
         MVC   PRTFIL,4(RE)                                                     
         B     DNAME9                                                           
*                                                                               
         CLI   CTDKFILE,X'FF'                                                   
         BNE   *+14                                                             
         MVC   PRTFIL(7),=C'DEFAULT'                                            
         B     DNAME9                                                           
*                                                                               
         LA    RE,L'FILTAB(RE)                                                  
         BCT   R1,DNAME7                                                        
*                                                                               
DNAME9   MVC   PRTMED,SPACES        GET MEDIA EXPANSION                         
         LA    R1,MEDIAS                                                        
         LA    RE,MEDTAB                                                        
DNAME10  CLC   CTDKMED,0(RE)                                                    
         BE    DNAME12                                                          
         LA    RE,L'MEDTAB(RE)                                                  
         BCT   R1,DNAME10                                                       
         CLI   CTDKMED,X'FF'                                                    
         BNE   *+10                                                             
         MVC   PRTMED(7),=C'DEFAULT'                                            
         B     DNAME15                                                          
DNAME12  MVC   PRTMED,1(RE)                                                     
*                                                                               
DNAME15  MVC   PRTAGY,SPACES         GET AGENCY EXPANSION                       
         MVC   PRTAGY(2),CTDKAGY                                                
         CLI   CTDKAGY,X'FF'                                                    
         BNE   *+10                                                             
         MVC   PRTAGY,=C'DEFAULT'                                               
*                                                                               
         CLI   CTDKCODE,X'FF'                                                   
         BE    DNAME17                                                          
         EDIT  CTDKCODE,(3,PRTCODE)  LOOKUP CODE IN HEADLINE                    
*                                                                               
DNAME17  EDIT  CTDKDEMO,(3,P+2)      DEMO NUMER ON PRINT LINE                   
*                                                                               
         L     R4,FILEC            PT TO RECORD WE READ IN                      
         LA    R7,CTDDATA          PT TO 1ST ELEMENT                            
DNAME20  CLI   0(R7),0             END OF RECD?                                 
         BE    DNAME52             NO DATA TO OUTPUT                            
         CLI   0(R7),X'02'         GET DESCRIPTION ELEMENT                      
         BE    DNAME25                                                          
         ZIC   R1,1(R7)            LENGTH OF ELEMENT                            
         AR    R7,R1               NEXT ELEMENT                                 
         B     DNAME20                                                          
*                                                                               
         USING CTDNAME,R7          USE --MY-- DEMO ELEMENT DSECT                
DNAME25  LA    RE,P                PT TO PRINT LINE                             
         USING PDNAME,RE                                                        
         MVC   PDNAM44A,CTDNM44A                                                
         MVC   PDNAM44B,CTDNM44B                                                
         MVC   PDNAM55A,CTDNM55A                                                
         MVC   PDNAM55B,CTDNM55B                                                
         MVC   PDNAM5,CTDNAM5                                                   
         MVC   PDNAM6,CTDNAM6                                                   
         MVC   PDNAM7,CTDNAM7                                                   
*                                                                               
DNAME50  MVC   HEAD3+7(L'PRTFIL),PRTFIL                                         
         MVC   HEAD4+7(L'PRTMED),PRTMED                                         
         MVC   HEAD5+7(L'PRTAGY),PRTAGY                                         
         MVC   HEAD6+7(L'PRTCODE),PRTCODE                                       
         GOTO1 REPORT              OUTPUT LINE                                  
DNAME52  B     DNAME5                                                           
         EJECT                                                                  
***********************************************************************         
*SVIRPT - SVI ADJUSTMEND CONTROLS                                               
***********************************************************************         
         TITLE 'SVI ADJUSTMENT REPORT'                                          
SVIRPT   CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         LA    RE,GLOBAL                                                        
         L     RF,=F'800'                                                       
         XCEF                                                                   
         MVI   DCONFRST,1                                                       
         LA    RE,DEMAREA          SET UP DBLOCK FOR DEMO NAME                  
         USING DBLOCK,RE           LOOKUP                                       
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELSRC,C'A'                                                    
         MVI   DBSELMED,C'T'                                                    
         MVI   DBINTMED,C'T'                                                    
         MVI   DBINTFIL,C'T'                                                    
         DROP  RE                                                               
SVIRPT1  LA    R4,KEY                                                           
         USING CTQREC,R4                                                        
         MVI   CTQKTYP,CTQKTEQU                                                 
         BAS   RE,HIGH                                                          
         B     *+8                                                              
SVIRPT2  BAS   RE,SEQ                                                           
         LA    R4,KEY                                                           
         CLI   CTQKTYP,CTQKTEQU    CHECK FOR RIGHT RECORD                       
         BNE   EXIT                                                             
         MVC   CONSRC,CTQKSRC                                                   
         MVC   CONMED,CTQKMED                                                   
         MVC   CONAGY,CTQKAGY                                                   
         MVC   CONLUC,CTQKCODE                                                  
         MVC   CONCLT,CTQKCLI                                                   
         MVC   CONBOOK,CTQKBOOK                                                 
         XC    CONBOOK,=X'FFFF'                                                 
         MVC   CONHFIL,CTQKHUT                                                  
         BAS   RE,SETALPHA         SET UP ALPHA EXPANSIONS                      
         LA    RE,SVIFLINK         SAVE SRV/FILE/AGENCY                         
         CLI   0(RE),0                                                          
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     *-12                                                             
         MVC   0(1,RE),CONSRC                                                   
         MVC   1(1,RE),CONHFIL                                                  
         MVC   2(2,RE),CONAGY                                                   
         SPACE 2                                                                
         MVI   DEMNUM,1                                                         
         LA    RE,SVIALINK         SAVE DEMOS TO BE ADJUSTED                    
         LA    RF,600                                                           
         XCEF                                                                   
         L     R4,FILEC                                                         
         LA    RE,28(R4)                                                        
SVRPT3   CLI   0(RE),0                                                          
         BE    SVRPT7                                                           
         CLI   0(RE),4                                                          
         BE    SVRPT4                                                           
SVRPT3A  ZIC   R1,1(RE)           GET NEXT ELEMENT                              
         AR    RE,R1                                                            
         B     SVRPT3                                                           
SVRPT4   ST    RE,FULL                                                          
         LA    RE,3(RE)                                                         
         LA    RF,SVIALINK         SEARCH FOR OPEN SLOT                         
         LA    R1,128              IN ADJUSTMENT LINK TABLE                     
         CLI   0(RF),0                                                          
         BE    *+12                                                             
         LA    RF,2(RF)                                                         
         B     *-12                                                             
         ZIC   R8,DEMNUM                                                        
SVRPT5   CLI   0(RE),0                                                          
         BE    SVRPT6                                                           
         BCTR  R8,0                                                             
         STC   R8,1(RF)            SAVE DEMO NUMBER                             
         ZIC   R0,0(RE)            GET SVI CODE                                 
         LA    R8,1(R8)                                                         
         STC   R0,0(RF)            SAVE SVI CODE                                
         LA    RF,2(RF)                                                         
SVRPT6   LA    R8,1(R8)            BUMP DEMO NUMBER                             
         LA    RE,1(RE)            TRY NEXT SLOT                                
         BCT   R1,SVRPT5                                                        
         STC   R8,DEMNUM                                                        
         L     RE,FULL             RESTORE ELEMENT ADDRESS                      
         B     SVRPT3A             GET NEXT ELEMENT                             
SVRPT7   LA    RE,SVIALINK         GET NUMER OF ENTRIES                         
         SR    RF,RE                                                            
         LTR   RF,RF               ANY DATA                                     
         BZ    SVIRPT2             NO GET NEXT RECORD                           
         SRL   RF,1                                                             
         GOTO1 XSORT,DMCB,(0,SVIALINK),(RF),2,2,0                               
         LA    RF,SVIALINK         NO FIND START OF EACH CATEGORY               
         LA    RE,SVITLINK                                                      
         XC    SVITLINK,SVITLINK                                                
         SR    R1,R1                                                            
         ST    RF,SVITLINK                                                      
SVRPT8   CLI   0(RF),0                                                          
         BE    SVRPT10                                                          
         CH    R1,=H'45'                                                        
         BE    SVRPT9A                                                          
         CLC   0(1,RF),2(RF)       SAME SVI TYPE                                
         BNE   SVRPT9                                                           
         LA    RF,2(RF)                                                         
         LA    R1,1(R1)                                                         
         B     SVRPT8                                                           
SVRPT9   LA    R1,1(R1)                                                         
         LA    RF,2(RF)                                                         
SVRPT9A  STC   R1,0(RE)                                                         
         LA    RE,4(RE)                                                         
         ST    RF,0(RE)                                                         
         SR    R1,R1               CLEAR LOOP CONTROL COUNTER                   
         B     SVRPT8                                                           
         SPACE 2                                                                
SVRPT10  MVI   FORCEHED,C'Y'       PRINT THE REPORT                             
         MVC   HEAD3+7(L'PRTSRC),PRTSRC                                         
         MVC   HEAD4+7(L'PRTMED),PRTMED                                         
         MVC   HEAD4+34(11),=C'START BOOK-'                                     
         MVC   HEAD4+45(L'PRTBOOK),PRTBOOK                                      
         MVC   HEAD5+7(L'PRTAGY),PRTAGY                                         
         MVC   HEAD5+34(11),=C'SVI SOURCE-'                                     
         MVC   HEAD5+45(L'PRTHFIL),PRTHFIL                                      
         MVC   HEAD6+7(L'PRTCLT),PRTCLT                                         
         LA    RE,SVITLINK         GET SVI TYPE NAMES                           
         LA    R3,HEAD8                                                         
         LA    R4,SVITLINK                                                      
SVRPT11  CLI   0(R4),0                                                          
         BE    SVRPT11A                                                         
         LA    R5,SVITYPE                                                       
         L     R6,0(R4)            GET FIRST SLOT FOR THIS TYPE                 
         ZIC   R7,0(R6)            GET SVI TYPE                                 
         BCTR  R7,0                                                             
         MH    R7,=H'7'                                                         
         LA    R7,0(R7,R5)         MOVE SVI TYPE TO HEADLINE                    
         MVC   0(7,R3),0(R7)                                                    
         MVC   132(7,R3),=C'-------'                                            
         LA    R3,8(R3)                                                         
         LA    R4,4(R4)            SET TO NEXT SLOT                             
         B     SVRPT11                                                          
         EJECT                                                                  
* PRINT SVI REPORT FOR THIS KEY                                                 
SVRPT11A LA    R3,P                POINT TO PRINT LINE                          
         LA    R4,SVITLINK         POINT TO TYPE LINKS                          
         MVI   ANYDATA,0           RESET DATA SWITCH                            
         LA    R6,16               SET UP LOOP                                  
SVRPT12  CLI   0(R4),0                                                          
         BNE   SVRPT13                                                          
         LA    R4,4(R4)            BUMP TO NEXT SVI TYPE LINK                   
         LA    R3,8(R3)            BUMP TO NEXT PRINT SLOT                      
         B     SVRPT14                                                          
SVRPT13  L     R5,0(R4)                                                         
         XC    DEMCODE,DEMCODE                                                  
         MVI   DEMMOD,C'R'         FORCE R MODIFIER                             
         MVC   DEMNUM,1(R5)        SET DEMO NUMBER                              
         GOTO1 DEMOCON,DMCB,DEMCODE,(6,WORK),DEMAREA                            
         CLI   WORK+1,C'*'                                                      
         BE    SVRPT15             INVALID DEMO                                 
SVRPT13A CLI   DCONFRST,1          RESET SEQ AFTER FIRST                        
         BNE   *+12                CALL TO DEMOCON                              
         BAS   RE,HIGH                                                          
         MVI   DCONFRST,0                                                       
         MVC   1(5,R3),WORK+1      MOVE DEMO NAME TO PRINT LINE                 
         ZIC   R1,0(R4)            DECREMENT COUNT                              
         BCTR  R1,0                                                             
         LA    R5,2(R5)            INCREMENT SLOT                               
         ST    R5,0(R4)            SAVE NEXT SLOT                               
         STC   R1,0(R4)            SAVE REMAINING COUNT                         
         MVI   ANYDATA,1           SET DATA SWITCH                              
         LA    R4,4(R4)                                                         
         LA    R3,8(R3)                                                         
SVRPT14  BCT   R6,SVRPT12          NEXT SLOT                                    
         SPACE 2                                                                
         CLI   ANYDATA,0           END                                          
         BE    SVIRPT2             GET NEXT RECORD                              
         GOTO1 REPORT                                                           
         B     SVRPT11A                                                         
SVRPT15  ZIC   R1,0(R4)            JUST GET NEXT ITEM                           
         BCTR  R1,0                                                             
         LA    R5,2(R5)                                                         
         ST    R5,0(R4)                                                         
         STC   R1,0(R4)                                                         
         B     SVRPT12                                                          
         TITLE 'DEMO LOOKUP REPORTING OPTIONS'                                  
ROPRPT   CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         LA    RE,GLOBAL                                                        
         L     RF,=F'800'                                                       
         XCEF                                                                   
ROPRPT1  LA    R4,KEY                                                           
         USING CTRREC,R4                                                        
         MVI   CTRKTYP,CTRKTEQU                                                 
         BAS   RE,HIGH                                                          
         B     *+14                                                             
ROPRPT2  BAS   RE,SEQ                                                           
         GOTO1 REPORT                                                           
         LA    R4,KEY                                                           
         CLI   CTRKTYP,CTRKTEQU                                                 
         BNE   EXIT                                                             
         MVC   CONSRC,CTRKSRC      SET UP FOR ALPHA EXPANSION                   
         MVC   CONMED,CTRKMED                                                   
         MVC   CONAGY,CTRKAGY                                                   
         MVC   CONLUC,CTRKCODE                                                  
         MVC   CONCLT,CTRKCLI                                                   
         MVC   CONBOOK,CTRKBOOK                                                 
         XC    CONBOOK,=X'FFFF'                                                 
         BAS   RE,SETALPHA                                                      
         SPACE 2                                                                
* CHECK FOR PRINT                                                               
         CLC   CONMED,ROPPRMED                                                  
         BNE   *+10                                                             
         CLC   CONSRC,ROPPRSRC                                                  
         BE    ROPRPT2A                                                         
         MVI   FORCEHED,C'Y'                                                    
         MVC   ROPPRMED,CONMED                                                  
         MVC   ROPPRSRC,CONSRC                                                  
         SPACE 2                                                                
ROPRPT2A L     R4,FILEC                                                         
         MVC   P+2(L'PRTAGY),PRTAGY                                             
         MVC   P+9(L'PRTLUC),PRTLUC                                             
         MVC   P+14(L'PRTCLT),PRTCLT                                            
         MVC   P+22(L'PRTBOOK),PRTBOOK                                          
         LA    R5,P+29                                                          
         LA    RE,28(R4)                                                        
ROPRPT3  CLI   0(RE),0             END OF RECORD                                
         BE    ROPRPT2                                                          
         CLI   0(RE),3             OPTIONS ELEMENT                              
         BE    ROPRPT4                                                          
         CLI   0(RE),4             MARKET ELEMENT                               
         BE    ROPRPT6                                                          
ROPRPT3A ZIC   R1,1(RE)            NEXT ELEMENT                                 
         AR    RE,R1                                                            
         B     ROPRPT3                                                          
ROPRPT4  MVC   0(19,R5),=C'REPORTING OPTIONS= '                                 
         LA    R5,19(R5)                                                        
         ST    RE,FULL             SAVE ELEMENT ADDRESS                         
         ST    R5,PLSTART          SAVE PRINT LINE ADDR                         
         BAS   RE,ROPPOPT          SET OPTIONS IN PRINT LINE                    
         BAS   RE,ROPHEAD                                                       
         GOTO1 REPORT                                                           
         MVI   P,0                                                              
         BAS   RE,ROPHEAD                                                       
         L     RE,FULL                                                          
         B     ROPRPT3A            GET NEXT ELEMENT                             
ROPRPT6  ST    RE,FULL             SAVE ELEMENT ADDRESS                         
         BAS   RE,ROPPMKT                                                       
         GOTO1 REPORT                                                           
         L     RE,FULL                                                          
         B     ROPRPT3A            GET NEXT ELEMENT                             
         SPACE 2                                                                
* SET UP PRINT LINES                                                            
ROPHEAD  MVC   HEAD3+7(L'PRTSRC),PRTSRC                                         
         MVC   HEAD4+7(L'PRTMED),PRTMED                                         
         BR    RE                                                               
         EJECT                                                                  
* PRINT OUT CONTROL OPTIONS                                                     
ROPPOPT  NTR1                                                                   
         LA    R7,RPTOPTS                                                       
         LA    R6,RPTOPT                                                        
         L     R5,PLSTART                                                       
         ST    R5,PLEND                                                         
ROPPOPT1 L     R4,FULL                                                          
         L     RF,PLEND                                                         
         CLC   PLSTART,PLEND                                                    
         BE    ROPPOPT3                                                         
         CLI   0(RF),C' '                                                       
         BNE   ROPPOPT2                                                         
         BCTR  RF,0                                                             
         ST    RF,PLEND                                                         
         B     ROPPOPT1                                                         
ROPPOPT2 LA    RF,1(RF)                                                         
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         ST    RF,PLSTART                                                       
         ST    RF,PLEND                                                         
ROPPOPT3 ZIC   R1,0(R6)            GET POSITION IN ELEMENT                      
         AR    R1,R4                                                            
         ZIC   RE,1(R6)            GET BIT MASK                                 
         MVC   0(20,RF),22(R6)     GET DEFAULT CAPTION                          
         EX    RE,*+8                                                           
         B     *+8                                                              
         TM    0(R1),X'00'         TEST ELEMENT FOR ACTIVE BITS                 
         BNO   *+10                                                             
         MVC   0(20,RF),2(R6)                                                   
         LA    RF,20(RF)                                                        
         ST    RF,PLEND                                                         
         LA    R6,L'RPTOPT(R6)                                                  
         BCT   R7,ROPPOPT1                                                      
         SPACE 2                                                                
* ELIMINATE COMMA AT END OF LINE                                                
         L     RF,PLEND                                                         
ROPPOPT4 CLI   0(RF),C','                                                       
         BNE   *+8                                                              
         MVI   0(RF),C' '                                                       
         CLI   0(RF),C' '                                                       
         BNE   EXIT                                                             
         BCTR  RF,0                                                             
         B     ROPPOPT4                                                         
         EJECT                                                                  
* --------------------------------------------------------------------          
* PRINT MARKET TYPES                                                            
* --------------------------------------------------------------------          
ROPPMKT  NTR1                                                                   
         L     R4,FULL                                                          
         CLI   2(R4),X'00'                                                      
         BE    REPPMKT2                                                         
         CLI   2(R4),X'FF'                                                      
         BE    REPPMKT4                                                         
         MVC   P+29(17),=C'MARKET OVERRIDES='                                   
         MVC   P+46(1),2(R4)                                                    
         MVI   P+47,C','                                                        
         LA    R5,P+48                                                          
         B     REPPMKTA            PRINT OUT OVERIDE LIST                       
*                                                                               
REPPMKT2 MVC   P+29(17),=C'VALID MARKETS   ='                                   
         LA    R5,P+46                                                          
         B     REPPMKT6                                                         
*                                                                               
REPPMKT4 MVC   P+29(17),=C'INVALID MARKETS ='                                   
         LA    R5,P+PMKTORG        R5=DISPLACEMENT INTO PRINT LINE              
         USING PMKTD,R5            USE MARKET DSECT                             
*                                                                               
REPPMKT6 ZIC   R6,1(R4)            PROCESSING FOR VAL & INVAL MKT LISTS         
         SH    R6,=H'3'                                                         
         SRA   R6,1                DIVIDE BY 2 LEAVING NUM OF MKTS              
         LTR   R6,R6                                                            
         BZ    EXIT                                                             
         LA    R4,3(R4)            PT TO FIRST MARKET                           
*                                                                               
REPPMKT7 MVC   MKT,0(R4)                                                        
         BAS   RE,GETMKTNM         GET MARKET NAME FRM NETTBL OR DEMAND         
*        BAS   RE,GETALPHA         GET ALPHA MKT CODE                           
         EDIT  (2,0(R4)),(4,PMKT),FILL=0                                        
*                                                                               
         LA    R4,2(R4)            BUMP TO NEXT MARKET                          
         LA    R5,PMKTDSP(R5)      BUMP TO NEXT MKT ON PRT LINE                 
         LA    R1,P                                                             
         CLC   0(R5),PMKTDSP*2(R1) IS THE PRINT LINE FULL?                      
         BNH   REPPMKT8            NO, ADD MORE MKTS                            
         GOTO1 REPORT                                                           
         LA    R5,P+PMKTORG        R5=DISPLACEMENT INTO PRINT LINE              
         USING PMKTD,R5            USE MARKET DSECT                             
*                                                                               
REPPMKT8 BCT   R6,REPPMKT7                                                      
         B     EXIT                                                             
         EJECT                                                                  
* --------------------------------------------------------------------          
*-- PRINT ROUTINE FOR MARKET OVERIDE LIST                                       
* --------------------------------------------------------------------          
REPPMKTA ZIC   R6,1(R4)                                                         
         SH    R6,=H'3'                                                         
         SRA   R6,1                DIVIDE BY 2 LEAVING NUM OF MKTS              
         LTR   R6,R6                                                            
         BZ    EXIT                                                             
         SR    R7,R7                                                            
         LA    R4,3(R4)                                                         
         CH    R6,=H'12'           IS MORE THEN 1 LINE NEEDED                   
         BH    REPPMKTB            NO START PRINTING                            
         LA    R7,13               SET R7 FOR SINGLE LINE                       
*                                                                               
REPPMKTB EDIT  (2,0(R4)),(5,0(R5)),FILL=0,TRAIL=C','                            
         LA    R5,5(R5)            BUMP PRINT LINE                              
         LA    R4,2(R4)            BUMP TO NEXT MARKET                          
         LA    R7,1(R7)            ADD TO MARKET COUNT                          
         CH    R7,=H'12'           IF EQUAL TO 15 SECOND LINE NEEDED            
         BNE   REPPMKTC                                                         
         SR    R7,R7               RESET MARKET COUNTER                         
         LA    R5,P+46             RESET OUTPUT POINTER                         
         GOTO1 REPORT                                                           
REPPMKTC BCT   R6,REPPMKTB                                                      
         SH    R5,=H'1'                                                         
         MVI   0(R5),C' '          BLANK OUT LAST COMMA                         
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
*GETMKNM - FOR NETWORK, GET MARKET NAME FROM NETTBL                             
*          ELSE, READ MKT RECORD FROM DEMAND AND EXTRACT NAME                   
*          INPUT - MKTNUM                                                       
*          OUTPUT- IO  HAS MARKET NAME                                          
*----------------------------------------------------------------------         
GETMKTNM NTR1                                                                   
*                                                                               
         XC    IO(30),IO                                                        
         L     RE,FILEC                                                         
         CLI   CTRKMED-CTRREC(RE),C'N'   NETWORK?                               
         BNE   GETMK10             NO                                           
*                                                                               
         LA    R1,NETTBL           LOOK UP MKT NUMBER IN NETTBL                 
GETMKT5  CLI   0(R1),X'FF'                                                      
         BE    GETMKTX             MKT # NOT FOUND                              
         CLC   MKT,0(R1)           MATCH ON MKT #?                              
         BE    *+12                                                             
         LA    R1,L'NETTBL(R1)                                                  
         B     GETMKT5                                                          
         MVC   IO(6),2(R1)         MOVE IN ALPHA MKT NAME                       
         B     GETMKTX                                                          
         SPACE 1                                                                
*                                                                               
GETMK10  DS    0H                  LOOK UP MKTRECD IN DEMAND                    
         L     RF,ACOMFACS         SWITCH TO SPOT SYSTEM                        
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'SPT',0                                              
         CLI   DMCB+4,0                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
         SR    R1,R1                                                            
         IC    R1,DMCB+4                                                        
         DC    H'0'                                                             
*                                                                               
         XC    DEMAREA,DEMAREA                                                  
         LA    RE,DEMAREA          SET UP DBLOCK FOR DEMO NAME                  
         USING DBLOCK,RE                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         LA    R1,IO                                                            
         MVC   DBAREC,0(R1)                                                     
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'R'                                                    
         MVC   DBSELRMK,MKT                                                     
         L     RF,ACOMFACS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,MKTHOOK                                         
*                                                                               
         L     RF,ACOMFACS         SWITCH BACK TO CONTROL SYSTEM                
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'CON',0                                              
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETMKTX  B     XIT                                                              
         SPACE 3                                                                
* -  -  -  -  -  -  -   -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -          
MKTHOOK  NTR1                      HOOK FOR MARKET NAME                         
         L     R6,DBAREC                                                        
         USING DMKEY,R6                                                         
         LA    R6,DMFRSTEL                                                      
         USING DMELEM,R6                                                        
         ZIC   R1,DMLEN                                                         
         SH    R1,=H'5'                                                         
         EXMVC R1,0(R6),DMMNAME                                                 
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
         TITLE 'STATION CALL LETTER CHANGE REPORT'                              
* STARPT - STATION CALL LETTER CHANGE REPORT '                                  
***********************************************************************         
STARPT   CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         LA    RE,GLOBAL                                                        
         L     RF,=F'800'                                                       
         XCEF                                                                   
         XC    SVCNT,SVCNT                                                      
         XC    SORTCT,SORTCT                                                    
         XC    PCALL1,PCALL1                                                    
         XC    PCALL2,PCALL2                                                    
         XC    STAPRSRC,STAPRSRC                                                
         XC    STAPRMED,STAPRMED                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTSREC,R4                                                        
         MVI   CTSKTYP,CTSKTEQU                                                 
         BAS   RE,HIGH                                                          
         B     *+8                                                              
STARPT2  BAS   RE,SEQ                                                           
         LA    R4,KEY                                                           
         MVC   CONSRC,CTSKSRC                                                   
         MVC   CONMED,CTSKMED                                                   
         MVC   CONBOOK,CTSKBOOK                                                 
         XC    CONBOOK,=X'FFFF'                                                 
         CLC   CONSRC,STAPRSRC     DIFFERENT SOURCE                             
         BNE   STARPT6             PRINT REPORT                                 
         CLC   CONMED,STAPRMED     DIFFERENT SOURCE                             
         BNE   STARPT6             PRINT REPORT                                 
         CLI   CTSKTYP,CTSKTEQU                                                 
         BNE   STARPT6             PRINT REPORT                                 
         CLC   CONBOOK(2),=X'5B07'                                              
         BL    STARPT2                                                          
STARPT2A L     R4,FILEC                                                         
         LA    RE,28(R4)           SET TO FIRST ELEMENT                         
         LA    RF,STACALLT         BUILD SORT RECD FOR SORTER                   
*                                                                               
STARPT3  CLI   0(RE),0                                                          
         BE    STARPT2                                                          
         CLI   0(RE),2             STA EQUIVALENCE ELEMENTS                     
         BE    STARPT5                                                          
STARPT4  ZIC   R1,1(RE)            NEXT ELEMENT                                 
         AR    RE,R1                                                            
         B     STARPT3                                                          
STARPT5  LA    RF,STACALLT                                                      
         XC    0(12,RF),0(RF)                                                   
         MVC   0(5,RF),2(RE)       SAVE OLD/BOOK/NEW                            
         MVC   5(2,RF),CONBOOK                                                  
         MVC   7(5,RF),7(RE)                                                    
         L     R1,SORTCT           BUMP COUNTER (# RECS SENT TO SORTER)         
         LA    R1,1(R1)                                                         
         ST    R1,SORTCT                                                        
         ST    RE,FULL                                                          
         GOTO1 =V(SORTER),DMCB,=C'PUT',(RF)     PASS TO SORTER                  
         L     RE,FULL                                                          
         B     STARPT4                                                          
*                                                                               
*--PRINT REPORT                                                                 
*                                                                               
STARPT6  OC    SORTCT,SORTCT       ANY RECDS TO PRINT?                          
         BZ    STA50               NO, NEXT SOURCE/MEDIA                        
         XC    SVCNT,SVCNT                                                      
         LA    RE,STACALLT                                                      
         L     RF,=F'5000'                                                      
         XCEF                                                                   
         MVI   ROW,1                                                            
         MVI   COL,C'A'                                                         
*                                                                               
STA10    GOTO1 =V(SORTER),DMCB,=C'GET'      GET ANOTHER RECD                    
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   *+14                NO MORE, PRINT WHAT WE HAVE                  
         XC    SORTCT,SORTCT                                                    
         B     STA22                                                            
*                                                                               
         ICM   RE,15,4(R1)         ADDRESS OF RECD                              
*                                                                               
         L     RF,SORTCT                                                        
         BCTR  RF,0                                                             
         ST    RF,SORTCT           # ENTRIES LEFT IN SORTER                     
*                                                                               
         L     RF,SVCNT            # RECDS IN SV BUFF SO FAR                    
         MH    RF,=H'15'           PT TO POSITION IN BUFFER                     
         LA    RF,STACALLT(RF)                                                  
         CLC   PCALL1,0(RE)                                                     
         BNE   STA15                                                            
         CLC   PCALL2,7(RE)                                                     
         BNE   STA15                                                            
         L     RF,SVCNT                                                         
         BCTR  RF,0                                                             
         MH    RF,=H'15'                                                        
         LA    RF,STACALLT(RF)                                                  
         MVI   14(RF),C'*'         MULTIPLE BOOKS INDICATOR                     
         B     STA10                                                            
*                                                                               
STA15    MVC   2(12,RF),0(RE)      MOVE REC FROM SORTER TO SV BUFF              
         MVC   0(1,RF),ROW                                                      
         MVC   1(1,RF),COL                                                      
         MVI   14(RF),C' '                                                      
         MVC   PCALL1,0(RE)                                                     
         MVC   PCALL2,7(RE)                                                     
         L     RF,SVCNT            BUMP COUNTER (RECS IN SVCALL BUFF)           
         LA    RF,1(RF)                                                         
         ST    RF,SVCNT                                                         
         ZIC   RF,ROW                                                           
         LA    RF,1(RF)                                                         
         STC   RF,ROW                                                           
*                                                                               
         CLI   ROW,45              MAX 45 ROWS ON PAGE                          
         BNH   STA20                                                            
         MVI   ROW,1                                                            
         ZIC   RF,COL                                                           
         LA    RF,1(RF)                                                         
         STC   RF,COL                                                           
*                                                                               
STA20    CLC   SVCNT,=F'270'       6 COLS*45 ROWS SV BUFFER FULL?               
         BL    STA10               NOT YET, GET ANOTHER                         
*                                                                               
STA22    L     RF,SVCNT            SORT A PAGE BUFFER                           
         LTR   RF,RF                                                            
         BZ    STA50               END                                          
         GOTO1 XSORT,DMCB,(0,STACALLT),(RF),15,9,0                              
         LA    R7,STACALLT         PT TO BUFFER                                 
         L     R8,SVCNT            NUMBER ENTRIES IN BUFFER                     
*                                                                               
STA25    MVC   ROW,0(R7)           FIRST ROW                                    
         LA    R5,P                PRINT LINE                                   
*                                                                               
STA30    CLC   ROW,0(R7)          IS THIS THE SAME ROW?                         
         BNE   STA35                                                            
         MVC   0(5,R5),2+0(R7)                                                  
         MVC   6(5,R5),2+7(R7)                                                  
         GOTO1 DATCON,DMCB,(3,7(R7)),(6,12(R5))                                 
         MVC   18(1,R5),14(R7)                                                  
         LA    R7,15(R7)           NEXT REC IN BUFFER                           
         LA    R5,20(R5)           NEXT COLUMN                                  
         BCT   R8,STA30                                                         
*                                                                               
STA35    DS    0H                                                               
         MVC   HEAD3+7(L'PRTSRC),PRTSRC                                         
         MVC   HEAD4+7(L'PRTMED),PRTMED                                         
         MVC   FOOT1(42),=C'* CHANGE ALSO APPEARS IN OTHER START BOOKS'         
         GOTO1 REPORT              PRINT LINE                                   
         LTR   R8,R8                                                            
         BNZ   STA25                                                            
         MVI   FORCEHED,C'Y'                                                    
         B     STARPT6                                                          
*                                                                               
STA50    MVC   STAPRSRC,CONSRC                                                  
         MVC   STAPRMED,CONMED                                                  
         GOTO1 =V(SORTER),DMCB,=C'END'             CLOSE SORT                   
         XC    SORTCT,SORTCT                                                    
         XC    PCALL1,PCALL1                                                    
         XC    PCALL2,PCALL2                                                    
         CLI   KEY,CTSKTEQU                                                     
         BNE   STARPTX                                                          
         BAS   RE,SETALPHA                                                      
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD  OPEN SORT FOR NEW KEY          
         B     STARPT2A                                                         
*                                                                               
STARPTX  B     EXIT                                                             
*                                                                               
FLG      DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
**********************************************************************          
         TITLE 'SET UP ALPHA EXPRESIONS'                                        
**********************************************************************          
* SET UP EXTERNAL ALPHA EXPRESSIONS                                             
SETALPHA NTR1                                                                   
         MVC   PRTSRC,SPACES            GET SOURCE EXPANSION                    
         LA    R1,SOURCES                                                       
         LA    RE,SRCTAB                                                        
SASRC    CLC   CONSRC,0(RE)                                                     
         BE    SASRC1                                                           
         LA    RE,L'SRCTAB(RE)                                                  
         BCT   R1,SASRC                                                         
         B     SAMED                                                            
SASRC1   MVC   PRTSRC,1(RE)                                                     
         SPACE 2                                                                
SAMED    MVC   PRTMED,SPACES            GET MEDIA EXPANSION                     
         LA    R1,MEDIAS                                                        
         LA    RE,MEDTAB                                                        
SAMED1   CLC   CONMED,0(RE)                                                     
         BE    SAMED2                                                           
         LA    RE,L'MEDTAB(RE)                                                  
         BCT   R1,SAMED1                                                        
         B     SAAGY                                                            
SAMED2   MVC   PRTMED,1(RE)                                                     
         SPACE 2                                                                
SAAGY    MVC   PRTAGY,SPACES            GET AGENCY EXPANSION                    
         MVC   PRTAGY(2),CONAGY                                                 
         CLI   CONAGY,X'FF'                                                     
         BNE   *+10                                                             
         MVC   PRTAGY,=C'DEFAULT'                                               
         SPACE 2                                                                
SALUC    MVC   PRTLUC,SPACES                                                    
         CLI   CONLUC,X'FF'                                                     
         BE    SACLT                                                            
         MVC   PRTLUC,CONLUC                                                    
         SPACE 2                                                                
SACLT    MVC   PRTCLT,SPACES            GET CLIENT EXPANSION                    
         MVC   PRTCLT(L'CONCLT),CONCLT                                          
         CLI   CONCLT,X'FF'                                                     
         BNE   *+10                                                             
         MVC   PRTCLT,=C'DEFAULT'                                               
SABOOK   MVC   PRTBOOK,SPACES           GET BOOK EXPANSION                      
         CLI   CONBOOK,0                                                        
         BE    SAHFIL                                                           
         GOTO1 DATCON,DMCB,(3,CONBOOK),(6,PRTBOOK)                              
         SPACE 2                                                                
SAHFIL   MVC   PRTHFIL,SPACES           GET SVI FILE EXPANSION                  
         CLI   CONHFIL,0                                                        
         BE    SAHFILX                                                          
         MVC   HALF(1),CONSRC                                                   
         MVC   HALF+1(1),CONHFIL                                                
         LA    R1,SVIFILES                                                      
         LA    RE,SVIFILE                                                       
SAHFIL1  CLC   HALF,0(RE)                                                       
         BE    SAHFIL2                                                          
         LA    RE,L'SVIFILE(RE)                                                 
         BCT   R1,SAHFIL1                                                       
         MVC   PRTHFIL(17),=C'UNKNOWN SVI FILE='                                
         MVC   PRTHFIL+17(1),CONHFIL                                            
         B     SAHFILX                                                          
SAHFIL2  MVC   PRTHFIL,2(RE)                                                    
SAHFILX  DS    0C                                                               
         SPACE 2                                                                
         XIT1                                                                   
         TITLE 'DEMO SYSTEM CONTROL FILE REPORT'                                
* I/O AREAS                                                                     
*                                                                               
HIGH     LA    RF,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     LINKDIR                                                          
         SPACE 2                                                                
SEQ      LA    RF,DMRSEQ                                                        
         MVC   KEYSAVE,KEY                                                      
         B     LINKDIR                                                          
         SPACE 2                                                                
LINKDIR  NTR                                                                    
         ST    RF,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         L     R2,FILEC                                                         
         GOTO1 DATAMGR,DMCB,,CTFILE,KEY,(R2),(0,DMWORK)                         
         MVC   KEY,0(R2)                                                        
         B     EXIT                                                             
         EJECT                                                                  
EXIT     XMOD1 1                                                                
XIT      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=12'                                    
SORTCARD DC    CL80'SORT FIELDS=(1,7,A),FORMAT=CH,WORK=1'                       
*                                                                               
* TABLE TO CONTROL RECORD PRINTING ROUTINES                                     
RECTAB   DC    C'B',AL1(1)         LATEST BOOK CONTROL                          
         DC    C'S',AL1(2)         STATION CALL LETTER EQUATES                  
         DC    C'D',AL1(3)         DEMO NAMES                                   
         DC    C'N',AL1(4)         DEMO MODIFIER DESCRIPTIONS                   
         DC    C'R',AL1(5)         DEMO LOOKUP CONTROLS                         
         DC    C'Q',AL1(6)         SVI ADJUSTMENT CONTROLS                      
         DC    X'00'                                                            
         SPACE 2                                                                
FILTAB   DS    0CL16                                                            
         DC    C'E',C'EVN',CL12'ESTIMATD VPH'                                   
         DC    C'T',C'TP ',CL12'TIME PERIOD'                                    
         DC    C'P',C'PAV',CL12'PROG AVERAGE'                                   
         DC    C'M',C'MPA',CL12'MKT PRG ANAL'                                   
         DC    C'N',C'NAD',CL12'NAD'                                            
         DC    C'I',C'INV',CL12'INVENTORY'                                      
FILES    EQU   (*-FILTAB)/L'FILTAB                                              
         SPACE 2                                                                
MEDTAB   DS    0CL11                                                            
         DC    C'T',CL10'TELEVISION'                                            
         DC    C'R',CL10'RADIO'                                                 
         DC    C'W',CL10'WEEKLY'                                                
         DC    C'U',CL10'UPGRADES'                                              
         DC    C'N',CL10'NETWORK'                                               
         DC    C'C',CL10'CANADA'                                                
         DC    C'P',CL10'MPA'                                                   
         DC    C'V',CL10'VPH'                                                   
MEDIAS   EQU   (*-MEDTAB)/L'MEDTAB                                              
         SPACE 2                                                                
SRCTAB   DS    0CL4                                                             
         DC    C'A',C'ARB'                                                      
         DC    C'N',C'NSI'                                                      
         DC    C'B',C'BBM'                                                      
         DC    C'C',C'CSI'                                                      
         DC    C'S',C'SRC'                                                      
SOURCES  EQU   (*-SRCTAB)/L'SRCTAB                                              
         SPACE 1                                                                
*                                                                               
NETTBL   DS    0XL8                NETWORK MKTS                                 
         DS    0XL2,0CL6           2BTYE MKT #, 6BYTE ALPHA MKT NAME            
         DC    AL2(001),C'NTI   '                                               
         DC    AL2(002),C'NTIAGG'                                               
         DC    AL2(010),C'CNN   '                                               
         DC    AL2(011),C'WTBS  '                                               
         DC    AL2(012),C'USA   '                                               
         DC    AL2(013),C'CBN   '                                               
         DC    AL2(014),C'ESPN  '                                               
         DC    AL2(015),C'MTV   '                                               
         DC    AL2(016),C'NICK  '                                               
         DC    AL2(017),C'AEN   '                                               
         DC    AL2(018),C'LFTM  '                                               
         DC    AL2(200),C'NAD   '                                               
         DC    AL2(210),C'NTIC  '                                               
         DC    AL2(211),C'NTIA  '                                               
         DC    AL2(300),C'CABLE '                                               
         DC    AL2(400),C'NADS  '                                               
         DC    AL2(500),C'NHTI  '                                               
         DC    X'FFFF',XL6'FF'                                                  
NETTBLQ  EQU   (*-NETTBL)/L'NETTBL   NUMBER OF NET TABLE ENTRIES                
         SPACE 1                                                                
RPTOPT   DS    0CL42                                                            
         DC    AL1(2),X'80',CL20'USE AGENCY DEFAULTS',CL20' '                   
         DC    AL1(2),X'40',CL20'FAST',CL20'NORMAL'                             
         DC    AL1(2),X'20',CL20'COMBINE P+S2',CL20'SEPARATE P+S2'              
         DC    AL1(2),X'10',CL20'2WEEK HIGHEST',CL20' '                         
         DC    AL1(2),X'08',CL20'4WEEK AVERAGE',CL20' '                         
         DC    AL1(2),X'04',CL20'EXTRA QTR HRS.',CL20' '                        
         DC    AL1(2),X'02',CL20'JUL211=YES',CL20'JUL211=NO'                    
         DC    AL1(2),X'01',CL20'SHARES=TOTAL',CL20'SHARES=QHR AVG.'            
         DC    AL1(3),X'80',CL20'TPT=YES',CL20' '                               
         DC    AL1(3),X'40',CL20'PAV=YES',CL20' '                               
         DC    AL1(3),X'20',CL20'NTI=YES',CL20' '                               
         DC    AL1(3),X'10',CL20'MPA=YES',CL20' '                               
         DC    AL1(3),X'08',CL20'DPT=YES',CL20' '                               
         DC    AL1(3),X'04',CL20'RDP=YES',CL20' '                               
         DC    AL1(4),X'80',CL20'EXTRA SPILL=YES',CL20' '                       
         DC    AL1(4),X'40',CL20'PUTS=2YEAR',CL20'PUTS=1YEAR'                   
         DC    AL1(4),X'20',CL20'ADI=YES',CL20'ADI=NO'                          
         DC    AL1(4),X'10',CL20'S7MIN=NO',CL20'S7MIN=YES'                      
         DC    AL1(4),X'08',CL20'E7MIN=NO',CL20'E7MIN=YES'                      
RPTOPTS  EQU   (*-RPTOPT)/L'RPTOPT                                              
         SPACE 1                                                                
SVIFILE  DS    0CL32                                                            
         DC    CL2'AA',CL30'A-BRISTOL MEYERS MONTHLY'                           
         DC    CL2'AB',CL30'B-ARB 1975 SVI'                                     
         DC    CL2'AC',CL30'C-SPECIAL-CREATED 1974'                             
         DC    CL2'AD',CL30'D-ARB 1976 SVI'                                     
         DC    CL2'AG',CL30'G-MCCANN SPECIAL-CREATED 1981'                      
         DC    CL2'AH',CL30'H-1992 ARB SAIR'                                    
         DC    CL2'AK',CL30'K-PEPSI FEB/92'                                     
         DC    CL2'AL',CL30'L-PEPSI FEB/92 AD1849'                              
         DC    CL2'AM',CL30'M-1981 ARB SAIR'                                    
         DC    CL2'AN',CL30'N-1980 ARB SIAR'                                    
         DC    CL2'AP',CL30'P-1978 ARB SVI'                                     
         DC    CL2'AR',CL30'R-1980 ARB SAIR REVISED'                            
         DC    CL2'AS',CL30'S-1987 ARB SAIR '                                   
         DC    CL2'AT',CL30'T-1988 ARB SAIR '                                   
         DC    CL2'AU',CL30'U-1989 ARB SAIR '                                   
         DC    CL2'AV',CL30'V-1990 ARB SAIR '                                   
         DC    CL2'AW',CL30'W-1991 ARB SAIR '                                   
         DC    CL2'A0',CL30'0-1980 ARB SVI'                                     
         DC    CL2'A1',CL30'1-1981 ARB SAIR'                                    
         DC    CL2'A2',CL30'2-1982 ARB SAIR'                                    
         DC    CL2'A3',CL30'3-1983 ARB SAIR'                                    
         DC    CL2'A4',CL30'4-1984 ARB SAIR'                                    
         DC    CL2'A5',CL30'5-1985 ARB SAIR'                                    
         DC    CL2'A6',CL30'6-1986 ARB SAIR'                                    
         DC    CL2'A7',CL30'7-ARB 1977'                                         
         DC    CL2'A8',CL30'8-ARB 1978'                                         
         DC    CL2'A9',CL30'9-ARB 1979'                                         
         DC    CL2'NA',CL30'A-NSI 1975 '                                        
         DC    CL2'NB',CL30'B-NSI 1976 '                                        
         DC    CL2'NC',CL30'C-JWT SPECIAL '                                     
         DC    CL2'ND',CL30'D-NSI 1977 '                                        
         DC    CL2'NE',CL30'E-CAMPBELL SOUP SPECIAL'                            
         DC    CL2'NF',CL30'F-CAMPBELL SOUP 1988'                               
         DC    CL2'NG',CL30'G-CAMPBELL SOUP 1989'                               
         DC    CL2'NM',CL30'M-NSI 1978 '                                        
         DC    CL2'NN',CL30'N-NSI 1991 '                                        
         DC    CL2'NO',CL30'O-NSI 1992 '                                        
         DC    CL2'NP',CL30'P-PEPSI SPECIAL 419/420'                            
         DC    CL2'N0',CL30'0-NSI 1990 '                                        
         DC    CL2'N1',CL30'1-NSI 1981 '                                        
         DC    CL2'N2',CL30'2-NSI 1982 '                                        
         DC    CL2'NJ',CL30'J-NSI 1982 REVISED'                                 
         DC    CL2'N3',CL30'3-NSI 1983 '                                        
         DC    CL2'N4',CL30'4-NSI 1984 '                                        
         DC    CL2'N5',CL30'5-NSI 1985 '                                        
         DC    CL2'N6',CL30'6-NSI 1986 '                                        
         DC    CL2'N7',CL30'7-NSI 1987 '                                        
         DC    CL2'N8',CL30'8-NSI 1988 '                                        
         DC    CL2'N9',CL30'9-NSI 1989 '                                        
SVIFILES EQU   (*-SVIFILE)/L'SVIFILE                                            
         SPACE 2                                                                
SVITYPE  DS    0CL7                                                             
         DC    C' HOMES '          1                                            
         DC    C'WMN TOT'          2                                            
         DC    C'WM18-49'          3                                            
         DC    C'MEN TOT'          4                                            
         DC    C'MN18-49'          5                                            
         DC    C' TEENS '          6                                            
         DC    C'CH2-11 '          7                                            
         DC    C' METRO '          8                                            
         DC    C'VW12-34'          9                                            
         DC    C'WM25-54'          10                                           
         DC    C'MN25-54'          11                                           
         DC    C'CH6-11 '          12                                           
         DC    C'AD18-49'          13                                           
SVITYPES EQU   (*-SVITYPE)/L'SVITYPE                                            
         EJECT                                                                  
TEMPD    DSECT                                                                  
GLOBAL   DS    800C                                                             
GLOBEND  DS    0C                                                               
         ORG   GLOBAL                                                           
* GLOBAL VARIABLES                                                              
CONSRC   DS    C                                                                
CONMED   DS    C                                                                
CONAGY   DS    CL2                                                              
CONLUC   DS    C                                                                
CONCLT   DS    CL3                                                              
CONBOOK  DS    CL2                                                              
CONHFIL  DS    C                                                                
PRTSRC   DS    CL3                                                              
PRTMED   DS    CL10                                                             
PRTAGY   DS    CL7                                                              
PRTLUC   DS    C                                                                
PRTCLT   DS    CL7                                                              
PRTBOOK  DS    CL6                                                              
PRTHFIL  DS    CL30                                                             
PRTFIL   DS    CL12                                                             
PRTCODE  DS    CL3                                                              
PKEY     DS    CL25                                                             
ANYDATA  DS    C                                                                
DEMCODE  DS    0CL3                                                             
         DS    C                                                                
DEMMOD   DS    C                                                                
DEMNUM   DS    C                                                                
DCONFRST DS    C                                                                
         DS    0F                                                               
DEMAREA  DS    CL256                                                            
MKT      DS    XL2                 2 BYTE MKT NUMBER                            
         ORG   GLOBEND                                                          
RPTVAR   DS    500C                                                             
RPTVEND  DS    0C                                                               
         ORG   RPTVAR                                                           
* REPORT VARIABLES GO HERE                                                      
         ORG   RPTVEND                                                          
IO       DS    1000C                                                            
RPTTAB   DS    CL12000                                                          
RPTTEND  DS    0C                                                               
PRTTLEN  EQU   RPTTEND-RPTTAB                                                   
TEMPX    DS    0C                                                               
         EJECT                                                                  
* SVI REPORT VARIABLES                                                          
         ORG   RPTTAB                                                           
SVIALINK DS    600C                2 BYTE ENTRIES                               
*                                   0= SVI TYPE                                 
*                                   1= DEMO NUMBER                              
SVIFLINK DS    1000C               4 BYTE ENTRIES                               
*                                   0  =RATING SERVICE                          
*                                   1  =FILE CODE                               
*                                   2-3=AGENCY                                  
SVITLINK DS    CL64                                                             
         SPACE 2                                                                
* REPORTING OPTIONS REPORT VARIABLES                                            
         ORG   RPTTAB                                                           
PLSTART  DS    F                                                                
PLEND    DS    F                                                                
ROPPRMED DS    C                                                                
ROPPRSRC DS    C                                                                
         SPACE 2                                                                
* STATION CALL LETTER CHANGES REPORT VARIABLES                                  
ROW      DS    C                                                                
COL      DS    C                                                                
PCALL1   DS    CL5                                                              
PCALL2   DS    CL5                                                              
SORTCT   DS    F                                                                
SVCNT    DS    F                                                                
STAPRSRC DS    C                                                                
STAPRMED DS    C                                                                
SVCALLCT DS    F                                                                
STATLINK DS    CL96                                                             
STACALLT DS    45000C                                                           
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE CTREPWORKD                                                     
       ++INCLUDE CTREPMODES                                                     
       ++INCLUDE CTGENFILE                                                      
*                                                                               
CTDNAME  DSECT                     DEMO NAMES ELEMENT DSECT                     
CTDNAMEL DS    X                   ELEMENT TYPE                                 
CTDNAMLQ EQU   X'02'                                                            
CTDNALN  DS    X                   LENGTH                                       
CTDNAM5  DS    CL5                 5 CHAR NAME                                  
CTDNAM6  DS    CL6                 6 CHAR NAME                                  
CTDNAM7  DS    CL7                 7 CHAR NAME                                  
CTDNM44A DS    CL4                 4 BY 4 CHAR NAME  PART1                      
CTDNM44B DS    CL4                 4 BY 4 CHAR NAME                             
CTDNM55A DS    CL5                 5 BY 5 CHAR NAME  PART1                      
CTDNM55B DS    CL5                 5 BY 5 CHAR NAME                             
CTDNALNQ EQU   *-CTDNAMEL          LENGTH OF ELEMENT                            
         EJECT                                                                  
* ******************************************************************            
* PMKTD  - PRINT LINE DSECT FOR INVALID/VALID MKTS IN LOOKUP RPT                
* ******************************************************************            
PMKTD    DSECT                     PRINT DSECT FOR DEMO NAMES                   
PMKT     DS    CL8                 MARKET NUMBER                                
         DS    CL1                                                              
PMKTALP  DS    CL3                 ALPHA MARKET CODE                            
         DS    CL1                                                              
PMKTNAM  DS    CL12                MARKET NAME                                  
         DS    CL3                                                              
*                                                                               
PMKTDSP  EQU   *-PMKT              DISPLACEMENT TO NEXT MKT                     
PMKTORG  EQU   46                  1ST MKT BEGINS AT P+46                       
         EJECT                                                                  
* ******************************************************************            
* PDNAME - PRINT LINE DSECT FOR DEMO NAMES REPORT                               
* ******************************************************************            
PDNAME   DSECT                     PRINT DSECT FOR DEMO NAMES                   
         DS    CL2                                                              
PDEMNUM  DS    CL3                 DEMO NUMBER                                  
         DS    CL6                                                              
PDNAM44A DS    CL4                 4 BY 4 CHAR NAME                             
         DS    CL1                                                              
PDNAM44B DS    CL4                 4 BY 4 CHAR NAME                             
         DS    CL4                                                              
PDNAM55A DS    CL5                 5 BY 5 CHAR NAME                             
         DS    CL1                                                              
PDNAM55B DS    CL5                 5 BY 5 CHAR NAME                             
         DS    CL4                                                              
PDNAM5   DS    CL5                 5 CHAR NAME                                  
         DS    CL4                                                              
PDNAM6   DS    CL6                 6 CHAR NAME                                  
         DS    CL4                                                              
PDNAM7   DS    CL7                 7 CHAR NAME                                  
         DS    CL4                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'176CTREP9102A05/01/02'                                      
         END                                                                    
