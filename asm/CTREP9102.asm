*          DATA SET CTREP9102  AT LEVEL 018 AS OF 05/08/14                      
*PROCESS USING(WARN(15))                                                        
*PHASE CT9102A                                                                  
         TITLE 'DEMO SYSTEM CONTROL FILE REPORT'                                
CT9102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TEMPX-TEMPD,**CT9102,R9                                          
         USING TEMPD,RC                                                         
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
*                                                                               
         CLI   MODE,REQFRST                                                     
         BL    EXIT                                                             
*                                                                               
         XC    STRTDAT3,STRTDAT3   INIT START DATE TO LOW VALUES                
         CLC   QSTART,SPACES                                                    
         BE    CHKENDDT                                                         
         GOTO1 DATCON,DMCB,QSTART,(3,STRTDAT3)                                  
CHKENDDT MVC   ENDDAT3,=X'FFFFFF'  INIT END DATE TO HIGH VALUES                 
         CLC   QEND,SPACES                                                      
         BE    RQF0                                                             
         GOTO1 DATCON,DMCB,QEND,(3,ENDDAT3)                                     
*                                                                               
RQF0     DS    0H                                                               
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
         B     DCODE               DEMO MODIFIERS                               
         B     ROPRPT              DEMO LOOKUP CONTROLS                         
         B     SVIRPT              SVI ADJUSTMENT CONTROLS                      
         EJECT                                                                  
         TITLE 'DEMO NAMES REPORT'                                              
***********************************************************************         
*DNAMES - PRINT DEMO NAMES FOR ALL FILES,AGY,MED,...                            
***********************************************************************         
DNAMES   CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
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
         MVC   PRTFIL(1),CTDKFILE                                               
*                                                                               
DNAME9   MVC   PRTMED,SPACES        GET MEDIA EXPANSION                         
         LA    R1,MEDIAS                                                        
         LA    RE,MEDTAB                                                        
DNAME10  CLC   CTDKMED,0(RE)                                                    
         BE    DNAME12                                                          
         LA    RE,L'MEDTAB(RE)                                                  
         BCT   R1,DNAME10                                                       
         MVC   PRTMED(1),CTDKMED                                                
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
         MVC   PRTAGY,=C'DEFAULT '                                              
*                                                                               
         LA    RE,LOOKTAB                                                       
DNAME16  CLC   CTDKCODE(1),0(RE)                                                
         BE    *+8                                                              
         CLI   0(RE),X'FF'                                                      
         BE    *+12                                                             
         LA    RE,L'LOOKTAB(RE)                                                 
         B     DNAME16                                                          
         MVC   PRTCODE,1(RE)                                                    
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
         TITLE 'DEMO NAMES REPORT'                                              
***********************************************************************         
*DCODE - PRINT DEMO CODE MODIFIER NAMES                                         
***********************************************************************         
DCODE    CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         LA    RE,GLOBAL                                                        
         L     RF,=F'800'                                                       
         XCEF                                                                   
         LA    R4,KEY                                                           
         USING CTNREC,R4           BUILD DEMO NAMES RECD KEY                    
         MVI   CTNKTYP,CTNKTEQU                                                 
         BAS   RE,HIGH                                                          
         MVC   PKEY,KEY                                                         
         B     *+8                                                              
*                                                                               
DCODE5   BAS   RE,SEQ              READ ALL DEMO NAMES RECDS                    
         LA    R4,KEY                                                           
         CLI   CTNKTYP,CTNKTEQU                                                 
         BNE   EXIT                NO MORE, EXIT                                
         CLI   CTNKTYP+1,0                                                      
         BNE   EXIT                NO MORE, EXIT                                
         CLC   PKEY(25),KEY        IF A KEY FLD CHGS--NEW PAGE                  
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   PKEY,KEY                                                         
*                                                                               
         MVC   PRTFIL,SPACES       GET FILE TYPE EXPANSION                      
         LA    R1,FILES                                                         
         LA    RE,FILTAB                                                        
*                                                                               
DCODE7   CLC   CTNKFILE,0(RE)                                                   
         BNE   *+14                                                             
         MVC   PRTFIL,4(RE)                                                     
         B     DCODE9                                                           
*                                                                               
         CLI   CTNKFILE,X'FF'                                                   
         BNE   *+14                                                             
         MVC   PRTFIL(7),=C'DEFAULT'                                            
         B     DCODE9                                                           
*                                                                               
         LA    RE,L'FILTAB(RE)                                                  
         BCT   R1,DCODE7                                                        
         MVC   PRTFIL(1),CTNKFILE                                               
*                                                                               
DCODE9   MVC   PRTMED,SPACES        GET MEDIA EXPANSION                         
         LA    R1,MEDIAS                                                        
         LA    RE,MEDTAB                                                        
DCODE10  CLC   CTNKMED,0(RE)                                                    
         BE    DCODE12                                                          
         LA    RE,L'MEDTAB(RE)                                                  
         BCT   R1,DCODE10                                                       
         MVC   PRTMED(1),CTNKMED                                                
         CLI   CTNKMED,X'FF'                                                    
         BNE   *+10                                                             
         MVC   PRTMED(7),=C'DEFAULT'                                            
         B     DCODE15                                                          
DCODE12  MVC   PRTMED,1(RE)                                                     
*                                                                               
DCODE15  MVC   PRTAGY,SPACES         GET AGENCY EXPANSION                       
         MVC   PRTAGY(2),CTNKAGY                                                
         CLI   CTNKAGY,X'FF'                                                    
         BNE   *+10                                                             
         MVC   PRTAGY,=C'DEFAULT '                                              
*                                                                               
         LA    RE,LOOKTAB                                                       
DCODE16  CLC   CTNKCODE(1),0(RE)                                                
         BE    *+8                                                              
         CLI   0(RE),X'FF'                                                      
         BE    *+12                                                             
         LA    RE,L'LOOKTAB(RE)                                                 
         B     DCODE16                                                          
         MVC   PRTCODE,1(RE)                                                    
*                                                                               
         MVC   HEAD3+7(L'PRTFIL),PRTFIL                                         
         MVC   HEAD4+7(L'PRTMED),PRTMED                                         
         MVC   HEAD5+7(L'PRTAGY),PRTAGY                                         
         MVC   HEAD6+7(L'PRTCODE),PRTCODE                                       
*                                                                               
         L     R4,FILEC            PT TO RECORD WE READ IN                      
         LA    R3,CTNDATA          PT TO 1ST ELEMENT                            
DCODE20  CLI   0(R3),0             END OF RECD?                                 
         BE    DCODE52             NO DATA TO OUTPUT                            
         CLI   0(R3),CTDSCELQ      GET DESCRIPTION ELEMENT                      
         BE    DCODE25                                                          
*                                                                               
DCODE23  DS    0H                                                               
         LLC   R1,1(R3)            LENGTH OF ELEMENT                            
         AR    R3,R1               NEXT ELEMENT                                 
         B     DCODE20                                                          
*                                                                               
         USING CTDSCD,R3           USE ELEMENT DSECT                            
DCODE25  DS    0H                                                               
         MVC   P+6(1),CTDSC        MODIFIER                                     
         MVC   P+12(5),CTDSC+1     DESCRIPTION                                  
*                                                                               
         GOTO1 REPORT              OUTPUT LINE                                  
*                                                                               
         B     DCODE23             NEXT ELEMENT                                 
         DROP  R3                                                               
*                                                                               
DCODE52  B     DCODE5                                                           
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
         LARL  R5,SVITYPE                                                       
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
         B     ROPRPT2E                                                         
*                                                                               
ROPRPT2  BAS   RE,SEQ                                                           
         BAS   RE,ROPHEAD                                                       
         GOTO1 REPORT                                                           
*                                                                               
ROPRPT2E DS    0H                                                               
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
         BAS   RE,ROPHEAD                                                       
         SPACE 2                                                                
* CHECK FOR PRINT                                                               
         CLC   CONMED,ROPPRMED                                                  
         BNE   *+14                                                             
         CLC   CONSRC,ROPPRSRC                                                  
         BE    ROPRPT2A                                                         
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   ROPPRMED,CONMED                                                  
         MVC   ROPPRSRC,CONSRC                                                  
         SPACE 2                                                                
ROPRPT2A L     R4,FILEC                                                         
         MVC   P+2(L'PRTAGY),PRTAGY                                             
         MVC   P+11(L'PRTLUC),PRTLUC                                            
         MVC   P+16(L'PRTCLT),PRTCLT                                            
         MVC   P+24(L'PRTBOOK),PRTBOOK                                          
         LA    R5,P+31                                                          
*                                                                               
         LA    RE,28(R4)                                                        
ROPRPT3  CLI   0(RE),0             END OF RECORD                                
         BE    ROPRPT2                                                          
         CLI   0(RE),CTROECDQ      OPTIONS ELEMENT (X'03')                      
         BE    ROPRPT4                                                          
         CLI   0(RE),CTRMECDQ      MARKET ELEMENT (X'04')                       
         BE    ROPRPT6                                                          
         CLI   0(RE),X'F1'         ACTIVITY ELEMENT                             
         BE    ROPRPT8                                                          
*                                                                               
ROPRPT3A ZIC   R1,1(RE)            NEXT ELEMENT                                 
         AR    RE,R1                                                            
         B     ROPRPT3                                                          
*                                                                               
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
*                                                                               
ROPRPT6  ST    RE,FULL             SAVE ELEMENT ADDRESS                         
         BAS   RE,ROPPMKT                                                       
         GOTO1 REPORT                                                           
         L     RE,FULL                                                          
         B     ROPRPT3A            GET NEXT ELEMENT                             
*                                                                               
ROPRPT8  ST    RE,FULL             SAVE ELEMENT ADDRESS                         
         BAS   RE,ROPACTV          PRINT ACTIVITY DATES                         
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
*                                  THIS IS A HARD-CODED DISPLACMENT...          
*                                  ...REFERRING TO FIELD CTROOPT1...            
*                                  ...CTROOPT2, CTROOPT3...ETC.                 
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
* PRINT MARKET TYPES                                                            
ROPPMKT  NTR1                                                                   
         L     R4,FULL                                                          
         USING CTRMD,R4                                                         
         CLI   CTRMTYPE,CTRMTPV    LIST OF VALID MARKETS?                       
         BE    REPPMKT2                                                         
         CLI   CTRMTYPE,CTRMTPI    LIST OF INVALID MARKETS?                     
         BE    REPPMKT4                                                         
         DC    H'0'                INVALID LIST TYPE                            
*                                                                               
REPPMKT2 MVC   P+29(17),=C'VALID MARKETS   ='                                   
         LA    R5,P+46                                                          
         B     REPPMKT6                                                         
REPPMKT4 MVC   P+29(17),=C'INVALID MARKETS ='                                   
         LA    R5,P+46                                                          
*                                                                               
REPPMKT6 ZIC   R6,1(R4)                                                         
         SHI   R6,CTRMFXLQ                                                      
*                                  L'CTRMMKTS = 2                               
         SRA   R6,1                DIVIDE BY 2 LEAVING NUM OF MKTS              
         LTR   R6,R6                                                            
         BZ    EXIT                                                             
*                                                                               
         SR    R7,R7                                                            
         LA    R4,CTRMFXLQ(R4)                                                  
         CHI   R6,15               IS MORE THEN 1 LINE NEEDED                   
         BH    REPPMKT7            NO START PRINTING                            
         LA    R7,16               SET R7 FOR SINGLE LINE                       
REPPMKT7 EDIT  (2,0(R4)),(4,0(R5)),FILL=0,TRAIL=C','                            
         LA    R5,4(R5)            BUMP PRINT LINE                              
         LA    R4,L'CTRMMKTS(R4)   BUMP TO NEXT MARKET                          
         LA    R7,1(R7)            ADD TO MARKET COUNT                          
         CH    R7,=H'15'           IF EQUAL TO 15 SECOND LINE NEEDED            
         BNE   REPPMKT8                                                         
         SR    R7,R7               RESET MARKET COUNTER                         
         LA    R5,P+46             RESET OUTPUT POINTER                         
         GOTO1 REPORT                                                           
REPPMKT8 BCT   R6,REPPMKT7                                                      
         SHI   R5,1                                                             
         MVI   0(R5),C' '          BLANK OUT LAST COMMA                         
         B     EXIT                                                             
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
* PRINT ACTIVITY DATES                                                          
ROPACTV  NTR1                                                                   
         L     R4,FULL                                                          
         USING ACTVD,R4                                                         
         MVC   P+29(20),=C'RECORD ADDED        '                                
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(8,P+49)                                
         GOTO1 REPORT                                                           
*                                                                               
         CLC   ACTVADDT,ACTVCHDT   SAME ADD AND CHANGE DATES?                   
         BE    ROPACTVX            YES                                          
         MVC   P+29(20),=C'RECORD LAST CHANGED '                                
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(8,P+49)                                
         GOTO1 REPORT                                                           
*                                                                               
ROPACTVX DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
         TITLE 'STATION CALL LETTER CHANGE REPORT'                              
* STARPT - STATION CALL LETTER CHANGE REPORT '                                  
***********************************************************************         
STARPT   CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         GOTO1 SORTER,DMCB,SORTCARD,(X'80',RECCARD),(X'80',0)                   
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
*                                                                               
* DON'T BOTHER PRINTING THE RECORD IF DEMADDR IGNORES IT ANYWAY                 
         CLI   CONBOOK,94          BYPASS OLD BOOKS                             
         BL    STARPT2                                                          
         CLC   CONSRC(2),=C'AR'    ARBITRON RADIO?                              
         BNE   *+14                NO                                           
         CLC   CONBOOK,=X'5D00'    BYPASS ANYTHING STARTING 1993                
         BH    STARPT2                                                          
*                                                                               
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
         GOTO1 SORTER,DMCB,=C'PUT',(RF)     PASS TO SORTER                      
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
STA10    GOTO1 SORTER,DMCB,=C'GET' GET ANOTHER RECD                             
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
         MH    RF,=H'15'                                                        
         LA    RF,STACALLT(RF)                                                  
*                                                                               
         CLC   PCALL1,0(RE)                                                     
         BNE   STA15                                                            
         CLC   PCALL2,7(RE)                                                     
         BNE   STA15                                                            
         CLC   PCALL1,PASTK1                                                    
         BNE   STA12                                                            
         CLC   PCALL2,PASTK2                                                    
         BE    STA15               DON'T ASTERISK AGAIN                         
STA12    MVC   PASTK1,PCALL1                                                    
         MVC   PASTK2,PCALL2                                                    
         BCTR  RF,0                                                             
         MVI   0(RF),C'*'          MULTIPLE BOOKS INDICATOR                     
         LA    RF,1(RF)                                                         
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
         GOTO1 SORTER,DMCB,=C'END' CLOSE SORT                                   
         XC    SORTCT,SORTCT                                                    
         XC    PCALL1,PCALL1                                                    
         XC    PCALL2,PCALL2                                                    
         CLI   KEY,CTSKTEQU                                                     
         BNE   STARPTX                                                          
         BAS   RE,SETALPHA                                                      
*                                  OPEN SORT FOR NEW KEY                        
         GOTO1 SORTER,DMCB,SORTCARD,(X'80',RECCARD),(X'80',0)                   
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
SETALPHA NTR1                                                                   
*                                                                               
         CLI   QOPT1,C'R'          DCON RECORDS USE A DIFFERENT TABLE           
         BNE   SETA10                                                           
         MVC   PRTSRC,SPACES       SOURCE EXPANSION                             
         MVC   PRTMED,SPACES       MEDIA EXPANSION                              
         LA    R1,SRCMEDS          # OF TABLE ENTRIES                           
         LA    RE,SRCMEDT          A(SOURCE/MEDIA COMBO TABLE)                  
SASRCMD  CLC   CONSRCMD,0(RE)                                                   
         BE    SASRCMD5                                                         
         LA    RE,L'SRCMEDT(RE)                                                 
         BCT   R1,SASRCMD                                                       
         MVC   PRTSRC(1),CONSRC                                                 
         MVC   PRTSRC+1(4),=C' ***'                                             
         MVC   PRTMED(1),CONMED                                                 
         MVC   PRTMED+1(4),=C' ***'                                             
         B     SAMED                                                            
SASRCMD5 MVC   PRTSRC,2(RE)                                                     
         MVC   PRTMED,10(RE)                                                    
         B     SAAGY                                                            
*                                                                               
SETA10   DS    0H                                                               
         MVC   PRTSRC,SPACES            GET SOURCE EXPANSION                    
         LA    R1,SOURCES                                                       
         LA    RE,SRCTAB                                                        
SASRC    CLC   CONSRC,0(RE)                                                     
         BE    SASRC1                                                           
         LA    RE,L'SRCTAB(RE)                                                  
         BCT   R1,SASRC                                                         
         MVC   PRTSRC(1),CONSRC                                                 
         MVC   PRTSRC+1(4),=C' ***'                                             
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
         MVC   PRTMED(1),CONMED                                                 
         MVC   PRTMED+1(4),=C' ***'                                             
         B     SAAGY                                                            
SAMED2   MVC   PRTMED,1(RE)                                                     
         SPACE 2                                                                
SAAGY    MVC   PRTAGY,SPACES            GET AGENCY EXPANSION                    
         MVC   PRTAGY(2),CONAGY                                                 
         CLI   CONAGY,X'FF'                                                     
         BNE   *+14                                                             
         MVC   PRTAGY,=C'DEFAULT '                                              
         B     SALUC                                                            
         CLC   PRTAGY,=C'AA'                                                    
         BNL   SALUC                                                            
         EDIT  (B2,CONAGY),(5,PRTAGY),ALIGN=LEFT                                
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
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=12'                                    
SORTCARD DC    CL80'SORT FIELDS=(1,7,A),FORMAT=CH,EQUALS'                       
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
SRCMEDT  DS    0CL20               FOR DCON RECORDS                             
*                                   TABLE WAS COPIED FROM CTSFM34               
* SOURCE CODE + MEDIA CODE + SOURCE NAME + MEDIA NAME                           
* ALL ALLOWED COMBINATIONS OF SOURCE/MEDIA                                      
* NAMES DEPEND ON COMBINATION OF THE TWO                                        
         DC    C'AC',CL8'BBM',CL10'CANTV'                                       
         DC    C'NC',CL8'NSI',CL10'CANTV'                                       
         DC    C'AR',CL8'ARB',CL10'RADIO'                                       
         DC    C'RR',CL8'RADAR',CL10'RADIO'                                     
         DC    C'MR',CL8'BBRADIO',CL10'RADIO'                                   
         DC    C'NR',CL8'NTI',CL10'RADIO'     - THIS IS DEAD                    
         DC    C'AT',CL8'ARB',CL10'USTV'                                        
         DC    C'NT',CL8'NSI',CL10'USTV'                                        
         DC    C'MT',CL8'MFX',CL10'USTV'      - THIS IS DEAD                    
         DC    C'ST',CL8'SQAD',CL10'USTV'                                       
         DC    C'NA',CL8'NTI',CL10'NAD'                                         
         DC    C'NN',CL8'NTI',CL10'NETTV'                                       
         DC    C'NB',CL8'NTI',CL10'CABLE'                                       
         DC    C'NW',CL8'NSI',CL10'WEEKLY'                                      
         DC    C'HN',CL8'NHT',CL10'NETTV'                                       
         DC    C'HW',CL8'NHT',CL10'WEEKLY'                                      
         DC    C'SR',CL8'SQAD',CL10'RADIO'                                      
         DC    C'NU',CL8'NSI',CL10'COUNTY'                                      
         DC    C'NO',CL8'NSI',CL10'OVRNIGHT'                                    
         DC    C'FT',CL8'FUS',CL10'USTV'                                        
SRCMEDS  EQU   (*-SRCMEDT)/L'SRCMEDT                                            
         SPACE 2                                                                
SRCTAB   DS    0CL9                                                             
         DC    C'A',CL8'ARB'                                                    
         DC    C'N',CL8'NSI'                                                    
         DC    C'M',CL8'MDF'                                                    
         DC    C'B',CL8'BBM'                                                    
         DC    C'C',CL8'CSI'                                                    
         DC    C'S',CL8'SRC'                                                    
SOURCES  EQU   (*-SRCTAB)/L'SRCTAB                                              
         SPACE 2                                                                
LOOKTAB  DS    0CL11                                                            
         DC    AL1(002),CL10'2 MISC'                                            
         DC    AL1(003),CL10'3 WOMEN'                                           
         DC    AL1(004),CL10'4 MEN'                                             
         DC    AL1(005),CL10'5 VIEWERS'                                         
         DC    AL1(006),CL10'6 LOH'                                             
         DC    AL1(007),CL10'7 WWORK'                                           
         DC    X'FF',CL10'DEFAULT'                                              
LOOKUPS  EQU   (*-LOOKTAB)/L'LOOKTAB                                            
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
         DC    AL1(4),X'10',CL20'S7MIN=YES',CL20'S7MIN=NO'                      
         DC    AL1(4),X'08',CL20'E7MIN=YES',CL20'E7MIN=NO'                      
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
         DC    CL2'NH',CL30'H-DDS GENERATED 1995/6'                             
         DC    CL2'NM',CL30'M-NSI 1978 '                                        
         DC    CL2'NN',CL30'N-NSI 1991 '                                        
         DC    CL2'NO',CL30'O-NSI 1992 '                                        
         DC    CL2'NP',CL30'P-PEPSI SPECIAL 419/420'                            
         DC    CL2'NQ',CL30'Q-NSI 1993 '                                        
         DC    CL2'NR',CL30'R-NSI 1995 '                                        
         DC    CL2'NT',CL30'T-NSI 1999 '                                        
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
         DS    0H                                                               
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
         DC    C'AD18-34'          14                                           
SVITYPES EQU   (*-SVITYPE)/L'SVITYPE                                            
         EJECT                                                                  
TEMPD    DSECT                                                                  
GLOBAL   DS    800C                                                             
GLOBEND  DS    0C                                                               
         ORG   GLOBAL                                                           
* GLOBAL VARIABLES                                                              
CONSRCMD DS    0CL2                SOURCE/MEDIA                                 
CONSRC   DS    C                                                                
CONMED   DS    C                                                                
CONAGY   DS    CL2                                                              
CONLUC   DS    C                                                                
CONCLT   DS    CL3                                                              
CONBOOK  DS    CL2                                                              
CONHFIL  DS    C                                                                
PRTSRC   DS    CL8                                                              
PRTMED   DS    CL10                                                             
PRTAGY   DS    CL8                                                              
PRTLUC   DS    C                                                                
PRTCLT   DS    CL7                                                              
PRTBOOK  DS    CL6                                                              
PRTHFIL  DS    CL30                                                             
PRTFIL   DS    CL12                                                             
PRTCODE  DS    CL10                                                             
PKEY     DS    CL25                                                             
ANYDATA  DS    C                                                                
DEMCODE  DS    0CL3                                                             
         DS    C                                                                
DEMMOD   DS    C                                                                
DEMNUM   DS    C                                                                
DCONFRST DS    C                                                                
STRTDAT3 DS    XL3                 BINARY START DATE                            
ENDDAT3  DS    XL3                 BINARY END DATE                              
         DS    0F                                                               
DEMAREA  DS    CL256                                                            
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
ROPPRSRC DS    C                                                                
ROPPRMED DS    C                                                                
         SPACE 2                                                                
* STATION CALL LETTER CHANGES REPORT VARIABLES                                  
ROW      DS    C                                                                
COL      DS    C                                                                
PCALL1   DS    CL5                                                              
PCALL2   DS    CL5                                                              
PASTK1   DS    CL5                                                              
PASTK2   DS    CL5                                                              
SORTCT   DS    F                                                                
SVCNT    DS    F                                                                
STAPRSRC DS    C                                                                
STAPRMED DS    C                                                                
SVCALLCT DS    F                                                                
STATLINK DS    CL96                                                             
STACALLT DS    45000C                                                           
         EJECT                                                                  
* ++INCLUDE DEDBLOCK                                                            
* ++INCLUDE CTREPWORKD                                                          
* ++INCLUDE CTREPMODES                                                          
* ++INCLUDE CTGENFILE                                                           
* ++INCLUDE DDACTIVD                                                            
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE CTREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
         PRINT ON                                                               
CTDNAME  DSECT                                                                  
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
*                                                                               
PDNAME   DSECT                     PRINT DSECT                                  
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
**PAN#1  DC    CL21'018CTREP9102 05/08/14'                                      
         END                                                                    
