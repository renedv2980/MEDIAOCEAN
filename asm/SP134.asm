*          DATA SET SP134      AT LEVEL 073 AS OF 11/13/13                      
*PHASE SP134A                                                                   
*INCLUDE BRDMON                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE LOGON                                                                  
*INCLUDE LOGOC                                                                  
*INCLUDE MEDGET                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE STXITER                                                                
*                                                                               
*===============================================================                
* 03AUG09 AKAT SUPPORT CLIENTS=MEDIA CARD TO REPORT EST BUCKET                  
*              TOTALS AND BUY GROSS TOTALS AT THE END                           
*              MOVE SOME CODE FOR MORE ADDRESSABILITY                           
* 18JUN09 AKAT NEW NOCARDS CARD                                                 
*              CHANGE DSPACE=T TEST TO DSPACE=                                  
*              MAKE SURE WE HAVE A WRITE=CARD                                   
* 13DEC01 MHER CHANGE TO PACKED BUCKETS                                         
*===============================================================                
SP134    TITLE 'SP134 - UPDATE ESTIMATE HEADERS FROM SPTFILE'                   
SP134    CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,**SP134,AREGSAVE                                               
*                                                                               
         LA    RA,2048(,RB)                                                     
         LA    RA,2048(,RA)                                                     
         USING SP134+4096,RA                                                    
*                                                                               
         L     RC,=A(SP134WRK)                                                  
         USING SP134WRK,RC                                                      
         ST    RD,SP134RD                                                       
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         XC    WORK,WORK                                                        
         ST    RB,WORK                                                          
         LAY   R4,REGSAVE                                                       
         AHI   R4,20000                                                         
         ST    R4,WORK+4                                                        
         OI    WORK+4,X'80'                                                     
         GOTO1 =V(STXITER),DMCB,WORK                                            
*                                                                               
         BRAS  RE,INIT              CALL INITIALIZATION ROUTINES                
*                                                                               
         CLI   NOCARDS,C'Y'        DO WE HAVE ANY CARDS TO PROCESS?             
         BE    ENDRPT              NO - EXIT!                                   
*                                                                               
SP2      ZAP   LINE,=P'99'                                                      
         MVI   EOFSW,0             RESET NEXT REQUEST                           
         ZAP   PAGE,=P'1'                                                       
*                                                                               
         CLI   SPOTCARD,C'N'       TEST SPOT CARD NOT PRESENT                   
         BE    SP4                 THEN FIRST 34 CARD HAS BEEN READ             
*                                                                               
         MVC   CARD,SPACES                                                      
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    ENDRPT                                                           
*                                                                               
SP4      MVI   SPOTCARD,C'*'      MAKE IT NOT AN 'N' ANYMORE                    
         MVC   AGYALPHA,ZAGY      GET AGY ALPHA CODE                            
         MVC   MEDALPHA,ZMED      GET MEDIA ALPHA CODE                          
         XC    WORK,WORK                                                        
         GOTO1 =V(MEDGET),DMCB,(MEDALPHA,AGYALPHA),V(DATAMGR),WORK              
         MVC   BAGYMD,WORK                                                      
         XC    WORK,WORK                                                        
         CLI   DM3,X'FF'           IS THIS A VALID A/M?                         
         BNE   SP10                 YES.                                        
*                                                                               
INVAM    MVC   P+10(7),=CL7'BAD A/M'                                            
         GOTO1 =V(PRINTER)                                                      
         B     INVALREQ                                                         
*                                                                               
AREGSAVE DC    A(REGSAVE)                                                       
*                                                                               
ENDRPT   CLI   PRTTOTAL,C'Y'                                                    
         BNE   ENDRPTX                                                          
         MVC   P(13),=C'EST TOTALS = '                                          
         EDIT  (P8,ALLEST),(15,P+14),2,COMMAS=YES,MINUS=YES                     
         GOTO1 =V(PRINTER)                                                      
         MVC   P(13),=C'BUY TOTALS = '                                          
         EDIT  (P8,ALLBUY),(15,P+14),2,COMMAS=YES,MINUS=YES                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
ENDRPTX  L     RD,SP134RD                                                       
         XBASE                                                                  
*                                                                               
SP10     CLI   BAGYMD,X'10'        A/M MUST BE GT X'10'                         
         BNH   INVAM                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         MVC   KEY+1(2),AGYALPHA                                                
         BAS   RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   AGYRDERR                                                         
         LAY   R6,AGYREC                                                        
         ST    R6,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         BNE   AGYRDERR                                                         
         MVC   AGYDA,KEY+14                                                     
         USING AGYHDRD,R6                                                       
         MVC   SVAPROF7,AGYPROF+7                                               
         DROP  R6                                                               
*                                                                               
         MVC   TITLE+20(14),=C'AGENCY=XX,MED='                                  
         MVC   TITLE+20+7(2),AGYALPHA                                           
         MVC   TITLE+20+14(1),ZMED                                              
         XC    BCLT,BCLT                                                        
         MVI   BPRD,0                                                           
         MVI   BEST,0                                                           
         CLC   ZCLT,SPACES         WAS CLIENT ENTERED                           
         BNE   SP12                YES                                          
         CLC   ZPRD,SPACES         PRD MUST BE BLANK                            
         BNE   MISCLTER                                                         
         CLC   ZEST,SPACES         EST MUST BE BLANK                            
         BE    SP30                                                             
         B     MISCLTER                                                         
         EJECT                                                                  
* CHECK CLTHDR                                                                  
*                                                                               
SP12     GOTO1 CLPACK,DMCB,ZCLT,BCLT                                            
         MVC   BUYKCLT,BCLT                                                     
         BAS   RE,GETCLT                                                        
         BNE   REQCLTER                                                         
         MVC   TITLE+35(5),=C',CLT='                                            
         MVC   TITLE+40(3),ZCLT                                                 
*                                                                               
         CLC   ZPRD,SPACES         WAS PRODUCT ENTERED                          
         BE    SP18                 NO                                          
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),ZPRD                                                    
         BAS   RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PRDHDRER                                                         
         LA    R0,220                                                           
         LAY   R1,CLTREC                                                        
         AHI   R1,CLIST-CLTHDRD                                                 
*                                                                               
SP14     CLC   ZPRD,0(R1)                                                       
         BE    SP16                                                             
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,SP14                                                          
         B     PRDCDER                                                          
*                                                                               
SP16     MVC   BPRD,3(R1)                                                       
         MVC   TITLE+43(5),=C',PRD='                                            
         MVC   TITLE+48(3),ZPRD                                                 
*                                                                               
SP18     CLC   ZEST,SPACES         WAS ESTIMATE ENTERED                         
         BE    SP30                NO                                           
         MVC   WORK(3),=CL3'000'   CK TO BE NUMERIC                             
         MVN   WORK(3),ZEST                                                     
         CLC   WORK(3),ZEST                                                     
         BNE   ESTNUMER                                                         
         PACK  DUB,ZEST(3)                                                      
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    REQESTER                                                         
         STC   R0,BEST                                                          
         CHI   R0,255                                                           
         BH    REQESTER                                                         
         EJECT                                                                  
* FIND BUYS AND EST HDR FOR REQ EST                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),BAGYMD                                                    
         BAS   RE,HIGH                                                          
*                                                                               
SP20     CLC   KEY(3),KEYSAVE                                                   
         BNE   ESTBUYER                                                         
         CLC   KEY+9(1),BEST                                                    
         BE    SP22                                                             
         BAS   RE,SEQ                                                           
         B     SP20                                                             
*                                                                               
SP22     XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD                                                  
         BAS   RE,HIGH                                                          
SP24     CLC   KEY(4),KEYSAVE                                                   
         BNE   ESTHDRER                                                         
         CLC   KEY+7(1),BEST                                                    
         BE    SP26                                                             
         BAS   RE,SEQ                                                           
         B     SP24                                                             
*                                                                               
SP26     MVC   TITLE+51(5),=C',EST='                                            
         MVC   TITLE+56(3),ZEST                                                 
*                                                                               
SP30     XC    KEY,KEY                                                          
         MVC   KEY(3),BAGYMD       AGENCY/MEDIA, CLIENT                         
         BAS   RE,HIGH                                                          
*                                                                               
         CLC   KEY(1),KEYSAVE      RIGHT A/M?                                   
         BNE   NODATAER                                                         
*                                                                               
SP32     OC    BCLT,BCLT                                                        
         BZ    SP34                                                             
         CLC   BCLT,KEY+1                                                       
         BNE   NODATAER                                                         
*                                                                               
SP34     XC    DMCB(12),DMCB                                                    
         GOTO1 =V(SORTER),DMCB,A(SORTCARD),A(RECCARD)                           
         B     SP42                                                             
         EJECT                                                                  
SP40     BAS   RE,SEQ                                                           
         B     SP42                                                             
*                                                                               
SP41     BAS   RE,HIGH                                                          
*                                                                               
SP42     CLC   KEY(1),KEYSAVE      RIGHT A/M?                                   
         BNE   SP100                NO. GET NEXT A/M                            
         CLI   KEY+10,0                                                         
         BNE   SP40                SKIP PASSIVE POINTERS                        
*                                                                               
         OC    BCLT,BCLT           ONLY 1 CLIENT                                
         BZ    SP46                                                             
*                                                                               
         CLC   BCLT,KEY+1          THIS IT                                      
         BNE   SP100                NO, END OF REQUEST                          
*                                                                               
         CLI   BEST,0              ANY EST REQ                                  
         BE    SP46                NO                                           
         CLC   KEY+9(1),BEST       REQUESTED EST                                
         BE    SP46                YES                                          
         BH    SP44                                                             
         MVC   KEY+9(1),BEST       READ FOR ESTIMATE                            
         XC    KEY+10(3),KEY+10                                                 
         B     SP41                GO READ HIGH                                 
*                                                                               
SP44     MVC   KEY+9(4),=4X'FF'    READ FOR NEXT STATION                        
         B     SP41                                                             
*                                                                               
SP46     LA    R0,REC                                                           
         ST    R0,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         MVC   SVKEYID,KEY+12                                                   
*                                                                               
         BAS   RE,BLD             BUILD SORT RECORDS                            
         BNE   SP40                 ERROR, BYPASS REC                           
*                                                                               
         AP    TOTCTR,=P'1'                                                     
*                                                                               
         LAY   R8,SRTBUFF                                                       
         USING SRTRECD,R8                                                       
*                                                                               
SP50     MVI   DUB,0               PUT RECORDS TO SORT                          
         LA    R4,SRTEL                                                         
         CLI   0(R4),0             TEST NO BUCKETS                              
         BE    SP58                                                             
*                                                                               
SP52     LA    R0,4                                                             
         LA    R1,2(R4)                                                         
*                                                                               
SP54     CP    0(6,R1),=P'0'                                                    
         BE    SP56                                                             
         MVI   DUB,C'$'            SET ACTIVITY FLAG                            
         B     *+12                                                             
*                                                                               
SP56     LA    R1,6(R1)                                                         
         BCT   R0,SP54                                                          
*                                                                               
SP58     LA    R4,L'SRTBCKTS(R4)                                                
         CLI   0(R4),0                                                          
         BNE   SP52                                                             
*                                                                               
         CLI   DUB,0               TEST ACTIVITY                                
         BE    SP60                NO                                           
*                                                                               
* R4 POINTS BEYOND LAST SET OF BUCKETS                                          
*                                                                               
         SR    R4,R8                                                            
         LA    R4,1(R4)            ADD X'00' TO REC FOR E-O-R FLAG              
         CHI   R4,400              DOES LENGTH EXCEED MAX                       
         BL    *+6                  YES                                         
         DC    H'0'                                                             
         STH   R4,SRTLEN                                                        
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R8)                                     
*                                                                               
SP60     LA    R8,SRTRECLN(,R8)                                                 
         C     R8,=A(SRTBUFFX)                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   SRTKEY,0            TEST MORE RECS                               
         BNE   SP50                                                             
         B     SP40                                                             
         EJECT                                                                  
SP100    LAY   RE,HEAD                                                          
         MVC   MID1(110),0(RE)                                                  
         ZAP   LINE,=P'99'                                                      
         MVI   FIRSTSW,C'F'        RESET SWITCH                                 
*                                                                               
         LAY   R6,ESTREC                                                        
         USING ESTHDRD,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
*                                                                               
         CLI   BPRD,0              PROD REQUESTED                               
         BE    *+10                                                             
         MVC   KEY+4(3),ZPRD                                                    
*                                                                               
         BAS   RE,HIGH                                                          
*                                                                               
SP102    CLC   KEY(2),KEYSAVE      ANY EST HDRS THIS AGENCY/MEDIA               
         BE    *+6                                                              
         DC    H'0'                NO EST HDRS                                  
*                                                                               
         OC    BCLT,BCLT           WAS REQUEST FOR 1 CLT                        
         BZ    SP110               NO, SKIP EST CK ALSO                         
*                                                                               
         CLC   BCLT,KEY+2                                                       
         BE    *+6                 NO EST HDRS                                  
         DC    H'0'                                                             
*                                                                               
         CLI   BPRD,0              WAS REQUEST 1 PROD                           
         BE    SP104                NO                                          
         CLC   ZPRD,KEY+4          THIS THE PROD                                
         BE    SP104                                                            
         BAS   RE,SEQ                                                           
         B     SP102                                                            
*                                                                               
SP104    CLI   BEST,0              WAS REQUEST 1 EST                            
         BE    SP110                NO                                          
         CLC   BEST,KEY+7          THIS THE EST                                 
         BE    SP110                                                            
         BAS   RE,SEQ                                                           
         B     SP102                                                            
*                                                                               
SP110    L     R8,ASVREC                                                        
         USING SRTRECD,R8                                                       
         BAS   RE,GETSORT                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   EKEYAM-ESTHDR+KEY,BAGYMD                                         
         MVC   EKEYCLT-ESTHDR+KEY,BCLT                                          
         BAS   RE,GEST            GO GET ESTIMATE HEADER                        
         CLC   KEY+1(7),SRTKEY    SET UP TO AVOID ANY TOTALS BREAK              
         BL    SP112                                                            
         MVC   SVKEY,SRTKEY                                                     
         B     SP120                                                            
*                                                                               
SP112    MVC   SVKEY(7),KEY+1                                                   
         EJECT                                                                  
* PROCESS SORTED BUYS INTO EST HDRS                                             
*                                                                               
SP120    CLC   EKEYAM(7),SRTAGMD   A/M/CLT/PRD/EST                              
         BL    SP140               GO WRITE OUT EST HDR, AND PRINT NEW          
         BH    SP160                NO EST HDR FOR THIS BUY                     
*                                                                               
         CLI   SRTKEY,255          END OF SORTED RECS                           
         BNE   SP122                                                            
         BAS   RE,ENDEST                                                        
*                                                                               
         CLI   EOFSW,255           AT END OF REQ                                
         BE    SP2                 YES, GET NEXT                                
         DC    H'0'                                                             
*                                                                               
SP122    CLC   SVKEY,SRTKEY                                                     
         BE    SP124                                                            
         BAS   RE,ENDSTA                                                        
*                                                                               
SP124    CLI   KEY,255             AT END OF FILE                               
         BE    SP150               YES PRINT TOTALS                             
         CLI   ESTSW,0                                                          
         BNE   SP130                                                            
         BRAS  RE,CLREST           CLEAR EST HDR, ADD TO TOTALS                 
*                                                                               
* POST BUYS TO EST BUCKETS                                                      
*                                                                               
SP130    LA    R4,SRTBCKTS                                                      
         USING SRTEL,R4                                                         
*                                                                               
* TEST BUY TO BE WITHIN ESTIMATE DATES *                                        
*                                                                               
         GOTO1 =V(DATCON),DMCB,(2,SRTSTDT),(0,WORK)                             
*                                                                               
         CLC   WORK(6),ESTART                                                   
         BL    SP132                                                            
         GOTO1 (RF),(R1),(2,SRTENDT),(0,WORK)                                   
*                                                                               
         CLC   WORK(6),EEND                                                     
         BNH   SP136                                                            
*                                                                               
* BUY DATE OUTSIDE ESTIMATE HEADER START/END DATES                              
*                                                                               
SP132    BAS   RE,PBUY                                                          
         MVC   P+40(9),=C'* ERROR *'                                            
         MVC   P+50(26),=CL26'BUY DATE NOT IN EST DATES('                       
*                                                                               
         MVC   P+76(6),ESTART                                                   
         MVI   P+82,C'-'                                                        
         MVC   P+83(6),EEND                                                     
         MVI   P+89,C')'                                                        
         GOTO1 =V(DATCON),DMCB,(2,SRTSTDT),(5,P+92)                             
         GOTO1 (RF),(R1),(2,SRTENDT),(5,P+102)                                  
         CLI   WRITESW,C'Y'                                                     
         BNE   *+18                                                             
         CLI   CARD+61,C'Y'                                                     
         BNE   *+10                                                             
         MVC   P+96(7),=CL7'DELETED'                                            
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD+61,C'Y'        IF NO DELETE, COUNT                          
         BNE   SP136                                                            
         CLI   WRITESW,C'Y'                                                     
         BNE   SP138               BYPASS THIS SET                              
         B     SP164                                                            
*                                                                               
SP136    SR    R5,R5                                                            
         IC    R5,SRTEL+1          GET MONTH                                    
         BCTR  R5,0                                                             
         MHI   R5,6                GIVES DSPL                                   
*                                                                               
         LA    RE,EORD(R5)         ORD GROSS                                    
         AP    0(6,RE),SRTELOG                                                  
*                                                                               
         LA    RE,EORDNET(R5)      ORD NET                                      
         AP    0(6,RE),SRTELON                                                  
*                                                                               
         LA    RE,EPAID(R5)        PAID GROSS                                   
         AP    0(6,RE),SRTELPG                                                  
*                                                                               
         LA    RE,EPDNET(R5)       PAID NET                                     
         AP    0(6,RE),SRTELPN                                                  
*                                                                               
         LA    R4,L'SRTBCKTS(R4)   NEXT BUCKET SET                              
         CLI   0(R4),0                                                          
         BNE   SP136                                                            
*                                                                               
* ACCUMULATE BUY TOTALS INTO EST HDR, AND PRINT DETAIL IF REQUESTED             
*                                                                               
         BAS   RE,FMTLIN                                                        
SP138    MVC   SVKEY,SRTKEY                                                     
*                                                                               
         BAS   RE,GETSORT                                                       
*                                                                               
         B     SP120                                                            
         DROP  R4                                                               
         EJECT                                                                  
* SEE IF TOTALS BREAK                                                           
*                                                                               
SP140    BAS   RE,ENDSTA           ONLY PRINT STATION TOTALS                    
*                                                                               
SP142    CLI   ESTSW,0                                                          
         BNE   SP144                                                            
         BRAS  RE,CLREST           CLEAR EST HDR, ADD TO TOTALS                 
*                                                                               
SP144    BAS   RE,TESTPRT                                                       
*                                                                               
         MVC   KEY(13),EKEY        WRITE ESTIMATE HEADER                        
         MVI   KEY+13,0                                                         
         MVC   KEY+14(4),ESTDA                                                  
*                                                                               
         CLI   WRITESW,C'Y'                                                     
         BNE   SP148                                                            
*                                                                               
         LA    RF,REC                                                           
         ST    RF,AIOAREA                                                       
         BAS   RE,GETREC                                                        
*                                                                               
         LAY   RF,ESTREC                                                        
         ST    RF,AIOAREA                                                       
*                                                                               
SP146    BAS   RE,WRITE                                                         
         BNE   ESWTERR                                                          
         AP    WRTCTR,=P'1'                                                     
*                                                                               
SP148    MVC   SVKEY(7),EKEYAM                                                  
         XC    SVKEY+7(3),SVKEY+7                                               
         BAS   RE,GEST                                                          
*                                                                               
SP150    LA    R2,EKEYAM                                                        
*                                                                               
         CLI   EKEYAM,255          AT END OF FILE FOR EST                       
         BNE   *+8                                                              
         LA    R2,SRTKEY                                                        
*                                                                               
         BAS   RE,ENDEST                                                        
*                                                                               
         CLI   EOFSW,255           AT END OF REQ                                
         BE    SP2                 YES, GET NEXT                                
         B     SP120                                                            
         EJECT                                                                  
* PROCESS UNMATCHED BUYS (NO EST HDR)                                           
*                                                                               
SP160    CLC   SVKEY,SRTKEY                                                     
         BE    SP162                                                            
         BAS   RE,ENDSTA                                                        
         MVC   SVKEY+7(5),SRTKEY+7 SAVE SORT KEY                                
         CLC   SVKEY,SRTKEY                                                     
         BE    SP162                                                            
         LA    R2,SRTKEY                                                        
         BAS   RE,ENDEST                                                        
         CLI   EOFSW,255           AT END OF REQ                                
         BE    SP2                 YES, GET NEXT                                
*                                                                               
SP162    BAS   RE,PBUY                                                          
*                                                                               
         MVC   PPTR+10(18),=C'NO EST HDR FOR BUY'                               
         GOTO1 =V(DATCON),DMCB,(2,SRTSTDT),(5,PPTR+31)                          
         GOTO1 (RF),(R1),(2,SRTENDT),(5,PPTR+41)                                
*                                                                               
         CLI   CARD+61,C'Y'                                                     
         BNE   SP164                                                            
         CLI   WRITESW,C'Y'                                                     
         BNE   SP164                                                            
*                                                                               
         MVC   PPTR+60(7),=CL7'DELETED'                                         
*                                                                               
SP164    GOTO1 =V(PRINTER)                                                      
         MVC   SVKEY,SRTKEY                                                     
         AP    BUYERCTR,=P'1'                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),SRTAGMD AND BCLT                                          
         MVC   KEY+3(1),SRTPTRPR                                                
         MVC   KEY+4(2),SRTMKT                                                  
         MVC   KEY+6(3),SRTSTA                                                  
         MVC   KEY+9(1),SRTEST                                                  
         MVC   KEY+11(1),SRTLIN                                                 
         MVC   KEY+12(1),SRTID                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    SP166                                                            
         MVC   PPTR+10(13),=CL13'BUY NOT FOUND'                                 
         GOTO1 =V(PRINTER)                                                      
         MVC   PPTR+10(3),=C'KEY'                                               
         GOTO1 =V(HEXOUT),DMCB,KEYSAVE,PPTR+20,13,=C'MIX'                       
         GOTO1 =V(PRINTER)                                                      
         MVC   PPTR+10(8),=C'SORT KEY'                                          
         GOTO1 =V(HEXOUT),DMCB,SRTKEY,PPTR+20,25,=C'MIX'                        
         GOTO1 =V(PRINTER)                                                      
         B     SP168                                                            
*                                                                               
SP166    LA    R0,REC                                                           
         ST    R0,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CARD+61,C'Y'                                                     
         BNE   SP168                                                            
         CLI   WRITESW,C'Y'                                                     
         BNE   SP168                                                            
*                                                                               
         BAS   RE,DELREC                                                        
         BE    SP168                                                            
         MVC   PPTR(14),=CL14'WRT ERR-DELETE'                                   
         MVC   WORK(1),DMCB+8                                                   
         GOTO1 =V(HEXOUT),DMCB,WORK,PPTR+23,1,=C'MIX'                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
SP168    GOTO1 =V(PRINTER)                                                      
*                                                                               
         BAS   RE,GETSORT                                                       
*                                                                               
         B     SP120                                                            
         DROP  R6                                                               
         EJECT                                                                  
GETSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     R6,4(R1)                                                         
         LTR   R6,R6                                                            
         BNZ   GETSORTB                                                         
* E-O-F -- SET SVREC TO X'FF'                                                   
         CLI   FIRSTSW,0           TEST FIRST TIME                              
         BE    GETSORTA             NO                                          
         CP    TOTCTR,=P'0'        ANY RECS READ                                
         BH    GETSORTA             YES, ALL MUST HAVE BEEN ZERO                
*                                                                               
         MVC   P(13),=C'NO INPUT RECS'                                          
         GOTO1 =V(PRINTER)                                                      
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
         ABEND 999                                                              
*                                                                               
GETSORTA L     R6,ASVREC                                                        
         MVI   0(R6),X'FF'                                                      
         MVC   1(200,R6),0(R6)                                                  
         B     EXIT                                                             
*                                                                               
* MOVE SORT REC TO SVREC SO CAN SEE IT IN DUMPS                                 
*                                                                               
GETSORTB LH    R7,0(R6)            GET 'FROM' REC LEN                           
         L     RE,ASVREC           SET 'TO' REC ADDR                            
         LA    RF,2(R7)            SET 'TO' LEN = 'FROM' LEN + 2                
         MVCL  RE,R6                                                            
         MVI   FIRSTSW,0           RESET SWITCH                                 
         L     R8,ASVREC                                                        
         USING SRTRECD,R8                                                       
         CLI   SRTEST,0            EST ZERO?                                    
         BNE   EXIT                                                             
* PRINT ERROR FOR BUY WITHOUT ESTIMATE NUMBER                                   
         BAS   RE,PBUY                                                          
         MVC   P+40(9),=C'* ERROR *'                                            
         MVC   P+50(8),=CL8'EST ZERO'                                           
         GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
         EJECT                                                                  
* GET CLIENT HEADER                                                             
*                                                                               
GETCLT   NTR1                                                                   
         MVC   WORK(L'KEY),KEY                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BUYKCLT                                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETCLTEX                                                         
         LAY   RE,CLTREC                                                        
         ST    RE,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         BNE   GETCLTEX                                                         
         MVC   CLTDA,KEY+14                                                     
         MVC   KEY,WORK                                                         
         BAS   RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
GETCLTEX B     EXIT                                                             
         EJECT                                                                  
*========================================================                       
* READ ESTIMATE HEADER                                                          
*========================================================                       
         SPACE 1                                                                
GEST     NTR1                                                                   
         BAS   RE,HIGH                                                          
*                                                                               
GEST10   BAS   RE,SEQ                                                           
         CLI   KEY,0               IF NOT, AT END                               
         BNE   GEST30               FORCE END                                   
*                                                                               
         CLC   KEY+1(1),BAGYMD     AT END?                                      
         BNE   GEST30               YES                                         
*                                                                               
         CLI   KEY+7,0             ESTIMATE NUMBER                              
         BE    GEST10               NO, BYPASS                                  
*                                                                               
         CLI   KEY+8,0             BILL REC                                     
         BNE   GEST10               YES, BYPASS                                 
*                                                                               
         OC    BCLT,BCLT           WAS REQ BY CLT                               
         BZ    GEST20               NO                                          
         CLC   BCLT,KEY+2          SAME CLT                                     
         BNE   GEST30               NO, STOP SEARCH                             
*                                                                               
         CLI   BPRD,0              WAS REQ BY PROD                              
         BE    GEST16               NO                                          
         CLC   ZPRD,KEY+4          REQUESTED PROD                               
         BNE   GEST10               NO                                          
*                                                                               
GEST16   CLI   BEST,0              WAS REQ BY EST                               
         BE    GEST20               NO                                          
         CLC   BEST,KEY+7          REQUESTED EST                                
         BNE   GEST10               NO                                          
*                                                                               
GEST20   LAY   R6,ESTREC                                                        
         ST    R6,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         BNE   ESTRDERR                                                         
*                                                                               
         USING ESTHDRD,R6                                                       
         MVC   ESTDA,KEY+14        SAVE DISK ADDRESS                            
         AP    ESTCTR,=P'1'                                                     
*                                                                               
* SAVE EXISTING BUCKETS                                                         
*                                                                               
         MVC   SVEORD(78),EORD                                                  
         MVC   SVEORDN(78),EORDNET                                              
         MVC   SVEPAID(78),EPAID                                                
         MVC   SVEPAIDN(78),EPDNET                                              
*                                                                               
* CLEAR ALL CTRS                                                                
*                                                                               
         LA    R1,EORD                                                          
         LA    R0,26                                                            
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         LA    R1,EPAID                                                         
         LA    R0,26                                                            
         ZAP   0(6,R1),=P'0'                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         MVI   ESTSW,0             SET ESTSW TO NOT PROCESSED                   
         B     EXIT                                                             
*                                                                               
GEST30   MVI   EKEY,255                                                         
         MVC   EKEY+1(12),EKEY                                                  
         MVC   KEY(13),EKEY                                                     
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
ENDSTA   NTR1                                                                   
         CLI   DTLSW,C'Y'          PRINTING DETAIL                              
         BNE   ENDSTA04            NO                                           
         CP    STACTR,=P'1'                                                     
         BNH   ENDSTA04                                                         
         CLC   TOTSTA(32),=4PL8'0'                                              
         BNE   ENDSTA06                                                         
*                                                                               
ENDSTA04 MVI   PRTOTSW,1           SUPPRESS PRINTING                            
*                                                                               
ENDSTA06 ZAP   STACTR,=P'0'                                                     
         LA    R3,4                                                             
         LA    R4,TOTSTA                                                        
         BAS   RE,FMTTOT2                                                       
         MVC   P+17(115),SPACES                                                 
         B     EXIT                                                             
         SPACE 1                                                                
*=============================================================                  
* ESTIMATE TOTALS                                                               
*=============================================================                  
         SPACE 1                                                                
ENDEST   NTR1                                                                   
*                                                                               
* SEE IF EST/BUY TOTALS ARE EQUAL                                               
*                                                                               
ENDEST04 MVI   PRTOTSW,0                                                        
         MVC   PPRD,SVKEY+3                                                     
*                                                                               
         CLI   PRTCLTSW,C'N'                                                    
         BE    ENDEST20                                                         
*                                                                               
         CLI   DTLSW,C'Y'          PRINTING DETAIL                              
         BE    ENDEST10                                                         
*                                                                               
         LA    R0,4                                                             
         LA    RE,TOTESTP                                                       
         LA    RF,TOTEST                                                        
*                                                                               
ENDEST06 CP    0(8,RE),0(8,RF)     GROSS ORDERED                                
         BNE   ENDEST10                                                         
         CP    16(8,RE),16(8,RF)   GROSS PAID                                   
         BNE   ENDEST10                                                         
*                                                                               
         MVI   PRTOTSW,3           NO PRINTING, IN BALANCE                      
         MVC   PPRD,SPACES                                                      
*                                                                               
ENDEST10 LA    R3,4                                                             
         LA    R4,TOTESTP          PRINT EST HDR                                
         BAS   RE,FMTTOT                                                        
*                                                                               
         CLI   PRTOTSW,2           WERE TOTALS IN BALANCE                       
         BNE   ENDEST22            NO, PRINT THEM                               
*                                                                               
         CLI   UNMATSW,C'Y'        ONLY PRINT UNMATCHED                         
         BE    ENDEST20            YES                                          
*                                                                               
         CLC   TOTEST(32),=4PL8'0'                                              
         BE    ENDEST30                                                         
         B     ENDEST22                                                         
*                                                                               
ENDEST20 MVI   PRTOTSW,1           NO PRINTING                                  
*                                                                               
ENDEST22 LA    R3,4                                                             
         LA    R4,TOTEST           PRINT BUYS                                   
         BAS   RE,FMTTOT                                                        
*                                                                               
ENDEST30 MVI   PRTOTSW,0                                                        
*                                                                               
         CLI   0(R2),255           END OF FILE                                  
         BE    ENDPRD                                                           
*                                                                               
         CLC   SVKEY(6),0(R2)      A/M/C/P                                      
         BE    EXIT                                                             
         EJECT                                                                  
*=============================================================                  
* PRODUCT TOTALS                                                                
*=============================================================                  
         SPACE 1                                                                
* SEE IF EST/BUY TOTALS ARE EQUAL                                               
*                                                                               
ENDPRD   CLI   PRTCLTSW,C'N'       TEST SUPPRESS PRINTING                       
         BE    ENDPRD06                                                         
*                                                                               
         CLC   TOTPRDP(24),TOTPRD                                               
         BNE   ENDPRD04                                                         
         MVI   PRTOTSW,3           NO PRINTING, IN BALANCE                      
*                                                                               
ENDPRD04 LA    R3,4                                                             
         LA    R4,TOTPRDP          PRINT EST HDR                                
         BAS   RE,FMTTOT                                                        
*                                                                               
         CLI   PRTOTSW,2           WERE TOTALS IN BALANCE                       
         BNE   ENDPRD08                                                         
*                                                                               
         CLI   UNMATSW,C'Y'        ONLY PRINT UNMATCHED                         
         BE    ENDPRD06            YES                                          
*                                                                               
         CLC   TOTPRD(32),=4PL8'0'                                              
         BE    ENDPRD10                                                         
         B     ENDPRD08                                                         
*                                                                               
ENDPRD06 MVI   PRTOTSW,1           NO PRINTING                                  
*                                                                               
ENDPRD08 LA    R3,4                                                             
         LA    R4,TOTPRD           PRINT BUYS                                   
         BAS   RE,FMTTOT                                                        
*                                                                               
ENDPRD10 MVI   PRTOTSW,0                                                        
         CLI   BPRD,0              SPECIFIC PRD REQ                             
         BE    ENDPRD20                                                         
*                                                                               
         CLI   BEST,0              WAS REQ BY EST                               
         BNE   ENDREQ                                                           
*                                                                               
ENDPRD20 CLI   0(R2),255           END OF FILE                                  
         BE    ENDCLT                                                           
*                                                                               
         CLC   SVKEY(3),0(R2)      A/M/C                                        
         BE    EXIT                                                             
         EJECT                                                                  
*===========================================================                    
* CLIENT TOTALS - SEE IF EST/BUY TOTALS ARE EQUAL *                             
*===========================================================                    
         SPACE 1                                                                
ENDCLT   CLC   TOTCLTP(24),TOTCLT                                               
         BNE   ENDCLT04                                                         
         MVI   PRTOTSW,3           NO PRINTING, IN BALANCE                      
*                                                                               
ENDCLT04 LA    R3,4                                                             
         LA    R4,TOTCLTP          PRINT EST HDR                                
         AP    ALLEST,0(8,R4)      ADD TO ESTIMATE TOTALS                       
         BAS   RE,FMTTOT                                                        
*                                                                               
         CLI   PRTOTSW,2           WERE TOTALS IN BALANCE                       
         BNE   ENDCLT08            NO                                           
*                                                                               
         OC    BCLT,BCLT           WAS REQ BY CLT                               
         BNZ   ENDCLT08                                                         
*                                                                               
         CLI   PRTCLTSW,C'Y'       PRINT CLIENT TOTALS                          
         BE    ENDCLT08            YES                                          
*                                                                               
         CLI   UNMATSW,C'Y'        ONLY PRINT UNMATCHED                         
         BE    ENDCLT06            YES                                          
*                                                                               
         CLC   TOTCLT(32),=4PL8'0'                                              
         BE    ENDCLT10                                                         
         B     ENDCLT08                                                         
*                                                                               
ENDCLT06 MVI   PRTOTSW,1           NO PRINTING                                  
*                                                                               
ENDCLT08 LA    R3,4                                                             
         LA    R4,SVKEY+1                                                       
         GOTO1 CLUNPK,DMCB,(R4),PCLT                                            
         LA    R4,TOTCLT           PRINT BUYS                                   
         AP    ALLBUY,0(8,R4)      ADD TO BUY TOTALS                            
         BAS   RE,FMTTOT                                                        
         OC    BCLT,BCLT           WAS REQ BY CLT                               
         BNZ   ENDREQ                                                           
         CLI   PRTOTSW,0           NO PRINTING                                  
         BNE   ENDCLT10                                                         
         GOTO1 =V(PRINTER)         SKIP A LINE                                  
*                                                                               
ENDCLT10 MVI   PRTOTSW,0                                                        
*                                                                               
         CLI   0(R2),255           END OF FILE                                  
         BE    ENDMED                                                           
*                                                                               
         CLC   SVKEY(1),0(R2)      A/M                                          
         BE    EXIT                                                             
         EJECT                                                                  
*=============================================================                  
* MEDIA TOTALS - SEE IF EST/BUY TOTALS ARE EQUAL *                              
*=============================================================                  
         SPACE 1                                                                
ENDMED   GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLC   TOTMEDP(24),TOTMED                                               
         BNE   ENDMED04                                                         
         MVI   PRTOTSW,1                                                        
*                                                                               
ENDMED04 LA    R3,4                                                             
         LA    R4,TOTMEDP                                                       
         BAS   RE,FMTTOT                                                        
*                                                                               
         LA    R3,4                                                             
         LA    R4,TOTMED                                                        
         BAS   RE,FMTTOT                                                        
         ZAP   LINE,=P'99'                                                      
         EJECT                                                                  
*=============================================================                  
* END OF REQUEST                                                                
*=============================================================                  
         SPACE 1                                                                
ENDREQ   DS    0H                                                               
         LA    R5,ESTCTR           FIRST CTR SET                                
         LA    R6,NBRCTRS          NUMBER OF CTR SETS                           
*                                                                               
ENDREQ04 EDIT  (P4,(R5)),(10,P+25),COMMAS=YES,ALIGN=LEFT                        
*                                                                               
         ZAP   0(4,R5),=P'0'                                                    
         MVC   P(20),4(R5)         ROW NAME                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R5,24(R5)                                                        
         BCT   R6,ENDREQ04                                                      
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
         LA    R5,TOTESTP                                                       
         LA    R6,9                                                             
*                                                                               
ENDREQ06 LA    R0,4                                                             
         LR    R1,R5                                                            
*                                                                               
ENDREQ08 ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(,R1)                                                        
         BCT   R0,ENDREQ08                                                      
         LA    R5,64(,R5)                                                       
         BCT   R6,ENDREQ06                                                      
*                                                                               
         MVI   EOFSW,255                                                        
         ZAP   LINE,=P'99'                                                      
         B     EXIT                GO FOR NEXT REQUEST                          
         EJECT                                                                  
*=============================================================                  
* R4 POINTS TO ACCUMULATORS                                                     
*=============================================================                  
         SPACE 1                                                                
FMTTOT   NTR1                                                                   
         MVC   P+15(32),32(R4)     ROW NAME                                     
         B     FMTTOT4                                                          
*                                                                               
FMTTOT2  NTR1                                                                   
*                                                                               
FMTTOT4  LA    R5,PORDG                                                         
*                                                                               
FMTTOT6  EDIT  (P8,0(R4)),(15,(R5)),2,COMMAS=YES,MINUS=YES                      
*                                                                               
         CP    0(8,R4),=P'0'                                                    
         BL    *+8                                                              
         MVI   14(R5),C'*'                                                      
*                                                                               
         ZAP   0(8,R4),=P'0'       CLEAR ACCUM                                  
*                                                                               
         LA    R4,8(R4)                                                         
         LA    R5,16(R5)                                                        
         BCT   R3,FMTTOT6                                                       
*                                                                               
         TM    PRTOTSW,1           BYPASS PRINT                                 
         BO    FMTTOT10            YES                                          
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
FMTTOT10 NI    PRTOTSW,X'FF'-X'01'                                              
         MVC   P,SPACES                                                         
         B     EXIT                                                             
         EJECT                                                                  
FMTLIN   NTR1                                                                   
*                                                                               
         CLI   DTLSW,C'Y'          PRINT DETAIL                                 
         BNE   LIN10               NO                                           
*                                                                               
         BAS   RE,PBUY                                                          
*                                                                               
* SUM BUCKETS                                                                   
*                                                                               
LIN10    LA    R4,SRTEL                                                         
         USING SRTEL,R4                                                         
*                                                                               
         LA    R1,PACC1            CLEAR ACCUMS                                 
         LA    R0,4                                                             
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
LIN12    AP    PACC1,SRTELOG                                                    
         AP    PACC2,SRTELON                                                    
         AP    PACC3,SRTELPG                                                    
         AP    PACC4,SRTELPN                                                    
*                                                                               
         LA    R4,26(R4)           NEXT BUCKET SET                              
         CLI   0(R4),0             TEST Y/M                                     
         BNZ   LIN12               MORE                                         
*                                                                               
         CLI   DTLSW,C'Y'          PRINT DETAIL                                 
         BNE   LIN30               NO                                           
         LA    R3,PORDG            PRINT LINE POSITION                          
         LA    R5,PACC1                                                         
         LA    R6,4                BCT                                          
*                                                                               
LIN24    CP    0(8,R5),=P'0'                                                    
         BE    LIN26                                                            
         EDIT  (P8,0(R5)),(15,(R3)),2,COMMAS=YES,MINUS=YES                      
*                                                                               
LIN26    LA    R3,16(R3)                                                        
         LA    R5,8(R5)                                                         
         BCT   R6,LIN24                                                         
*                                                                               
         AP    STACTR,=P'1'                                                     
         GOTO1 =V(PRINTER)                                                      
         DROP  R4                                                               
*                                                                               
* ADD TO STA/EST/PRD/CLT/MED TOTALS                                             
*                                                                               
LIN30    LA    R5,TOTSTA           INDX                                         
         LA    R0,5                SET FOR 5 ROWS                               
*                                                                               
LIN40    AP    0(8,R5),PACC1       OG                                           
         AP    8(8,R5),PACC2       ON                                           
         AP    16(8,R5),PACC3      PG                                           
         AP    24(8,R5),PACC4      PN                                           
         LA    R5,64(R5)           NEXT ACCUMS                                  
         BCT   R0,LIN40            BCT FOR 5 ROWS                               
         B     EXIT                                                             
*                                                                               
CVD      CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         BR    RE                                                               
         EJECT                                                                  
PBUY     NTR1                                                                   
         MVC   PAGY,SRTAGY                                                      
*                                                                               
         LLC   RE,SRTAGMD                                                       
         N     RE,=X'0000000F'                                                  
         IC    RE,MDTAB-1(RE)                                                   
         STC   RE,PMED                                                          
*                                                                               
         LA    R4,SRTCLT                                                        
         GOTO1 CLUNPK,DMCB,(R4),PCLT                                            
*                                                                               
         MVC   PPRD,SRTPROD                                                     
*                                                                               
         LLC   R0,SRTEST                                                        
         BAS   RE,CVD                                                           
         MVC   PEST,WORK+7                                                      
*                                                                               
         MVC   SVMKT,SRTMKT                                                     
         MVC   SVSTA,SRTSTA                                                     
         BAS   RE,PMS              PRINT MARKET/STATION                         
*                                                                               
         LLC   R0,SRTLIN                                                        
         BAS   RE,CVD                                                           
         MVC   PLIN,WORK+7                                                      
*                                                                               
         GOTO1 =V(DATCON),DMCB,(2,SRTSTDT),(5,PDTS)                             
         MVI   PDTS+8,C'-'                                                      
         GOTO1 (RF),(R1),(2,SRTENDT),(5,PDTS+9)                                 
*                                                                               
         LA    R1,SPACES                                                        
         CLC   SRTPRD,SRTPTRPR     TEST ACTIVE PRD                              
         BE    PBUY06                                                           
*                                                                               
         LA    R1,=C'POL'                                                       
         CLI   SRTPTRPR,X'FF'                                                   
         BE    PBUY06                                                           
*                                                                               
         LA    R0,220                                                           
         LAY   R1,CLTREC                                                        
         AHI   R1,CLIST-CLTHDRD                                                 
PBUY04   CLC   SRTPTRPR,3(R1)                                                   
         BE    PBUY06                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,PBUY04                                                        
         LA    R1,=C'???'                                                       
PBUY06   MVC   PPTR,0(R1)                                                       
*                                                                               
         CLC   SRTEST,SRTPTRES                                                  
         BE    EXIT                                                             
         MVI   PPTR+3,C'-'                                                      
         LLC   R0,SRTPTRES                                                      
         BAS   RE,CVD                                                           
         MVC   PPTREST,WORK+7                                                   
         B     EXIT                                                             
         EJECT                                                                  
TESTPRT  NTR1                                                                   
         LAY   R6,ESTREC                                                        
         USING ESTHDRD,R6                                                       
         ZAP   DUB,MAXLINE                                                      
         SP    DUB,LINE                                                         
         CP    DUB,=P'5'                                                        
         BH    *+10                                                             
         ZAP   LINE,=P'99'                                                      
*                                                                               
         MVC   PAGY,AGYALPHA                                                    
         LLC   RE,EKEYAM                                                        
         N     RE,=X'0000000F'                                                  
         IC    RE,MDTAB-1(RE)                                                   
         STC   RE,PMED                                                          
         LA    R4,EKEYCLT                                                       
         GOTO1 CLUNPK,DMCB,(R4),PCLT                                            
*                                                                               
         MVC   PPRD,EKEYPRD                                                     
*                                                                               
         LLC   R0,EKEYEST                                                       
         BAS   RE,CVD                                                           
         MVC   PEST,WORK+7                                                      
         GOTO1 =V(DATCON),P1,(0,ESTART),(5,P+20)                                
         MVI   P+28,C'-'                                                        
         GOTO1 =V(DATCON),P1,(0,EEND),(5,P+29)                                  
         CLC   TOTESTP(24),=4PL8'0'                                             
         BNE   TSTPRT02                                                         
         CLC   TOTEST(32),=4PL8'0'                                              
         BNE   TSTPRT02                                                         
*                                                                               
* DON'T PRINT OUT EST/BUYS ZERO ANY MORE                                        
*                                                                               
*        MVC   P+40(17),=C'EST AND BUYS ZERO'                                   
         CLI   UNMATSW,C'Y'        ONLY PRINT UNMATCHED                         
         BE    TSTPRT40            YES                                          
         MVC   P,SPACES                                                         
*        GOTO1 =V(PRINTER)                                                      
         B     EXIT                                                             
*                                                                               
TSTPRT02 CLI   PRTCLTSW,C'N'       TEST SUPPRESS PRINTING                       
         BE    TSTPRT40                                                         
*                                                                               
         CLC   SVEPAID(78),EPAID   ANY DIFF BETWEEN EST HDR AND BUYS            
         BNE   TSTPRT04                                                         
         CLC   SVEPAIDN(78),EPDNET                                              
         BNE   TSTPRT04                                                         
*                                                                               
         CLC   SVEORD(78),EORD     ANY DIFF BETWEEN EST HDR AND BUYS            
         BNE   TSTPRT04                                                         
         CLC   SVEORDN(78),EORDNET                                              
         BNE   TSTPRT04                                                         
*                                                                               
         CLI   UNMATSW,C'Y'        ONLY PRINT UNMATCHED                         
         BE    TSTPRT40            YES                                          
*                                                                               
         GOTO1 =V(PRINTER)         PRINT PREV LINE                              
         B     TSTPRT06                                                         
*                                                                               
TSTPRT04 GOTO1 =V(PRINTER)         PRINT PREV LINE                              
         MVC   P(10),=10C'*'                                                    
*                                                                               
TSTPRT06 L     RE,=A(MIDMONS)                                                   
         MVC   P+10(100),0(RE)                                                  
         GOTO1 =V(PRINTER)                                                      
         EJECT                                                                  
* PRINT GROSS ORD, NET ORD, PAID, WITH DIFFERENCES                              
*                                                                               
TSTPRT08 CLC   SVEORD(78),EORD     ANY DIFF BETWEEN EST HDR AND BUYS            
         BNE   *+14                NO                                           
         CLC   SVEORDN(78),EORDNET                                              
         BE    TSTPRT20                                                         
*                                                                               
         LA    R4,SVEORD                                                        
         MVC   P(8),=C'GORD YTD'                                                
         MVI   P+9,C'P'                                                         
         BRAS  RE,TESTFMT                                                       
*                                                                               
TSTPRT10 LA    R4,EORD                                                          
         MVC   P(8),=C'GORD YTD'                                                
         BRAS  RE,TESTFMT                                                       
*                                                                               
         CLI   NETSW,C'Y'                                                       
         BNE   TSTPRT20                                                         
         LA    R4,EORDNET                                                       
         MVC   P(8),=C'NORD YTD'                                                
         BRAS  RE,TESTFMT                                                       
*                                                                               
TSTPRT20 CLC   SVEPAID(78),EPAID                                                
         BNE   *+14                                                             
         CLC   SVEPAIDN(78),EPDNET                                              
         BE    TSTPRT30                                                         
*                                                                               
         LA    R4,SVEPAID                                                       
         MVC   P(8),=C'GPAY YTD'                                                
         MVI   P+9,C'P'                                                         
         BRAS  RE,TESTFMT                                                       
*                                                                               
TSTPRT30 LA    R4,EPAID                                                         
         MVC   P(8),=C'GPAY YTD'                                                
         BRAS  RE,TESTFMT                                                       
*                                                                               
         CLI   NETSW,C'Y'                                                       
         BNE   TSTPRT32                                                         
*                                                                               
         LA    R4,EPDNET                                                        
         MVC   P(8),=C'NPAY YTD'                                                
         BRAS  RE,TESTFMT                                                       
*                                                                               
TSTPRT32 B     EXIT                                                             
*                                                                               
TSTPRT40 MVC   P,SPACES                                                         
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
HIGH     NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         CLI   COSW,C'Y'                                                        
         BNE   *+8                                                              
         OI    DMINBTS,X'08'                                                    
         GOTO1 =V(DATAMGR),DMCB,(DMINBTS,=C'DMRDHI'),                  X        
               =C'SPTDIR',KEYSAVE,KEY                                           
         NI    DMINBTS,X'F7'                                                    
         B     PUTRECX                                                          
*                                                                               
SEQ      NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         CLI   COSW,C'Y'                                                        
         BNE   *+8                                                              
         OI    DMINBTS,X'08'                                                    
         GOTO1 =V(DATAMGR),DMCB,(DMINBTS,=C'DMRSEQ'),                  X        
               =C'SPTDIR',KEYSAVE,KEY                                           
         NI    DMINBTS,X'F7'                                                    
         B     PUTRECX                                                          
GETREC   NTR1                                                                   
         LA    RF,=CL7'GETREC'                                                  
         B     PUTREC2                                                          
*                                                                               
PUTREC   NTR1                                                                   
         LA    RF,=CL7'PUTREC'                                                  
         B     PUTREC2                                                          
*                                                                               
DELREC   NTR1                                                                   
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMDEL',=C'SPTFILE',KEY,             C        
               AIOAREA,DMWORK                                                   
         CLI   8(R1),0             SET CC ON EXIT                               
         B     EXIT                                                             
*                                                                               
WRITE    NTR1                                                                   
         LA    RF,=CL7'DMWRT'                                                   
*                                                                               
PUTREC2  ST    RF,DMCB                                                          
         CLI   COSW,C'Y'                                                        
         BNE   *+8                                                              
         OI    DMCB,X'08'                                                       
         GOTO1 =V(DATAMGR),DMCB,,=C'SPTFILE',KEY+14,AIOAREA,DMWORK              
*                                                                               
PUTRECX  CLI   8(R1),0             SET CC ON EXIT                               
         BE    EXIT                                                             
         CLI   COSW,C'Y'           TEST DELETES OK?                             
         BNE   EXIT                                                             
         CLI   8(R1),X'02'         IS IT ONLY DELETE?                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
CLTERR   LAY   R1,CLTREC                                                        
         MVC   0(13,R1),KEYSAVE    FORCE KEY EQUAL                              
         XC    CLTDA,CLTDA                                                      
         XC    ESTDA,ESTDA                                                      
         MVC   P+10(6),=C'CLIENT'                                               
         B     COMERR                                                           
*                                                                               
PRDERR   ST    RE,SVRE                                                          
         GOTO1 CLUNPK,DMCB,BUYREC+1,PCLT                                        
*                                                                               
         LLC   R0,BUYREC+9                                                      
         BAS   RE,CVD                                                           
         MVC   PEST,WORK+7                                                      
*                                                                               
         MVC   SVMKT,BUYREC+4                                                   
         MVC   SVSTA,BUYREC+6                                                   
         BAS   RE,PMS              PRINT MARKET/STATION                         
*                                                                               
         LLC   R0,BUYREC+10                                                     
         BAS   RE,CVD                                                           
         MVC   PLIN,WORK+7                                                      
         GOTO1 =V(PRINTER)                                                      
         L     R0,SVRE                                                          
*                                                                               
         MVC   P(9),=C'* ERROR *'                                               
         MVC   P+10(4),=C'PROD'                                                 
         LA    R8,BUYKEY                                                        
COMERR   MVC   P+17(9),=CL9'NOT FOUND'                                          
         B     ERRX                                                             
*                                                                               
ESTRDERR GOTO1 =V(PRINTER)                                                      
         MVC   P+10(20),=CL20'ESTIMATE READ ERROR *'                            
         LA    R0,ABEND                                                         
         B     ERRX                                                             
*                                                                               
ESWTERR  MVC   P+10(18),=C'CAN''T WRITE ESTHDR'                                 
         LA    R0,EXIT                                                          
         B     ERRX                                                             
*                                                                               
ERRX     XC    ESTDA,ESTDA                                                      
         MVC   P(9),=C'* ERROR *'                                               
         ST    R0,FULL                                                          
         GOTO1 =V(HEXOUT),DMCB,(R8),P+31,17,=C'MIX'                             
         GOTO1 =V(PRINTER)                                                      
         L     RE,FULL                                                          
         CR    RB,RD               SET NE CC                                    
         BR    RE                                                               
*                                                                               
ABEND    ABEND 999                                                              
         EJECT                                                                  
BLD      NTR1                                                                   
*                                                                               
         LAY   R8,SRTBUFF                                                       
         USING SRTRECD,R8                                                       
*                                                                               
         CLI   COSW,C'Y'           DELETES OK                                   
         BE    *+12                                                             
         TM    BUYREC+15,X'80'     TEST DELETED                                 
         BO    BLDX                YES - BYPASS                                 
*                                                                               
         MVI   RECSW,0             SET RECORD FLAGS TO NEW                      
*                                                                               
         LAY   RE,CLTREC                                                        
         USING CLTHDRD,RE                                                       
         CLC   BUYKAM(3),CKEYAM    AGY-MD/CLIENT                                
         BE    BLD10                                                            
         DROP  RE                                                               
         BAS   RE,GETCLT                                                        
         BNE   REQCLTMS                                                         
*                                                                               
BLD10    LA    R3,BUYKEY+3                                                      
*                                                                               
         CLI   BPRD,0              SPECIFIC PRD REQ                             
         BE    BLD20                NO                                          
         CLC   BPRD,KEY+3          REQUESTED PRD                                
         BE    BLD20                YES                                         
         CLI   BUYKEY+3,X'FF'      TEST POL                                     
         BNE   BLDX                 IF NOT POL, BYPASS REC                      
*                                                                               
* BYPASSING BLDKEY WILL DROP SAVING TOTALS FOR THIS PROD IN BLDPROC *           
*                                                                               
BLD20    BAS   RE,BLDKEY                                                        
         BNE   BLDX                                                             
*                                                                               
BLD24    CLI   BDTIME,0            TEST PIGGYBACK                               
         BNE   BLDPIG                                                           
         CLI   BUYKEY+3,X'FF'      TEST POL                                     
         BE    BLDPOL                                                           
         B     BLDPROC                                                          
*                                                                               
BLDPOL   LA    R2,BDELEM                                                        
         SR    R0,R0                                                            
BLDPOL4  IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    BLDPROC                                                          
         CLI   0(R2),11                                                         
         BL    BLDPOL4                                                          
         CLI   0(R2),13                                                         
         BH    BLDPOL4                                                          
         LA    R3,10(R2)           POINT TO ALLOCATIONS                         
         IC    R0,1(R2)                                                         
         SHI   R0,10                                                            
         BZ    BLDPOL4                                                          
*                                                                               
         CLI   BPRD,0              SPECIFIC PRD REQ                             
         BE    BLDPOL10             NO                                          
         CLC   BPRD,0(R3)          REQUESTED PRD                                
         BNE   BLDPOL4              NO                                          
*                                                                               
BLDPOL10 SRL   R0,2                SET FOR BCT                                  
*                                                                               
         CLI   1(R2),14            TEST ONE PRD ALLOCATED                       
         BNE   *+12                                                             
         TM    BDSTAT3,BDST3_SPODS                                              
         BO    BLDPOL12                                                         
         BAS   RE,CHKSLN                                                        
*                                                                               
BLDPOL12 DS    0H                                                               
*                                                                               
* TEST PRD IN BUFFER                                                            
*                                                                               
BLDPOL20 LAY   R8,SRTBUFF                                                       
         B     BLDPOL26                                                         
*                                                                               
BLDPOL22 LA    R8,SRTRECLN(R8)                                                  
         C     R8,=A(SRTBUFFX)                                                  
         BL    *+6                                                              
         DC    H'0'                EXCEEDED SORT BUFFER SIZE                    
*                                                                               
BLDPOL26 CLC   SRTPRD,0(R3)                                                     
         BE    BLDPOL30                                                         
*                                                                               
         CLI   SRTPRD,0            CHECK FOR BUFFER END                         
         BNE   BLDPOL22                                                         
*                                                                               
         CLI   BPRD,0              SPECIFIC PRD REQ                             
         BE    BLDPOL28             NO                                          
*                                                                               
         CLC   BPRD,0(R3)          REQUESTED PRD                                
         BNE   BLDPOL30             NO                                          
*                                                                               
* BYPASSING BLDKEY WILL DROP SAVING TOTALS FOR THIS PROD IN BLDPROC *           
*                                                                               
BLDPOL28 BAS   RE,BLDKEY           ADD NEW PRD TO BUFFER                        
         BNE   BLDX                                                             
*                                                                               
BLDPOL30 LA    R3,4(R3)            NEXT PRD                                     
         BCT   R0,BLDPOL20                                                      
*                                                                               
         B     BLDPOL4             NEXT ELEM                                    
         EJECT                                                                  
BLDPIG   LA    R2,BDELEM                                                        
         SR    R0,R0                                                            
BLDPIG2  IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),4                                                          
         BNE   BLDPIG2                                                          
         IC    R0,1(R2)                                                         
         SRDA  R0,32                                                            
         D     R0,=F'7'                                                         
         LR    R0,R1                                                            
         LA    R3,2(R2)                                                         
BLDPIG4  BAS   RE,BLDKEY                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* BACK UP TO PREVIOUS TO FIX EST                                                
*                                                                               
         LR    RE,R8                                                            
         LA    RF,SRTRECLN                                                      
         SR    RE,RF                                                            
         LA    RE,SRTEST-SRTREC(RE)                                             
         MVC   0(1,RE),1(R3)                                                    
*                                                                               
         LA    R3,7(R3)                                                         
         BCT   R0,BLDPIG4                                                       
         B     BLDPROC                                                          
         EJECT                                                                  
*=========================================================*                     
* SUBROUTINE TO CHECK THAT SUM OF ALLOCATED SPOT LENGTHS  *                     
* IS EQUAL TO THE BUY SPOT LENGTH. IF NOT ===> ERROR MSG  *                     
*=========================================================*                     
*                                                                               
CHKSLN   NTR1                                                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
CHKSLN2  CLI   0(R3),0             TEST BAD PRODUCT ALLOC                       
         BE    CHKSLN4             0 IS NOT DEAD GOOD                           
         ICM   RF,1,1(R3)          GET ALLOCATED SLN                            
         BZ    CHKSLN4                                                          
         AR    RE,RF               ADD                                          
*                                                                               
         LA    R3,4(R3)                                                         
         BCT   R0,CHKSLN2                                                       
*                                                                               
         CLM   RE,1,BDSEC                                                       
         BE    CHKSLNX                                                          
         B     CHKSLN10                                                         
*                                                                               
* PRINT ERROR MESSAGE HERE *                                                    
*                                                                               
CHKSLN4  MVC   PPTR+75(13),=C'BAD POL ALLOC'                                    
         B     CHKCOM                                                           
*                                                                               
CHKSLN10 LLC   R0,BDSEC                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PPTR+60(3),DUB                                                   
         MVC   PPTR+65(3),=C'BUY'                                               
*                                                                               
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PPTR+70(3),DUB                                                   
         MVC   PPTR+75(13),=C'SLN ALLOC ERR'                                    
*                                                                               
CHKCOM   MVC   PAGY,AGYALPHA                                                    
         MVC   PMED,MEDALPHA                                                    
*                                                                               
         GOTO1 CLUNPK,DMCB,KEY+1,PCLT                                           
*                                                                               
         LLC   R0,KEY+9                                                         
         BAS   RE,CVD                                                           
         MVC   PEST,WORK+7                                                      
*                                                                               
         MVC   SVMKT,KEY+4                                                      
         MVC   SVSTA,KEY+6                                                      
         BAS   RE,PMS              PRINT MARKET/STATION                         
*                                                                               
         LLC   R0,KEY+11                                                        
         BAS   RE,CVD                                                           
         MVC   PLIN,WORK+7                                                      
*                                                                               
         MVC   PPTR(4),=C'KEY='                                                 
         GOTO1 =V(HEXOUT),DMCB,KEY,PPTR+4,13,=C'MIX'                            
*                                                                               
         MVC   PPTR+33(3),=C'DA='                                               
         GOTO1 (RF),(R1),KEY+14,PPTR+36,4,=C'MIX'                               
*                                                                               
         MVC   PPTR+47(5),=C'DISP='                                             
         S     R2,AIOAREA                                                       
         CVD   R2,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PPTR+52(4),DUB                                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CHKSLNX  B     EXIT                                                             
*                                                                               
* PRINT MAKET/STATION                                                           
*                                                                               
PMS      NTR1                                                                   
         L     R1,=A(STAWORK)                                                   
         XC    0(32,R1),0(R1)                                                   
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGYALPHA                                                 
         MVC   STAPMED,MEDALPHA                                                 
         MVC   STAPACOM,=A(COMFACS)                                             
         MVC   STAPMKT,SVMKT                                                    
         MVC   STAPSTA,SVSTA                                                    
*                                                                               
         LAY   RE,AGYREC                                                        
         USING AGYHDRD,RE                                                       
         MVC   STAPCTRY,SVAPROF7   MOVE COUNTRY CODE                            
         DROP  RE                                                               
         GOTO1 STAPACK,(R1)                                                     
         CLI   STAPERR,0                                                        
         BE    PMS8                                                             
         CLI   PRTCLTSW,C'N'       IF NOT PRINTING                              
         BE    PMS8                LET THE ERROR GO                             
         DC    H'0'                                                             
*                                                                               
PMS8     MVC   PMKT(4),STAPQMKT                                                 
*                                                                               
         MVC   PSTA(4),STAPQSTA                                                 
         CLI   PSTA,C'0'           TEST STATION NUMERIC                         
         BL    PMS10                                                            
         MVI   PSTA+4,C'/'                                                      
         MVC   PSTA+5(3),STAPQNET                                               
         B     PMSX                                                             
*                                                                               
PMS10    CLI   STAPQSTA+4,C' '                                                  
         BE    *+14                                                             
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(1),STAPQSTA+4                                             
*                                                                               
PMSX     B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
*=======================================================*                       
* SUBROUTINE TO CHECK THAT MARKET ZERO POL BUY ELEMS    *                       
* HAVE COST OVERRIDES PRESENT - IF NOT PRINT ERROR MSG  *                       
*=======================================================*                       
         EJECT                                                                  
BLDKEY   DS   0H                                                                
         XC    SRTREC(200),SRTREC                                               
         XC    SRTREC+200(200),SRTREC+200                                       
         MVC   SRTAGY,BUYREC+20                                                 
         MVC   SRTAGMD,BUYREC                                                   
         MVC   SRTCLT,BUYREC+1                                                  
         LAY   R1,CLTREC                                                        
         USING CLTHDRD,R1                                                       
         CLC   SRTCLT,CKEYCLT                                                   
         BE    BLDKEY10                                                         
         DC    H'0'                                                             
         DROP  R1                                                               
*                                                                               
BLDKEY10 LAY   R1,CLTREC                                                        
         AHI   R1,CLIST-CLTHDRD                                                 
*                                                                               
BLDKEY12 CLC   0(1,R3),3(R1)                                                    
         BE    BLDKEY14                                                         
         LA    R1,4(R1)                                                         
         CLI   0(R1),C'A'                                                       
         BNL   BLDKEY12                                                         
         B     PRDERR                                                           
*                                                                               
BLDKEY14 MVC   SRTPROD,0(R1)                                                    
         MVC   SRTPRD,0(R3)                                                     
         MVC   SRTEST,BUYREC+9                                                  
         MVC   SRTSTA,BUYREC+6                                                  
         MVC   SRTLIN,BUYREC+10                                                 
         MVC   SRTPTRPR,BUYREC+3                                                
         MVC   SRTPTRES,BUYREC+9                                                
         MVC   SRTSTDT(4),=X'FFFF0000'                                          
         ST    R0,SVR0                                                          
         ST    R2,SVR2                                                          
         LA    R2,BDELEM                                                        
*                                                                               
BLDKEY20 LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    BLDKEY30            DONE                                         
         CLI   0(R2),6                                                          
         BL    BLDKEY20                                                         
         CLI   0(R2),13                                                         
         BH    BLDKEY20                                                         
*                                                                               
         CLC   SRTSTDT,2(R2)                                                    
         BNH   *+10                                                             
         MVC   SRTSTDT,2(R2)                                                    
*                                                                               
         CLC   SRTENDT,2(R2)                                                    
         BNL   *+10                                                             
         MVC   SRTENDT,2(R2)                                                    
         B     BLDKEY20                                                         
*                                                                               
BLDKEY30 L     R0,SVR0                                                          
         L     R2,SVR2                                                          
         MVC   SRTMKT,BUYREC+4                                                  
         MVC   SRTID,SVKEYID                                                    
         LA    R8,SRTRECLN(R8)                                                  
         C     R8,=A(SRTBUFFX)                                                  
         BL    *+6                                                              
         DC    H'0'                EXCEEDED SORT BUFFER SIZE                    
         XC    SRTLEN,SRTLEN                                                    
         XC    SRTKEY,SRTKEY                                                    
         CR    RB,RB               SET CC EQ                                    
         BR    RE                                                               
         EJECT                                                                  
BLDPROC  LAY   R8,SRTBUFF                                                       
         USING SRTRECD,R8                                                       
         B     BLDP6                                                            
*                                                                               
BLDP4    LA    R8,SRTRECLN(R8)                                                  
         C     R8,=A(SRTBUFFX)                                                  
         BL    *+6                                                              
         DC    H'0'                EXCEEDED SORT BUFFER SIZE                    
         CLI   SRTAGMD,0           AT END OF TABLE                              
         BE    BLDX                YES                                          
*                                                                               
BLDP6    LA    R2,BDELEM                                                        
*                                                                               
BLDP8    SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    BLDP4               NEXT PRD                                     
         CLI   0(R2),6                                                          
         BL    BLDP8                                                            
         CLI   0(R2),10                                                         
         BL    BLDP28                                                           
         CLI   0(R2),11                                                         
         BL    BLDP8                                                            
         CLI   0(R2),14                                                         
         BL    BLDP10                                                           
         B     BLDP8                                                            
*                                                                               
* ELEMENT PROCESSING FOR POL                                                    
*                                                                               
BLDP10   TM    6(R2),X'04'         TEST HIATUS                                  
         BO    BLDP8                YES-NEXT ELEM                               
*                                                                               
         TM    BDSTAT,X'80'        POL NPW DATA IN 1ST 6 BITS OF RPCOST         
         BZ    BLDP12                                                           
         TM    7(R2),X'FC'         MUST BE ON OR ERROR                          
         BNZ   BLDP12                                                           
         BAS   RE,PBUY                                                          
         MVC   P+40(11),=CL11'MISSING NPW'                                      
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         B     BLDP8                GET-NEXT ELEM                               
*                                                                               
BLDP12   LA    R3,10(R2)                                                        
         IC    R0,1(R2)                                                         
         AHI   R0,-10                                                           
         BNZ   BLDP20                                                           
         CLI   SRTPRD,X'FF'        SKIP UNALL UNLESS PROCSNG POL                
         BE    BLDP28                                                           
         B     BLDP8                                                            
*                                                                               
BLDP20   SRL   R0,2                SET FOR BCT                                  
*                                                                               
BLDP24   CLC   SRTPRD,0(R3)                                                     
         BE    BLDP28                                                           
*                                                                               
BLDP26   LA    R3,4(R3)                                                         
         BCT   R0,BLDP24                                                        
         B     BLDP8                                                            
*                                                                               
BLDP28   LA    RE,SPOTS                                                         
         ST    RE,P1                                                            
         MVC   P1(1),SRTPRD                                                     
         LA    RE,BUYREC                                                        
         ST    RE,P2                                                            
         ST    R2,P3                                                            
*                                                                               
         CLI   SVAPROF7,C'C'       TEST CANADIAN AGENCY                         
         BNE   BLDP30                                                           
         TM    BDCIND2,X'40'       TEST EXCHANGE ELEMENT PRESENT                
         BZ    BLDP30                                                           
         MVI   P3,C'X'             INDICATE EXCHANGE AREA PRESENT               
         LA    RE,XCHDOLS                                                       
         ST    RE,P4                                                            
         MVI   P4,C'C'             REQUEST CANADIAN CURRENCY                    
*                                                                               
BLDP30   GOTO1 GETRATE,P1                                                       
*                                                                               
         LM    R6,R7,GROSS                                                      
         CLI   BDXFRAGY,0          TEST TRANSFERRED BUYREC                      
         BE    *+8                                                              
         SR    R6,R6                                                            
         SR    R7,R7                                                            
         CVD   R6,DUBORDG                                                       
         CVD   R7,DUBORDN                                                       
         EJECT                                                                  
* NOW GET MONTH AND YEAR                                                        
*                                                                               
         GOTO1 =V(BRDMON),P1,(X'FF',2(R2)),DUB                                  
*                                                                               
         LH    R4,DUB                                                           
         SLL   R4,16                                                            
         SRDL  R4,25                                                            
         STC   R4,DUB                                                           
         SRL   R5,28                                                            
         STC   R5,DUB+1                                                         
* TEST IF HAVE ELEMENT FOR THIS Y/M                                             
         LA    R4,SRTBCKTS                                                      
         USING SRTEL,R4                                                         
*                                                                               
BLDP34   CLI   0(R4),0                                                          
         BE    BLDP36                                                           
         CLC   0(2,R4),DUB                                                      
         BE    BLDP40                                                           
         LA    R4,L'SRTBCKTS(R4)   NEXT BUCKET                                  
         B     BLDP34                                                           
*                                                                               
BLDP36   MVC   SRTEL,DUB           CREATE NEW ELEMENT                           
         LA    RE,2(R4)                                                         
         LA    RF,4                                                             
         ZAP   0(6,RE),=P'0'                                                    
         LA    RE,6(RE)                                                         
         BCT   RF,*-10                                                          
*                                                                               
BLDP40   AP    SRTELOG,DUBORDG                                                  
         AP    SRTELON,DUBORDN                                                  
*                                                                               
         OC    4(2,R2),4(R2)       TEST PAID                                    
         BZ    BLDP52                                                           
*                                                                               
BLDP50   AP    SRTELPG,DUBORDG     GROSS PAID/NET PAID                          
         AP    SRTELPN,DUBORDN                                                  
*                                                                               
BLDP52   CLI   BUYKEY+3,X'FF'      TEST POOL                                    
         BNE   BLDP8                                                            
         CLI   1(R2),10            TEST UNALL                                   
         BE    BLDP8                                                            
         B     BLDP26              TEST FOR MORE ALLOCATIONS                    
*                                                                               
BLDX     B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
REQESTER MVC   P+10(13),=CL13'EST NOT 1-255'                                    
         B     INVALREQ                                                         
AGYRDERR MVC   P+10(6),=C'AGENCY'                                               
         B     NOHDRER                                                          
REQCLTMS GOTO1 CLUNPK,DMCB,BUYKEY+1,P+17                                        
*                                                                               
REQCLTER MVC   P+10(6),=C'CLIENT'                                               
         B     NOHDRER                                                          
PRDHDRER DS   0H                                                                
         MVC   P+10(4),=CL4'PROD'                                               
         B     NOHDRER                                                          
ESTHDRER MVC   P+10(3),EST                                                      
*                                                                               
NOHDRER  DS   0H                                                                
         MVC   P+21(9),=CL9'NOT FOUND'                                          
         B     INVALREQ                                                         
MISCLTER MVC   P+10(16),=CL16'PRD/EST NEED CLT'                                 
         B     INVALREQ                                                         
ESTBUYER MVC   P+10(15),=CL15'NO BUYS FOR EST'                                  
         B     INVALREQ                                                         
ESTNUMER MVC   P+10(15),ESTNONUM                                                
         B     INVALREQ                                                         
PRDCDER  MVC   P+10(15),=CL15'NO PRD IN CLIST'                                  
         B     INVALREQ                                                         
NODATAER MVC   P+10(11),=CL11'NO DATA FOR REQ'                                  
*                                                                               
INVALREQ MVC   P(9),=C'* SP134 *'                                               
         GOTO1 =V(PRINTER)                                                      
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
         B     SP2                 CHECK NEXT REQUEST                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
INIT     NTR1  BASE=*,LABEL=*                                                   
         MVI   SPOTCARD,C'N'       SET NO SPOT CARD READ                        
         MVI   NOCARDS,C'N'        ASSUME WE HAVE CARDS TO PROCESS              
*                                                                               
         L     R1,=A(SSB)                                                       
         OI    SSOSTAT2-SSB(R1),SSOSNRCV   SET RECOVERY OFF                     
*                                                                               
         LA    RE,SVREC-REC        ESTABLISH ADDRESSABILITY TO SVREC            
         LA    RE,REC(RE)                                                       
         ST    RE,ASVREC                                                        
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(3,TODAYB)                                 
         GOTO1 (RF),(R1),,(2,TODAYP2)                                           
         GOTO1 (RF),(R1),,(1,TODAYP3)                                           
*                                                                               
INIT2    GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLC   =C'NOCARDS',CARD        HAVE NOCARDS CARD?                       
         BNE   INIT3                   YES - DON'T PROCESS REPORT!              
         MVI   NOCARDS,C'Y'            SET NOCARDS TO Y                         
         B     INITX                   AND EXIT INIT ROUTINE                    
*                                                                               
INIT3    CLI   CARD,C'*'                                                        
         BE    INIT2                                                            
*                                                                               
         CLC   =C'DDSIO=',CARD                                                  
         BNE   INIT4                                                            
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6                                                   
         B     INIT2                                                            
*                                                                               
INIT4    CLC   =C'DSPACE=',CARD                                                 
         BNE   INIT6                                                            
         L     RE,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RE),CARD+7                                     
         B     INIT2                                                            
*                                                                               
INIT6    CLC  =C'UPDID=',CARD                                                   
         BNE   INIT10                                                           
         GOTO1 =V(DATAMGR),DMCB,=C'UPDID'                                       
         L     RE,12(R1)                                                        
         MVC   0(2,RE),CARD+6                                                   
         B     INIT2                                                            
*                                                                               
INIT10   CLC   =C'SPOT',CARD       IF NO SPOT CARD, READ ACCESS REC             
         BNE   INIT32                                                           
         MVI   SPOTCARD,C'Y'       WE READ A SPOT CARD                          
         SPACE 1                                                                
*==================================================                             
* GET SE NUMBER                                                                 
*==================================================                             
         SPACE 1                                                                
         L     RE,=V(UTL)                                                       
         MVI   4(RE),10            SET TO READ IN CONTROL SYSTEM                
         LA    R2,REC                                                           
         USING CTWREC,R2           READ SYSTEM LIST RECORD (FOR FILE)           
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'S'                                                     
         MVI   CTWKSYSN,2          GET LIST ONLY FOR THIS SYSTEM TYPE           
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'CONTROL',CTFLIST                  
         GOTO1 =V(DATAMGR),DMCB,=C'DMREAD',=C'CTFILE',CTWREC,CTWREC             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INIT12   SR    R0,R0                                                            
         LA    R1,CTWDATA                                                       
         USING SYSELD,R1           LOCATE SYSTEM ELEMENT FOR SENO               
*                                                                               
INIT14   CLI   SYSEL,0             TEST E-O-R                                   
         BE    IERR                                                             
         CLI   SYSEL,SYSELQ        TEST SYSTEM ELEMENT                          
         BNE   INIT16                                                           
         CLI   SYSSYS,2            TEST RIGHT SYSTEM                            
         BE    INIT8                                                            
*                                                                               
INIT16   IC    R0,SYSLEN           BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     INIT14                                                           
*                                                                               
INIT8    CLC   CARD(6),SYSNAME     TEST RIGHT FILE SET                          
         BNE   INIT16              TRY ANOTHER                                  
         L     RE,=V(UTL)                                                       
         MVC   4(1,RE),SYSSEN                                                   
         B     INIT20                                                           
         DROP  R1,R2                                                            
*                                                                               
IERR     MVC   P(30),=C'** ERROR * SYSTEM NOT VALID **'                         
         GOTO1 =V(PRINTER)                                                      
         ABEND 999                                                              
*                                                                               
CTFLIST  DC    C'NCTFILE X'                                                     
         EJECT                                                                  
INIT20   MVC   SVSYS,CARD                                                       
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
INIT30   GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
INIT32   CLC   =C'/*',CARD                                                      
         BE    INIT60                                                           
         CLI   CARD,C'*'                                                        
         BE    INIT30                                                           
*                                                                               
         CLC   =C'WRITE=NO',CARD                                                
         BNE   INIT36                                                           
         MVI   FLIST,C' '          SUPPRESS EOF SEARCH ON SPTFILE               
         MVI   WRITESW,C'N'                                                     
*                                                                               
INIT34   MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
         B     INIT30                                                           
*                                                                               
INIT36   CLC   =C'TOTALS=CLIENT',CARD                                           
         BNE   *+12                                                             
         MVI   PRTCLTSW,C'Y'                                                    
         B     INIT34                                                           
*                                                                               
         CLC   =C'TOTALS=MEDIA',CARD                                            
         BNE   INIT37                                                           
         MVI   PRTCLTSW,C'Y'                                                    
         MVI   PRTTOTAL,C'Y'                                                    
         B     INIT34                                                           
*                                                                               
INIT37   CLC   =C'TOTALS=NO',CARD                                               
         BNE   INIT38                                                           
         MVI   PRTCLTSW,C'N'                                                    
         B     INIT34                                                           
*                                                                               
INIT38   CLC   =C'PRINT=D',CARD                                                 
         BNE   INIT40                                                           
         MVI   DTLSW,C'Y'                                                       
         MVI   UNMATSW,C'N'        PRINT ALL TOTALS                             
         B     INIT34                                                           
*                                                                               
INIT40   CLC   =C'UNMATCHED',CARD                                               
         BNE   INIT42                                                           
         CLI   DTLSW,C'Y'          IF PRINTING DETAIL, PRT ALL TOTS             
         BE    INIT34                                                           
         MVI   UNMATSW,C'Y'                                                     
         B     INIT34                                                           
*                                                                               
INIT42   CLC   =C'WRITE=Y',CARD                                                 
         BNE   INIT44                                                           
         MVI   WRITESW,C'Y'                                                     
         B     INIT34                                                           
*                                                                               
INIT44   CLC   =C'GETRATE=',CARD                                                
         BNE   INIT46                                                           
         MVC   GTRTVRSN,CARD+8                                                  
         B     INIT34                                                           
*                                                                               
INIT46   CLC   =C'CLOSEOUT=Y',CARD                                              
         BNE   INIT48                                                           
         MVI   COSW,C'Y'                                                        
         MVI   WRITESW,C'N'        FORCE WRITE=N                                
         B     INIT34                                                           
*                                                                               
INIT48   CLC   =C'NET=Y',CARD                                                   
         BNE   INIT50                                                           
         MVI   NETSW,C'Y'                                                       
         B     INIT34                                                           
*                                                                               
INIT50   MVC   P(30),=C'*SP134* UNKNOWN PARAMETER CARD'                         
         ABEND 999                                                              
         EJECT                                                                  
*==================================================================             
* OPEN FILES                                                                    
*                                                                               
INIT60   DS    0H                                                               
         CLI   WRITESW,0           HAVE A WRITE=CARD?                           
         BE    ERRWRITE            NO - GENERATE ERR MSG AND ABEND              
         CLI   SPOTCARD,C'Y'       DID WE READ A SPOT CARD                      
         BE    INIT62                                                           
         BAS   RE,RDACCESS         GET THE FIRST 34 CARD                        
*                                                                               
INIT62   GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'SPOT',FLIST,REC                   
*                                                                               
         MVI   TITLE,C' '                                                       
         MVC   TITLE+1(59),TITLE                                                
         MVC   TITLE(19),=CL19'SP134 FULL ACTIVITY'                             
         MVC   TITLE(5),SVSYS                                                   
*                                                                               
         MVC   DUB,SPACES          LOAD STAPACK                                 
         MVC   DUB(4),=C'T00A'                                                  
         MVI   WORK,QSTAPACK                                                    
         GOTO1 =V(HEXOUT),DMCB,WORK,DUB+4,1,0                                   
         GOTO1 =V(LOADER),DMCB,DUB,0                                            
         L     RE,4(R1)                                                         
         ST    RE,STAPACK                                                       
*                                                                               
         MVC   DUB,SPACES          LOAD CABLETAB                                
         MVC   DUB(4),=C'T00A'                                                  
         MVI   WORK,QCABLETB                                                    
         GOTO1 =V(HEXOUT),DMCB,WORK,DUB+4,1,0                                   
         GOTO1 =V(LOADER),DMCB,DUB,0                                            
         L     RE,4(R1)                                                         
         ST    RE,CABLETAB                                                      
*                                                                               
         MVI   WORK,QGETRATE                                                    
         GOTO1 =V(HEXOUT),DMCB,WORK,DUB+4,1,0                                   
         MVC   DUB+6(1),GTRTVRSN                                                
         GOTO1 =V(LOADER),DMCB,DUB,0                                            
         L     RE,4(R1)                                                         
         ST    RE,GETRATE                                                       
*                                                                               
         MVI   WORK,QCLUNPK                                                     
         GOTO1 =V(HEXOUT),DMCB,WORK,DUB+4,1,0                                   
         GOTO1 =V(LOADER),DMCB,DUB,0                                            
         L     RE,4(R1)                                                         
         ST    RE,CLUNPK                                                        
*                                                                               
         MVI   WORK,QCLPACK                                                     
         GOTO1 =V(HEXOUT),DMCB,WORK,DUB+4,1,0                                   
         GOTO1 =V(LOADER),DMCB,DUB,0                                            
         L     RE,4(R1)                                                         
         ST    RE,CLPACK                                                        
*                                                                               
         L     R1,=A(STAWORK)      PASS A(T00A9E) TO STAPACK                    
         XC    0(32,R1),0(R1)                                                   
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,QSTP_T00A9E                                              
         MVC   STAPACOM,CABLETAB                                                
         GOTO1 STAPACK,(R1)                                                     
*                                                                               
INITX    J     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
ERRWRITE GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'WRITEMSG,WRITEMSG)                 
         ABEND 201                                                              
WRITEMSG DC    C'** SP134 ** WARNING - MISSING WRITE= CARD'                     
*                                                                               
*================================================================               
* NO SPOT CARD                                                                  
* READ FIRST 34 REQUEST CARD AND THEN READ ACCESS RECORD FOR THAT               
*   AGENCY                                                                      
*================================================================               
                                                                                
RDACCESS NTR1                                                                   
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         CLC   =C'34',CARD         MAKE SURE IT IS A REQUEST CARD               
         BNE   RDACCSER                                                         
*                                                                               
         L     RE,=V(UTL)                                                       
         MVI   4(RE),10            READ CTFILE FROM CONTROL SYSTEM              
*                                                                               
         LA    R2,REC                                                           
         USING CT5REC,R2           READ ACCESS RECORD                           
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,C'5'                                                     
         MVC   CT5KALPH,CARD+2                                                  
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'CONTROL',CTFLIST                  
         GOTO1 =V(DATAMGR),DMCB,=C'DMREAD',=C'CTFILE',CT5REC,CT5REC             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         LA    R1,CT5DATA                                                       
         USING CTSYSD,R1           LOCATE SYSTEM ELEMENT FOR SENO               
*                                                                               
RDACCS2  CLI   CTSYSEL,0           TEST E-O-R                                   
         BE    RDACCSER                                                         
         CLI   CTSYSEL,X'21'       TEST SYSTEM ELEMENT                          
         BNE   *+12                                                             
         CLI   CTSYSNUM,2          TEST SPOT                                    
         BE    RDACCS4                                                          
         IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     RDACCS2                                                          
*                                                                               
RDACCS4  L     RE,=V(UTL)                                                       
         MVC   4(1,RE),CTSYSSE                                                  
         J     EXIT                                                             
*                                                                               
RDACCSER MVC   P(29),=C'** ERROR * NO SPOT ACCESS FOR'                          
         MVC   P+30(2),CARD+2                                                   
         GOTO1 =V(PRINTER)                                                      
         ABEND 999                                                              
         DROP  R1,R2                                                            
*                                                                               
GTRTVRSN DC    C' '                                                             
FLIST    DS    0H                                                               
         DC    CL8'USPTFILE'                                                    
         DC    CL8'USPTDIR '                                                    
         DC    CL8' STAFIL '                                                    
         DC    CL8'X       '                                                    
         LTORG                                                                  
         EJECT                                                                  
TESTFMT  NTR1  BASE=*,LABEL=*                                                   
         LA    R0,13                                                            
         LR    R1,R4                                                            
         CP    0(6,R1),=P'0'                                                    
         BNE   TESTFM1                                                          
         LA    R1,6(R1)                                                         
         BCT   R0,*-14                                                          
         MVC   P,SPACES                                                         
         J     EXIT                                                             
*                                                                               
TESTFM1  LA    R5,P+10                                                          
         LA    R6,12                                                            
*                                                                               
TESTFM2  ZAP   DUB,0(6,R4)                                                      
         SRP   DUB,64-2,5                                                       
         EDIT  (P8,DUB),(7,(R5))                                                
         LA    R4,6(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   R6,TESTFM2                                                       
         GOTO1 =V(PRINTER)                                                      
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
* ADD TO TOTAL CTRS, AND ZERO ESTIMATE HEADER CTRS                              
*                                                                               
CLREST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* ADD TO EST/PRD/CLT/MED EST HDR TOTALS                                         
*                                                                               
         ZAP   PACC1,=P'0'                                                      
         LA    R0,13                                                            
         LA    R1,SVEORD                                                        
*                                                                               
         AP    PACC1,0(6,R1)                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         ZAP   PACC2,=P'0'                                                      
         LA    R0,13                                                            
         LA    R1,SVEORDN                                                       
*                                                                               
         AP    PACC2,0(6,R1)                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         ZAP   PACC3,=P'0'                                                      
         LA    R0,13                                                            
         LA    R1,SVEPAID                                                       
         AP    PACC3,0(6,R1)                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         ZAP   PACC4,=P'0'                                                      
         LA    R0,13                                                            
         LA    R1,SVEPAIDN                                                      
         AP    PACC4,0(6,R1)                                                    
         LA    R1,6(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         LA    R5,TOTESTP          FIRST ROW OF ACCUMS                          
         LA    R0,4                FOR 4 ROWS                                   
*                                                                               
CLREST10 AP    0(8,R5),PACC1       ORD GROSS                                    
         AP    8(8,R5),PACC2       ORD NET                                      
         AP    16(8,R5),PACC3      PAID GROSS                                   
         AP    24(8,R5),PACC4      PAID NET                                     
*                                                                               
         LA    R5,64(R5)           NEXT ROW                                     
         BCT   R0,CLREST10                                                      
*                                                                               
         MVI   ESTSW,1             SET ESTIMATE SW TO PROCESSED                 
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
SP134WRK DS    0D                                                               
*                                                                               
DMCB     DS    0CL24                                                            
         ORG   DMCB                                                             
DM1      DS    F                                                                
DM2      DS    F                                                                
DM3      DS    F                                                                
DM4      DS    F                                                                
DM5      DS    F                                                                
DM6      DS    F                                                                
DMWORK   DS    24F                                                              
*                                                                               
P1       DS    A                                                                
P2       DS    A                                                                
P3       DS    A                                                                
P4       DS    A                                                                
P5       DS    A                                                                
P6       DS    A                                                                
*                                                                               
SVRE     DS    F                                                                
SVR0     DS    F                                                                
SVR2     DS    F                                                                
SP134RD  DS    F                                                                
*                                                                               
STAPACK  DS    A                                                                
GETRATE  DS    A                                                                
CLPACK   DS    A                                                                
CLUNPK   DS    A                                                                
CABLETAB DS    A                                                                
AGYDA    DC    F'0'                                                             
CLTDA    DC    F'0'                                                             
ESTDA    DC    F'0'                                                             
*                                                                               
PACC1    DS    PL8                                                              
PACC2    DS    PL8                                                              
PACC3    DS    PL8                                                              
PACC4    DS    PL8                                                              
*                                                                               
KEY      DS    CL20                                                             
KEYSAVE  DS    CL20                                                             
SVSYS    DS    CL8                                                              
DMINBTS  DC    X'00'                                                            
EOFSW    DC    X'00'               SET NON-ZERO AT EOF                          
FIRSTSW  DC    C'F'                NON-ZERO AT FIRST SORT OUT REC               
WRITESW  DC    X'00'               DEFAULT TO WRITE=N!                          
DUMPSW   DC    C'N'                                                             
DTLSW    DC    C'N'                                                             
NETSW    DC    C'N'                                                             
UNMATSW  DC    C'N'                                                             
PRTCLTSW DC    C'N'                                                             
PRTTOTAL DC    C'N'                                                             
TESTSW   DC    C'N'                                                             
COSW     DC    C'N'                CLOSED OUT RECS                              
UPSISW   DC    X'00'                                                            
PRTOTSW  DC    X'00'               02 = THIS LEVEL OF TOTALS IN BAL             
*                                  01 = BYPASS PRINTING TOTALS                  
ESTSW    DC    X'00'               00 = EST HDR READ                            
*                                  01 = EST HDR TOTALS SAVED, ZEROED            
RECSW    DC    X'00'               00 = BUY REC READ                            
*                                  80 = FOUND BAD $0 OVERRIDE THIS REC          
AIOAREA  DS    A                                                                
ASVREC   DS    A                                                                
NEXTSRT  DS    A                                                                
FULL     DS    F                                                                
* GETRATE WORK AREA                                                             
SPOTS    DS    F                                                                
GROSS    DS    F                                                                
NET      DS    F                                                                
ADJ      DS    F                                                                
*                                                                               
DUB      DS    D                                                                
DUBORDG  DS    D                                                                
DUBORDN  DS    D                                                                
DUBPAIDG DS    D                                                                
DUBPAIDN DS    D                                                                
*                                                                               
XCHDOLS  DS    0XL32                                                            
XGROSS   DS    F                                                                
XNET     DS    F                                                                
XTAX     DS    F                                                                
XC58     DS    F                                                                
XGSTCD   DS    C                                                                
XGSTRD   DS    XL3                                                              
XGST     DS    F                                                                
         DS    F                                                                
         DS    F                                                                
CARD     DS    CL80                                                             
         ORG   CARD                                                             
ZAREA    DS    0CL80                                                            
ZTYPE    DS    CL2              1 -  2                                          
ZAGY     DS    CL2              3 -  4                                          
ZMED     DS    CL1              5                                               
ZCLT     DS    CL3              6 -  8                                          
ZPGR     DS    CL1              9                                               
ZMGR     DS    CL1             10                                               
         DS    CL1             11                                               
ZPRD     DS    CL3             12 - 14                                          
ZMKT     DS    CL4             15 - 18                                          
ZSTA     DS    CL5             19 - 23                                          
ZDATE    DS    CL6             24 - 29                                          
ZEST     EQU   ZDATE           24 - 26                                          
ZREP     DS    CL3             30 - 32                                          
ZREPTYPE DS    CL1                                                              
         DS    CL2                                                              
ZCONT    DS    CL1                                                              
         DS    CL1                                                              
ZSTART   DS    CL6                                                              
ZEND     DS    CL6                                                              
ZPRD2    DS    CL3                                                              
ZPRD3    DS    CL3                                                              
ZMODE    EQU   ZPRD3                                                            
ZAMTTYPE DS    CL1                                                              
ZAMT     DS    CL10                                                             
         DS    CL2                                                              
ZUESTOR  DS    CL12                                                             
ZINV     EQU   ZUESTOR                                                          
         ORG                                                                    
         DS    0F                                                               
SVAGYPTR DS    A                   POINTER INTO AGYLIST                         
AGYALPHA DS    CL2                                                              
SVAPROF7 DS    C                                                                
MEDALPHA DS    C                                                                
SPOTCARD DS    C                                                                
SVMKT    DS    XL2                                                              
SVSTA    DS    XL3                                                              
*                                                                               
EST      DS    CL3                                                              
ESTNONUM DC    CL15'EST NOT NUMERIC'                                            
         DS    0D                                                               
STAWORK  DS    XL32                                                             
SORTCARD DC    CL80'SORT FIELDS=(4,14,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=400'                                   
* SVEORD TO SVEPAID MUST BE TOGETHER                                            
*                                                                               
SVEORD   DC    13PL6'0'                                                         
SVEORDN  DC    13PL6'0'                                                         
SVEPAID  DC    13PL6'0'                                                         
SVEPAIDN DC    13PL6'0'                                                         
*                                                                               
WORK     DS    CL64                                                             
MDTAB    DC    C'TRNX'                                                          
* 2                                                                             
SVKEY    DC    XL12'00'                                                         
SVKEYID  DC    XL1'00'                                                          
BAGYMD   DS    XL1                                                              
BCLT     DS    XL2                                                              
BPRD     DS    XL1                                                              
BEST     DS    XL1                                                              
*                                                                               
TODAYP2  DS    XL2                                                              
TODAYP3  DS    XL3                                                              
TODAYB   DS    XL3                                                              
ESTMON13 DS    H                                                                
ESTMON12 DS    H                                                                
*                                                                               
NOCARDS  DS    CL1                                                              
*                                                                               
QFIRST   DC    C'Q'                NON-ZERO FIRST TIME                          
QKEY     DC    XL13'00'                                                         
*                                                                               
STACTR   DC    PL4'0'                                                           
*                                                                               
ESTCTR   DC    PL4'0',CL20'ESTIMATE HEADERS    '                                
WRTCTR   DC    PL4'0',CL20'EST HDRS WRITTEN'                                    
TOTCTR   DC    PL4'0',CL20'TOTAL BUYS THIS REQ '                                
BUYERCTR DC    PL4'0',CL20'BUYS WITHOUT EST HDR'                                
BUYDTER  DC    PL4'0',CL20'BUYS NOT IN EST DATE'                                
NBRCTRS  EQU   (*-ESTCTR)/24                                                    
*                                                                               
TOTESTP  DC    4PL8'0',CL32'   ESTIMATE TOTALS EST HDR    '                     
TOTPRDP  DC    4PL8'0',CL32'  * PRODUCT TOTALS EST HDR *  '                     
TOTCLTP  DC    4PL8'0',CL32' ** CLIENT  TOTALS EST HDR ** '                     
TOTMEDP  DC    4PL8'0',CL32' **  MEDIA  TOTALS EST HDR ** '                     
*                                                                               
TOTSTA   DC    4PL8'0',CL32'    STATION TOTALS      '                           
TOTEST   DC    4PL8'0',CL32'   ESTIMATE TOTALS BUYS'                            
TOTPRD   DC    4PL8'0',CL32'  * PRODUCT TOTALS BUYS *    '                      
TOTCLT   DC    4PL8'0',CL32' ** CLIENT  TOTALS BUYS **   '                      
TOTMED   DC    4PL8'0',CL32' **  MEDIA  TOTALS BUYS **   '                      
         EJECT                                                                  
*                                                                               
ALLEST   DC    PL8'0'                                                           
ALLBUY   DC    PL8'0'                                                           
*                                                                               
RECLN    DS    F                                                                
REC      DS    1000F                                                            
*                                                                               
SVREC    DS    1000F                                                            
*                                                                               
         ORG   REC                                                              
       ++INCLUDE SPGENBUY                                                       
         ORG                                                                    
*                                                                               
MIDMONS  DC    CL100'   JAN     FEB     MAR     APR     MAY     JUN    X        
                JUL     AUG     SEP     OCT     NOV     DEC'                    
*                                                                               
HEAD     DC    CL110'AG M CLT PRD EST  MARKT STAT     LIN  PTR-EST   GRX        
               OSS ORDERED     NET ORDERED      GROSS PAID        NET PX        
               AID'                                                             
*                                                                               
         DS    0D                                                               
UTL      DC    F'0',X'00'                                                       
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
*                                                                               
         DS    0D                                                               
         DC    CL8'*COMFACS'                                                    
COMFACS  DS    0D                                                               
         DC    V(DATAMGR)                                                       
         DC    71A(0)                                                           
         EJECT                                                                  
         DC    CL8'*AGYREC*'                                                    
AGYREC   DS    1000C                                                            
*                                                                               
         DC    CL8'*CLTREC*'                                                    
CLTREC   DS    1000C                                                            
*                                                                               
         DC    CL8'*ESTREC*'                                                    
ESTREC   DS    CL1000                                                           
         EJECT                                                                  
         ORG                                                                    
*                                                                               
SRTBUFF  DC    40000X'00'          100 X 400 BYTES/REC                          
SRTBUFFX EQU   *                                                                
* 3                                                                             
         DS    0D                                                               
         DC    CL8'*REGSAVE'                                                    
REGSAVE  DS    20000C                                                           
         EJECT                                                                  
SRTRECD  DSECT                                                                  
*                                                                               
SRTRECLN EQU   400                                                              
*                                                                               
SRTREC   DS    0C                  MAX LEN = 400                                
*                                                                               
SRTLEN   DS    F                                                                
*                                                                               
SRTKEY   DS    0CL14                                                            
SRTAGMD  DS    CL1       B         AGY/MD                                       
SRTCLT   DS    CL2                 CLIENT                                       
SRTPROD  DS    CL3                 PRODUCT                                      
SRTEST   DS    CL1                 ESTIMATE                                     
SRTMKT   DS    CL2                 MARKET                                       
SRTSTA   DS    CL3                 STATION                                      
SRTLIN   DS    CL1                 LINE                                         
SRTPRD   DS    XL1                 PRODUCT                                      
*                                                                               
SRTAGY   DS    CL2                 ALPHA AGENCY                                 
SRTPTRPR DS    CL1                 BUYKEY PRD CODE                              
SRTPTRES DS    CL1                 BUYKEY EST                                   
*                                                                               
SRTSTDT  DS    XL2                                                              
SRTENDT  DS    XL2                                                              
SRTMKTA  DS    XL2                                                              
SRTID    DS    XL1                                                              
*                                                                               
SRTBCKTS DS    0XL26                                                            
*                                                                               
SRTEL    DS    CL2       B         Y/M                                          
SRTELOG  DS    PL6                 ORDERED-GROSS                                
SRTELON  DS    PL6                 ORDERED-NET                                  
SRTELPG  DS    PL6                 PAID-GROSS                                   
SRTELPN  DS    PL6                 PAID-NET                                     
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
* DSECT FOR SP100 PRINT LINE                                                    
         ORG   P                                                                
PAGY     DS    CL2                                                              
         DS    C                                                                
PMED     DS    CL1                                                              
         DS    C                                                                
PCLT     DS    CL2                                                              
         DS    2C                                                               
PPRD     DS    CL3                                                              
         DS    C                                                                
PEST     DS    CL3                                                              
         DS    2C                                                               
PMKT     DS    CL4                                                              
         DS    2C                                                               
PSTA     DS    CL9                                                              
         DS    C                                                                
PLIN     DS    CL3                                                              
         DS    2C                                                               
PPTR     DS    CL3                                                              
         DS    C'-'                                                             
PPTREST  DS    CL3                                                              
         DS    C                                                                
PIND     DS    CL1                                                              
PORDG    DS    CL16                                                             
PORDN    DS    CL16                                                             
PPAYG    DS    CL16                                                             
PPAYN    DS    CL16                                                             
         DS    CL3                                                              
PDTS     DS    CL17                                                             
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
*                                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSYSELD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073SP134     11/13/13'                                      
         END                                                                    
