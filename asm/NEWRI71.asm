*          DATA SET NEWRI71    AT LEVEL 046 AS OF 06/28/06                      
*PHASE T32071A,*                                                                
*INCLUDE NETACC                                                                 
*INCLUDE NETNET                                                                 
         TITLE 'T32071 - BOZELL VENDOR TAPE'                                    
T32071   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NW71**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,4095(RB)       RA=2ND BASE REGISTER                           
         LA    RA,1(RA)                                                         
         USING T32071,RB,RA                                                     
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1          R7-ANETWS1+ANETWS2/WORKING STORAGE           
         LA    R7,300(R7)             (ACTUALLY ANETWS1+300 =WRKSTRG)           
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         L     R1,ANETWS3                                                       
         ST    R1,NBANBUFF                                                      
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         L     R1,BOOKVAL                                                       
         A     R1,RELO                                                          
         ST    R1,ANTWKTP                                                       
*                                                                               
         CLI   MODE,VALREC                                                      
         BE    VK                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LR                                                               
         CLI   MODE,RUNLAST                                                     
         BE    LASTRUN                                                          
         CLI   MODE,RUNFRST                                                     
         BE    FIRSTRUN                                                         
EXIT     XIT1                                                                   
         EJECT                                                                  
* - RUNFIRST                                                                    
FIRSTRUN DS    0H                                                               
         L     RE,ATWA                                                          
         MVI   29(RE),2            SET RUNLAST HOOK INDICATOR                   
         B     EXIT                                                             
                                                                                
*                                                                               
* - CLOSES TAPE                                                                 
LASTRUN  DS    0H                                                               
         L     R2,ANTWKTP             IF WE WROTE TAPE                          
         CLC   =X'90EC',0(R2)                                                   
         BE    LASTRX                                                           
         CLOSE ((R2),)                CLOSE IT                                  
LASTRX   B     EXIT                                                             
         EJECT                                                                  
*************************************                                           
* VALIDATE REQUEST SCREEN DATA                                                  
*                                                                               
VK       DS    0H                                                               
         L     R6,ATWA                                                          
         USING T320FFD,R6                                                       
         MVI   NBQINIT,0                                                        
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
*                                                                               
         LA    R2,SPLCLIH              CLIENT                                   
         NETGO NVCLIALL,DMCB,SPLCLIN                                            
         OI    SPLCLINH+6,X'80'                                                 
*                                                                               
*                                                                               
         LA    R2,SPLPROH               PRODUCT                                 
         NETGO NVPRDALL,DMCB,SPLPRON                                            
         OI    SPLPRONH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB,SPLESTN                                            
*                                                                               
         LA    R2,SPLSDTH          START                                        
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLEDTH          END                                          
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
*                                                                               
         MVI   TAPEOPT,C'N'                                                     
         MVI   ERROR,INVALID                                                    
         LA    R2,SPLTAPEH                                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    VKEXIT                                                           
         MVC   TAPEOPT,FLD                                                      
         CLI   FLD,C'Y'                                                         
         BE    VKEXIT                                                           
         CLI   FLD,C'N'                                                         
         BE    VKEXIT                                                           
         B     ERREX                                                            
*                                                                               
VKEXIT   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*****************************************                                       
LR       DS    0H                                                               
* - INITIALIZE SORTER                                                           
         XC    DMCB,DMCB                                                        
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
* - OPEN TAPE                                                                   
         CLI   TAPEOPT,C'Y'                                                     
         BNE   LR5                                                              
         L     RE,BOOKVAL          SET DCB TO BOOKVAL/OPEN TAPE                 
         CLC   =X'90EC',0(RE)                                                   
         BNE   LR5                                                              
         LA    RF,NTWKTP                                                        
         MVC   0(128,RE),0(RF)                                                  
         MVC   DSNAME+13(2),NBSELAGY                                            
         L     R4,ATWA                                                          
         USING T320FFD,R4                                                       
         L     RF,TWADCONS                                                      
         L     RF,TDYNALLO-TWADCOND(RF)                                         
         GOTO1 (RF),DMCB,DDNAME,DSNAME                                          
*****    GOTO1 =V(DYNALLOC),DMCB,DDNAME,DSNAME                                  
         L     R2,ANTWKTP                                                       
         OPEN  ((R2),(OUTPUT))                                                  
         B     LR5                                                              
         DROP  R4                                                               
                                                                                
DDNAME   DC    CL8'NTWKTP'                                                      
DSNAME   DC    CL20'NETTAPE.NE0BJAA1'                                           
DCBOPEN  DC    C'N'                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,20,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=200'                                   
*                                                                               
*                                                                               
         SPACE                                                                  
* - SET UP NETIO READ PARAMETERS                                                
LR5      DS    0H                                                               
         MVI   NBDATA,C'U'                UNITS ONLY                            
         MVI   NBSELUOP,C'B'              ESTIMATED+ACTUAL SCHEDULE             
         LA    R1,UNITHOOK                UNIT HOOK                             
         ST    R1,NBHOOK                                                        
* CLEAR TOTAL FIELDS                                                            
         ZAP   TOTGORD,=P'0'                                                    
         ZAP   TOTNORD,=P'0'                                                    
         ZAP   TOTNCLRD,=P'0'                                                   
         ZAP   TOTPCTCD,=P'0'                                                   
         ZAP   TOTCLRBL,=P'0'                                                   
         ZAP   TOTUNITS,=P'0'                                                   
         ZAP   ALLGORD,=P'0'                                                    
         ZAP   ALLNORD,=P'0'                                                    
         ZAP   ALLNCLRD,=P'0'                                                   
         ZAP   ALLCLRBL,=P'0'                                                   
         ZAP   ALLUNITS,=P'0'                                                   
*                                                                               
LR10     NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BNE   LR10                                                             
         BAS   RE,DOTAPE                 DO TAPE/REPORT                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*  PROCESS UNIT / PUT TO SORTER                                                 
*                                                                               
UNITHOOK NTR1                                                                   
*                                                                               
         MVI   NBUPUNIT,C'N'                                                    
         MVI   NBNOWRIT,C'N'                                                    
*                                                                               
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
* - UNITS ONLY                                                                  
         CLI   NBMODE,NBPROCUN                                                  
         BNE   DOUNTX                                                           
         LA    RE,SORTREC                      CLEAR SORTREC AREA               
         L     RF,=F'300'                                                       
         XCEF                                                                   
         LA    R2,SORTREC                                                       
         USING SORTRECD,R2                                                      
* - SORTER KEY FIELDS                                                           
         MVC   SRTKMED,NBEFFMED             MEDIA                               
         MVC   SRTKNET,NBACTNET             NETWORK                             
         ZAP   SRTUNITS,=P'1'               UNITS                               
* - DATA FIELDS                                                                 
         MVI   BYTE,28             TIME+INT+SPECIAL                             
         BAS   R5,GETDOLS                                                       
         ZAP   SRTGORD,WORK+1(8)                                                
                                                                                
         MVI   BYTE,38             NET TIME+INT+SPECIAL                         
         BAS   R5,GETDOLS                                                       
         ZAP   SRTNORD,WORK+1(8)                                                
                                                                                
         MVI   BYTE,58             CLEARED NET TIME+INT+SPECIAL                 
         BAS   R5,GETDOLS                                                       
         ZAP   SRTNCLRD,WORK+1(8)                                               
                                                                                
         ZAP   SRTPCTCD,=P'0'      PERCENT CLEARED(CALC AT OUTPUT)              
                                                                                
         ZAP   SRTCLRBL,=P'0'      TO BE CLEARED(CALC AT OUTPUT)                
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*                                                                               
         B     DOUNT40                                                          
         GOTO1 HEXOUT,DMCB,SORTREC,P,200                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
DOUNT40  DS    0H                                                               
*                                                                               
DOUNTX   B     EXIT                                                             
         DROP  R2                                                               
                                                                                
* ROUTINE RETURNS $ / RETURNS TO CALLER VIA R5                                  
* INPUT   BYTE = $ CODE                                                         
* OUTPUT  WORK = 1 BYTE CODE + 8 BYTE PACKED OUTPUT                             
*                                                                               
GETDOLS  DS    0H                                                               
         XC    DMCB(16),DMCB                                                    
         LA    RE,WORK                                                          
         ST    RE,DMCB                                                          
         MVC   DMCB(1),BYTE                                                     
         GOTO1 =V(NETACC),DMCB,,NETBLOCK                                        
         BR    R5                                                               
         EJECT                                                                  
*                                                                               
*        WRITE REPORT / TAPE                                                    
*                                                                               
DOTAPE   NTR1                                                                   
         LA    RE,SORTREC          CLEAR SORTREC AREA                           
         L     RF,=F'200'                                                       
         XCEF                                                                   
                                                                                
DOT0     GOTO1 SORTER,DMCB,=C'GET'       TAKE RECS FROM SORTER                  
         L     R3,4(R1)                                                         
         LTR   R3,R3                                                            
         BNZ   DOT10               IF EOF                                       
         BAS   RE,WRTLINE          PRINT ACCUMS                                 
         BAS   RE,WRTTOTS          PRINT TOTALS                                 
         B     DOTX                                                             
*                                                                               
DOT05    LA    RF,RECWORK          FOR TESTING MOVE REC TO RECWORK              
***      LA    R1,SRTRECL          SO WE CAN SEE IT                             
***      LR    RE,R3                                                            
***      MOVE  ((RF),(R1)),(RE)                                                 
                                                                                
DOT10    CLI   SORTREC,0                  IF FIRST TIME                         
         BNE   DOT20                                                            
         LA    RF,SORTREC                                                       
         LA    R1,SRTRECL                                                       
         LR    RE,R3                                                            
         MOVE  ((RF),(R1)),(RE)           YES/SAVE REC IN SORTREC AREA          
                                                                                
DOT20    CLC   0(SRTKEYL,R3),SORTREC       IS IT SAME KEY                       
         BE    DOT30                                                            
         BAS   RE,WRTLINE                    NO-PRINT LINE                      
         USING SORTRECD,R3                                                      
DOT30    AP    TOTGORD,SRTGORD               YES - SO ADD TO COUNTERS           
         AP    TOTNORD,SRTNORD                                                  
         AP    TOTNCLRD,SRTNCLRD                                                
         AP    TOTUNITS,=P'1'                                                   
         B     DOT0                           AND GET NEXT RECORD               
         DROP  R3                                                               
                                                                                
DOTX     B     EXIT                                                             
         EJECT                                                                  
* BREAK ON KEY - WRITE CURRENT NETWORK DATA                                     
*                ADD TO REPROT ACCULULATORS                                     
WRTLINE  NTR1                                                                   
         LA    R6,RECWORK                                                       
         USING VENDTAPE,R6                                                      
         LA    R4,SORTREC                                                       
         USING SORTRECD,R4                                                      
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         OC    SRTKMED(2),SRTKMED  ANY DATA                                     
         BZ    WRTX                NO/EXIT                                      
* NEW MEDIA                                                                     
         CLC   MEDIASV,SRTKMED                                                  
         BE    WRT20                                                            
         CLI   MEDIASV,0           FIRST TIME                                   
         BNE   *+14                                                             
         MVC   MEDIASV,SRTKMED     YES/                                         
         B     WRT05                                                            
         MVC   MEDIASV,SRTKMED     NO/SKIP LINES BEFORE NEW MEDIA               
         MVI   SPACING,2                                                        
         BAS   RE,SPOOLIT                                                       
WRT05    MVC   P2(12),=C'____________'                                          
         MVC   P(1),SRTKMED                                                     
         MVC   P+2(11),=C'NETWORK    '                                          
         CLI   MEDIASV,C'N'                                                     
         BE    WRT10                                                            
         MVC   P+2(11),=C'CABLE      '                                          
         CLI   MEDIASV,C'C'                                                     
         BE    WRT10                                                            
         MVC   P+2(11),=C'SYNDICATION'                                          
         CLI   MEDIASV,C'S'                                                     
         BE    WRT10                                                            
         MVC   P+2(11),=C'OTHER      '                                          
         CLI   MEDIASV,C'O'                                                     
         BE    WRT10                                                            
         MVC   P+2(11),=C'RADIO      '                                          
         CLI   MEDIASV,C'D'                                                     
         BE    WRT10                                                            
         MVC   P+2(11),=C'HISPANIC   '                                          
         CLI   MEDIASV,C'H'                                                     
         BE    WRT10                                                            
         DC    H'0'                                                             
WRT10    BAS   RE,SPOOLIT                                                       
* MEDIA AND NETWORK FROM SAVED RECORD KEY                                       
WRT20    MVC   MEDIASV,SRTKMED     MEDIA                                        
         MVC   PNET,SRTKNET        NETWORK                                      
         MVC   VMED,SRTKMED        *TAPE                                        
         MVC   VNET,SRTKNET        *TAPE                                        
* UNITS                                                                         
         EDIT  (P8,TOTUNITS),(8,PUNITS)                                         
         ZAP   VUNITS,TOTUNITS     *TAPE                                        
* GROSS ORDERED                                                                 
         MVC   DUB,TOTGORD                                                      
         BAS   R5,EDITO                                                         
         MVC   PGORD,WORK+2                                                     
         ZAP   VGORD,TOTGORD       *TAPE                                        
* NET ORDERED                                                                   
         MVC   DUB,TOTNORD                                                      
         BAS   R5,EDITO                                                         
         MVC   PNORD,WORK+2                                                     
         ZAP   VNORD,TOTNORD       *TAPE                                        
* NET CLEARED                                                                   
         MVC   DUB,TOTNCLRD                                                     
         BAS   R5,EDITO                                                         
         MVC   PNCLRD,WORK+2                                                    
         ZAP   VNCLRD,TOTNCLRD     *TAPE                                        
* PERCENT CLEARED                                                               
         CP    TOTNORD,=PL8'0'                                                  
         BNE   WRT30                                                            
         ZAP   PLWORK(8),=P'0'                                                  
         B     WRT35                                                            
WRT30    ZAP   PLWORK,TOTNCLRD                                                  
         MP    PLWORK,=P'10000'                                                 
         DP    PLWORK,TOTNORD                                                   
WRT35    LA    R4,PNPCTCD+5                                                     
         EDIT  (P8,PLWORK),(6,0(R4)),2                                          
         ZAP   VPCTCD,PLWORK(8)    *TAPE                                        
* TO BE CLEARED                                                                 
WRT40    ZAP   PLWORK,TOTNORD                                                   
         SP    PLWORK,TOTNCLRD                                                  
         MVC   DUB,PLWORK+8                                                     
         BAS   R5,EDITO                                                         
         MVC   PNTOBECD,WORK+2                                                  
         ZAP   VNCLRBL,PLWORK+8(8) *TAPE                                        
* ADD TO REPORT TOTALS                                                          
         AP    ALLGORD,TOTGORD                                                  
         AP    ALLNORD,TOTNORD                                                  
         AP    ALLNCLRD,TOTNCLRD                                                
         AP    ALLUNITS,TOTUNITS                                                
* CLEAR BREAK TOTALS                                                            
         ZAP   TOTGORD,=P'0'                                                    
         ZAP   TOTNORD,=P'0'                                                    
         ZAP   TOTNCLRD,=P'0'                                                   
         ZAP   TOTPCTCD,=P'0'                                                   
         ZAP   TOTCLRBL,=P'0'                                                   
         ZAP   TOTUNITS,=P'0'                                                   
* WRITE LINE                                                                    
         BAS   RE,SPOOLIT                                                       
         BAS   RE,WRITAPE                                                       
* SAVE NEW SORTREC KEY                                                          
         LA    RF,SORTREC                                                       
         LA    R1,SRTRECL                                                       
         LR    RE,R3                                                            
         MOVE  ((RF),(R1)),(RE)                                                 
WRTX     XIT1                                                                   
         DROP  R4                                                               
                                                                                
* - DUB CONTAINS PACKED NUMBER TO BE EDITED                                     
EDITO    DS    0H                                                               
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),DUB                                                     
         BR    R5                                                               
         EJECT                                                                  
WRITAPE  NTR1                                                                   
         CLI   TAPEOPT,C'Y'                                                     
         BNE   WRI10                                                            
         L     R1,ANTWKTP                                                       
         PUT   (R1),RECWORK                   WRITE TAPE                        
         XC    RECWORK,RECWORK                                                  
WRI10    B     EXIT                                                             
         GOTO1 HEXOUT,DMCB,RECWORK,P,100                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         DROP  R2                                                               
                                                                                
* REPORT TOTALS                                                                 
WRTTOTS  NTR1                                                                   
         MVI   SPACING,2                                                        
         BAS   RE,SPOOLIT                                                       
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         MVC   PNET,=C'ALL '                                                    
* UNITS                                                                         
         EDIT  (P8,ALLUNITS),(8,PUNITS)                                         
* GROSS ORDERED                                                                 
         MVC   DUB,ALLGORD                                                      
         BAS   R5,EDITO                                                         
         MVC   PGORD,WORK+2                                                     
* NET ORDERED                                                                   
         MVC   DUB,ALLNORD                                                      
         BAS   R5,EDITO                                                         
         MVC   PNORD,WORK+2                                                     
* NET CLEARED                                                                   
         MVC   DUB,ALLNCLRD                                                     
         BAS   R5,EDITO                                                         
         MVC   PNCLRD,WORK+2                                                    
* PERCENT CLEARED                                                               
         CP    ALLNORD,=PL8'0'                                                  
         BNE   WRTT30                                                           
         ZAP   PLWORK(8),=P'0'                                                  
         B     WRTT35                                                           
WRTT30   ZAP   PLWORK,ALLNCLRD                                                  
         MP    PLWORK,=P'10000'                                                 
         DP    PLWORK,ALLNORD                                                   
WRTT35   LA    R4,PNPCTCD+5                                                     
         EDIT  (P8,PLWORK),(6,0(R4)),2                                          
* TO BE CLEARED                                                                 
         ZAP   PLWORK,ALLNORD                                                   
         SP    PLWORK,ALLNCLRD                                                  
         MVC   DUB,PLWORK+8                                                     
         BAS   R5,EDITO                                                         
         MVC   PNTOBECD,WORK+2                                                  
         BAS   RE,SPOOLIT                                                       
         XIT1                                                                   
         EJECT                                                                  
         SPACE                                                                  
*******************************                                                 
*                                                                               
SPOOLIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
HDRTN    NTR1                                                                   
*                                                                               
HD10     LA    R2,H10                                                           
         USING PLINED,R2                                                        
         MVC   PNET,=C'NET '                                                    
         MVC   PUNITS,=C' UNITS'                                                
         MVC   PGORD+1(13),=C'GROSS ORDERED'                                    
         MVC   PNORD+1(13),=C'  NET ORDERED'                                    
         MVC   PNCLRD+1(13),=C'  NET CLEARED'                                   
         MVC   PNPCTCD(11),=C'PCT CLEARED'                                      
         MVC   PNTOBECD+1(13),=C'TO BE CLEARED'                                 
*                                                                               
         DS    0H                                                               
         L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    HDX                 YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         SPACE                                                                  
         LA    R4,BOXCOLS                                                       
         USING PLINED,R4                                                        
         MVI   PSTR,C'L'                                                        
         MVI   PC1,C'C'                                                         
         MVI   PC2,C'C'                                                         
         MVI   PC3,C'C'                                                         
         MVI   PC4,C'C'                                                         
         MVI   PC5,C'C'                                                         
         MVI   PC6,C'C'                                                         
         MVI   PEND,C'R'                                                        
         SPACE                                                                  
         LA    R4,BOXROWS                                                       
         LA    R4,8(R4)                                                         
         MVI   0(R4),C'T'                                                       
         MVI   2(R4),C'M'                                                       
         LA    R4,49(R4)                                                        
         MVI   0(R4),C'B'                                                       
         SPACE                                                                  
HDX      B     EXIT                 (XIT1)                                      
         DROP  R2,R4                                                            
         EJECT                                                                  
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C' NETWORK MEDIA TAPE REPORT'                              
         SSPEC H2,52,C' -------------------------'                              
         SSPEC H3,53,PERIOD                                                     
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H3,98,REPORT                                                     
         SSPEC H4,98,RUN                                                        
         SSPEC H5,98,PAGE                                                       
         DC    X'00'                                                            
*                                                                               
NTWKTP   DCB   DDNAME=NTWKTP,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00200,                                            X        
               BLKSIZE=02000,                                          X        
               MACRF=PM                                                         
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS1/ANETWS2                 
*                                                                               
*                                                                               
RELO     DS    F                                                                
ANTWKTP  DS    F                                                                
TAPEOPT  DS    CL1                                                              
MEDIASV  DS    CL1                                                              
*                                                                               
SORTREC  DS    CL200                                                            
SRTKEYSV DS    CL(L'SRTKEYL)                                                    
PLWORK   DS    PL16                PACKED LENGTH WORK AREA                      
RECWORK  DS    CL200               WORK AREA                                    
*                                                                               
TOTGORD  DS    PL8                 TOTAL GROSS ORDERED                          
TOTNORD  DS    PL8                 TOTAL NET ORDERED                            
TOTNCLRD DS    PL8                 TOTAL NET CLEARED                            
TOTPCTCD DS    PL8                 TOTAL PCT CLEARED                            
TOTCLRBL DS    PL8                 TOTAL TO BE CLEARED                          
TOTUNITS DS    PL8                 TOTAL UNITS                                  
*                                                                               
ALLGORD  DS    PL8                 REPORT GROSS ORDERED                         
ALLNORD  DS    PL8                 REPORT NET ORDERED                           
ALLNCLRD DS    PL8                 REPORT NET CLEARED                           
ALLCLRBL DS    PL8                 REPORT NET CLEARABLE                         
ALLUNITS DS    PL8                 REPORT UNITS                                 
*                                                                               
WORKLENE EQU   *-RELO              MAX LEN=1700                                 
*                                  ANETWS3 FOR 4000 = NBANBUFF                  
                                                                                
                                                                                
*                                                                               
SORTRECD DSECT                                                                  
         DS    0CL200                                                           
SRTKMED  DS    CL1                 EFFECTIVE MEDIA                              
SRTKNET  DS    CL4                 NETWORK                                      
         DS    CL15                SPARE                                        
SRTKEYL  EQU   *-SRTKMED                                                        
SRTUNITS DS    PL8                 UNITS                                        
SRTGORD  DS    PL8                 GROSS ORDERED(ACT+INT+SPECIALS)              
SRTNORD  DS    PL8                 NET ORDERED  (ACT+INT+SPECIALS)              
SRTNCLRD DS    PL8                 NET CLEARED  (ACT+INT+SPECIALS)              
SRTPCTCD DS    PL8                 PCT CLEARED                                  
SRTCLRBL DS    PL8                 TO BE CLEARED (SRTNORD-SRTNCLRD)             
         DS    CL132               SORTSPARE                                    
SRTRECL  EQU   *-SRTKMED                                                        
         EJECT                                                                  
*                                                                               
PLINED   DSECT                    DSECT FOR PRINTED REPORT                      
         DS    CL20                                                             
PSTR     DS    CL1                                                              
PNET     DS    CL4                                                              
PC1      DS    CL1                                                              
PUNITS   DS    CL8                                                              
PC2      DS    CL1                                                              
PGORD    DS    CL16                                                             
PC3      DS    CL2                                                              
PNORD    DS    CL16                                                             
PC4      DS    CL2                                                              
PNCLRD   DS    CL16                                                             
PC5      DS    CL2                                                              
PNPCTCD  DS    CL11                                                             
PC6      DS    CL2                                                              
PNTOBECD DS    CL16                                                             
PEND     DS    CL1                                                              
PLENE    EQU   *-PLINED                                                         
*                                                                               
*                                                                               
         EJECT                                                                  
         SPACE                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIDFD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE VENTAPED                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046NEWRI71   06/28/06'                                      
         END                                                                    
