*          DATA SET SPREPMN02  AT LEVEL 003 AS OF 09/13/01                      
*PHASE SPMN02A                                                                  
*INCLUDE TIMVAL                                                                 
         TITLE 'SPMN02 - WORKER FILES FROM DDS CCUSA AGENCIES'                  
SPMN02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPMN02,R8,RR=R2                                                
         ST    R2,RELO                                                          
                                                                                
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
                                                                                
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCBUY                                                     
         BE    MNHDR                                                            
         CLI   MODE,REQLAST                                                     
         BE    MNX                                                              
                                                                                
         CLI   MODE,MGR1FRST                                                    
         BNE   EXIT                                                             
         CLC   MGR1+1(2),=C'99'                                                 
         BE    MNX                                                              
         B     EXIT                                                             
                                                                                
RELO     DC    A(0)                                                             
                                                                                
REQF     DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(1,TDAY)                                       
         BAS   RE,WRKROPEN                                                      
         XC    LASTBUY,LASTBUY                                                  
         XC    LASTMKT,LASTMKT                                                  
         XC    LASTEST,LASTEST                                                  
         XC    LASTPRD,LASTPRD                                                  
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                         HEADER RECORD                               *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
MNHDR    DS    0H                                                               
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         CLC   BPRD,BDMASPRD                                                    
         BNE   EXIT                                                             
                                                                                
         LA    R6,BDELEM                                                        
         USING REGELEM,R6                                                       
         XC    BUYDATE,BUYDATE                                                  
                                                                                
MNHDR10  DS    0H                  FIND DATE IN LAST 0B/0C ELEMENT              
         CLI   RCODE,0             NO MORE ELEMENTS                             
         BE    MNHDR20                                                          
         CLI   RCODE,X'0B'         READ THE '0B' & '0C' ELEMENTS                
         BE    *+12                                                             
         CLI   RCODE,X'0C'                                                      
         BNE   *+10                                                             
         MVC   BUYDATE,RDATE                                                    
         ZIC   R1,RLEN                                                          
         AR    R6,R1                                                            
         B     MNHDR10                                                          
         DROP  R6                                                               
                                                                                
MNHDR20  DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,BUYDATE),(0,WORK)                                 
         CLC   QSTART,WORK         IGNORE BUYS BEFORE START DATE                
         BH    EXIT                                                             
                                                                                
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         LA    R3,IO2+10                                                        
         LA    R1,IO2L                                                          
         XC    0(14,R1),0(R1)                                                   
         CLI   RCWRITE,C'Y'                                                     
         BE    MNHDR30                                                          
         GOTO1 HEXOUT,DMCB,(R6),P1,13                                           
         GOTO1 REPORT                                                           
                                                                                
MNHDR30  NI    RECFLAG,X'FF'-NONEWHDR                                           
                                                                                
MNHDR40  DS    0H                                                               
         CLC   LASTBUY,BUYKEY      A/M,CLT                                      
         BNE   MNHDR50                                                          
         CLC   LASTPRD,BPRD        PRD                                          
         BNE   MNHDR50                                                          
         CLC   LASTMKT,BUYMSTA     MARKET                                       
         BNE   MNHDR50                                                          
         CLC   LASTEST,BUYKEST     EST                                          
         BNE   MNHDR50                                                          
         OI    RECFLAG,NONEWHDR                                                 
         B     MNHDR90                                                          
                                                                                
MNHDR50  DS    0H                                                               
         LA    R1,300                                                           
         C     R1,SEQNUM           IF AT LEAST 300 RECORDS,                     
         BH    MNHDR60              OPEN ANOTHER WORKER FILE                    
         BAS   RE,WRKRCLSE                                                      
         BAS   RE,WRKROPEN                                                      
                                                                                
MNHDR60  DS    0H                  MAKE SURE WE HAVE RIGHT EST RECORD           
         L     R5,ADEST                                                         
         USING ESTHDRD,R5                                                       
         CLC   EKEYAM(3),BUYKEY    A/M,CLT                                      
         BNE   MNHDR65                                                          
         CLC   EKEYPRD,PRD         PRD                                          
         BNE   MNHDR65                                                          
         CLC   EKEYEST,BUYKEST     EST                                          
         BE    MNHDR70                                                          
                                                                                
MNHDR65  DS    0H                                                               
         MVC   SVBKEY,KEY                                                       
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         MVC   EKEYAM(3),BUYKEY    A/M,CLT                                      
         MVC   EKEYPRD,PRD         PRD                                          
         MVC   EKEYEST,BUYKEST     EST                                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 GETEST                                                           
         DROP  R5                                                               
                                                                                
         XC    KEY,KEY             RESET BUY                                    
         MVC   KEY(L'SVBKEY),SVBKEY                                             
         GOTO1 HIGH                                                             
         CLC   SVBKEY,KEY                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
MNHDR70  DS    0H                                                               
         MVC   LASTBUY,BUYKEY                                                   
         MVC   LASTMKT,BUYMSTA                                                  
         MVC   LASTEST,BUYKEST                                                  
         MVC   LASTPRD,BPRD                                                     
         USING SHDRD,R3                                                         
         XC    SHDRERNO(SHDRTYPE-SHDRODAT),SHDRERNO                             
         MVI   SHDRSTRT,C' '       PAD WITH SPACES                              
         MVC   SHDRSTRT+1(SHDRRLNQ-1),SHDRSTRT                                  
                                                                                
         MVC   SHDRLEN,=AL2(SHDRLENQ)                                           
         MVC   SHDRTYPE,=C'HDR*'   OBJECT TYPE                                  
         MVI   SHDRSYS,C'S'        SYSTEM                                       
         MVC   SHDRUTYP,=C'BUY'    UPLOAD TYPE                                  
                                                                                
         MVC   SHDRAGID,=C'CK'                                                  
         MVC   SHDRMED,MED                                                      
                                                                                
         CLC   QAGY,=C'LI'                                                      
         BNE   *+10                                                             
         MVC   SHDRCLT,=C'LIB'                                                  
         CLC   QAGY,=C'AP'                                                      
         BNE   *+10                                                             
         MVC   SHDRCLT,=C'APS'                                                  
         CLI   QMGR,C' '                                                        
         BE    *+10                                                             
         MVC   SHDRCLT,MGR1NM                                                   
         CLC   =C'***',MGR1NM                                                   
         BNE   *+6                                                              
         DC    H'0'                MARKET NOT IN MARKET GROUP                   
                                                                                
MNHDR80  DS     0H                                                              
         MVC   SHDRPRD,PRD                                                      
         EDIT  (B1,BUYKEST),(3,SHDREST),FILL=0                                  
         L     R5,ADEST                                                         
         USING ESTHDRD,R5                                                       
         MVC   SHDRSTPD,ESTART                                                  
         MVC   SHDRENPD,EEND                                                    
         DROP  R5                                                               
                                                                                
MNHDR90  DS    0H                                                               
         MVI   DEMOVAL,C'0'                                                     
         MVC   DEMOVAL+1(83),DEMOVAL                                            
                                                                                
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   MNHDR130                                                         
         USING NDELEM,R6                                                        
         CLI   NDELEM,2                                                         
         BNE   MNHDR130                                                         
                                                                                
         LA    R4,NDEMNO           START OF DEMOS                               
         SR    R4,R6               LENGTH OF ELEMENT BEFORE DEMOS               
         ZIC   R5,NDLEN            LENGTH OF ELEMENT                            
         SR    R5,R4               LENGTH OF DEMOS IN ELEMENT                   
         CH    R5,=H'8'            CHECK IF ANY DEMOS LEFT                      
         BL    MNHDR130                                                         
                                                                                
         LA    R2,NDEMNO                                                        
         LA    R4,DEMOVAL                                                       
         LA    R7,SHDRDEMO                                                      
                                                                                
         L     R0,AMYBLOCK         SET 'TO' ADDRESS                             
         LA    R1,L'MYBLOCK        SET 'TO' LEN                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R1,MYBLOCK          TEMPORARY USAGE                              
         USING DBLOCKD,R1                                                       
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE(3),=C'TP '                                                
         MVC   DBSELMED,MED                                                     
         DROP  R1                                                               
                                                                                
MNHDR100 DS    0H                                                               
         L     RE,ADEST                                                         
         USING ESTHDRD,RE                                                       
         LA    R1,42               MAX NUMBER OF DEMOS                          
         LA    RE,EDEMOS                                                        
                                                                                
MNHDR110 DS    0H                                                               
         OC    0(3,RE),0(RE)                                                    
         BZ    MNHDR120                                                         
         CLC   0(3,RE),0(R2)                                                    
         BE    *+12                                                             
         LA    RE,3(RE)                                                         
         BCT   R1,MNHDR110                                                      
         DROP  RE                                                               
                                                                                
         ST    R7,DMCB+4                                                        
         MVI   DMCB+4,2                                                         
         GOTO1 DEMOCON,DMCB,(R2),,(C'S',AMYBLOCK),(SPOTPROF+9,SPUSRNMS)         
         EDIT  (B3,5(R2)),(6,(R4)),0,FILL=0                                     
                                                                                
         LA    R7,L'SHDRDEMO(R7)   POINT TO NEXT "TO" DEMO                      
         LA    R4,L'DEMOVAL(R4)    POINT TO NEXT "TO" DEMO VALUE                
                                                                                
MNHDR120 DS    0H                                                               
         SH    R5,=H'8'            DEMO LENGTH                                  
         CH    R5,=H'8'            DEMO LENGTH                                  
         BL    MNHDR130            CHECK IF ANY DEMOS LEFT                      
                                                                                
         LA    R2,8(R2)            POINT TO NEXT "FROM" DEMO                    
         B     MNHDR100                                                         
         DROP  R6                                                               
                                                                                
MNHDR130 TM    RECFLAG,NONEWHDR                                                 
         BO    MNBUY                                                            
         MVC   IO2L(2),=Y(SHDRLENQ+16)                                          
         BAS   RE,WRKR                                                          
                                                                                
         DROP  R3                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                            BUY RECORD                               *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
MNBUY    DS    0H                                                               
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         USING SBUYD,R3                                                         
                                                                                
         XC    SBUYODAT(SBUYLINE-SBUYODAT),SBUYODAT                             
         MVI   SBUYSTRT,C' '       PAD WITH SPACES                              
         MVC   SBUYSTRT+1(SBUYRLNQ-1),SBUYSTRT                                  
                                                                                
         MVC   SBUYLEN,=AL2(SBUYLENQ)                                           
         MVC   SBUYTYPE,=C'BUY*'   OBJECT TYPE                                  
                                                                                
         MVC   SBUYSTA(L'STA),STA         STATION                               
                                                                                
         LA    R7,SBUYROT+6                                                     
         LA    RE,7                                                             
         ZIC   R4,BDDAY                                                         
MNBUY10  SR    R5,R5                                                            
         MVI   0(R7),C'N'                                                       
         SRDL  R4,1                SHIFT BIT TO R5                              
         LTR   R5,R5               TEST IF ON                                   
         BZ    *+12                NO                                           
         MVI   0(R7),C'Y'                                                       
         STC   RE,BYTE                                                          
         BCTR  R7,0                                                             
         BCT   RE,MNBUY10                                                       
                                                                                
         EDIT  (B1,BYTE),(1,SBUYRDAY)                ROTATION START DAY         
         EDIT  (B2,BDTIMST),(4,SBUYSTIM),0,FILL=0    START TIME                 
         EDIT  (B2,BDTIMEND),(4,SBUYETIM),0,FILL=0   END TIME                   
                                                                                
         MVC   SBUYDPT,BDDAYPT                       DAYPART                    
         EDIT  (B1,BDSEC),(3,SBUYSLEN),0,FILL=0      SPOT LENGTH                
         MVI   SBUYLUNT,C'S'       SPOT LENGTH UNITS ALWAYS SECONDS             
                                                                                
         MVC   SBUYPROG(L'B1PRGMNM),BDPROGRM          PROGRAM NAME              
         LA    R4,L'SBUYPROG       GET RID OF ALL COMMAS                        
         LA    R5,SBUYPROG                                                      
MNBUY20  DS    0H                                                               
         CLI   0(R5),C','                                                       
         BNE   *+8                                                              
         MVI   0(R5),C' '                                                       
         LA    R5,1(R5)                                                         
         BCT   R4,MNBUY20                                                       
                                                                                
         EDIT  (B3,BDCOST),(9,SBUYCOST),0,FILL=0      SPOT COST                 
                                                                                
         TM    BDCIND2,X'04'       CONVERTED BDCIND FLAG?                       
         BNO   *+14                                                             
         MVC   SBUYCQLF,BDCIND                                                  
         B     MNBUY50                                                          
                                                                                
         LA    R7,COSTTAB                                                       
         LA    RE,L'COSTTAB                                                     
         ZIC   R4,BDCIND                                                        
         SRDL  R4,1                IGNORE 01 BIT, JUST MEANS MINUS SPOT         
MNBUY30  SR    R5,R5                                                            
         SRDL  R4,1                SHIFT BIT TO R5                              
         LTR   R5,R5               TEST IF ON                                   
         BZ    MNBUY40             NO                                           
         MVC   SBUYCQLF,0(R7)                                                   
         B     MNBUY50                                                          
MNBUY40  LA    R7,1(R7)                                                         
         BCT   RE,MNBUY30                                                       
                                                                                
         TM    BDCIND2,X'80'                                                    
         BO    *+6                                                              
         DC    H'0'                                                             
         MVI   SBUYCQLF,C'C'                                                    
                                                                                
MNBUY50  MVC   SBUYUID+0(3),EST          EST(3),BUY#(3)                         
         EDIT  (1,BUYKBUY),(3,SBUYUID+3),0,FILL=0   BUYLINE NUMBER              
                                                                                
         MVI   ELCODE,X'70'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SBUYNUM+L'SBUYNUM(6),3(R6)                                       
                                                                                
         L     R6,ADBUY                                                         
         MVC   IO2L(2),=Y(SBUYLENQ+16)                                          
         BAS   RE,WRKR                                                          
                                                                                
         DROP  R3                                                               
                                                                                
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                       SCHEDULE RECORD                               *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
         USING SSKDD,R3                                                         
         XC    SSKDODAT(SSKDSTRT-SSKDODAT),SSKDODAT                             
         MVI   SSKDSTRT,C' '       PAD WITH SPACES                              
         MVC   SSKDSTRT+1(SSKDLNQ-1),SSKDSTRT                                   
                                                                                
         MVC   SSKDLEN,=AL2(SSKDLENQ)                                           
         MVC   SSKDTYPE,=C'SKD*'   OBJECT TYPE                                  
                                                                                
         GOTO1 DATCON,DMCB,(3,BDSTART),(0,SSKDSDT)                              
         MVC   BUYSTART,SSKDSDT                                                 
                                                                                
         XC    SCHEDTAB(14),SCHEDTAB       INTIALIZE TO NO SPOTS                
         LA    R6,BDELEM                                                        
         USING REGELEM,R6                                                       
                                                                                
MNSCHD20 CLI   RCODE,0             NO MORE ELEMENTS                             
         BE    MNSCHD40                                                         
         CLI   RCODE,X'0B'         READ THE '0B' &'0C' ELEMENTS                 
         BE    *+12                                                             
         CLI   RCODE,X'0C'                                                      
         BNE   MNSCHD30                                                         
                                                                                
         MVC   SVRDATE,RDATE                                                    
         BAS   RE,SCHED            ADD TO SCHEDULE TABLE                        
                                                                                
MNSCHD30 ZIC   R1,RLEN                                                          
         AR    R6,R1                                                            
         B     MNSCHD20                                                         
         DROP  R6                                                               
                                                                                
MNSCHD40 L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
         LA    R2,SCHEDTAB                                                      
         LA    R4,SSKDCNTR                                                      
         LA    R5,14               NUMBER OF WEEKS                              
                                                                                
MNSCHD50 EDIT  (B1,(R2)),(2,(R4)),0,FILL=0                                      
         LA    R4,L'SSKDCNTR(R4)                                                
         LA    R2,L'SCHEDTAB(R2)                                                
         BCT   R5,MNSCHD50                                                      
                                                                                
         MVC   IO2L(2),=Y(SSKDLENQ+16)                                          
         BAS   RE,WRKR                                                          
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                      COMMENT RECORD                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
         MVI   ELCODE,X'66'                                                     
         BAS   RE,GETEL                                                         
         BNE   MNDEMO                                                           
                                                                                
         USING SCOMD,R3                                                         
MNCOM10  XC    SCOMODAT(SCOMSTRT-SCOMODAT),SCOMODAT                             
         MVI   SCOMDATA,C' '       PAD WITH SPACES                              
         MVC   SCOMDATA+1(SCOMLNQ-5),SCOMDATA                                   
         MVC   SCOMLEN,=AL2(SCOMLENQ)                                           
         MVC   SCOMTYPE,=C'COM*'   OBJECT TYPE                                  
                                                                                
         USING COMELEM,R6                                                       
         ZIC   R1,CMLEN            LENGTH OF COMMENT ELEMENT                    
         SH    R1,=H'3'            LENGTH OF COMMENT-1                          
         EX    R1,*+4                                                           
         MVC   SCOMDATA(0),CMDATA                                               
                                                                                
         MVC   IO2L(2),=Y(SCOMLENQ+16)                                          
         BAS   RE,WRKR                                                          
         BAS   RE,NEXTEL                                                        
         BE    MNCOM10                                                          
                                                                                
         DROP  R3,R6                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                           DEMO RECORD                               *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
MNDEMO   DS    0H                                                               
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
                                                                                
         USING SDEMD,R3                                                         
         XC    SDEMODAT(SDEMSTRT-SDEMODAT),SDEMODAT                             
         MVI   SDEMDEM,C' '        PAD WITH SPACES                              
         MVC   SDEMDEM+1(SDEMRLNQ-4),SDEMDEM                                    
                                                                                
         MVC   SDEMLEN,=AL2(SDEMLENQ)                                           
         MVC   SDEMTYPE,=C'DEM*'   OBJECT TYPE                                  
                                                                                
         MVC   SDEMDEM(84),DEMOVAL                                              
                                                                                
         MVC   IO2L(2),=Y(SDEMLENQ+16)                                          
         BAS   RE,WRKR                                                          
         DROP  R3                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   END-OF-BUY RECORD                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
         USING SEBYD,R3                                                         
         XC    SEBYODAT(SEBYSTRT-SEBYODAT),SEBYODAT                             
                                                                                
         MVC   SEBYLEN,=AL2(SEBYLENQ)                                           
         MVC   SEBYTYPE,=C'EBY*'   OBJECT TYPE                                  
                                                                                
         MVC   IO2L(2),=Y(SEBYLENQ+16)                                          
         BAS   RE,WRKR                                                          
         DROP  R3                                                               
         B     EXIT                                                             
                                                                                
MNX      DS    0H                                                               
         LA    R1,5                PURGE IF NO NEW ENTRIES                      
         C     R1,SEQNUM                                                        
         BL    *+8                                                              
         OI    RECFLAG,PRGWRKR     DELETE LAST WORKER FILE                      
         BAS   RE,WRKRCLSE                                                      
         TM    RECFLAG,PRGWRKR     DELETE LAST WORKER FILE                      
         BNO   EXIT                                                             
         GOTO1 DATAMGR,DMCB,=C'PURGE   ',WRKFILEN,WRKRINDX,IO2,AWKBUFF          
         NI    RECFLAG,X'FF'-PRGWRKR                                            
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   OPEN WORKER FILE                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKROPEN NTR1                                                                   
         XC    SEQNUM,SEQNUM                                                    
         USING WLHDRD,R3                                                        
         LA    R3,IO2                                                           
         XC    0(255,R3),0(R3)     BUILD HEADER                                 
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
                                                                                
         MVC   WLUSRID,=AL2(4401)                                               
         MVI   WLSUBPRG,C'1'       DESTINATION = FACADV1                        
         CLI   QOPT1,C'T'          OPTION TO GENERATE ON TST                    
         BNE   *+8                                                              
         MVI   WLSUBPRG,C'T'       DESTINATION = FACTST                         
                                                                                
         MVC   WLSYSPRG,=C'CCX'                                                 
         MVC   WLDAY,TDAY+2                                                     
         MVI   WLCLASS,C'T'        CLASS R FOR SCRIPT EXEC                      
         MVI   WLTYPE,C'A'         TYPE A FOR IMMEDIATE EXECUTION               
         MVC   WLPSWD,SPACES                                                    
         OI    WLATTB,WLATOBJ                                                   
                                                                                
         MVI   FIXED,C'Y'                                                       
         BAS   RE,WRKR                                                          
                                                                                
         XC    SEQNUM,SEQNUM                                                    
         XC    WRKRINDX,WRKRINDX                                                
         LA    R5,WRKRINDX                                                      
         USING UKRECD,R5                                                        
         MVC   UKUSRID,=X'1131'    FOR COKEAT                                   
         MVC   UKSYSPRG(3),=C'CCX'                                              
         MVC   UKFILENO,WLREPRNO   WORKER FILE NUMBER                           
         DROP  R5                                                               
                                                                                
         MVC   WRKFNO,WLREPRNO     WORKER FILE NUMBER                           
         MVI   FIXED,C'N'                                                       
         DROP  R3                                                               
                                                                                
         LA    R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   0(4,R1),=F'2101'                                                 
         MVC   4(6,R1),=C'SCRIPT'                                               
         MVC   10(8,R1),=C'SPCOKUPL'                                            
         MVI   18(R1),C'S'        INSERT MODE/USER SETS FINAL STATUS            
         MVC   30(5,R1),=C'00006'    HEADER LENGTH                              
         MVC   35(5,R1),=C'00100'    INPUT  DATA LENGTH                         
         MVC   40(5,R1),=C'00020'    OUTPUT DATA LENGTH                         
         MVI   45(R1),C'N'           DO NOT INSERT ERRORS AT FILE END           
                                                                                
         MVC   IO2L(2),=H'50'        46 + 4 BYTES FOR QSAM                      
         BAS   RE,WRKR                                                          
                                                                                
         LA    R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   10(20,R1),=CL20'SIGN-ON INFORMATION '                            
         MVC   30(6,R1),=C'COKEAT'                                              
                                                                                
         MVC   IO2L(2),=H'40'        36 + 4 BYTES FOR QSAM                      
         BAS   RE,WRKR                                                          
                                                                                
         LA    R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   30(4,R1),=C'@#$%'              MEDIA                             
         EDIT  WRKFNO,(10,34(R1)),0,ALIGN=LEFT   BUYER(WORKER FILE NO.)         
                                                                                
         MVC   IO2L(2),=H'47'                 43 + 4 BYTES FOR QSAM             
         BAS   RE,WRKR                                                          
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                       CLOSE WORKER FILE                             *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKRCLSE NTR1                                                                   
         USING WLHDRD,R4                                                        
         MVI   FIXED,C'Y'                                                       
         LA    R4,IO2                                                           
         XC    0(255,R4),0(R4)     BUILD HEADER                                 
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
         XC    SEQNUM,SEQNUM                                                    
         BAS   RE,WRKR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                              WORKER                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKR     NTR1                                                                   
         OC    SEQNUM,SEQNUM                                                    
         BZ    WRKR10                                                           
         MVC   IO2(4),=F'2102'                                                  
         EDIT  SEQNUM,(6,IO2+4),0,FILL=0                                        
                                                                                
WRKR10   DS    0H                                                               
         LA    R3,IO2                                                           
         CLI   FIXED,C'Y'                                                       
         BE    *+8                                                              
         LA    R3,IO2L                                                          
                                                                                
         GOTO1 DATAMGR,DMCB,DMPRINT,WRKFILE,0,(R3),AWKBUFF                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         ST    R1,SEQNUM                                                        
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                             SCHEDULE                                *         
*  BEFORE: SVRDATE HAS RDATE FROM '0B' OR '0C' ELEMENT                *         
*          BUYSTART HAS BUY START DATE                                *         
*          R6 POINTS TO ELEMENT                                                 
*  AFTER:  FINDS ENTRY IN SCHDTAB TO INCREMENT NUMBER OF SPOTS/WEEK   *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
SCHED    NTR1                                                                   
         GOTO1 DATCON,DMCB,(2,SVRDATE),(0,EBCRDATE)                             
         MVC   THISWEEK,BUYSTART                                                
         LA    R4,14               NUMBER OF WEEKS                              
         LA    R2,SCHEDTAB                                                      
         B     *+14                                                             
                                                                                
SCHED10  MVC   THISWEEK,NEXTWEEK                                                
         LA    R2,1(R2)                                                         
         GOTO1 ADDAY,DMCB,THISWEEK,NEXTWEEK,F'7'                                
         CLC   EBCRDATE,THISWEEK                                                
         BL    SCHED20                                                          
         CLC   EBCRDATE,NEXTWEEK                                                
         BNL   SCHED20                                                          
*                                                                               
         ZIC   R1,0(R2)            GET CURRENT ACCUM                            
         LA    R0,1                SET 1 SPOT                                   
         L     RE,ADBUY                                                         
         TM    BDSTAT-BUYREC(RE),X'80'  TEST POL NPW DATA                       
         BZ    SCHED18                  NO                                      
         IC    R0,7(R6)                 GET NUMBER OF SPOTS                     
         SRL   R0,2                     RIGHT ALIGN IN REG                      
*                                                                               
SCHED18  TM    6(R6),X'80'         TEST MINUS                                   
         BZ    *+6                                                              
         LCR   R0,R0               MAKE SPOTS NEGATIVE                          
         AR    R1,R0                                                            
         STC   R1,0(R2)                                                         
*                                                                               
SCHED20  BCT   R4,SCHED10                                                       
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
         GETEL R6,24,ELCODE                                                     
         LTORG                                                                  
         EJECT                                                                  
COSTTAB  DC    CL7'XSVN QF'                                                     
DMPRINT  DC    CL8'DMPRINT'                                                     
WRKFILE  DC    CL8'WRKF1  '                                                     
WRKFILEN DC    CL8'WRKFILE'                                                     
AWKBUFF  DC    A(WKBUFF)                                                        
AMYBLOCK DC    A(MYBLOCK)                                                       
ADEMO    DS    F                   ADDRESS OF DEMO                              
SEQNUM   DS    F                   SEQUENCE NUMBER OF RECORDS                   
RECFLAG  DS    X                                                                
PRGWRKR  EQU   X'80'               PURGE WORKER FILE                            
NONEWHDR EQU   X'40'               DON'T PUT OUT HDR RECORD                     
ELCODE   DS    X                                                                
SVBKEY   DS    XL13                SAVE BUY KEY                                 
LASTBUY  DS    CL3                 LAST BUY PROCESSED A/M,CLT                   
LASTMKT  DS    XL2                 LAST BUY PROCESSED MARKET                    
LASTEST  DS    X                   LAST BUY PROCESSED EST                       
LASTPRD  DS    X                   LAST BUY PROCESSED PRD                       
BUYDATE  DS    XL2                 DATE OF LAST 0B/0C ELEMENT                   
BUYSTART DS    CL6                 BUY START DATE                               
EBCRDATE DS    CL6                 SPOT DATE                                    
THISWEEK DS    CL6                                                              
NEXTWEEK DS    CL6                                                              
SVRDATE  DS    XL2                                                              
TDAY     DS    CL3                 YYMMDD PWOS                                  
DEMNUM   DS    XL3                                                              
WRKFNO   DS    XL2                 WORKER FILE NUMBER                           
FIXED    DS    CL1                 FIXED LENGTH                                 
SIGNON2H DS    XL2                 2 BYTE HEX AGENCY ID                         
RFLAG    DS    CL1                                                              
LASTREC  DS    CL2                                                              
STARTIME DS    CL5                 START TIME                                   
         DC    C'-'                                                             
ENDTIME  DS    CL5                 END TIME                                     
BSTTIME  DS    XL2                                                              
BENTIME  DS    XL2                                                              
SVSTDATE DS    CL6                 SAVE START DATE                              
SVSTATN  DS    CL8                 SAVE STATION                                 
DMACTN   DS    CL5                                                              
WRKRCMD  DS    CL7                                                              
WRKRINDX DS    CL42                                                             
ELEM     DS    CL256                                                            
IO       DS    XL256               IO AREA                                      
IO2L     DS    F                                                                
IO2      DS    XL400               IO AREA                                      
DEMOVAL  DS    14CL6               DEMO VALUES                                  
SCHEDTAB DS    14X                 SCHEDULE TABLE                               
MYBLOCK  DS    XL265                                                            
WKBUFF   DS    14336C                                                           
*                                                                               
UH1      DSECT                                                                  
         DS    CL127                                                            
H1STATN  DS    CL8                                                              
H1STDATE DS    CL5                                                              
H1ENDATE DS    CL5                                                              
H1STYEAR DS    CL2                                                              
*                                                                               
UH2      DSECT                                                                  
         DS    CL100                                                            
H2DEMOS  DS    CL48                                                             
*                                                                               
UB1      DSECT                                                                  
         DS    CL5                                                              
B1PRGMNM DS    CL14                                                             
B1DAYPRT DS    CL2                                                              
         DS    CL16                                                             
B1STTIME DS    CL5                                                              
B1ENTIME DS    CL5                                                              
B1ROTATN DS    CL7                                                              
B1SPTLEN DS    CL3                                                              
         DS    CL6                                                              
B1SPCOST DS    CL10                                                             
         DS    CL10                                                             
B1SKD    DS    52CL3                                                            
*                                                                               
         EJECT                                                                  
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
                                                                                
         PRINT OFF                                                              
       ++INCLUDE SPTUPLOADD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE SPGENBUY                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPMN02 09/13/01'                                      
         END                                                                    
