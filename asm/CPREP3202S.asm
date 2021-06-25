*          DATA SET CPREP3202S AT LEVEL 039 AS OF 05/01/02                      
*PHASE CP3202A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE EDITOR                                                                 
CP3202   TITLE 'CPREP3202-CPP INDUSTRY CPP REPORT'                              
*                                                                               
*  QGRP   = S FOR SPOT MARKET GROUPS                                            
*  QGRP+1 = T FOR INTERAGENCY INTERFACE TAPE                                    
*  QGRP+1 = D FOR INTERAGENCY INTERFACE DISK                                    
*  QGRP+1 = P FOR PLANNING INTERFACE TAPE                                       
*           GROUP ID IF QGRP=S                                                  
*                                                                               
         PRINT NOGEN                                                            
CP3202   CSECT                                                                  
         NMOD1 0,CP3202,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING CPWORKD,RA,RC                                                    
         LA    R6,2048(RB)                                                      
         LA    R6,2048(R6)                                                      
         USING CP3202+4096,R6                                                   
         L     R2,=V(CP32WK)                                                    
         USING CP32WK,R2                                                        
         STM   RA,RC,CP32RA                                                     
         ST    R6,CP32R6                                                        
         ST    R2,CP32R2                                                        
         L     R3,ADDATA                                                        
         USING CPKEYD,R3                                                        
         L     RE,=V(MYHEAD)                                                    
         ST    RE,HEADHOOK                                                      
         GOTO1 =V(SETUP),DMCB,(RA)                                              
         LA    RE,P1                                                            
         LA    RF,13                                                            
         XC    0(132,RE),0(RE)                                                  
         LA    RE,132(RE)                                                       
         BCT   RF,*-10                                                          
*                                                                               
         CLI   MODE,PROCDATA                                                    
         BNE   FRSTMKT                                                          
         OC    CPKMKT,CPKMKT                                                    
         BZ    EXIT                                                             
         XC    SRREC,SRREC                                                      
         MVC   SRPNTW,=PL8'0'                                                   
         MVC   SRAGY,QAGY                                                       
*                                                                               
         CLI   CPKTYPE,X'06'       CPPRS REPORT BANK RECORD                     
         BNE   *+10                                                             
         MVC   SRAGY,=X'FFFF'                                                   
*                                                                               
         CLC   QPROG,=C'42'        CPPAGY REPORT AGENCY RECORD                  
         BNE   *+8                                                              
         CLI   CPKTYPE,X'04'                                                    
         BNE   *+10                                                             
         MVC   SRAGY,=X'FFFF'                                                   
*                                                                               
         MVC   SRDEMO,CPKDEMO                                                   
         MVC   CPDPT,CPKDAYPT                                                   
         MVC   SRDPT,CPDPT                                                      
         MVI   SRSLN,0             BUILD PROTOTYPE RECORD                       
         MVC   SRSRV,CPKSERV                                                    
         CLI   VSOURCE,C' '        VALID FOR ALL                                
         BE    *+14                                                             
         CLC   SRSRV,VSOURCE                                                    
         BNE   EXIT                                                             
         MVC   SRTGT,CPKTARGT                                                   
         CLI   BANKNUM,0                                                        
         BE    PROD02                                                           
*                                                                               
*        FILTER OUT NON-BANK TARGETS                                            
*                                                                               
         LA    RE,BANKTGT                                                       
         CLI   QMED,C'C'           CHECK FOR CANADIAN TARGETS                   
         BNE   *+8                                                              
         LA    RE,BANKTGTC                                                      
         ZIC   R1,0(RE)            GET NUMBER OF TARGETS                        
         LA    RE,1(RE)                                                         
         CLC   SRTGT,0(RE)         NOW SELECT THEM                              
         BE    *+16                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-14                                                          
         B     EXIT                                                             
*                                                                               
PROD02   MVC   SRDEMO,CPKDEMO                                                   
         MVC   SRPRG,CPKPROG                                                    
         MVC   SRAFL,CPKAFFIL                                                   
         MVC   SRMKT,CPKMKT                                                     
         CLC   SRMKT,=AL2(601)                                                  
         BNE   *+10                                                             
         MVC   SRMKT,=AL2(511)                                                  
         XC    SRWGHT,SRWGHT                                                    
         MVC   SRWGHT+2(2),MKTWT                                                
         LA    R4,PERTABLE                                                      
         USING CPERD,R4                                                         
         L     RF,NPERIODS                                                      
         MVI   CURRPER,1                                                        
*                                                                               
PROD04   CLI   DATESW,1                                                         
         BNE   *+10                                                             
         MVC   SRDTSEQ,CURRPER     SET RELATIVE DATE IN RECORD                  
         L     RE,SRDOL            ACCUMULATE DATA                              
         A     RE,CPCASH                                                        
         ST    RE,SRDOL                                                         
         L     RE,SRSPOT                                                        
         A     RE,CPSPOTS                                                       
         ST    RE,SRSPOT                                                        
         L     RE,SRPNT                                                         
         A     RE,CPOINTS                                                       
         ST    RE,SRPNT                                                         
         L     RE,SRIMP                                                         
         A     RE,CPIMPS                                                        
         ST    RE,SRIMP                                                         
         L     R8,CPOINTS                                                       
         SRDA  R8,32                                                            
         M     R8,SRWGHT                                                        
         CVD   R9,DUB                                                           
         AP    SRPNTW,DUB                                                       
         L     R8,CPCASH                                                        
         SRDA  R8,32                                                            
         M     R8,=F'1000'                                                      
         SLDA  R8,1                                                             
         D     R8,EQVFACT                                                       
         A     R9,=F'1'                                                         
         SRA   R9,1                                                             
         A     R9,SREQUIV                                                       
         ST    R9,SREQUIV                                                       
*                                                                               
         A     R4,WIDTHPER         SET FOR NEXT SLOT                            
         CLI   DATESW,1            DATE DETAIL REQUIRED                         
         BE    *+12                RELEASE TO SORT                              
         BCT   RF,PROD04           OTHERWISE CONTINUE SUMMATION                 
         B     PROD06              UNTIL END                                    
*                                                                               
         ZIC   R9,CURRPER          PRESET TO NEXT PERIOD                        
         LA    R9,1(R9)                                                         
         STC   R9,CURRPER                                                       
         ST    RF,SVPERCNT         PERIOD COUNTER                               
         ST    R4,SVPERPTR         PERIOD POINTER                               
         OC    SRDOL(8),SRDOL      BYPASS IF NO DATA                            
         BZ    PROD08                                                           
*                                                                               
PROD06   XC    SRREN,SRREN         CREATE MARKET RECORDS                        
         MVC   SRMSEQ,MSEQ                                                      
         OC    MKTRANK,MKTRANK     SET SMA MARKET RANK                          
         BNZ   *+10                                                             
         MVC   MKTRANK,=H'240'                                                  
         CLC   MKTRANK,=H'240'                                                  
         BNH   *+10                                                             
         MVC   MKTRANK,=H'240'                                                  
         CLC   QPROG,=C'33'                                                     
         BE    *+10                                                             
         CLC   QPROG,=C'42'                                                     
         BE    *+10                                                             
         CLC   QPROG,=C'32'                                                     
         BNE   *+10                                                             
         MVC   SRMSEQ,MKTRANK                                                   
         MVC   SRRST,MKTRANK                                                    
         MVC   SRREN,SRRST                                                      
         GOTO1 =V(PUTSORT),DMCB,(RA)                                            
*                                                                               
PROD08   CLI   DATESW,1            IF NOT PROCESSING DATES                      
         BNE   EXIT                                                             
         L     RF,SVPERCNT         OTHERWISE SET UP FOR NEXT                    
         L     R4,SVPERPTR                                                      
         XC    SRDATA,SRDATA                                                    
         MVC   SRPNTW,=PL8'0'                                                   
         BCT   RF,PROD04           AND RETURN IF NEEDED                         
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
FRSTMKT  CLI   MODE,MKTFRST                                                     
         BNE   LASTREQ                                                          
         L     RE,ADMKTTAB                                                      
         LA    RF,1                                                             
*                                                                               
FRSTM02  CLC   CPKMKT,0(RE)        FIND MARKET NAME                             
         BE    FRSTM06                                                          
         CLI   0(RE),X'FF'         END                                          
         BE    FRSTM04                                                          
         AH    RE,WIDTHMKT                                                      
         LA    RF,1(RF)            COUNT FOR MARKET SEQ                         
         B     FRSTM02                                                          
*                                                                               
FRSTM04  LA    RF,240                                                           
*                                                                               
FRSTM06  STH   RF,MSEQ             SAVE THE SEQ NUMBER                          
         B     EXIT                                                             
         EJECT                                                                  
LASTREQ  CLI   MODE,REQLAST                                                     
         BNE   LASTRUN                                                          
*                                                                               
         MVC   OPTRDSEQ,SVRDSEQ    RESTORE READ SEQUENCE                        
         CLC   QPROG,=C'38'        TREND REPORT                                 
         BNE   LREQ06                                                           
         LA    R4,PERTABLE                                                      
         USING CPERD,R4                                                         
         ZIC   RE,CPENDB+1                                                      
         SRL   RE,2                DIVIDE BY 4 TO GET QUARTER                   
         SLL   RE,2                X 4 TO GET DISPLACEMENT                      
         L     R1,=A(QCONTAB2)                                                  
         AR    R1,RE                                                            
         MVC   CNH13,0(R1)                                                      
         LA    RF,CNH8                                                          
         A     RE,=A(QCONTAB)                                                   
         LA    R1,4                                                             
*                                                                               
LREQ02   MVC   2(4,RF),0(RE)                                                    
         LA    RE,4(RE)                                                         
         LA    RF,L'CNH8(RF)                                                    
         BCT   R1,LREQ02                                                        
*                                                                               
         MVC   CNH6(4),=C'19  '    SET UP YEAR HEADERS                          
         MVC   CNH6+8(4),=C'19  '                                               
         LA    R1,4                                                             
         LA    R4,PERTABLE                                                      
         LA    R5,CPEND                                                         
         LA    RF,32*4(R5)                                                      
         LA    R9,QYRDATE                                                       
*                                                                               
LREQ04   MVC   0(2,R9),3(R5)       NOW FILL IN ACTUAL YEARS                     
         MVC   2(2,R9),3(RF)                                                    
         LA    R5,32(R5)                                                        
         LA    RF,32(RF)                                                        
         LA    R9,4(R9)                                                         
         BCT   R1,LREQ04                                                        
*                                                                               
LREQ06   XC    HLD2REC,HLD2REC                                                  
         XC    HOLDREC,HOLDREC                                                  
         MVI   EOFSW,0                                                          
         LA    RE,P1                                                            
         LA    RF,13                                                            
         MVC   0(132,RE),SPACES                                                 
         LA    RE,132(RE)                                                       
         BCT   RF,*-10                                                          
*                                                                               
         OC    RECCNT,RECCNT                                                    
         B     LREQ08              NOOP TO PRINT RECORD COUNTS                  
         MVC   P+10(20),=C'TOTAL RECORDS OUTPUT'                                
         EDIT  (B4,RECCNT),(9,P)                                                
         GOTO1 REPORT                                                           
*                                                                               
LREQ08   CLI   ACTSW,1             ACTIVITY                                     
         BE    PRINTIT                                                          
         MVI   ACTSW,2              NO - SET TO BYPASS OPEN                     
         B     EXIT                                                             
*                                                                               
LASTRUN  CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
         CLI   TAPEFRST,1                                                       
         BE    LRUN04                                                           
         L     R3,=A(OUT)                                                       
         CLI   QOPT4,C'Y'          WAS THIS CSV FORMAT?                         
         BNE   LRUN02              NO                                           
         L     R3,=A(MRKCSV)       YES, CLOSE EACH DATASET                      
         CLOSE (OUT)                                                            
         L     R3,=A(DAYCSV)                                                    
         CLOSE (OUT)                                                            
         L     R3,=A(PRGCSV)                                                    
         CLOSE (OUT)                                                            
         L     R3,=A(ACTCSV)                                                    
*                                                                               
LRUN02   CLOSE (R3)                                                             
*                                                                               
LRUN04   CLI   ACTSW,2                                                          
         BNE   EXIT                                                             
         GOTO1 VSORTER,DMCB,=C'END'                                             
         B     EXIT                                                             
         EJECT                                                                  
PRINTIT  MVC   SRREC,HLD2REC                                                    
         MVC   CURRRPT,HLD2RTYP                                                 
         CLI   EOFSW,1                                                          
         BE    PRINT12                                                          
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         L     RE,4(R1)                                                         
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   PRINT2                                                           
         GOTO1 VSORTER,DMCB,=C'END'                                             
         MVI   EOFSW,1             SET EOF AND RELEASE PREV. RECORD             
         XC    HLD2REC,HLD2REC                                                  
         B     PRINT8                                                           
*                                                                               
PRINT2   MVC   HLD2REC,0(RE)       ACCUMULATE DUPS                              
         LA    RE,HLD2REC          SET SORT REC ADDRESS                         
         ST    RE,ASRTREC                                                       
         BAS   RE,RESTDK                                                        
         OC    SRREC,SRREC                                                      
         BZ    PRINTIT                                                          
         CLC   HLD2KEY,SRREC                                                    
         BNE   PRINT8                                                           
*                                                                               
PRINT4   DS    0H                                                               
         LA    RF,HLD2DATA                                                      
         LA    RE,SRSPOT                                                        
         LA    R9,4                                                             
         LA    R1,0                                                             
*                                                                               
PRINT6   L     R8,0(R1,RE)                                                      
         A     R8,0(R1,RF)                                                      
         ST    R8,0(R1,RF)                                                      
         LA    R1,4(R1)                                                         
         BCT   R9,PRINT6                                                        
*                                                                               
         LA    R8,0(R1,RE)                                                      
         LA    R9,0(R1,RF)                                                      
         AP    0(8,R9),0(8,R8)                                                  
         LA    R1,SREQUIV-SRDATA                                                
         L     R8,0(R1,RE)                                                      
         A     R8,0(R1,RF)                                                      
         ST    R8,0(R1,RF)                                                      
         B     PRINTIT                                                          
*                                                                               
PRINT8   CLC   QUESTOR(6),=C'ZTRACE'                                            
         BNE   PRINT10                                                          
         GOTO1 HEXOUT,DMCB,SRREC,P,54                                           
         GOTO1 REPORT                                                           
*                                                                               
PRINT10  CLC   SRMSEQ,=X'FFFF'     CROSS MARKET TOTAL                           
         BNE   PRINT12                                                          
         CLC   SRAGY(SRDATA-SRAGY),HLD2KEY+(SRAGY-SRKEY)                        
         BNE   *+10                                                             
         CLC   SRKEY(SRMKT-SRKEY),HLD2KEY                                       
         BE    PRINT4                                                           
*                                                                               
PRINT12  CLI   DPTORDER,C'Y'                                                    
         BE    *+8                                                              
         CLI   RCSUBPRG,1                                                       
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         OC    HOLDREC,HOLDREC                                                  
         BZ    PRINT18                                                          
         CLC   SRSRV,HLDSRV                                                     
         BNE   PRTCUR                                                           
         CLI   DPTORDER,C'Y'                                                    
         BE    PRINT14                                                          
         CLC   SRRPT,HLDRPT                                                     
         BNE   PRTCUR                                                           
         CLC   SRTGT,HLDTGT        NEW REPORT                                   
         BNE   PRTCUR                                                           
         CLC   SRDEMO,HLDDEMO                                                   
         BNE   PRTCUR                                                           
         CLC   SRDPT,HLDDPT                                                     
         BNE   PRTCUR                                                           
*                                                                               
PRINT14  CLC   HIGROUP,SRRST       SET HIGEST RANK                              
         BH    *+10                                                             
         MVC   HIGROUP,SRRST                                                    
         CLI   DPTORDER,C'Y'                                                    
         BE    *+8                                                              
         CLI   DATESW,1            NEED MARKET BREAK IF DATE TYPE RPT           
         BNE   PRINT18                                                          
         CLC   SRMSEQ,=X'FFFF'                                                  
         BNE   PRINT16                                                          
         CLC   SRREN(4),HLDREN                                                  
         BNE   PRTCUR                                                           
         B     PRINT18                                                          
*                                                                               
PRINT16  CLC   SRMKT,HLDMKT                                                     
         BNE   PRTCUR                                                           
*                                                                               
PRINT18  LA    R8,WORK             BUILD MARKET TABLES                          
         XC    WORK,WORK                                                        
         USING MTABD,R8                                                         
         MVC   MTMSEQ,SRMSEQ                                                    
         OC    MTMSEQ,MTMSEQ       ENSURE RANK IS NOT ZERO                      
         BNZ   *+10                                                             
         MVC   MTMSEQ,=H'255'                                                   
         MVC   MTREN,SRREN                                                      
         MVC   MTRST,SRRST                                                      
         MVC   MTMKT,SRMKT                                                      
         CLC   SRMSEQ,=X'FFFF'                                                  
         BNE   *+10                                                             
         MVC   MTMKT,=H'1'                                                      
         MVC   MTTGT,SRTGT                                                      
         MVC   MTDEMO,SRDEMO                                                    
         MVC   MTDPT,SRDPT                                                      
         MVC   MTRPT,SRRPT                                                      
         CLC   QPROG,=C'33'        IND VS. CLIENT ONLY                          
         BNE   *+8                                                              
         CLI   SRAGY,X'FF'         AGENCY RECORD                                
         BNE   PRINT20             INSERT IT                                    
*              BYPASS BANK IF AGENCY NOT THERE                                  
         GOTO1 BINSRCH,DMCB,(X'00',WORK),MTAB,MTCNT,MTABLN,MTKLN,MTMAX          
         CLI   0(R1),0             FOUND                                        
         BNE   PRINT32             NO - EXIT                                    
         LA    R1,WORK                                                          
         ST    R1,DMCB                                                          
*                                                                               
PRINT20  GOTO1 ,DMCB,,MTAB,MTCNT,MTABLN,MTKLN,MTMAX                             
         CLI   DPTORDER,C'Y'                                                    
         BNE   PRINT23                                                          
         MVI   MTDPT,0             INSERT 0 DAYPART FOR REPORT                  
         MVI   MTRPT,0             INSERT 0 DAYPART FOR REPORT                  
         GOTO1 BINSRCH,DMCB,(X'01',WORK)                                        
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),0                                                          
         BE    PRINT22                                                          
         L     R8,0(R1)                                                         
         LA    R4,MTIND                                                         
         USING DPD,R4                                                           
         XC    MTIND,MTIND         CLEAR ACCUMULATORS                           
         MVC   DPDPNTW,=PL8'0'                                                  
         DROP  R4                                                               
         LA    R0,NUMAGY           CLEAR ALL ACCUMULATORS                       
         LA    RE,MTAGY                                                         
         MVC   0(L'MTAGY,RE),MTIND                                              
         LA    RE,36(RE)                                                        
         BCT   R0,*-10                                                          
*                                                                               
PRINT22  MVC   MTCNT,8(R1)                                                      
*                                                                               
PRINT23  LA    R8,WORK                                                          
         MVC   MTDPT,SRDPT                                                      
         MVC   MTRPT,SRRPT                                                      
         GOTO1 BINSRCH,DMCB,(X'01',WORK),MTAB,MTCNT                             
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         CLI   0(R1),0             WAS IT FOUND                                 
         BE    PRINT24                                                          
         L     R8,0(R1)                                                         
         LA    R4,MTIND                                                         
         USING DPD,R4                                                           
         XC    MTIND,MTIND         CLEAR ACCUMULATORS                           
         MVC   DPDPNTW,=PL8'0'                                                  
         DROP  R4                                                               
         LA    R0,NUMAGY           CLEAR ALL ACCUMULATORS                       
         LA    RE,MTAGY                                                         
         MVC   0(L'MTAGY,RE),MTIND                                              
         LA    RE,36(RE)                                                        
         BCT   R0,*-10                                                          
*                                                                               
PRINT24  MVC   MTCNT,8(R1)                                                      
         L     R8,0(R1)                                                         
         CLI   DATESW,1            SET UP SLOT FOR DATES                        
         BNE   PRINT26                                                          
         ZIC   R4,SRDTSEQ                                                       
         BCTR  R4,0                                                             
         MH    R4,=H'36'                                                        
         LA    R4,MTAGY(R4)                                                     
         B     PRINT30                                                          
*                                                                               
PRINT26  LA    R4,MTAGY                                                         
         USING DPD,R4                                                           
         CLC   QPROG,=C'36'        ALWAYS PUT IN AGY SLOTS                      
         BE    PRINT30                                                          
         CLI   SRAGY,X'FF'         INDUSTRY DATA                                
         BNE   *+12                                                             
         LA    R4,MTIND            YES - SET TO IND. TOTALS                     
         B     PRINT30                                                          
*                                                                               
         L     RE,=A(AGYRS)                                                     
         CLI   QMED,C'C'           CANADIAN                                     
         BNE   *+8                                                              
         L     RE,=A(AGYCAN)                                                    
*                                                                               
PRINT28  CLI   0(RE),X'FF'         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SRAGY(2),0(RE)      CALCULATE INDEXED SLOT                       
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     PRINT28                                                          
         ZIC   R4,3(RE)                                                         
         BCTR  R4,0                                                             
         MH    R4,=H'36'                                                        
         CLC   QPROG,=C'34'        REPORT 34                                    
         BE    *+8                 LEAVE INDEXED SLOT                           
         LA    R4,0                FORCE TO SLOT 0                              
         LA    R4,MTAGY(R4)                                                     
*                                                                               
PRINT30  DS    0H                                                               
         L     RE,DPDWGHT                                                       
         A     RE,SRWGHT                                                        
         ST    RE,DPDWGHT                                                       
         AP    DPDPNTW,SRPNTW                                                   
         L     RE,DPDSPT                                                        
         A     RE,SRSPOT                                                        
         ST    RE,DPDSPT                                                        
         L     RE,DPDDOL                                                        
         A     RE,SRDOL                                                         
         ST    RE,DPDDOL                                                        
         L     RE,DPDDOLE                                                       
         A     RE,SREQUIV                                                       
         ST    RE,DPDDOLE                                                       
         L     RE,DPDPNT                                                        
         A     RE,SRPNT                                                         
         ST    RE,DPDPNT                                                        
         L     RE,DPDIMP                                                        
         A     RE,SRIMP                                                         
         ST    RE,DPDIMP                                                        
*                                                                               
PRINT32  MVC   HOLDREC,SRREC                                                    
         B     PRINTIT                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        PRINT CURRENT REPORT                                         *         
***********************************************************************         
*                                                                               
PRTCUR   CLI   DPTORDER,C'Y'                                                    
         BE    *+8                                                              
         BAS   RE,SUMGRP           SUM THE REQUIRED MKT GROUPS                  
         CLI   QOPT3,C'3'          TREND                                        
         BE    PRTRND                                                           
         MVC   CURRRPT,HLDRPT                                                   
         L     R3,MTAB                                                          
         A     R3,=F'600000'                                                    
         ST    R3,MTLAST                                                        
         LA    RE,MTABLN                                                        
         LA    R3,MTABLN           SET TABLE DISPLACEMENT FOR                   
         MH    R3,=H'46'           PRINTING IN ALPHA ORDER                      
         L     R8,MTAB             SET TO PRINT FROM START                      
         MVI   PUTTSW,1                                                         
*                                                                               
PRTC02   CLI   DPTORDER,C'Y'       DETERMINE COLUMNS                            
         BNE   PRTC12                                                           
         CLI   QGRP+1,C'P'         PLANNING TAPE                                
         BE    PRTC04                                                           
         CLI   QGRP+1,C'T'         SEE IF TAPE REQUESTED                        
         BE    PRTC04                                                           
         CLI   QGRP+1,C'D'         SEE IF DISK REQUESTED                        
         BNE   *+8                                                              
*                                                                               
PRTC04   CLI   PUTTSW,1            ONLY DO ON FIRST PASS                        
         BNE   PRTC06                                                           
         CLI   QGRP+1,C'T'                                                      
         BNE   *+8                                                              
         BAS   RE,PUTTAPE          WRITE THE OUTPUT TAPE                        
         CLI   QGRP+1,C'D'                                                      
         BNE   PRTC06                                                           
         BAS   RE,PUTDISK          WRITE THE OUTPUT DISK                        
*                                                                               
PRTC06   MVI   PUTTSW,0                                                         
         LA    R1,0                                                             
         ST    R8,LHSTART                                                       
*                                                                               
PRTC08   LA    R1,1(R1)                                                         
         LA    R8,MTABLN(R8)                                                    
         CLI   MTDPT,0                                                          
         BNE   PRTC08                                                           
         CH    R1,=H'46'                                                        
         BH    *+12                                                             
         ST    R8,LHEND                                                         
         B     PRTC08                                                           
*                                                                               
         L     R8,LHEND                                                         
         ST    R8,RHSTART                                                       
         LA    R1,0                                                             
*                                                                               
PRTC10   LA    R1,1(R1)                                                         
         LA    R8,MTABLN(R8)                                                    
         CLI   MTDPT,0                                                          
         BNE   PRTC10                                                           
         CH    R1,=H'46'                                                        
         BH    *+12                                                             
         ST    R8,RHEND                                                         
         B     PRTC10                                                           
*                                                                               
         LA    R9,46                                                            
         L     R8,LHSTART                                                       
         L     R3,LHEND                                                         
         SR    R3,R8                                                            
         B     PRTC14                                                           
*                                                                               
PRTC12   LA    R9,46               SET NO. OF MARKETS ON PAGE                   
         ST    R8,LHSTART          SET LEFT HAND START                          
         AR    R8,R3                   LEFT HAND END                            
         ST    R8,LHEND                                                         
         ST    R8,RHSTART              RIGHT HAND START                         
         AR    R8,R3                                                            
         ST    R8,RHEND                RIGHT HAND END                           
         L     R8,LHSTART                                                       
*                                                                               
PRTC14   OC    0(2,R8),0(R8)       END OF LIST                                  
         BZ    PRTCLR              EXIT                                         
         LA    R7,P1               SET TO LEFT SIDE OF PAGE                     
         USING PLD,R7                                                           
         MVC   P1(132),SPACES                                                   
         C     R8,LHEND            LIMIT TO BLOCK                               
         BNL   PRTC16                                                           
         BAS   RE,GETMNM           GET MARKET NAME                              
         MVC   HLDDPT,MTDPT        SET DAYPART FOR DAYPART ORDER                
         MVC   CURRRPT,MTRPT                                                    
         BAS   RE,PRTLN            PRINT HALF A LINE                            
*                                                                               
PRTC16   AR    R8,R3               SET UP FOR RIGHT HAND SIDE                   
         C     R8,MTLAST           BYPASS IF OUT OF RANGE                       
         BNL   PRTC18                                                           
         C     R8,RHEND            LIMIT TO BLOCK                               
         BNL   PRTC18                                                           
         OC    0(2,R8),0(R8)       BYPASS SECOND COL IF NO DATA                 
         BZ    PRTC18                                                           
         MVC   HLDDPT,MTDPT        SET DAYPART FOR DAYPART ORDER                
         MVC   CURRRPT,MTRPT                                                    
         AH    R7,=H'64'                                                        
         BAS   RE,PRTLN                                                         
*                                                                               
PRTC18   GOTO1 REPORT                                                           
         SR    R8,R3               RESET TO ORIGINAL SLOT                       
         LA    R8,MTABLN(R8)                                                    
         BCT   R9,PRTC14           PRINT NEXT LINE                              
*                                                                               
         L     R8,RHEND            NEXT PAGE                                    
         OC    0(2,R8),0(R8)                                                    
         BZ    PRTCLR                                                           
         B     PRTC02              PRINT NEXT PAGE                              
         EJECT                                                                  
***********************************************************************         
*        PRINT TREND REPORT                                           *         
***********************************************************************         
*                                                                               
PRTRND   MVC   CURRRPT,HLDRPT                                                   
         CLI   DATESW,1                                                         
         BE    CPMB2R                                                           
         L     R3,MTAB                                                          
         A     R3,=F'600000'                                                    
         ST    R3,MTLAST                                                        
         LA    R3,MTABLN           SET TABLE DISPLACEMENT FOR                   
         MH    R3,=H'46'           PRINTING IN ALPHA ORDER                      
         L     R8,MTAB             SET TO PRINT FROM START                      
*                                                                               
PRTR02   OC    0(2,R8),0(R8)                                                    
         BZ    PRTCLR                                                           
         LA    R7,P1                                                            
         USING PLD,R7                                                           
         BAS   RE,PRFLN                                                         
         CLI   QOPT2,C'Y'          LIMIT REPORT ONLY                            
         BNE   *+12                                                             
         CLI   LIMITSW,0           CHECK IF WITHIN LIMITS                       
         BE    PRTR04                                                           
         GOTO1 REPORT                                                           
         MVI   P1,X'00'                                                         
         GOTO1 REPORT                                                           
*                                                                               
PRTR04   XC    P1(132),P1                                                       
         XC    P2(132),P2                                                       
         LA    R8,MTABLN(R8)                                                    
         B     PRTR02                                                           
         EJECT                                                                  
***********************************************************************         
*        CLEAR FOR NEXT REPORT                                        *         
***********************************************************************         
*                                                                               
PRTCLR   L     RE,MTAB                                                          
         L     RF,=F'600000'                                                    
         XCEF                                                                   
         XC    MTCNT,MTCNT                                                      
         XC    HIGROUP,HIGROUP                                                  
         MVI   FORCEHED,C'Y'                                                    
         CLI   SRSRV,0            EOF                                           
         BNE   PRINT18                                                          
*                                                                               
PRTCLRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        CLEAR FOR NEXT REPORT                                        *         
***********************************************************************         
*                                                                               
* TREND REPORT                                                                  
CPMB2R   MVC   CURRRPT,HLDRPT                                                   
         LA    R0,0                                                             
         L     R8,MTAB                                                          
*                                                                               
CPMB2R2  OC    0(2,R8),0(R8)                                                    
         BZ    PRTCLR                                                           
         LA    R3,MTAGY                                                         
         USING DPD,R3                                                           
         CLI   MTDPT,0                                                          
         BE    CPMB2R5                                                          
         CLC   MTTGT,HLDTGT                                                     
         BNE   *+10                                                             
         CLC   MTDEMO,HLDDEMO                                                   
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   HLDTGT,MTTGT                                                     
         MVC   HLDDEMO,MTDEMO                                                   
         MVC   CURRRPT,MTRPT                                                    
         MVC   HLDDPT,MTDPT                                                     
         BAS   RE,GETMNM                                                        
         BAS   RE,GETDPT                                                        
         BAS   RE,GETRPT                                                        
*                                                                               
CPMB2R3  LR    R1,R0                                                            
         LA    R1,TRNSLT(R1)                                                    
         ZIC   RE,0(R1)                                                         
         L     R1,=A(TRNTAB)                                                    
         BCTR  RE,0                                                             
         MH    RE,=H'36'                                                        
         AR    R1,RE                                                            
         XC    0(36,R1),0(R1)                                                   
         OC    DPDSPT,DPDSPT       MOVE DATA TO COMMON WORK AREAS               
         BZ    CPMB2R4                                                          
         MVC   SPOTS,DPDSPT                                                     
         MVC   POINTS,DPDPNT                                                    
         MVC   POINTSW,DPDPNTW                                                  
         L     RE,POINTS                                                        
         CVD   RE,DUB                                                           
         MVC   POINTSW,DUB                                                      
         MVC   IMPS,DPDIMP                                                      
         MVC   DOLLARS,DPDDOL                                                   
         MVC   DOLLARE,DPDDOLE                                                  
         BAS   RE,CALCPP           CALCULATE EXTRA DATA                         
         BAS   RE,CALCPM                                                        
         BAS   RE,CALIPS                                                        
         BAS   RE,CALCPS                                                        
         BAS   RE,CALPPS                                                        
         LR    R1,R0                                                            
         LA    R1,TRNSLT(R1)                                                    
         ZIC   RE,0(R1)                                                         
         L     R1,=A(TRNTAB)                                                    
         BCTR  RE,0                                                             
         MH    RE,=H'36'                                                        
         AR    R1,RE                                                            
         USING TRND,R1                                                          
         MVC   TRNPNT,POINTS       SET DATA IN PRINT SETUP BUFFER               
         MVC   TRNIMP,IMPS                                                      
         MVC   TRNSPT,SPOTS                                                     
         MVC   TRNDOL,DOLLARS                                                   
         MVC   TRNCPP,CNCPP                                                     
         MVC   TRNCPM,CNCPM                                                     
         MVC   TRNIPS,CNIPS                                                     
         MVC   TRNCPS,CNCPS                                                     
         MVC   TRNPPS,CNPPS                                                     
*                                                                               
CPMB2R4  LA    R3,36(R3)                                                        
         A     R0,=F'1'                                                         
         C     R0,=F'8'                                                         
         BNE   CPMB2R3                                                          
         BAS   RE,TRNIDX                                                        
         BAS   RE,TRNPRT                                                        
*                                                                               
CPMB2R5  LA    R8,MTABLN(R8)                                                    
         SR    R0,R0                                                            
         B     CPMB2R2                                                          
         EJECT                                                                  
***********************************************************************         
*        CALCULATE ALL THE TREND REPORT INDEXES                       *         
***********************************************************************         
*                                                                               
TRNIDX   NTR1                                                                   
         LA    R1,TRNIDXT          POINT TO TREND DATA                          
*                                                                               
TRNX02   OC    0(3,R1),0(R1)                                                    
         BZ    TRNXX                                                            
         ZIC   R7,0(R1)            BASE                                         
         ZIC   R8,1(R1)            PROJECT                                      
         ZIC   R9,2(R1)            OUTPUT                                       
         BCTR  R7,0                                                             
         BCTR  R8,0                                                             
         BCTR  R9,0                                                             
         MH    R7,=H'36'                                                        
         MH    R8,=H'36'                                                        
         MH    R9,=H'36'                                                        
         A     R7,=A(TRNTAB)       POINT TO DATA SLOTS                          
         A     R8,=A(TRNTAB)                                                    
         A     R9,=A(TRNTAB)                                                    
         LA    R0,9                SET LOOP CONTROL                             
*                                                                               
TRNX04   MVC   DUB(4),0(R8)        CALCULATE INDEX                              
         MVC   DUB+4(4),0(R7)                                                   
         BAS   RE,CALIDX                                                        
         MVC   0(4,R9),FULL        THEN SAVE IN OUTPUT                          
         LA    R7,4(R7)            AND GO TO NEXT SLOT                          
         LA    R8,4(R8)                                                         
         LA    R9,4(R9)                                                         
         BCT   R0,TRNX04                                                        
*                                                                               
         LA    R1,3(R1)            TRY NEXT SET OF ACCUMULATORS                 
         B     TRNX02                                                           
*                                                                               
TRNXX    B     EXIT                                                             
         DROP  1                                                                
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
***********************************************************************         
*                                                                               
TRNPRT   NTR1                                                                   
         LA    RF,QDATATYP         SET UP ALLOW LINES                           
         LA    R1,1                                                             
         LA    R9,9                                                             
         CLI   0(RF),C' '                                                       
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   R9,*-12                                                          
         ZIC   R9,LINE             NEED TO PREDICT END OF PAGE                  
         AR    R1,R9                                                            
         ZIC   R9,MAXLINES                                                      
         CR    R1,R9               NEW PAGE IF I WILL OVERFLOW                  
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         LA    R5,P+7              SET PRINT POSITION                           
         LA    R7,4                SET LOOP CONTROL                             
         LA    R9,9                                                             
         L     R4,=A(TRNTAB)                                                    
         USING TRNPNT,R4                                                        
         MVC   P(10),CNRPT                                                      
         MVI   P+10,C'/'                                                        
         MVC   P+11(14),CNDPT                                                   
         GOTO1 REPORT                                                           
         LA    RF,QDATATYP                                                      
         ST    RF,ACURDTYP                                                      
*                                                                               
TRNP02   L     RF,ACURDTYP                                                      
         MVC   DATATYPE,0(RF)                                                   
         BAS   RE,GETDNM                                                        
         MVC   P+2(6),CNDTYP                                                    
         LA    RF,1(RF)                                                         
         ST    RF,ACURDTYP                                                      
         CLI   DATATYPE,C' '                                                    
         BE    EXIT                                                             
         NI    DATATYPE,X'0F'                                                   
         ZIC   RF,DATATYPE                                                      
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         L     R4,=A(TRNTAB)                                                    
         AR    R4,RF                                                            
*                                                                               
TRNP04   LA    RE,WORK                                                          
         USING EDBLOCK,RE                                                       
         L     RF,ADTYPE                                                        
         XC    WORK,WORK                                                        
         ST    R4,EBAIN                                                         
         MVI   EBLIN,4                                                          
         MVI   EBTIN,C'B'                                                       
         MVC   EBSCIN,7(RF)                                                     
         ST    R5,EBAOUT                                                        
         MVI   EBLOUT,7                                                         
         MVC   EBDECS,9(RF)                                                     
         MVC   EBSCOUT,8(RF)                                                    
         GOTO1 =V(EDITOR),DMCB,WORK                                             
         LA    RE,WORK                                                          
         LA    R5,8(R5)            NEXT PRINT POSITION                          
         LA    R4,36(R4)                                                        
         ST    R4,EBAIN                                                         
         ST    R5,EBAOUT                                                        
         GOTO1 =V(EDITOR),DMCB,WORK                                             
         LA    R5,8(R5)                                                         
         LA    R4,36(R4)           NEXT TABLE SLOT                              
         EDIT  (B4,0(R4)),(3,0(R5))                                             
         LA    R5,4(R5)                                                         
         LA    R4,36(R4)           NEXT TABLE SLOT                              
         BCT   R7,TRNP04                                                        
         LA    R7,8                                                             
         LA    R5,4(R5)                                                         
*                                                                               
TRNP06   EDIT  (B4,0(R4)),(3,0(R5))                                             
         LA    R4,36(R4)                                                        
         LA    R5,4(R5)                                                         
         BCT   R7,TRNP06                                                        
*                                                                               
         GOTO1 REPORT                                                           
         LA    R7,4                                                             
         LA    R5,P+7                                                           
         BCT   R9,TRNP02                                                        
*                                                                               
TRNPX    B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT HALF A LINE                                            *         
***********************************************************************         
*                                                                               
PRTLN    NTR1                                                                   
         LA    R3,MTAGY                                                         
         USING DPD,R3                                                           
         LA    R4,MTIND                                                         
         USING DID,R4                                                           
         CLI   FORMATSW,1          WHICH FORMAT                                 
         BE    PRT1LN                                                           
         BAS   RE,GETMNM                                                        
         MVC   PLMNUM(20),CNMNAM                                                
*                                                                               
         CLI   DPTORDER,C'Y'                                                    
         BNE   PRTL04                                                           
         MVC   TEMPDEMO,HLDDEMO                                                 
         MVC   HLDDEMO,MTDEMO                                                   
*        BAS   RE,RDUNIV                                                        
         GOTO1 =V(READU),DMCB,(RA)                                              
         MVC   HLDDEMO,TEMPDEMO                                                 
         CLI   HLDDPT,0            DEMO SLOT FOR DAYPART PRINT                  
         BNE   PRTL02                                                           
         MVC   TEMPDEMO,MTDEMO     SET UP DEMO NAME                             
         MVC   TEMPTGT,MTTGT                                                    
         BAS   RE,GETDEM                                                        
         BAS   RE,GETTGT                                                        
         MVC   PLMNUM(3),=C'***'                                                
         MVC   PLMNUM+3(7),CNDNAM                                               
         MVI   PLMNUM+10,C' '                                                   
         MVC   PLMNUM+11(13),CNTGT                                              
         B     PRTLX                                                            
*                                                                               
PRTL02   BAS   RE,GETDPT                                                        
         BAS   RE,GETRPT                                                        
         MVI   PLMNUM,C' '                                                      
         MVC   PLMNUM+1(4),CNRPT                                                
         MVI   PLMNUM+5,C'/'                                                    
         MVC   PLMNUM+6(14),CNDPT                                               
*                                                                               
PRTL04   CLI   QMED,C'C'                                                        
         BE    PRTL06                                                           
         CLI   MTMSEQ,X'FF'                                                     
         BE    PRTL06                                                           
         EDIT  DIDUNIV,(5,PLUNIV)                                               
*                                                                               
PRTL06   EDIT  DIDSPT,(6,PLCSPOTS) PRINT THE SPOTS                              
         EDIT  DPDSPT,(5,PLASPOTS)                                              
         CLC   DIDSPT,DPDSPT       BANK MUST HAVE AT LEAST AGENCY SPOTS         
         BNL   *+6                                                              
         DC    H'0'               ERROR- CHECK DELETE LOGIC IN CONCRETE         
         L     RF,DIDPNT           CALCULATE AVE RTG                            
         L     R1,DIDSPT                                                        
         CLI   MTMSEQ,X'FF'                                                     
         BE    PRTL08                                                           
         BAS   R9,CALAVG                                                        
         EDIT  (RF),(4,PLAVRTG),1                                               
*                                                                               
PRTL08   OC    DIDSPT,DIDSPT       ANY INDUSTRY SPOTS                           
         BZ    PRTLX                                                            
*                                                                               
*                                  CALCULATE INDUSTRY CPP                       
         MVC   WEIGHT,DIDWGHT                                                   
         MVC   POINTS,DIDPNT                                                    
         MVC   POINTSW,DIDPNTW                                                  
         MVC   DOLLARE,DIDDOLE                                                  
         BAS   RE,CALCPP                                                        
         MVC   SVINDCPP,CNCPP                                                   
         MVC   SVINDCP1,SVINDCPP                                                
*                                                                               
*                                  CALCULATE INDUSTRY/AGENCY CPP                
         CLI   MTMSEQ,X'FF'                                                     
         BNE   PRTL10                                                           
         LA    R3,MTAGY+DPDLEN                                                  
         MVC   WEIGHT,DPDWGHT                                                   
         MVC   POINTS,DPDPNT                                                    
         MVC   POINTSW,DPDPNTW                                                  
         MVC   DOLLARE,DPDDOLE                                                  
         BAS   RE,CALCPP                                                        
         MVC   SVINDCP1,CNCPP                                                   
         LA    R3,MTAGY                                                         
*                                                                               
*                                  CALCULATE AGENCY CPP                         
PRTL10   MVC   WEIGHT,DPDWGHT                                                   
         MVC   POINTS,DPDPNT                                                    
         MVC   POINTSW,DPDPNTW                                                  
         MVC   DOLLARE,DPDDOLE                                                  
         BAS   RE,CALCPP                                                        
         MVC   SVAGYCPP,CNCPP                                                   
*                                                                               
*                                  CALCULATE AGENCY TO INDUSTRY INDEX           
         MVC   DUB(4),SVAGYCPP                                                  
         MVC   DUB+4(4),SVINDCP1                                                
         BAS   RE,CALIDX                                                        
         MVC   SVINDEX,FULL                                                     
*                                                                               
*                                  PRINT THE CPPS AND INDEX                     
         L     RF,SVINDCPP                                                      
         BAS   R9,EDTCPP                                                        
         MVC   PLCCPP,EDCPP                                                     
         L     RF,SVAGYCPP                                                      
         BAS   R9,EDTCPP                                                        
         MVC   PLACPP,EDCPP                                                     
         L     RF,SVINDEX                                                       
         EDIT  (RF),(3,PLCPPIDX)                                                
         CH    RF,=H'999'                                                       
         BL    *+10                                                             
         MVC   PLCPPIDX,=C' HI'                                                 
*                                                                               
PRTLX    B     EXIT                                                             
         EJECT                                                                  
         USING BA1LINE,R7                                                       
PRT1LN   BAS   RE,GETMNM           GET MARKET NAME                              
         MVC   BA1MNAM(20),CNMNAM                                               
*                                                                               
         CLI   DPTORDER,C'Y'       SET UP FOR DAYPART                           
         BNE   PRT1L04                                                          
*                                                                               
         CLI   HLDDPT,0            DEMO SLOT FOR DAYPART PRINT                  
         BNE   PRT1L02                                                          
         MVC   TEMPDEMO,MTDEMO     SET UP DEMO NAME                             
         MVC   TEMPTGT,MTTGT                                                    
         BAS   RE,GETDEM                                                        
         BAS   RE,GETTGT                                                        
         MVC   BA1MNAM(3),=C'***'                                               
         MVC   BA1MNAM+3(7),CNDNAM                                              
         MVI   BA1MNAM+10,C' '                                                  
         MVC   BA1MNAM+11(13),CNTGT                                             
         B     PRT1LX                                                           
*                                                                               
PRT1L02  BAS   RE,GETDPT                                                        
         BAS   RE,GETRPT                                                        
         MVI   BA1MNAM,C' '                                                     
         MVC   BA1MNAM+1(10),CNRPT                                              
         MVI   BA1MNAM+11,C'/'                                                  
         MVC   BA1MNAM+12(13),CNDPT                                             
*                                                                               
PRT1L04  MVC   WEIGHT,DPDWGHT      PRINT THE CPP                                
         MVC   POINTS,DPDPNT                                                    
         MVC   POINTSW,DPDPNTW                                                  
         MVC   DOLLARE,DPDDOLE                                                  
         BAS   RE,CALCPP                                                        
         L     RF,CNCPP                                                         
         BAS   R9,EDTCPP                                                        
         MVC   BA1CPP,EDCPP                                                     
         L     RF,DPDIMP           PRINT THE IMPRESSIONS                        
         EDIT  (RF),(7,BA1IMP),,COMMAS=YES                                      
         MVC   IMPS,DPDIMP         CALC CPM                                     
         BAS   RE,CALCPM                                                        
         L     RF,CNCPM                                                         
         BAS   R9,EDTCPP                                                        
         MVC   BA1CPM,EDCPP                                                     
         EDIT  DPDSPT,(6,BA1SPOTS)                                              
         EDIT  DPDDOL,(8,BA1DOL)                                                
*                                                                               
PRT1LX   B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
* PRINT  A LINE                                                                 
         USING PLD,R7                                                           
PRFLN    NTR1                                                                   
         LA    R3,MTAGY                                                         
         USING DPD,R3                                                           
         LA    R4,MTIND                                                         
         USING DID,R4                                                           
         MVI   LIMITSW,0                                                        
         BAS   RE,GETMNM                                                        
         MVC   PLMNUM(25),CNMNAM                                                
         EDIT  DIDUNIV,(5,PLUNIV)                                               
         EDIT  DIDSPT,(6,PLCSPOTS) PRINT THE SPOTS                              
         OC    DIDSPT,DIDSPT       ANY INDUSTRY SPOTS                           
         BZ    PRFLX                                                            
*                                                                               
*                                  CALCULATE INDUSTRY CPP                       
         MVC   WEIGHT,DIDWGHT                                                   
         MVC   POINTS,DIDPNT                                                    
         MVC   POINTSW,DIDPNTW                                                  
         MVC   DOLLARE,DIDDOLE                                                  
         BAS   RE,CALCPP                                                        
         MVC   SVINDCPP,CNCPP                                                   
         CP    DIDPNTW,=PL8'0'     ANY INDUSTRY POINTS                          
         BE    PRFLX               NO - EXIT                                    
         BAS   R9,EDTCPP                                                        
         MVC   PLCCPP,EDCPP                                                     
         LA    R1,NUMAGY                                                        
*                                                                               
PRFL02   STC   R1,PLNUM            SAVE THE LOOP COUNT                          
         TM    PLNUM,X'01'         IS IT ODD                                    
         BO    *+8                 DO NOTHING                                   
         LA    R7,132(R7)          SET TO SECOND PRINT LINE                     
         MVC   PLCTAIDX,=C'  .'                                                 
*                                                                               
*                                  CALCULATE AGENCY CPP                         
         MVC   WEIGHT,DPDWGHT                                                   
         MVC   POINTS,DPDPNT                                                    
         MVC   POINTSW,DPDPNTW                                                  
         MVC   DOLLARE,DPDDOLE                                                  
         BAS   RE,CALCPP                                                        
         MVC   SVAGYCPP,CNCPP                                                   
*                                                                               
*                                  CALCULATE AGENCY TO INDUSTRY INDEX           
         MVC   DUB(4),SVAGYCPP                                                  
         MVC   DUB+4(4),SVINDCPP                                                
         BAS   RE,CALIDX                                                        
         MVC   SVINDEX,FULL                                                     
*                                                                               
         L     RF,SVINDEX                                                       
*                                                                               
         C     RF,=F'200'          CHECK IF WITHIN PRINTABLE LIMITS             
         BL    *+8                                                              
         MVI   LIMITSW,1                                                        
         C     RF,=F'50'                                                        
         BH    *+8                                                              
         MVI   LIMITSW,1                                                        
*                                                                               
         EDIT  (RF),(3,PLCTAIDX)                                                
         CH    RF,=H'999'                                                       
         BL    *+10                                                             
         MVC   PLCTAIDX,=C' HI'                                                 
         LA    R3,36(R3)                                                        
         TM    PLNUM,X'01'         IS IT ODD                                    
         BO    *+12                DO NOTHING                                   
         SH    R7,=H'132'          SET TO FIRST PRINT LINE                      
         LA    R7,4(R7)            BUMP TO NEXT SLOT                            
         BCT   R1,PRFL02                                                        
*                                                                               
PRFLX    B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        REFORMAT INTO STANDARD KEY                                   *         
***********************************************************************         
*                                                                               
RESTDK   NTR1                                                                   
         L     RE,ARPTKEY          GET KEY FOR THIS REPORT                      
         L     R4,ASRTREC                                                       
         LA    R4,19(R4)                                                        
         CLI   0(R4),0                                                          
         BE    *+8                                                              
         L     RE,ARPTKEYT                                                      
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         L     RE,ARPTKEYT                                                      
         OC    0(2,RE),0(RE)       EXIT IF DEFAULT                              
         BZ    RESTDX                                                           
         XC    BLDKEY,BLDKEY                                                    
         L     R4,ASRTREC          SET FOR REFORMAT                             
*                                                                               
RESTD2   ZIC   R1,1(RE)            GET LENGTH                                   
         BCTR  R1,0                ADJUST FOR EXEC                              
         ZIC   R8,0(RE)            SET OUTPUT POINTER                           
         LA    R8,BLDKEY(R8)                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(R4)                                                    
         LA    R4,1(R1,R4)         POINT TO NEXT INPUT                          
         LA    RE,2(RE)                                                         
         OC    0(2,RE),0(RE)       CHECK FOR MORE                               
         BNZ   RESTD2                                                           
         L     R4,ASRTREC          REPLACE KEY IN RECORD                        
         MVC   0(L'SRKEY,R4),BLDKEY                                             
*                                                                               
RESTDX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        PUT RECORDS TO TAPE                                          *         
***********************************************************************         
*                                                                               
PUTTAPE  NTR1                                                                   
         GOTO1 =V(PUTTAPEC),DMCB,(RA)                                           
         XIT1                                                                   
***********************************************************************         
*        PUT RECORDS TO DISK                                          *         
***********************************************************************         
*                                                                               
* PUT RECORDS TO DISK                                                           
PUTDISK  NTR1                                                                   
         GOTO1 =V(PUTDISKC),DMCB,(RA)                                           
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        GET MARKET OR GROUP NAME                                     *         
***********************************************************************         
*                                                                               
GETMNM   NTR1                                                                   
         L     RE,ADMKTTAB                                                      
         MVC   CNMNAM,SPACES                                                    
         MVC   CNMNAM+4(7),=C'UNKNOWN'                                          
         LA    R9,CNMNAM+12                                                     
         EDIT  (B2,MTMKT),(3,(R9))                                              
         CLI   MTMSEQ,X'FF'                                                     
         BNE   GETMN2                                                           
         MVC   CNGCAP,=C'GROUP  '                                               
         CLC   MTRST,=X'FFFE'                                                   
         BNE   *+10                                                             
         MVC   MTRST,=H'1'                                                      
         EDIT  MTRST,(3,CNGST)                                                  
         MVI   CNGDSH,C'-'                                                      
         EDIT  MTREN,(3,CNGEN),,ALIGN=LEFT                                      
         B     GETMNX                                                           
*                                                                               
GETMN2   CLC   MTMKT,0(RE)         FIND MARKET NAME                             
         BE    GETMN4                                                           
         CLI   0(RE),X'FF'                                                      
         BE    GETMN6                                                           
         LA    RE,34(RE)                                                        
         B     GETMN2                                                           
*                                                                               
GETMN4   MVC   CNMNAM+4(24),6(RE)                                               
         MVC   DIDUNIV,=F'1'                                                    
         CLI   QMED,C'C'                                                        
         BE    GETMN6                                                           
*        BAS   RE,RDUNIV                                                        
         GOTO1 =V(READU),DMCB,(RA)                                              
*                                                                               
GETMN6   CLC   MTRST,=H'240'                                                    
         BE    GETMNX                                                           
         EDIT  MTRST,(3,CNMNAM)                                                 
*                                                                               
GETMNX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        GET DAYPART EXPANSIONS                                       *         
***********************************************************************         
*                                                                               
GETDPT   ST    RF,SAVERF                                                        
         LA    RF,DPTNAM2                                                       
         CLI   DPTORDER,C'Y'                                                    
         BNE   *+8                                                              
         LA    RF,DPTNAM3                                                       
         XC    CNDPT,CNDPT                                                      
*                                                                               
GETDP2   CLC   HLDDPT,0(RF)        SCAN FOR CURRENT DAYPART                     
         BE    GETDP6                                                           
         CLI   0(RF),X'FF'                                                      
         BNE   GETDP4                                                           
         MVC   CNDPT(7),=C'UNKNOWN'    NOT FOUND                                
         B     GETDPX                                                           
*                                                                               
GETDP4   LA    RF,15(RF)           TRY NEXT ENTRY                               
         CLI   DPTORDER,C'Y'                                                    
         BE    *+8                                                              
         LA    RF,10(RF)                                                        
         B     GETDP2                                                           
*                                                                               
GETDP6   MVC   CNDPT,1(RF)         SET NAME IN OUTPUT                           
*                                                                               
GETDPX   L     RF,SAVERF                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        GET REPORT DESCRIPTIONS                                      *         
***********************************************************************         
*                                                                               
GETRPT   ZIC   RF,CURRRPT                                                       
         LA    RF,RPTCAPT(RF)                                                   
         ZIC   R1,0(RF)                                                         
         BCTR  R1,0                                                             
         MH    R1,=H'46'                                                        
         LA    R1,RPTCAPS(R1)                                                   
         MVC   CNRPT,36(R1)                                                     
         MVC   CNRPTL,0(R1)                                                     
         BR    RE                                                               
*                                                                               
*                                                                               
***********************************************************************         
*        GET CLIENT INFORMATION                                       *         
***********************************************************************         
*                                                                               
GETCLT   L     R1,ADCLTBUF                                                      
         LA    R1,4(R1)                                                         
         L     RF,NCLTS                                                         
*                                                                               
GETCL2   CLC   QCLT(3),0(R1)                                                    
         BE    GETCL4                                                           
         AH    R1,WIDTHCLT                                                      
         BCT   RF,GETCL2                                                        
         MVC   CLTNM(7),=C'UNKNOWN'                                             
         BR    RE                                                               
*                                                                               
GETCL4   MVC   CLIENT,0(R1)                                                     
         MVC   CLTNM,3(R1)                                                      
         MVC   CLTOFF,23(R1)                                                    
         BR    RE                                                               
*                                                                               
*                                                                               
***********************************************************************         
*        GET THE DATA TYPE NAMES                                      *         
***********************************************************************         
*                                                                               
GETDNM   ST    RF,SAVERF                                                        
         LA    RF,DTYPETAB                                                      
         XC    CNDTYP,CNDTYP                                                    
*                                                                               
GETDN2   CLC   DATATYPE,0(RF)      SCAN FOR CURRENT DAYPART                     
         BE    GETDN6                                                           
         CLI   0(RF),X'FF'                                                      
         BNE   GETDN4                                                           
         MVC   CNDTYP(7),=C'UNKNOWN'   NOT FOUND                                
         B     GETDNX                                                           
*                                                                               
GETDN4   LA    RF,LNDTYPTB(RF)     TRY NEXT ENTRY                               
         B     GETDN2                                                           
*                                                                               
GETDN6   MVC   CNDTYP,1(RF)       SET NAME IN OUTPUT                            
*                                                                               
GETDNX   ST    RF,ADTYPE                                                        
         L     RF,SAVERF                                                        
         BR    RE                                                               
*                                                                               
*                                                                               
***********************************************************************         
*        GET EXPANDED DEMO                                            *         
***********************************************************************         
*                                                                               
GETDEM   NTR1                                                                   
         L     RF,ADDEMBUF         POINT TO TABLE                               
         SR    R9,R9                                                            
         IC    R9,TEMPDEMO         GET REQUESTED DEMO                           
         BCTR  R9,0                                                             
         MH    R9,=H'7'            INDEX TO SLOT                                
         AR    R9,RF                                                            
         MVC   CNDNAM,0(R9)        SET IN GLOBAL AREA                           
         B     EXIT                                                             
*                                                                               
*                                                                               
***********************************************************************         
*        GET EXPANDED TARGET                                          *         
***********************************************************************         
*                                                                               
GETTGT   NTR1                                                                   
         L     R8,=A(TGTTAB)       POINT TO TABLE                               
         LA    R9,0                                                             
         CLC   TEMPDEMO,TEMPTGT                                                 
         BNE   *+8                                                              
         LA    R9,1                'TARGETS'                                    
         CLI   TEMPTGT,0                                                        
         BNE   *+8                                                              
         LA    R9,2                'ALL TARGETS'                                
         CLI   TEMPTGT,240                                                      
         BNE   *+8                                                              
         LA    R9,3                'GROUP TARGETS                               
*                                                                               
         LTR   R9,R9               IF SET GET TITLE                             
         BNZ   GETTG2                                                           
         MVC   TEMPDEMO,TEMPTGT    ELSE SET TITLE FROM TARGET                   
         MVC   WORK(7),CNDNAM                                                   
         BAS   RE,GETDEM                                                        
         MVC   0(7,R8),CNDNAM                                                   
         MVC   CNDNAM,WORK                                                      
*                                                                               
GETTG2   MH    R9,=H'13'                                                        
         LA    R8,0(R9,R8)                                                      
         MVC   CNTGT(13),0(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        CALCULATE EXTRA DATA                                         *         
***********************************************************************         
*                                                                               
*        CALCULATE CPP                                                *         
*                                                                               
CALCPP   NTR1                                                                   
         XC    CNCPP,CNCPP                                                      
         L     RE,WEIGHT                                                        
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         LA    RE,1                                                             
         CP    POINTSW,=PL8'0'     EXIT IF NO POINTS                            
         BE    CALCPPX                                                          
         CVD   RE,DUB              CONVERT TO PACKED                            
         ZAP   WORK(16),POINTSW                                                 
         DP    WORK(16),DUB        UNWEIGHT THE POINTS                          
         ZAP   DUB,WORK(8)                                                      
         MVC   POINTSW,DUB                                                      
         CLI   QOPT1,C'Y'          USE WEIGHTS                                  
         BNE   CALCPP2                                                          
         L     RE,POINTS           OVERLAY WEIGHTED WITH UNWGTED                
         CVD   RE,DUB                                                           
         MVC   POINTSW,DUB                                                      
*                                                                               
CALCPP2  CP    POINTSW,=PL8'0'     CALCULATE CP                                 
         BE    CALCPPX             (EQUIV. DOLLARS/POINTS)                      
         L     RE,DOLLARE                                                       
         CVD   RE,DUB                                                           
         ZAP   WORK(16),DUB                                                     
         MP    WORK(16),=P'1000'                                                
         DP    WORK(16),POINTSW                                                 
         ZAP   DUB,WORK(8)                                                      
         CVB   RF,DUB                                                           
         ST    RF,CNCPP            RETURN CPP                                   
*                                                                               
CALCPPX  B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*        INDEX 2 NUMBERS                                              *         
*                DIVIDEND IN DUB                                      *         
*                DIVISOR IN DUB+4                                     *         
*                QUOTIENT IN FULL                                     *         
*                                                                               
CALIDX   NTR1                                                                   
         L     RF,DUB                                                           
         L     R5,DUB+4                                                         
         LTR   R5,R5               CHECK FOR ZERO                               
         BNZ   *+6                                                              
         SR    RF,RF                                                            
         LTR   RF,RF               IN BOTH OPERANDS                             
         BZ    CALIDXX                                                          
         SR    RE,RE                                                            
         MH    RF,=H'100'                                                       
         SLDA  RE,1                                                             
         DR    RE,R5                                                            
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
*                                                                               
CALIDXX  ST    RF,FULL                                                          
         B     EXIT                                                             
*                                                                               
*        CALCULATE CPM                                                *         
*                                                                               
CALCPM   NTR1                                                                   
         XC    CNCPM,CNCPM                                                      
         L     RE,IMPS             CONVERT IMPS TO PACKED                       
         LTR   RE,RE                                                            
         BZ    EXIT                                                             
         CVD   RE,DMCB                                                          
         L     RE,DOLLARE          CONVERT DOLLARS TO PACKED                    
         CVD   RE,DUB                                                           
         ZAP   WORK(16),DUB                                                     
         MP    WORK(16),=P'100'                                                 
         DP    WORK(16),DMCB(8)                                                 
         ZAP   DUB,WORK(8)                                                      
         CVB   RF,DUB                                                           
         ST    RF,CNCPM            SAVE CPM                                     
         B     EXIT                                                             
*                                                                               
*        CALCULATE COST PER SHOT (CPS)                                *         
*                                                                               
*                                                                               
CALCPS   NTR1                                                                   
         XC    CNCPS,CNCPS                                                      
         L     R1,SPOTS                                                         
         L     RF,DOLLARS                                                       
         BAS   R9,CALAVG                                                        
         ST    RF,CNCPS                                                         
         B     EXIT                                                             
*                                                                               
*        CALCULATE POINTS PER SHOT (PPS)                              *         
*                                                                               
*                                                                               
CALPPS   NTR1                                                                   
         XC    CNPPS,CNPPS                                                      
         L     R1,SPOTS                                                         
         L     RF,POINTS                                                        
         BAS   R9,CALAVG                                                        
         ST    RF,CNPPS                                                         
         B     EXIT                                                             
*                                                                               
*        CALCULATE IMPS PER SHOT (IPS)                              *           
*                                                                               
*                                                                               
CALIPS   NTR1                                                                   
         XC    CNIPS,CNIPS                                                      
         L     R1,SPOTS                                                         
         L     RF,IMPS                                                          
         BAS   R9,CALAVG                                                        
         ST    RF,CNIPS                                                         
         B     EXIT                                                             
*                                                                               
*        CALCULATE THE AVERAGE (RF/R1)                              *           
*                                                                               
*                                                                               
CALAVG   LTR   R1,R1                                                            
         BZ    EXIT                                                             
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         DR    RE,R1                                                            
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         BR    R9                                                               
         EJECT                                                                  
***********************************************************************         
*        EDIT CPP TO 5 CHARACTERS                                     *         
***********************************************************************         
*                                                                               
EDTCPP   MVC   EDCPP,SPACES                                                     
         LTR   RF,RF                                                            
         BZR   R9                                                               
         CH    RF,=H'9999'         PRINT FULL PRECISION                         
         BH    EDTCPP2                                                          
         EDIT  (RF),(5,EDCPP),2                                                 
         B     EDTCPPX                                                          
*                                                                               
EDTCPP2  SR    RE,RE               PRINT DIME PRECISION                         
         SLDA  RE,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         CH    RF,=H'9999'                                                      
         BH    EDTCPP4                                                          
         EDIT  (RF),(5,EDCPP),1                                                 
         B     EDTCPPX                                                          
*                                                                               
EDTCPP4  SR    RE,RE               PRINT DOLLAR PRECISION                       
         SLDA  RE,1                                                             
         D     RE,=F'10'                                                        
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         CH    RF,=H'9999'                                                      
         BH    EDTCPP6                                                          
         EDIT  (RF),(5,EDCPP)                                                   
         B     EDTCPPX                                                          
*                                                                               
EDTCPP6  MVC   EDCPP,=C' HIGH'                                                  
*                                                                               
EDTCPPX  BR    R9                                                               
         EJECT                                                                  
***********************************************************************         
*        SUM THE MARKETS INTO CORRECT GROUPS                          *         
***********************************************************************         
*                                                                               
SUMGRP   NTR1                                                                   
         L     R4,MTCNT            CREATE MARKET GROUPS                         
         L     R8,MTAB                                                          
         LTR   R4,R4               NO ACTIVE MARKETS                            
         BZ    SUMGRX                                                           
*                                                                               
SUMGR2   LA    RF,MTRECORD         MOVE SLOT TO SAVE AREA                       
         LR    RE,R8                                                            
         LA    R1,MTKLN+36+36*NUMAGY                                            
         MOVE  ((RF),(R1)),(RE)                                                 
         ST    R8,SAVER8           SAVE CURRENT RECORD POINTER                  
         LA    R8,MTRECORD                                                      
         LA    R7,MTAGY            FORCE CPP TOTAL ACCROSS MARKETS              
         USING DPD,R7                                                           
         L     R1,DPDPNT                                                        
         LTR   R1,R1                                                            
         BZ    SUMGR4                                                           
         BAS   R9,ONESPOT                                                       
         CLI   QPROG+1,C'2'        32 OR 42                                     
         BNE   SUMGR6                                                           
*                                                                               
SUMGR4   LA    R7,MTIND            LOOK AT INDUSTRY TOTALS                      
         L     R1,DPDPNT                                                        
         LTR   R1,R1               DON'T INCLUDE IF SPOTS                       
         BZ    SUMGR20                                                          
         OC    DPDDOL,DPDDOL       OR DOLLARS ARE ZERO                          
         BZ    SUMGR20                                                          
         BAS   R9,ONESPOT                                                       
*                                                                               
         CLC   QPROG,=C'33'        CLIENT VS. BANK REPORT                       
         BE    *+10                                                             
         CLC   QPROG,=C'42'        CROSS CLIENT REPORT                          
         BE    *+10                                                             
         CLC   QPROG,=C'32'        CROSS AGENCY CPP REPORT                      
         BNE   SUMGR6                                                           
         LA    R7,MTAGY                                                         
         OC    DPDSPT,DPDSPT       SET FOR INDEX IF AGENCY ACTIVE               
         BZ    SUMGR6                                                           
         MVC   MTAGY+DPDLEN(DIDLEN),MTIND                                       
*                                                                               
SUMGR6   MVC   MTMSEQ,=X'FFFF'     FORCE TO SORT AT END                         
         LA    RE,SUMCTRL          FIND GROUP FOR THIS MARKET                   
*                                                                               
SUMGR8   CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID MARKET                               
         CLC   MTRST,2(RE)         WITHIN THIS GROUP                            
         BNH   *+12                                                             
         LA    RE,4(RE)            TRY NEXT GROUP                               
         B     SUMGR8                                                           
         ST    RE,FULL             SAVE THE POINTER                             
         MVC   MTREN,2(RE)                                                      
         MVC   MTRST,0(RE)                                                      
         XC    MTMKT,MTMKT                                                      
*                                                                               
SUMGR10  CLC   MTREN,HIGROUP                                                    
         BL    *+10                                                             
         MVC   MTREN,HIGROUP       FORCE HIGHEST TO HIGHEST ACTIVE RANK         
         GOTO1 BINSRCH,DMCB,(X'01',MTRECORD),MTAB,MTCNT,MTABLN,MTKLN,  X        
               MTMAX                                                            
         MVC   MTCNT,8(R1)         SAVE COUNT                                   
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         CLI   0(R1),0             ALREADY IN TABLE                             
         BNE   SUMGR18             DO NOT ADD IF INSERTED THIS TIME             
         L     R8,0(R1)            SET UP TO ADD 2 RECORDS TOGETHER             
         LA    R5,MTIND                                                         
         LA    R8,MTRECORD                                                      
         LA    RE,MTIND                                                         
         LA    R1,8                                                             
         L     R0,NOACUMS                                                       
         LA    R7,FLDCTRL                                                       
*                                                                               
SUMGR12  CLI   0(R7),C'P'          CHECK FOR PACKED ACCUMS                      
         BE    SUMGR16                                                          
         ICM   R9,15,0(RE)         SUM THE ACCUMULATORS                         
         ICM   RF,15,0(R5)                                                      
         AR    R9,RF                                                            
         STCM  R9,15,0(R5)                                                      
         LA    RE,4(RE)                                                         
         LA    R5,4(R5)                                                         
         LA    R7,1(R7)                                                         
         BCT   R1,SUMGR12                                                       
*                                                                               
SUMGR14  LA    R1,8                SET OR NEXT SLOT                             
         LA    R7,FLDCTRL                                                       
         BCT   R0,SUMGR12                                                       
         B     SUMGR18                                                          
*                                                                               
SUMGR16  AP    0(8,R5),0(8,RE)     ADD UP PACKED ACCUMS                         
         LA    RE,8(RE)                                                         
         LA    R5,8(R5)                                                         
         LA    R7,1(R7)                                                         
         BCT   R1,SUMGR12                                                       
         B     SUMGR14                                                          
*                                                                               
SUMGR18  L     RE,FULL             ADD UP BIG GROUPS                            
         CLC   0(2,RE),=H'1'       DONT DUP FIRST TOTAL                         
         BE    *+14                                                             
         CLC   MTRST,=X'FFFE'      CHECK FOR FIRST TIME                         
         BNE   *+8                 YES - BYPASS BUMP                            
         LA    RE,4(RE)            SET TO NEXT GROUP                            
         CLC   HIGROUP,0(RE)       END                                          
         BNH   SUMGR20                                                          
         MVC   MTREN,2(RE)         SET CURRENT END RANK                         
         ST    RE,FULL                                                          
         MVC   MTRST,=X'FFFE'      SEND BIG GROUP                               
         B     SUMGR10                                                          
*                                                                               
SUMGR20  L     R8,SAVER8           RESTORE TABLE POINTER                        
         LA    R8,MTABLN(R8)                                                    
         BCT   R4,SUMGR2                                                        
*                                                                               
SUMGRX   B     EXIT                                                             
         EJECT                                                                  
ONESPOT  CLI   ONESPTSW,1          ONE SPOT REQUIRED                            
         BNER  R9                                                               
         LA    R5,FLDCTRL+3        SET FOR DATA TYPE                            
         LA    R0,5                DIVIDE ACCUMS BY POINTS                      
         LA    R7,DPDDOL                                                        
*                                                                               
ONESP2   CLI   0(R5),C'P'                                                       
         BE    ONESP6                                                           
         L     RE,0(R7)                                                         
         SRDA  RE,32                                                            
         M     RE,=F'1000'                                                      
         SLDA  RE,1                                                             
         DR    RE,R1                                                            
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R7)                                                         
         LA    R7,4(R7)                                                         
*                                                                               
ONESP4   LA    R5,1(R5)                                                         
         BCT   R0,ONESP2                                                        
         BR    R9                                                               
*                                                                               
ONESP6   CVD   R1,DUB              DIVIDE A PACKED FIELD                        
         ZAP   WORK(16),0(8,R7)                                                 
         MP    WORK(16),=P'10000'                                               
         DP    WORK(16),DUB                                                     
         ZAP   DUB,WORK(8)                                                      
         AP    DUB,=P'5'                                                        
         ZAP   WORK(16),DUB                                                     
         DP    WORK(16),=PL8'10'                                                
         ZAP   0(8,R7),WORK(8)                                                  
         LA    R7,8(R7)                                                         
         B     ONESP4                                                           
         DROP  R7                                                               
         EJECT                                                                  
*DUNIV   NTR1                                                                   
*        GOTO1 =V(READU),DMCB,(RA)                                              
*        B     EXIT                                                             
*                                                                               
         USING *,RF                                                             
SUBNTR   NTR1  BASE=CP32RB                                                      
         DROP  RF                                                               
         LM    RA,RC,CP32RA                                                     
         SLL   R1,2                *4                                           
         L     RF,BRANCH-4(R1)                                                  
         BASR  RE,RF                                                            
         XIT1                                                                   
BRANCH   DC    A(GETRPT)                                                        
         DC    A(GETDPT)                                                        
         DC    A(GETTGT)                                                        
         DC    A(GETDEM)                                                        
         DC    A(GETCLT)                                                        
*                                                                               
EQGETRPT EQU   1                                                                
EQGETDPT EQU   2                                                                
EQGETTGT EQU   3                                                                
EQGETDEM EQU   4                                                                
EQGETCLT EQU   5                                                                
         LTORG                                                                  
         EJECT                                                                  
* SUBROUTINE EQUATES                                                            
SELECT   EQU   1                                                                
TDYES    EQU   2                                                                
DEMYES   EQU   3                                                                
DEMNO    EQU   4                                                                
DGRPYES  EQU   5                                                                
DGRPNO   EQU   6                                                                
OVERRIDE EQU   7                                                                
TGTYES   EQU   8                                                                
TGTNO    EQU   9                                                                
TGRPYES  EQU   10                                                               
TGRPNO   EQU   11                                                               
*                                                                               
* POSITION OF FIELD WITHIN RECORD                                               
POSDPT   EQU   SRDPT-SRREC                                                      
POSAFL   EQU   SRAFL-SRREC                                                      
POSPRG   EQU   SRPRG-SRREC                                                      
POSTGT   EQU   SRTGT-SRREC                                                      
POSSRV   EQU   SRSRV-SRREC                                                      
POSSSEQ  EQU   SRSSEQ-SRREC                                                     
POSDEMO  EQU   SRDEMO-SRREC                                                     
POSSLN   EQU   SRSLN-SRREC                                                      
POSMSEQ  EQU   SRMSEQ-SRREC                                                     
POSREN   EQU   SRREN-SRREC                                                      
POSRST   EQU   SRRST-SRREC                                                      
POSMKT   EQU   SRMKT-SRREC                                                      
POSAGY   EQU   SRAGY-SRREC                                                      
POSRPT   EQU   SRRPT-SRREC                                                      
POSSDT   EQU   SRDTSEQ-SRREC                                                    
*                                                                               
* LENGTH OF FIELDS                                                              
LENTGT   EQU   L'SRTGT                                                          
LENDPT   EQU   L'SRDPT                                                          
LENAFL   EQU   L'SRAFL                                                          
LENPRG   EQU   L'SRPRG                                                          
LENSRV   EQU   L'SRTGT                                                          
LENSSEQ  EQU   L'SRSSEQ                                                         
LENDEMO  EQU   L'SRDEMO                                                         
LENSLN   EQU   L'SRSLN                                                          
LENMSEQ  EQU   L'SRMSEQ                                                         
LENREN   EQU   L'SRREN                                                          
LENRST   EQU   L'SRRST                                                          
LENMKT   EQU   L'SRMKT                                                          
LENAGY   EQU   L'SRAGY                                                          
LENRPT   EQU   L'SRRPT                                                          
         EJECT                                                                  
***********************************************************************         
*============================ TAPE OUTPUT ============================*         
PUTTAPEC CSECT                                                                  
         NMOD1 0,PUTTAPEC                                                       
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING CPWORKD,RA,RC                                                    
         USING CP3202+4096,R6                                                   
         USING CP32WK,R2                                                        
*                                                                               
         L     R3,=A(OUT)                                                       
         CLI   TAPEFRST,1          PUT OUT TABLES ON FIRST TIME                 
         BNE   PT30                                                             
         MVI   TAPEFRST,0                                                       
         MVC   DSNCP32+13(2),QAGY  GET AGENCY FOR DYNALLOC                      
         GOTO1 ADYNALLOC,DMCB,DDCP32,DSNCP32                                    
*                                                                               
         L     R7,=A(IOT)          POINT TO IO AREA                             
         USING CPTM,R7                                                          
*                                                                               
         OPEN  (OUT,(OUTPUT))      OPEN THE TAPE FILE                           
         XC    RECCNT,RECCNT       CLEAS THE COUNTER                            
         L     R9,ADMKTTAB                                                      
*                                                                               
PT01     CLI   0(R9),X'FF'         END OF MARKET TABLE?                         
         BE    PT10                YES, PUT OUT DAYPART TABLE                   
         MVI   CPTMID,C' '         CLEAR THE RECORD                             
         MVC   CPTMID+1(CPTMLEN-1),CPTMID                                       
         MVI   CPTMID,CPTMIDEQ     SET RECORD ID                                
         SR    R1,R1               CONVERT MARKET NUMBER                        
         ICM   R1,3,0(R9)                                                       
         CVD   R1,DUB                                                           
         UNPK  CPTMNO,DUB                                                       
         OI    CPTMNO+3,X'F0'                                                   
         MVC   CPTMNAME,6(R9)      SET MARKET NAME                              
*                                                                               
         PUT   (R3),(R7)                                                        
*                                                                               
         L     R1,RECCNT           UPDATE TAPE COUNT                            
         LA    R1,1(R1)                                                         
         ST    R1,RECCNT                                                        
*                                                                               
         LA    R9,34(R9)           GET NEXT MARKET                              
         B     PT01                                                             
         DROP  R7                                                               
*                                                                               
PT10     LA    R9,DPTNAM2          PUT OUT DAYPART TABLE                        
         LA    R8,DPTNAM3                                                       
*                                                                               
PT12     CLI   0(R9),X'FF'         END OF DAYPART TABLE                         
         BE    PT20                                                             
         USING CPTD,R7                                                          
         MVI   CPTDID,C' '         CLEAR THE RECORD                             
         MVC   CPTDID+1(CPTDLEN-1),CPTDID                                       
         MVI   CPTDID,CPTDIDEQ     SET RECORD ID                                
         MVC   CPTDCOD,0(R9)       SET THE CODE                                 
         MVC   CPTDN24,1(R9)       SET BIG NAME                                 
         MVC   CPTDN14,1(R8)       SET SMALL NAME                               
*                                                                               
         PUT   (R3),(R7)                                                        
*                                                                               
         L     R1,RECCNT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,RECCNT                                                        
*                                                                               
         LA    R9,25(R9)           CONTINUE TO END                              
         LA    R8,15(R8)                                                        
         B     PT12                                                             
         DROP  R7                                                               
*                                                                               
         USING CPTP,R7                                                          
PT20     L     R9,=A(RPTCAPS)      SEND PROGRAM TYPE TABLE                      
         LA    R0,6                LOOP FOR US                                  
         CLI   QMED,C'C'                                                        
         BNE   *+8                                                              
         LA    R0,9                LOOP FOR CANADA                              
         LA    R1,1                START THE PROGRAM COUNTER                    
*                                                                               
PT22     MVI   CPTPID,C' '         CLEAR THE RECORD                             
         MVC   CPTPID+1(CPTPLEN-1),CPTPID                                       
         MVI   CPTPID,CPTPIDEQ     SET RECORD ID                                
         STC   R1,CPTPCOD          SET THE PROGRAM TYPE CODE                    
         MVC   CPTPN36,0(R9)       SET BIG NAME                                 
         MVC   CPTPN10,36(R9)      SET SMALL NAME                               
         STM   R0,R1,DUB                                                        
*                                                                               
         PUT   (R3),(R7)                                                        
*                                                                               
         L     R1,RECCNT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,RECCNT                                                        
*                                                                               
         LM    R0,R1,DUB                                                        
         LA    R9,46(R9)           CONTINUE TO END                              
         LA    R1,1(R1)                                                         
         BCT   R0,PT22                                                          
         DROP  R7                                                               
*                                                                               
         USING CPTC,R7                                                          
PT30     CLI   REQSENT,C'Y'                                                     
         BE    PT31                                                             
         MVI   CPTCID,C' '         CLEAR THE RECORD                             
         MVC   CPTCID+1(CPTCLEN-1),CPTCID                                       
         MVI   CPTCID,C'R'                                                      
         MVC   CPTCID+1(80),QAREA                                               
         PUT   (R3),(R7)                                                        
         L     R1,RECCNT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,RECCNT                                                        
         MVI   REQSENT,C'Y'                                                     
*                                                                               
PT31     L     R8,MTAB             SEND THE ENTIRE CPP TABLE                    
*                                                                               
PT32     OC    0(2,R8),0(R8)                                                    
         BZ    PT40                                                             
         MVI   CPTCID,C' '         CLEAR THE RECORD                             
         MVC   CPTCID+1(CPTCLEN-1),CPTCID                                       
         MVI   CPTCID,CPTCIDEQ     SET RECORD ID                                
         MVC   CPTCRS,HLDSRV       SET RATING SERVICE                           
         CLI   MTDPT,0             SUPPRESS DUMMY DAYPART                       
         BE    PT34                                                             
         SR    RE,RE               CONVERT MARKET NUMBER                        
         ICM   RE,3,MTMKT                                                       
         CVD   RE,DUB                                                           
         UNPK  CPTCMNO,DUB                                                      
         OI    CPTCMNO+3,X'F0'                                                  
         MVC   HLDDEMO,MTDEMO      GET THE UNIVERSE                             
*        BAS   RE,RDUNIV                                                        
         MVC   TEMPDEMO,MTDEMO     SET UP FOR DEMO NAMES                        
         MVC   TEMPTGT,MTTGT                                                    
*        BAS   RE,GETDEM           GET THE DEMO NAME                            
         LA    R1,EQGETDEM                                                      
         L     RF,=A(SUBNTR)                                                    
         BASR  RE,RF                                                            
         MVC   CPTCREP,CNDNAM                                                   
*        BAS   RE,GETTGT           GET THE TARGET NAME                          
         LA    R1,EQGETTGT                                                      
         L     RF,=A(SUBNTR)                                                    
         BASR  RE,RF                                                            
         CLI   MTTGT,0                                                          
         BNE   *+10                                                             
         MVC   CNTGT,=CL7'ALL'                                                  
         CLI   MTTGT,240                                                        
         BNE   *+10                                                             
         MVC   CNTGT,=CL7'GROUP'                                                
         MVC   CPTCTRG,CNTGT                                                    
         MVC   CPTCDPT,MTDPT       SEND DAYPART CODE                            
         ZIC   RE,MTRPT            SEND THE PROGRAM TYPE                        
         LA    RE,RPTCAPT(RE)                                                   
         MVC   CPTCPTY,0(RE)                                                    
         LA    R5,MTAGY            SET TO AGENCY DATA                           
         USING DPD,R5                                                           
         LA    R4,MTIND            SET TO INDUSTRY DATA                         
         USING DID,R4                                                           
* SEND THE AGENCY DATA                                                          
         L     RE,DIDUNIV                                                       
         CVD   RE,DUB                                                           
         MVC   CPTCUNIV,DUB                                                     
         L     RE,DPDSPT           SPOTS                                        
         CVD   RE,DUB                                                           
         MVC   CPTCAS,DUB                                                       
         L     RE,DPDDOL           DOLLARS                                      
         CVD   RE,DUB                                                           
         MVC   CPTCAD,DUB                                                       
         L     RE,DPDDOLE          EQUIVALENCED DOLLARS                         
         CVD   RE,DUB                                                           
         MVC   CPTCADE,DUB                                                      
         L     RE,DPDPNT           POINTS                                       
         CVD   RE,DUB                                                           
         MVC   CPTCAP,DUB                                                       
         L     RE,DPDIMP           IMPRESSIONS                                  
         CVD   RE,DUB                                                           
         MVC   CPTCAI,DUB                                                       
* SEND THE INDUSTRY DATA                                                        
         L     RE,DIDSPT           SPOTS                                        
         CVD   RE,DUB                                                           
         MVC   CPTCIS,DUB                                                       
         L     RE,DIDDOL           DOLLARS                                      
         CVD   RE,DUB                                                           
         MVC   CPTCIDL,DUB                                                      
         L     RE,DIDDOLE          EQUIVALENCED DOLLARS                         
         CVD   RE,DUB                                                           
         MVC   CPTCIDE,DUB                                                      
         L     RE,DIDPNT           POINTS                                       
         CVD   RE,DUB                                                           
         MVC   CPTCIP,DUB                                                       
         L     RE,DIDIMP           IMPRESSIONS                                  
         CVD   RE,DUB                                                           
         MVC   CPTCII,DUB                                                       
         PUT   (R3),(R7)                                                        
         L     R1,RECCNT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,RECCNT                                                        
*                                                                               
PT34     LA    R8,MTABLN(R8)                                                    
         B     PT32                CONTINUE TO END                              
         DROP  R7                                                               
*                                                                               
PT40     XIT1                                                                   
*                                                                               
DDCP32   DC    CL8'OUT'                                                         
DSNCP32  DC    CL20'CPPTAPE.CP032XX1'                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*============================ DISK OUTPUT ============================*         
PUTDISKC CSECT                                                                  
         NMOD1 0,PUTDISKC                                                       
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING CPWORKD,RA,RC                                                    
         USING CP3202+4096,R6                                                   
         USING CP32WK,R2                                                        
*                                                                               
         L     R3,=A(OUT2)                                                      
         CLI   QOPT4,C'Y'          CSV FORMAT?                                  
         BNE   *+8                 NO                                           
         L     R3,=A(MRKCSV)       YES,START WITH MARKET DATASET                
*                                                                               
         CLI   TAPEFRST,1          PUT OUT TABLES ON FIRST TIME                 
         BNE   PD20                                                             
         MVI   TAPEFRST,0                                                       
         LA    R9,DDCP33                                                        
         CLI   QOPT4,C'Y'                                                       
         BNE   *+8                                                              
         LA    R9,DDCP33CM                                                      
         MVC   DSNCP33+13(2),QAGY                                               
         LA    R7,4                NUMBER OF CSV DATASETS                       
*                                                                               
PD02     GOTO1 ADYNALLOC,DMCB,(R9),DSNCP33                                      
         CLI   QOPT4,C'Y'          CSV FORMAT?                                  
         BNE   PD04                NO                                           
         LA    R9,8(0,R9)          YES, GET NEXT DCB NAME                       
         BCT   R7,PD02                                                          
*                                                                               
         USING CPDM,R7                                                          
PD04     L     R7,=A(IOT2)         POINT TO IO AREA                             
*                                                                               
         OPEN  ((R3),(OUTPUT))                                                  
         XC    RECCNT,RECCNT                                                    
         L     R9,ADMKTTAB                                                      
*                                                                               
PD06     CLI   0(R9),X'FF'         END OF MARKET TABLE                          
         BE    PD08                PUT OUT DAYPART TABLE                        
         MVI   CPDMID,C' '         CLEAR THE RECORD                             
         MVC   CPDMID+1(CPDMLEN-1),CPDMID                                       
         MVI   CPDMID,CPDMIDEQ     SET RECORD ID                                
         SR    R1,R1               CONVERT MARKET NUMBER                        
         ICM   R1,3,0(R9)                                                       
         CVD   R1,DUB                                                           
         UNPK  CPDMNO,DUB                                                       
         OI    CPDMNO+(L'CPDMNO-1),X'F0'                                        
         MVC   CPDMNAME,6(R9)      SET MARKET NAME                              
*                                                                               
         CLI   QOPT4,C'Y'          CSV FORMAT?                                  
         BNE   *+8                                                              
         BAS   RE,CSVMRKT          YES, REFORMAT THE DATA                       
*                                                                               
         PUT   (R3),(R7)                                                        
*                                                                               
         L     R1,RECCNT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,RECCNT                                                        
*                                                                               
         LA    R9,34(R9)                                                        
         B     PD06                CONTINUE TILL END OF TABLE                   
         DROP  R7                                                               
*                                                                               
*                                                                               
PD08     CLI   QOPT4,C'Y'          CSV FORMAT?                                  
         BNE   PD10                NO                                           
         L     R3,=A(DAYCSV)       YES, CHANGE DATASETS                         
         OPEN  ((R3),(OUTPUT))     OPEN DAYCSV                                  
*                                                                               
PD10     LA    R9,DPTNAM2          PUT OUT DAYPART TABLE                        
         LA    R8,DPTNAM3                                                       
*                                                                               
PD12     CLI   0(R9),X'FF'         END OF DAYPART TABLE                         
         BE    PD14                                                             
         USING CPDD,R7                                                          
         MVI   CPDDID,C' '         CLEAR THE RECORD                             
         MVC   CPDDID+1(CPDDLEN-1),CPDDID                                       
         MVI   CPDDID,CPDDIDEQ     SET RECORD ID                                
         MVC   CPDDCOD,0(R9)       SET THE CODE                                 
         MVC   CPDDN24,1(R9)       SET BIG NAME                                 
         MVC   CPDDN14,1(R8)       SET SMALL NAME                               
*                                                                               
         CLI   QOPT4,C'Y'          CSV FORMAT?                                  
         BNE   *+8                                                              
         BAS   RE,CSVDAY           YES, REFORMAT THE DATA                       
*                                                                               
         PUT   (R3),(R7)                                                        
*                                                                               
         L     R1,RECCNT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,RECCNT                                                        
*                                                                               
         LA    R9,25(R9)           CONTINUE TO END                              
         LA    R8,15(R8)                                                        
         B     PD12                                                             
         DROP  R7                                                               
*                                                                               
PD14     CLI   QOPT4,C'Y'          CSV FORMAT?                                  
         BNE   PD16                NO                                           
         L     R3,=A(PRGCSV)       YES, CHANGE DATASETS                         
         OPEN  ((R3),(OUTPUT))     OPEN PRGCSV                                  
*                                                                               
         USING CPDP,R7                                                          
PD16     L     R9,=A(RPTCAPS)      SEND PROGRAM TYPE TABLE                      
         LA    R0,6                LOOP FOR US                                  
         CLI   QMED,C'C'                                                        
         BNE   *+8                                                              
         LA    R0,9                LOOP FOR CANADA                              
         LA    R1,1                START THE PROGRAM COUNTER                    
*                                                                               
PD18     MVI   CPDPID,C' '         CLEAR THE RECORD                             
         MVC   CPDPID+1(CPDPLEN-1),CPDPID                                       
         MVI   CPDPID,CPDPIDEQ     SET RECORD ID                                
         CVD   R1,DUB              SET THE PROGRAM TYPE CODE                    
         UNPK  CPDPCOD,DUB                                                      
         OI    CPDPCOD+(L'CPDPCOD-1),X'F0'                                      
         MVC   CPDPN36,0(R9)       SET BIG NAME                                 
         MVC   CPDPN10,36(R9)      SET SMALL NAME                               
         STM   R0,R1,DUB                                                        
*                                                                               
         CLI   QOPT4,C'Y'          CSV FORMAT?                                  
         BNE   *+8                                                              
         BAS   RE,CSVPRG           YES, REFORMAT THE DATA                       
*                                                                               
         PUT   (R3),(R7)                                                        
*                                                                               
         L     R1,RECCNT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,RECCNT                                                        
*                                                                               
         LM    R0,R1,DUB                                                        
         LA    R9,46(R9)           CONTINUE TO END                              
         LA    R1,1(R1)                                                         
         BCT   R0,PD18                                                          
         DROP  R7                                                               
*                                                                               
PD20     CLI   QOPT4,C'Y'          CSV FORMAT?                                  
         BNE   PD22                NO                                           
         L     R3,=A(ACTCSV)       YES, CHANGE DATASETS                         
         OPEN  ((R3),(OUTPUT))     OPEN ACTCSV                                  
*                                                                               
         USING CPDC,R7                                                          
PD22     CLI   REQSENT,C'Y'                                                     
         BE    PD24                                                             
         MVI   CPDCID,C' '         CLEAR THE RECORD                             
         MVC   CPDCID+1(CPDCLEN-1),CPDCID                                       
         MVI   CPDCID,C'R'                                                      
         MVC   CPDCID+1(80),QAREA                                               
*                                                                               
         CLI   QOPT4,C'Y'          CSV FORMAT?                                  
         BNE   *+8                                                              
         BAS   RE,CSVACT           YES, REFORMAT THE DATA                       
*                                                                               
         PUT   (R3),(R7)                                                        
*                                                                               
         L     R1,RECCNT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,RECCNT                                                        
*                                                                               
         MVI   REQSENT,C'Y'                                                     
*                                                                               
PD24     L     R8,MTAB             SEND THE ENTIRE CPP TABLE                    
*                                                                               
PD26     OC    0(2,R8),0(R8)                                                    
         BZ    PDX                                                              
         MVI   CPDCID,C' '         CLEAR THE RECORD                             
         MVC   CPDCID+1(CPDCLEN-1),CPDCID                                       
         MVI   CPDCID,CPDCIDEQ     SET RECORD ID                                
         MVC   CPDCRS,HLDSRV       SET RATING SERVICE                           
         CLI   MTDPT,0             SUPPRESS DUMMY DAYPART                       
         BE    PD28                                                             
         SR    RE,RE               CONVERT MARKET NUMBER                        
         ICM   RE,3,MTMKT                                                       
         CVD   RE,DUB                                                           
         UNPK  CPDCMNO,DUB                                                      
         OI    CPDCMNO+(L'CPDCMNO-1),X'F0'                                      
         MVC   HLDDEMO,MTDEMO      GET THE UNIVERSE                             
*        BAS   RE,RDUNIV                                                        
         GOTO1 =V(READU),DMCB,(RA)                                              
         MVC   TEMPDEMO,MTDEMO     SET UP FOR DEMO NAMES                        
         MVC   TEMPTGT,MTTGT                                                    
*                                                                               
         LA    R1,EQGETDEM                                                      
         L     RF,=A(SUBNTR)                                                    
         BASR  RE,RF                                                            
*                                                                               
         MVC   CPDCREP,CNDNAM                                                   
*                                                                               
         LA    R1,EQGETTGT                                                      
         L     RF,=A(SUBNTR)                                                    
         BASR  RE,RF                                                            
*                                                                               
         CLI   MTTGT,0                                                          
         BNE   *+10                                                             
         MVC   CNTGT,=CL7'ALL'                                                  
         CLI   MTTGT,240                                                        
         BNE   *+10                                                             
         MVC   CNTGT,=CL7'GROUP'                                                
         MVC   CPDCTRG,CNTGT                                                    
         MVC   CPDCDPT,MTDPT       SEND DAYPART CODE                            
         ZIC   RE,MTRPT            SEND THE PROGRAM TYPE                        
         LA    RE,RPTCAPT(RE)                                                   
         ZIC   R1,0(RE)                                                         
         CVD   R1,DUB                                                           
         UNPK  CPDCPTY,DUB                                                      
         OI    CPDCPTY+(L'CPDCPTY-1),X'F0'                                      
         LA    R5,MTAGY            SET TO AGENCY DATA                           
         USING DPD,R5                                                           
         LA    R4,MTIND            SET TO INDUSTRY DATA                         
         USING DID,R4                                                           
* SEND THE AGENCY DATA                                                          
         L     RE,DIDUNIV                                                       
         CVD   RE,DUB                                                           
         UNPK  CPDCUNIV,DUB                                                     
         OI    CPDCUNIV+(L'CPDCUNIV-1),X'F0'                                    
         L     RE,DPDSPT           SPOTS                                        
         CVD   RE,DUB                                                           
         UNPK  CPDCAS,DUB                                                       
         OI    CPDCAS+(L'CPDCAS-1),X'F0'                                        
         L     RE,DPDDOL           DOLLARS                                      
         CVD   RE,DUB                                                           
         UNPK  CPDCAD,DUB                                                       
         OI    CPDCAD+(L'CPDCAD-1),X'F0'                                        
         L     RE,DPDDOLE          EQUIVALENCED DOLLARS                         
         CVD   RE,DUB                                                           
         UNPK  CPDCADE,DUB                                                      
         OI    CPDCADE+(L'CPDCADE-1),X'F0'                                      
         L     RE,DPDPNT           POINTS                                       
         CVD   RE,DUB                                                           
         UNPK  CPDCAP,DUB                                                       
         OI    CPDCAP+(L'CPDCAP-1),X'F0'                                        
         L     RE,DPDIMP           IMPRESSIONS                                  
         CVD   RE,DUB                                                           
         UNPK  CPDCAI,DUB                                                       
         OI    CPDCAI+(L'CPDCAI-1),X'F0'                                        
* SEND THE INDUSTRY DATA                                                        
         L     RE,DIDSPT           SPOTS                                        
         CVD   RE,DUB                                                           
         UNPK  CPDCIS,DUB                                                       
         OI    CPDCIS+(L'CPDCIS-1),X'F0'                                        
         L     RE,DIDDOL           DOLLARS                                      
         CVD   RE,DUB                                                           
         UNPK  CPDCIDL,DUB                                                      
         OI    CPDCIDL+(L'CPDCIDL-1),X'F0'                                      
         L     RE,DIDDOLE          EQUIVALENCED DOLLARS                         
         CVD   RE,DUB                                                           
         UNPK  CPDCIDE,DUB                                                      
         OI    CPDCIDE+(L'CPDCIDE-1),X'F0'                                      
         L     RE,DIDPNT           POINTS                                       
         CVD   RE,DUB                                                           
         UNPK  CPDCIP,DUB                                                       
         OI    CPDCIP+(L'CPDCIP-1),X'F0'                                        
         L     RE,DIDIMP           IMPRESSIONS                                  
         CVD   RE,DUB                                                           
         UNPK  CPDCII,DUB                                                       
         OI    CPDCII+(L'CPDCII-1),X'F0'                                        
*                                                                               
         CLI   QOPT4,C'Y'          CSV FORMAT?                                  
         BNE   *+8                                                              
         BAS   RE,CSVACT           YES, REFORMAT THE DATA                       
*                                                                               
         PUT   (R3),(R7)                                                        
*                                                                               
         L     R1,RECCNT                                                        
         LA    R1,1(R1)                                                         
         ST    R1,RECCNT                                                        
*                                                                               
PD28     LA    R8,MTABLN(R8)                                                    
         B     PD26                CONTINUE TO END                              
         DROP  R7                                                               
*                                                                               
PDX      XIT1                                                                   
*                                                                               
DDCP33   DC    CL8'OUT2'                                                        
DDCP33CM DC    CL8'MRKCSV'                                                      
DDCP33CD DC    CL8'DAYCSV'                                                      
DDCP33CP DC    CL8'PRGCSV'                                                      
DDCP33CA DC    CL8'ACTCSV'                                                      
DSNCP33  DC    CL20'CPPTAPE.CP033XX1'                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         USING CPDM,R7                                                          
CSVMRKT  NTR1                                                                   
         L     R9,=A(IOTCSV)                                                    
         XC    0(CPDCLEN,R9),0(R9)                                              
         LA    R9,4(R9)            ALLOW FOR RECORD LENGTH                      
*                                                                               
         MVI   0(R9),QUOTE         START WITH A QUOTE                           
         LA    R9,1(R9)                                                         
         MVC   0(1,R9),CPDMID      MOVE RECORD ID                               
         LA    R8,L'CPDMID                                                      
         MVI   WORK,X'40'          CHARACTER DATA                               
         BAS   RE,CSVWRAP          ADD QUOTE, COMMA AND BUMP                    
*                                                                               
         MVC   0(L'CPDMNO,R9),CPDMNO                                            
         LA    R8,L'CPDMNO                                                      
         MVI   WORK,X'F0'          NUMERICA DATA                                
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDMNAME,R9),CPDMNAME                                        
         LA    R8,L'CPDMNAME                                                    
         MVI   WORK,X'40'          CHARACTER DATA                               
         BAS   RE,CSVWRAP                                                       
*                                                                               
         BCTR  R9,0                                                             
         MVI   0(R9),C' '          CLEAR LAST QUOTE                             
         BCTR  R9,0                                                             
         MVI   0(R9),C' '          CLEAR LAST COMMA                             
*                                                                               
         L     R8,=A(IOTCSV)                                                    
         SR    R9,R8               FIND LENGTH                                  
         STCM  R9,3,0(R8)          STORE IT AT BEGINNING OF RECORD              
*                                                                               
         XC    0(L'IOT,R7),0(R7)   CLEAR OUT OLD DATA                           
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R8)                                                    
         XIT1                                                                   
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         USING CPDD,R7                                                          
CSVDAY   NTR1                                                                   
         L     R9,=A(IOTCSV)                                                    
         XC    0(CPDCLEN,R9),0(R9)                                              
         LA    R9,4(R9)            ALLOW FOR RECORD LENGTH                      
*                                                                               
         MVI   0(R9),QUOTE         START WITH A QUOTE                           
         LA    R9,1(R9)                                                         
         MVC   0(1,R9),CPDDID      MOVE RECORD ID                               
         LA    R8,L'CPDDID                                                      
         MVI   WORK,X'40'          CHARACTER DATA                               
         BAS   RE,CSVWRAP          ADD QUOTE, COMMA AND BUMP                    
*                                                                               
         MVC   0(L'CPDDCOD,R9),CPDDCOD                                          
         LA    R8,L'CPDDCOD                                                     
         MVI   WORK,X'40'          CHARACTER DATA                               
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDDN24,R9),CPDDN24                                          
         LA    R8,L'CPDDN24                                                     
         MVI   WORK,X'40'          CHARACTER DATA                               
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDDN14,R9),CPDDN14                                          
         LA    R8,L'CPDDN14                                                     
         MVI   WORK,X'40'          CHARACTER DATA                               
         BAS   RE,CSVWRAP                                                       
*                                                                               
         BCTR  R9,0                                                             
         MVI   0(R9),C' '          CLEAR LAST QUOTE                             
         BCTR  R9,0                                                             
         MVI   0(R9),C' '          CLEAR LAST COMMA                             
*                                                                               
         L     R8,=A(IOTCSV)                                                    
         SR    R9,R8               FIND LENGTH                                  
         STCM  R9,3,0(R8)          STORE IT AT BEGINNING OF RECORD              
*                                                                               
         XC    0(L'IOT,R7),0(R7)   CLEAR OUT OLD DATA                           
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R8)                                                    
         XIT1                                                                   
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         USING CPDP,R7                                                          
CSVPRG   NTR1                                                                   
         L     R9,=A(IOTCSV)                                                    
         XC    0(CPDCLEN,R9),0(R9)                                              
         LA    R9,4(R9)            ALLOW FOR RECORD LENGTH                      
*                                                                               
         MVI   0(R9),QUOTE         START WITH A QUOTE                           
         LA    R9,1(R9)                                                         
         MVC   0(1,R9),CPDPID      MOVE RECORD ID                               
         LA    R8,L'CPDPID                                                      
         MVI   WORK,X'40'          CHARACTER DATA                               
         BAS   RE,CSVWRAP          ADD QUOTE, COMMA AND BUMP                    
*                                                                               
         MVC   0(L'CPDPCOD,R9),CPDPCOD                                          
         LA    R8,L'CPDPCOD                                                     
         MVI   WORK,X'F0'          NUMERICA DATA                                
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDPN36,R9),CPDPN36                                          
         LA    R8,L'CPDPN36                                                     
         MVI   WORK,X'40'          CHARACTER DATA                               
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDPN10,R9),CPDPN10                                          
         LA    R8,L'CPDPN10                                                     
         MVI   WORK,X'40'          CHARACTER DATA                               
         BAS   RE,CSVWRAP                                                       
*                                                                               
         BCTR  R9,0                                                             
         MVI   0(R9),C' '          CLEAR LAST QUOTE                             
         BCTR  R9,0                                                             
         MVI   0(R9),C' '          CLEAR LAST COMMA                             
*                                                                               
         L     R8,=A(IOTCSV)                                                    
         SR    R9,R8               FIND LENGTH                                  
         STCM  R9,3,0(R8)          STORE IT AT BEGINNING OF RECORD              
*                                                                               
         XC    0(L'IOT,R7),0(R7)   CLEAR OUT OLD DATA                           
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R8)                                                    
         XIT1                                                                   
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         USING CPDC,R7                                                          
CSVACT   NTR1                                                                   
         L     R9,=A(IOTCSV)                                                    
         XC    0(CPDCLEN,R9),0(R9)                                              
         LA    R9,4(R9)            ALLOW FOR RECORD LENGTH                      
*                                                                               
         MVI   0(R9),QUOTE         START WITH A QUOTE                           
         LA    R9,1(R9)                                                         
         MVC   0(1,R9),CPDCID      MOVE RECORD ID                               
         LA    R8,L'CPDCID                                                      
         MVI   WORK,X'40'          CHARACTER DATA                               
         BAS   RE,CSVWRAP          ADD QUOTE, COMMA AND BUMP                    
*                                                                               
         MVC   0(L'CPDCRS,R9),CPDCRS                                            
         LA    R8,L'CPDCRS                                                      
         MVI   WORK,X'40'          CHARACTER DATA                               
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDCMNO,R9),CPDCMNO                                          
         LA    R8,L'CPDCMNO                                                     
         MVI   WORK,X'F0'          NUMERICA DATA                                
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDCREP,R9),CPDCREP                                          
         MVI   WORK,X'40'          CHARACTER DATA                               
         LA    R8,L'CPDCREP                                                     
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDCTRG,R9),CPDCTRG                                          
         LA    R8,L'CPDCTRG                                                     
         MVI   WORK,X'40'          CHARACTER DATA                               
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDCDPT,R9),CPDCDPT                                          
         LA    R8,L'CPDCDPT                                                     
         MVI   WORK,X'40'          CHARACTER DATA                               
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDCPTY,R9),CPDCPTY                                          
         LA    R8,L'CPDCPTY                                                     
         MVI   WORK,X'40'          CHARACTER DATA                               
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDCUNIV,R9),CPDCUNIV                                        
         LA    R8,L'CPDCUNIV                                                    
         MVI   WORK,X'F0'          NUMERICA DATA                                
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDCAS,R9),CPDCAS                                            
         LA    R8,L'CPDCAS                                                      
         MVI   WORK,X'F0'          NUMERICA DATA                                
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDCAD,R9),CPDCAD                                            
         LA    R8,L'CPDCAD                                                      
         MVI   WORK,X'F0'          NUMERICA DATA                                
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDCADE,R9),CPDCADE                                          
         LA    R8,L'CPDCADE                                                     
         MVI   WORK,X'F0'          NUMERICA DATA                                
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDCAP,R9),CPDCAP                                            
         LA    R8,L'CPDCAP                                                      
         MVI   WORK,X'F0'          NUMERICA DATA                                
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDCAI,R9),CPDCAI                                            
         LA    R8,L'CPDCAI                                                      
         MVI   WORK,X'F0'          NUMERICA DATA                                
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDCIS,R9),CPDCIS                                            
         LA    R8,L'CPDCIS                                                      
         MVI   WORK,X'F0'          NUMERICA DATA                                
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDCIDL,R9),CPDCIDL                                          
         LA    R8,L'CPDCIDL                                                     
         MVI   WORK,X'F0'          NUMERICA DATA                                
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDCIDE,R9),CPDCIDE                                          
         LA    R8,L'CPDCIDE                                                     
         MVI   WORK,X'F0'          NUMERICA DATA                                
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDCIP,R9),CPDCIP                                            
         LA    R8,L'CPDCIP                                                      
         MVI   WORK,X'F0'          NUMERICA DATA                                
         BAS   RE,CSVWRAP                                                       
*                                                                               
         MVC   0(L'CPDCII,R9),CPDCII                                            
         LA    R8,L'CPDCII                                                      
         MVI   WORK,X'F0'          NUMERICA DATA                                
         BAS   RE,CSVWRAP                                                       
*                                                                               
         BCTR  R9,0                                                             
         MVI   0(R9),C' '          CLEAR LAST QUOTE                             
         BCTR  R9,0                                                             
         MVI   0(R9),C' '          CLEAR LAST COMMA                             
*                                                                               
         L     R8,=A(IOTCSV)                                                    
         SR    R9,R8               FIND LENGTH                                  
         STCM  R9,3,0(R8)          STORE IT AT BEGINNING OF RECORD              
*                                                                               
         XC    0(L'IOT,R7),0(R7)   CLEAR OUT OLD DATA                           
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R8)                                                    
         XIT1                                                                   
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
CSVWRAP  DS    0H                                                               
         LR    R1,R8               SAVE IT FOR LOOP                             
         SHI   R8,1                SUBTRACT 1 FROM LENGTH                       
*                                                                               
CSVW02   CLC   0(1,R9),WORK        CLEAR LEADING ZEROS OR BLANKS                
         BH    CSVW04                                                           
         SHI   R8,1                LESS 1 FOR MOVE                              
         BM    CSVW06              NO DATA, JUST PUT A COMMA                    
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R9),1(R9)       SHIFT DATA OVER                              
         AHI   R8,1                PUT BACK 1 FOR LOAD                          
         LA    RF,0(R9,R8)                                                      
         MVI   0(RF),0             CLEAR LAST BYTE                              
         SHI   R8,1                LESS THE 1 AGAIN                             
         BCT   R1,CSVW02                                                        
         B     CSVW08                                                           
*                                                                               
CSVW04   LA    R9,0(R9,R8)        FIND END                                      
         CLI   0(R9),C' '                                                       
         BH    CSVW08                                                           
         BCTR  R9,0                                                             
         BCT   R8,*-10                                                          
*                                                                               
CSVW06   BCTR  R9,0                NO DATA, BACK UP BEFORE QUOTE                
         B     CSVW10              NOW ADD COMMA                                
*                                                                               
CSVW08   LA    R9,1(R9)            WRAP IT UP                                   
         MVI   0(R9),QUOTE                                                      
         LA    R9,1(R9)                                                         
*                                                                               
CSVW10   MVI   0(R9),COMMA                                                      
         LA    R9,1(R9)                                                         
         MVI   0(R9),QUOTE                                                      
         LA    R9,1(R9)                                                         
         BR    RE                                                               
*                                                                               
QUOTE    EQU   C'"'                                                             
COMMA    EQU   C','                                                             
         EJECT                                                                  
SETUP    CSECT                                                                  
         NMOD1 0,SETUP                                                          
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING CPWORKD,RA,RC                                                    
         USING CP3202+4096,R6                                                   
         USING CP32WK,R2                                                        
         MVI   DPTORDER,C'N'                                                    
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   *+24                                                             
         MVC   SVRDSEQ,OPTRDSEQ                                                 
         MVC   SVQOPT3,QOPT3                                                    
         MVI   REQSENT,C'N'                                                     
*                                                                               
         MVI   RCSUBPRG,0                                                       
         CLC   QPROG,=C'33'        CLIENT VS. BANK                              
         BE    *+10                                                             
         CLC   QPROG,=C'32'        BANK REPORT                                  
         BNE   *+8                                                              
         MVI   QOPT1,C' '          FORCE WEIGHTING                              
         CLC   QPROG,=C'36'                                                     
         BNE   *+8                                                              
         MVI   QOPT3,C'2'                                                       
         CLC   QPROG,=C'38'                                                     
         BNE   *+8                                                              
         MVI   QOPT3,C'3'                                                       
         CLI   QOPT3,C'Y'          DAYPART ORDER                                
         BNE   *+8                                                              
         MVI   QOPT3,C'4'                                                       
         MVI   DATESW,0                                                         
         MVI   FCMONTH,C'N'                                                     
         MVI   ONESPTSW,1                                                       
         LA    RE,MKTKY                                                         
*                                                                               
RP1      MVI   FORMATSW,0          DAYPART REPORT                               
         CLI   QOPT3,C'2'                                                       
         BNE   RP2                                                              
         MVI   FORMATSW,1                                                       
         LA    RE,MKTKY                                                         
         MVI   ONESPTSW,0                                                       
         MVI   RCSUBPRG,2                                                       
         CLI   SVQOPT3,C'Y'                                                     
         BNE   RP2                                                              
         LA    RE,DPTKY                                                         
         MVI   DPTORDER,C'Y'                                                    
         MVI   RCSUBPRG,3                                                       
*                                                                               
RP2      CLI   QOPT3,C'4'          MARKET REPORT                                
         BNE   RP3                                                              
         LA    RE,DPTKY                                                         
         MVI   ONESPTSW,0                                                       
         MVI   RCSUBPRG,3                                                       
         MVI   DPTORDER,C'Y'                                                    
         MVI   FORMATSW,1                                                       
         CLC   QPROG,=C'33'                                                     
         BE    *+10                                                             
         CLC   QPROG,=C'42'                                                     
         BE    *+10                                                             
         CLC   QPROG,=C'32'                                                     
         BNE   RP3                                                              
         MVI   FORMATSW,0                                                       
         MVI   ONESPTSW,1                                                       
         MVI   RCSUBPRG,4                                                       
*                                                                               
RP3      CLI   QOPT3,C'3'          TREND REPORT                                 
         BNE   RP99                                                             
         MVI   FCMONTH,C'Y'        TREND                                        
         MVI   DATESW,1                                                         
         MVI   ONESPTSW,0                                                       
         LA    RE,QTRKY                                                         
         MVI   DPTORDER,C'Y'                                                    
*                                                                               
RP99     CLC   QPROG,=C'38'                                                     
         BNE   *+8                                                              
         MVI   RCSUBPRG,5                                                       
         ST    RE,ASRTKEY                                                       
         ST    RE,ARPTKEY                                                       
*                                                                               
         SR    R1,R1               CLEAR CROSS MARKET TOTALS                    
         LA    RF,QTRKY                                                         
         CR    RE,RF                                                            
         BNE   *+8                                                              
         LA    R1,QTRKYT           SET FOR QUARTERLY TOTALS                     
*                                                                               
         LA    RF,DPTKY                                                         
         CR    RE,RF                                                            
         BNE   *+8                                                              
         LA    R1,DPTKYT           SET FOR DAYPART TOTALS                       
         ST    R1,ASRTKEYT                                                      
         ST    R1,ARPTKEYT                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   *+8                                                              
         MVI   ACTSW,0                                                          
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   BK09X                                                            
*                                                                               
BK08     DS    0H                                                               
*                                                                               
KEEPAGY  L     RE,MTAB                                                          
         L     RF,=F'600000'                                                    
         XCEF                                                                   
         XC    HIGROUP,HIGROUP                                                  
*                                                                               
         MVC   ARBCAP,=C'ARB'      SET PROPER ARB CAPTION                       
         CLI   QMED,C'C'                                                        
         BNE   *+10                                                             
         MVC   ARBCAP,=C'BBM'                                                   
*                                                                               
         CLI   FIRST,1             BUILD DEMO TABLES ONLY ONCE                  
         BNE   BK08A                                                            
         MVI   FIRST,0                                                          
         L     R9,ACOMFACS                                                      
         USING COMFACSD,R9                                                      
         GOTO1 CDEMADDR,DMCB,(X'FF',ATABLES),ACOMFACS                           
*                                                                               
BK08A    CLI   ACTSW,2             SORT ALREADY OPEN                            
         BE    BK09                                                             
         L     RF,=V(SORTER)                                                    
         ST    RF,VSORTER                                                       
         GOTO1 (RF),DMCB,SORT,RECCARD,0                                         
*                                                                               
BK09     MVI   ACTSW,0                                                          
         XC    UTCNT,UTCNT         AND COUNT                                    
*                                                                               
         MVI   VSOURCE,C' '        SET FOR BOTH RATING SERVICES                 
         CLC   QPROG,=C'34'        34 REPORT ALWAYS DOES BOTH                   
         BE    BK09X                                                            
         L     RE,=A(AGYRS)        SET UP RATING SERVICE                        
         CLI   QMED,C'C'           CANADIAN                                     
         BNE   *+8                                                              
         L     RE,=A(AGYCAN)                                                    
*                                                                               
BK09A    CLI   0(RE),X'FF'         END OF LIST                                  
         BNE   *+6                                                              
         DC    H'0'                NOT A VALID AGENCY                           
         CLC   QAGY(2),0(RE)       AGENCY EQUAL                                 
         BE    *+12                YES - SET VALID SOURCE CODE                  
         LA    RE,4(RE)                                                         
         B     BK09A                                                            
         MVC   VSOURCE,2(RE)                                                    
         MVC   BANKNUM,3(RE)                                                    
         CLI   QSERVICE,C' '       FORCE SOURCE TO VALID IF REQUESTED           
         BE    BK09X                                                            
         MVC   VSOURCE,QSERVICE                                                 
*                                                                               
BK09X    XMOD1 1                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DROP  R6                                                               
*HEADLINES ROUTINES                                                             
MYHEAD   CSECT                                                                  
         USING *,RF                                                             
         L     R2,=V(CP32WK)                                                    
         NMOD1 0,MYHEAD                                                         
         L     RA,CP32RA                                                        
         L     RC,CP32RC                                                        
         L     R6,CP32R6                                                        
         DROP  RF                                                               
         USING CP3202+4096,R6                                                   
         CLI   QGRP,C'S'                                                        
         BNE   MYHMGRX                                                          
         MVC   H6(20),=C'*MGROUP        ONLY*'                                  
         MVC   H6+8(1),QGRP+1                                                   
         MVC   H6+9(4),QMKT                                                     
*                                                                               
MYHMGRX  DS    0H'0'                                                            
         CLI   QCLOFFC,C' '                                                     
         BE    MYHOFFX                                                          
         MVC   H6(20),=C'***OFFICE   ONLY ***'                                  
         MVC   H6+10(1),QCLOFFC                                                 
*                                                                               
MYHOFFX  CLC   QPROG,=C'32'        IND VS. AGENCY                               
         BNE   *+16                                                             
         MVC   H1+23(33),AGYNM                                                  
         MVC   H1+57(30),CNH1                                                   
         CLI   QGRP,C'T'                                                        
         BNE   *+16                                                             
         MVC   H1+23(33),SPACES                                                 
         MVC   H1+23(15),=C'MULTIPLE AGENCY'                                    
*                                                                               
         CLC   QPROG,=C'33'        IND VS CLIENT                                
         BNE   MYHR42                                                           
         LA    R1,EQGETCLT                                                      
         L     RF,=A(SUBNTR)                                                    
         BASR  RE,RF                                                            
         MVC   H1+23(L'CLTNM),CLTNM                                             
         MVC   H1+57(30),CNH1                                                   
         MVC   H7+21(35),CNH15                                                  
         MVC   H7+85(35),CNH15                                                  
         B     MYHEAD1                                                          
*                                                                               
MYHR42   CLC   QPROG,=C'42'        CLIENT VS. AGENCY                            
         BNE   MYHEAD1                                                          
         MVC   H1+57(16),CNH2                                                   
         LA    R1,EQGETCLT                                                      
         L     RF,=A(SUBNTR)                                                    
         BASR  RE,RF                                                            
         MVC   H1+23(L'CLTNM),CLTNM                                             
         MVC   H7+21(35),CNH3                                                   
         MVC   H7+85(35),CNH3                                                   
*                                                                               
MYHEAD1  CLC   QPROG,=C'36'                                                     
         BNE   *+10                                                             
         MVC   H1+57(10),CNH4                                                   
         CLC   QPROG,=C'38'                                                     
         BNE   *+10                                                             
         MVC   H1+57(12),CNH5                                                   
         CLC   QCLT(3),=C'ALL'                                                  
         BE    MYH1CLTX                                                         
         LA    R1,EQGETCLT                                                      
         L     RF,=A(SUBNTR)                                                    
         BASR  RE,RF                                                            
         MVC   H3(09),=C'CLIENT - '                                             
         MVC   H3+10(L'QCLT),QCLT                                               
         MVC   H3+15(L'CLTNM),CLTNM                                             
*                                                                               
MYH1CLTX GOTO1 SQUASHER,DMCB,H1+23,64                                           
         GOTO1 CENTER,DMCB,H1+23,64                                             
         GOTO1 UNDERLIN,DMCB,(63,H1+23),H2+23                                   
*                                                                               
         CLI   DPTORDER,C'Y'                                                    
         BNE   MYHEAD2                                                          
         MVC   H5+40(8),=C'MARKET -'                                            
         MVC   H5+50(24),CNMNAM                                                 
         CLC   QPROG,=C'38'                                                     
         BNE   MYHEAD3                                                          
         MVC   H7(17),=C'PROG TYPE/DAYPART'                                     
         MVC   H9+2(4),=C'DATA'                                                 
         MVC   H10+2(4),=C'----'                                                
         MVC   H08+10(16),CNH8                                                  
         MVC   H09+10(16),CNH6                                                  
         LA    R9,QYRDATE                                                       
         MVC   H09+10+2(2),0(R9)                                                
         MVC   H09+10+10(2),2(R9)                                               
         CLC   H09+10+2(2),=C'50'                                               
         BH    *+10                                                             
         MVC   H09+10(2),=C'20'                                                 
         CLC   H09+10+10(2),=C'50'                                              
         BH    *+10                                                             
         MVC   H09+10+8(2),=C'20'                                               
         LA    R9,4(R9)                                                         
         MVC   H10+10(16),CNH7                                                  
         MVC   H08+30(16),CNH9                                                  
         MVC   H09+30(16),CNH6                                                  
         MVC   H09+30+2(2),0(R9)                                                
         MVC   H09+30+10(2),2(R9)                                               
         CLC   H09+30+2(2),=C'50'                                               
         BH    *+10                                                             
         MVC   H09+30(2),=C'20'                                                 
         CLC   H09+30+10(2),=C'50'                                              
         BH    *+10                                                             
         MVC   H09+30+8(2),=C'20'                                               
         LA    R9,4(R9)                                                         
         MVC   H10+30(16),CNH7                                                  
         MVC   H08+50(16),CNH10                                                 
         MVC   H09+50(16),CNH6                                                  
         MVC   H09+50+2(2),0(R9)                                                
         MVC   H09+50+10(2),2(R9)                                               
         CLC   H09+50+2(2),=C'50'                                               
         BH    *+10                                                             
         MVC   H09+50(2),=C'20'                                                 
         CLC   H09+50+10(2),=C'50'                                              
         BH    *+10                                                             
         MVC   H09+50+8(2),=C'20'                                               
         LA    R9,4(R9)                                                         
         MVC   H10+50(16),CNH7                                                  
         MVC   H08+70(16),CNH11                                                 
         MVC   H09+70(16),CNH6                                                  
         MVC   H09+70+2(2),0(R9)                                                
         MVC   H09+70+10(2),2(R9)                                               
         CLC   H09+70+2(2),=C'50'                                               
         BH    *+10                                                             
         MVC   H09+70(2),=C'20'                                                 
         CLC   H09+70+10(2),=C'50'                                              
         BH    *+10                                                             
         MVC   H09+70+8(2),=C'20'                                               
         MVC   H10+70(16),CNH7                                                  
         MVC   H08+91(27),CNH12                                                 
         MVC   H09+91(27),CNH13                                                 
         MVC   H10+91(27),CNH14                                                 
         B     MYHEAD2A                                                         
*                                                                               
MYHEAD2  LA    R1,EQGETRPT         GET REPORT CAPTION                           
         L     RF,=A(SUBNTR)                                                    
         BASR  RE,RF                                                            
         MVC   H5+41(36),CNRPTL                                                 
*                                                                               
MYHEAD2A MVC   H5(13),=C'DEMOGRAPHIC -'                                         
         MVC   TEMPDEMO,HLDDEMO                                                 
         LA    R1,EQGETDEM                                                      
         L     RF,=A(SUBNTR)                                                    
         BASR  RE,RF                                                            
         MVC   H5+15(7),CNDNAM                                                  
         MVC   TEMPDEMO,HLDDEMO                                                 
         MVC   TEMPTGT,HLDTGT                                                   
         LA    R1,EQGETTGT                                                      
         L     RF,=A(SUBNTR)                                                    
         BASR  RE,RF                                                            
         MVC   H5+24(13),CNTGT                                                  
*                                                                               
         CLC   QPROG,=C'38'                                                     
         BE    MYHEAD3                                                          
         MVC   H8+20(7),CNDNAM     MOVE IN UNIVERSE DEMO                        
         CLI   QMED,C'C'                                                        
         BE    *+10                                                             
         CLC   QPROG,=C'34'                                                     
         BE    *+10                                                             
         MVC   H8+84(7),CNDNAM                                                  
*                                                                               
*                                                                               
MYHEAD3  DS    0C                                                               
*                                                                               
MYHEAD4  MVC   H6+100(17),=C'RATING SERVICE - '                                 
         MVC   H6+118(3),=C'NSI'                                                
         CLI   HLDSRV,C'A'                                                      
         BNE   *+10                                                             
         MVC   H6+118(3),ARBCAP                                                 
*                                                                               
         CLC   QPROG,=C'36'                                                     
         BE    MYHEAD5                                                          
         CLI   QMED,C'C'                                                        
         BNE   HNOTCAN                                                          
         CLI   RCSUBPRG,2                                                       
         BL    MYHEAD5                                                          
         CLI   RCSUBPRG,4                                                       
         BNE   HNOTCAN                                                          
*                                                                               
MYHEAD5  MVC   H8+20(7),=C'       '                                             
         MVC   H9+21(5),=C'     '                                               
         MVC   H10+21(5),=C'     '                                              
         MVC   H8+84(7),=C'       '                                             
         MVC   H9+85(5),=C'     '                                               
         MVC   H10+85(5),=C'     '                                              
*                                                                               
HNOTCAN  DS    0C                                                               
         CLI   DPTORDER,C'Y'                                                    
         BE    MYHEAD9                                                          
         MVC   H4(13),=C'DAYPART     -'                                         
         LA    RF,DPTNAM2                                                       
         MVC   H4+15(24),=CL24'UNKNOWN='                                        
         MVC   H4+24(1),HLDDPT                                                  
*                                                                               
MYHEAD7  CLC   HLDDPT,0(RF)        PRINT DAYPART CAPTION                        
         BE    MYHEAD8                                                          
         CLI   0(RF),X'FF'         NOT FOUND                                    
         BE    MYHEAD9                                                          
         LA    RF,25(RF)                                                        
         B     MYHEAD7                                                          
*                                                                               
MYHEAD8  MVC   H4+15(24),1(RF)                                                  
         EJECT                                                                  
*                                                                               
MYHEAD9  CLC   QPROG,=C'34'        PRINT REPORT 34 HEADLINES                    
         BNE   MYHEADX                                                          
         LA    R8,H8+45                                                         
         LA    R9,1                                                             
         LA    R7,NUMAGY                                                        
*                                                                               
MYHEAD10 EDIT  (R9),(2,0(R8))     PRINT AGENCY NUMBER                           
         MVC   132(2,R8),=C'--'                                                 
         LA    R8,4(R8)                                                         
         LA    R9,2(R9)            BUMP TO NEXT PRINTABLE                       
         BCTR  R7,0                                                             
         LTR   R7,R7                                                            
         BZ    MYHEADX                                                          
         BCT   R7,MYHEAD10                                                      
*                                                                               
MYHEADX  XMOD1 1                                                                
*                                                                               
H08      EQU   H8                                                               
H09      EQU   H9                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* SET UP RECORDS BASED ON RPTCTRL RECORDS                                       
PUTSORT  CSECT                                                                  
         NMOD1 0,PUTSORT                                                        
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING CPWORKD,RA,RC                                                    
         USING CP3202+4096,R6                                                   
         USING CP32WK,R2                                                        
*                                                                               
         MVI   RPTCID,C' '         DEFAULT TABLE ID                             
         L     R7,=A(RPTCTRL)      DEFAULT REPORT CONTROL TABLE                 
*        CLC   QEND,=C'88'                                                      
*        BL    *+12                                                             
         MVI   RPTCID,C'8'         1988 US BANK TABLE                           
         L     R7,=A(RPTCTRL8)                                                  
*                                                                               
         CLI   QGRP+1,C'X'         INDEX DEMO TO TARGET                         
         BE    *+8                                                              
         CLI   QGRP+1,C'P'         PLANNING TAPE SPECIAL                        
         BNE   *+8                                                              
         L     R7,=A(RPTCTRLX)                                                  
*                                                                               
         CLI   QMED,C'C'                                                        
         BNE   *+12                                                             
         MVI   RPTCID,C'C'         CANADIAN BANK                                
         L     R7,=A(RPTCTLC)                                                   
*        CLC   QAGY,=C'FM'                                                      
*        BNE   *+12                                                             
*        MVI   RPTCID,C'A'         AGENCY SPECIAL                               
*        L     R7,=A(RPTCTLFM)                                                  
         CLC   QAGY,=C'TB'                                                      
         BNE   *+12                                                             
         MVI   RPTCID,C'A'         AGENCY SPECIAL                               
         L     R7,=A(RPTCTLTB)                                                  
*                                                                               
         MVC   HLDKEY,SRKEY        SAVE ORIGINAL KEY                            
*                                                                               
PUTS02   LA    R8,3(R7)            SET TO PROCESS CONTROLS                      
*                                                                               
PUTS04   CLI   0(R8),X'FF'         END OF ALL PROCESS LISTS                     
         BE    PUTS06                                                           
         MVC   SRKEY,HLDKEY        RESTORE ORIGINAL KEY                         
         MVI   OK,1                SET TO PROCESS RECORD                        
         ZIC   R1,0(R8)            GET ROUTINE ADDRESS                          
         SLL   R1,2                                                             
         L     RE,RPTSEL(R1)                                                    
         BASR  R9,RE               GO TO ROUTINE                                
         CLI   OK,0                FAILED A ROUTINE                             
         BE    PUTS16              TRY NEXT REPORT                              
         LA    R8,1(R1,R8)         NEXT PROCESS (R1 HAS INC. LIST END)          
         B     PUTS04              PROCESS CURRENT ROUTINE                      
*                                                                               
PUTS06   MVC   SRRPT,2(R7)         SET REPORT CODE                              
         CLC   QTARGET,=C'   '     ALL TAREGETS                                 
         BE    PUTS08                                                           
         CLC   QTARGET,QSELECT     TARGETS ONLY                                 
         BNE   *+14                                                             
         CLC   SRTGT,SRDEMO        INSURE THIS IS A TARGET                      
         BNE   PUTS16                                                           
*                                                                               
PUTS08   MVC   SAVSKY,SRREC        SAVE THE SORT KEY                            
         BAS   RE,RESRTK           REFORMAT SORT KEY                            
         GOTO1 VSORTER,DMCB,=C'PUT',SRREC                                       
         MVI   ACTSW,1                                                          
         MVC   SRKEY,SAVSKY        RESTORE BASIC KEY                            
         B     PUTS16                                                           
*                                                                               
PUTS10   CLC   SRRST,2(R9)         FIND FIRST GROUP                             
         BH    PUTS12                                                           
         CLI   0(R9),X'FF'                                                      
         BNE   PUTS14                                                           
         DC    H'0'                                                             
*                                                                               
PUTS12   LA    R9,4(R9)                                                         
         B     PUTS10                                                           
*                                                                               
PUTS14   MVC   SRRST,0(R9)         INITIAL SEQUENCE                             
         MVC   SRREN,2(R9)                                                      
         CLC   0(2,R9),=H'1'       DON'T REPLICATE IF ALREADY 1                 
         BNE   *+8                                                              
         MVI   RGROUPSW,0                                                       
         CLI   RGROUPSW,1          FIRST PASS                                   
         BE    *+10                LEAVE RANK ALONE                             
         MVC   SRRST,=H'1'                                                      
         XC    SRRST,=X'FFFE'                                                   
         MVC   SRMSEQ,=X'FFFF'                                                  
         BAS   RE,RESRTK                                                        
         GOTO1 VSORTER,DMCB,=C'PUT',SRREC                                       
         MVC   SRREC(L'SRKEY),SAVSKY                                            
         CLI   RGROUPSW,1          REPROCESS ENTRY IF FIRST PASS                
         BE    *+8                                                              
         LA    R9,4(R9)                                                         
         MVI   RGROUPSW,0                                                       
         CLI   0(R9),X'FF'         END OF GROUPS                                
         BNE   PUTS14                                                           
*                                                                               
PUTS16   SR    RE,RE                                                            
         ICM   RE,3,0(R7)          BUMP TO NEXT REPORT                          
         AR    R7,RE                                                            
         CLI   0(R7),X'FF'         END OF REPORTS                               
         BNE   PUTS02                                                           
         MVC   SRKEY,HLDKEY        RESTORE FOR DATE REPORTS                     
*                                                                               
PUTSX    XMOD1 1                                                                
         EJECT                                                                  
*              ROUTINE SELECTION TABLE                                          
RPTSEL   DC    A(0)                                                             
         DC    A(RSELECT)          KEY SELECTION                                
         DC    A(RTDYES)           TARGET LINE SELECTION                        
         DC    A(RDEMYES)          DEMO INCLUSION                               
         DC    A(RDEMNO)           DEMO EXCLUSION                               
         DC    A(RDGYES)           DEMO GROUP INCLUSION                         
         DC    A(RDGNO)            DEMO GROUP EXCLUSION                         
         DC    A(ROVRIDE)          OVERRIDE KEY FIELD VALUES                    
         DC    A(RTGTYES)          TARGET DEMO INCLUSION                        
         DC    A(RTGTNO)           TARGET DEMO EXCLUSION                        
         DC    A(RTGYES)           TRGT GROUP INCLUSION                         
         DC    A(RTGNO)            TRGT GROUP EXCLUSION                         
         EJECT                                                                  
* RPTCTRL ROUTINE DEFINITIONS                                                   
* RECORD SELECTION                                                              
RSELECT  LA    R1,1                DISP TO END                                  
         MVI   OK,0                SET TO FAILED                                
         MVC   PREVCOL,1(R8)       FORCE EQUAL                                  
*                                                                               
RSELECT1 CLC   PREVCOL,1(R8)       SAME COLUMN                                  
         BE    RSELECT2            DONT CHECK SWITCH                            
         CLI   OK,0                EXIT IF ALL FILTERS FAILED                   
         BER   R9                                                               
         MVC   PREVCOL,1(R8)       SET FOR NEXT COL                             
         MVI   OK,0                                                             
*                                                                               
RSELECT2 ZIC   R4,1(R8)            FILTER KEY FIELDS ON TABLE VALUES            
         LA    R4,SRREC(R4)                                                     
         ZIC   RE,2(R8)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   3(0,R8),0(R4)                                                    
         BNE   *+8                                                              
         MVI   OK,1                SET TO PASSED A FILTER                       
         ZIC   R4,2(R8)            GET NEXT ENTRY                               
         LA    R8,2(R4,R8)                                                      
         CLI   1(R8),X'FF'                                                      
         BNE   RSELECT1                                                         
         BR    R9                                                               
*                                                                               
ENDLST   CLI   OK,2                OK TO SELECT                                 
         BE    *+10                                                             
         MVI   OK,1                SET TO SELECT                                
         BR    R9                                                               
         MVI   OK,0                SET TO REJECT                                
         BR    R9                                                               
*                                                                               
* SELECT ONLY IF TARGET AND REPORT DEMO ARE THE SAME                            
RTDYES   LA    R1,1                                                             
*                                                                               
         CLC   QPROG,=C'38'        SPECIAL TO MAINTAIN COMPATIBILITY            
         BNE   RTDYES20            FOR 1988 AND PRIOR                           
         CLI   RPTCID,C'8'         ONLY 1988 US BANK AGENCY TRENDS              
         BNE   RTDYES20                                                         
         CLC   QTARGET,=C'   '     ONLY FOR GROUP TARGETS                       
         BNE   RTDYES20                                                         
         CLC   QSELECT,=C'042'     W1849                                        
         BNE   RTDYES2                                                          
         CLI   SRTGT,240                                                        
         BE    *+8                                                              
         CLI   SRTGT,W1849                                                      
         BE    *+8                                                              
         CLI   SRTGT,W1834                                                      
         BE    *+8                                                              
         CLI   SRTGT,W2554                                                      
         BNE   RTDYES20                                                         
         B     RTDYES10                                                         
*                                                                               
RTDYES2  CLC   QSELECT,=C'092'     M1849                                        
         BNE   RTDYES3                                                          
         CLI   SRTGT,240                                                        
         BE    *+8                                                              
         CLI   SRTGT,M1849                                                      
         BE    *+8                                                              
         CLI   SRTGT,M1834                                                      
         BE    *+8                                                              
         CLI   SRTGT,M2554                                                      
         BNE   RTDYES20                                                         
         B     RTDYES10                                                         
*                                                                               
RTDYES3  CLC   QSELECT,=C'142'     V1849                                        
         BNE   RTDYES20                                                         
         CLI   SRTGT,240                                                        
         BE    *+8                                                              
         CLI   SRTGT,V1849                                                      
         BE    *+8                                                              
         CLI   SRTGT,V1834                                                      
         BE    *+8                                                              
         CLI   SRTGT,V2554                                                      
         BNE   RTDYES20                                                         
         B     RTDYES10                                                         
*                                                                               
RTDYES10 MVI   SRTGT,240           SET THE GROUP TARGET                         
         MVI   HLDKEY+SRTGT-SRREC,240                                           
         BR    R9                                                               
*                                                                               
RTDYES20 CLC   SRDEMO,SRTGT                                                     
         BER   R9                                                               
         MVI   OK,0                SET TO REJECT                                
         BR    R9                                                               
*                                                                               
RDEMYES  LA    R1,0                INCLUDE LISTED DEMOS                         
         LA    R8,1(R8)                                                         
         MVI   OK,0                                                             
*                                                                               
RDEMYES2 CLI   0(R8),X'FF'                                                      
         BER   R9                                                               
         CLC   0(1,R8),SRDEMO                                                   
         BNE   *+8                                                              
         MVI   OK,1                                                             
         LA    R8,1(R8)                                                         
         B     RDEMYES2                                                         
*                                                                               
RDEMNO   LA    R1,0                EXCLUDE LISTED DEMOS                         
         LA    R8,1(R8)                                                         
*                                                                               
RDEMNO2  CLI   0(R8),X'FF'                                                      
         BER   R9                                                               
         CLC   0(1,R8),SRDEMO                                                   
         BNE   *+8                                                              
         MVI   OK,0                                                             
         LA    R8,1(R8)                                                         
         B     RDEMNO2                                                          
*                                                                               
RTGTYES  LA    R1,0                INCLUDE LISTED DEMOS                         
         LA    R8,1(R8)                                                         
         MVI   OK,0                                                             
*                                                                               
RTGTYES2 CLI   0(R8),X'FF'                                                      
         BER   R9                                                               
         CLC   0(1,R8),SRTGT                                                    
         BNE   *+8                                                              
         MVI   OK,1                                                             
         LA    R8,1(R8)                                                         
         B     RTGTYES2                                                         
*                                                                               
RTGTNO   LA    R1,0                EXCLUDE LISTED DEMOS                         
         LA    R8,1(R8)                                                         
*                                                                               
RTGTNO2  CLI   0(R8),X'FF'                                                      
         BER   R9                                                               
         CLC   0(1,R8),SRTGT                                                    
         BNE   *+8                                                              
         MVI   OK,0                                                             
         LA    R8,1(R8)                                                         
         B     RTGTNO2                                                          
*                                                                               
RDGNO    LA    R1,0                EXCLUDE DEMO GROUPS                          
         LA    R8,1(R8)                                                         
*                                                                               
RDGNO2   CLI   0(R8),X'FF'                                                      
         BER   R9                                                               
         CLC   SRDEMO,0(R8)                                                     
         BL    RDGNO3                                                           
         CLC   SRDEMO,1(R8)                                                     
         BH    RDGNO3                                                           
         MVI   OK,0                                                             
*                                                                               
RDGNO3   LA    R8,2(R8)                                                         
         B     RDGNO2                                                           
*                                                                               
RDGYES   LA    R1,0                INCLUDE DEMO GROUPS                          
         LA    R8,1(R8)                                                         
         MVI   OK,0                                                             
*                                                                               
RDGYES2  CLI   0(R8),X'FF'                                                      
         BER   R9                                                               
         CLC   SRDEMO,0(R8)                                                     
         BL    RDGYES3                                                          
         CLC   SRDEMO,1(R8)                                                     
         BH    RDGYES3                                                          
         MVI   OK,1                                                             
*                                                                               
RDGYES3  LA    R8,2(R8)                                                         
         B     RDGYES2                                                          
*                                                                               
RTGNO    LA    R1,0                EXCLUDE DEMO GROUPS                          
         LA    R8,1(R8)                                                         
*                                                                               
RTGNO2   CLI   0(R8),X'FF'                                                      
         BER   R9                                                               
         CLC   SRTGT,0(R8)                                                      
         BL    RTGNO3                                                           
         CLC   SRTGT,1(R8)                                                      
         BH    RTGNO3                                                           
         MVI   OK,0                                                             
*                                                                               
RTGNO3   LA    R8,2(R8)                                                         
         B     RTGNO2                                                           
*                                                                               
RTGYES   LA    R1,0                INCLUDE DEMO GROUPS                          
         LA    R8,1(R8)                                                         
         MVI   OK,0                                                             
*                                                                               
RTGYES2  CLI   0(R8),X'FF'                                                      
         BER   R9                                                               
         CLC   SRTGT,0(R8)                                                      
         BL    RTGYES3                                                          
         CLC   SRTGT,1(R8)                                                      
         BH    RTGYES3                                                          
         MVI   OK,1                                                             
*                                                                               
RTGYES3  LA    R8,2(R8)                                                         
         B     RTGYES2                                                          
*                                                                               
ROVRIDE  LA    R1,0                OVERRIDE KEY FIELDS                          
         LA    R8,1(R8)                                                         
*                                                                               
ROVRIDE2 ZIC   R4,0(R8)                                                         
         LA    R4,SRREC(R4)                                                     
         ZIC   RE,1(R8)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8              MOVE IN OVERRIDES                            
         B     *+10                                                             
         MVC   0(0,R4),2(R8)                                                    
         ZIC   R4,1(R8)                                                         
         LA    R8,2(R4,R8)                                                      
         CLI   0(R8),X'FF'                                                      
         BER   R9                                                               
         B     ROVRIDE2                                                         
         EJECT                                                                  
***********************************************************************         
*        REFORMAT INTO SORT KEY                                       *         
***********************************************************************         
*                                                                               
RESRTK   NTR1                                                                   
         L     RE,ASRTKEY          GET THE KEY DEFINITION                       
         MVI   TSWITCH,0                                                        
         OC    ASRTKEYT,ASRTKEYT   SETUP FOR X-MARKET TOTALS                    
         BZ    RESRT2              NO - DO NORMAL KEY                           
         CLC   SRMSEQ,=X'FFFF'     CROSS MARKET RECORD                          
         BNE   *+12                                                             
         L     RE,ASRTKEYT         YES - USE ALTERNATE KEY                      
         MVI   TSWITCH,1                                                        
*                                                                               
RESRT2   OC    0(2,RE),0(RE)       EXIT IF NO REFORMATTING                      
         BZ    RESRTX                                                           
         XC    BLDKEY,BLDKEY       TEMP BUILD AREA                              
         LA    R4,BLDKEY           POINT TO OUTPUT                              
*                                                                               
RESRT4   ZIC   R8,0(RE)            GET DISPLACEMENT                             
         LA    R8,SRKEY(R8)        POINT TO INPUT                               
         ZIC   R1,1(RE)            SET THE LENGTH                               
         BCTR  R1,0                SET UP FOR EXECUTE                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R8)       MOVE TO NEW POS.                             
         LA    RE,2(RE)            NEXT INPUT                                   
         LA    R4,1(R1,R4)         BUMP OUTPUT                                  
         OC    0(2,RE),0(RE)                                                    
         BNZ   RESRT4                                                           
*                                                                               
         MVC   SRKEY,BLDKEY                                                     
         CLI   TSWITCH,1                                                        
         BNE   *+8                                                              
         MVI   SRKEY+19,1                                                       
*                                                                               
RESRTX   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* SET UP RECORDS BASED ON RPTCTRL RECORDS                                       
READU    CSECT                                                                  
         NMOD1 0,READU                                                          
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING CPWORKD,RA,RC                                                    
         USING CP3202+4096,R6                                                   
         USING CP32WK,R2                                                        
         STM   RA,RC,READURA                                                    
         ST    R2,READUR2                                                       
         ST    R6,READUR6                                                       
         MVC   FULL(1),HLDDEMO                                                  
         MVC   FULL+1(2),MTMKT                                                  
         GOTO1 BINSRCH,DMCB,(X'00',FULL),UTAB,UTCNT,UTLN,UTKL,UTMAX             
         CLI   0(R1),X'00'         DID WE FIND IT                               
         BE    RDUNIV2                                                          
         L     RF,ADBLOCK                                                       
         USING DBLOCK,RF                                                        
         XC    0(256,RF),0(RF)     CLEAR THE DBLOCK                             
         MVC   DBFILE,=C'TP '      SET TO READ MARKET LEVEL RECORDS             
         MVI   DBSELMED,C'T'                                                    
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBSELBK,=X'640B'                                                 
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBAREC,ADEMREC                                                   
         MVC   DBSELRMK,MTMKT                                                   
         SR    R1,R1               CPP MARKETS ARE 400 TO HIGH                  
         ICM   R1,3,DBSELRMK                                                    
         SH    R1,=H'400'                                                       
         STCM  R1,3,DBSELRMK                                                    
         MVC   DBSELAGY,=C'SJ'                                                  
         MVI   DBSELDAY,X'40'      SET TO READ MON 5-515P                       
         MVC   DBSELTIM(2),=H'1700'                                             
         MVC   DBSELTIM+2(2),=H'1715'                                           
         MVI   DBFUNCT,DBGETTOT                                                 
         L     RE,UTL              SAVE CURRENT UTL                             
         MVC   SVUTL,0(RE)                                                      
*        MVI   4(RE),X'02'                                                      
         MVC   4(1,RE),RCSSE                                                    
         XC    WORK,WORK           SET UP FOR MISSING UNIV.                     
         LA    RF,WORK                                                          
         USING UTABD,RF                                                         
         MVC   UTDEM,HLDDEMO                                                    
         MVC   UTMKT,MTMKT                                                      
         DROP  RF                                                               
         L     R9,ACOMFACS                                                      
         USING COMFACSD,R9                                                      
         GOTO1 CDEMAND,DMCB,ADBLOCK,SVUNIV                                      
         GOTO1 BINSRCH,DMCB,(X'01',WORK),UTAB,UTCNT,UTLN,UTKL,UTMAX             
         MVC   UTCNT,8(R1)                                                      
         L     RE,UTL              RESTORE THE UTL                              
         MVC   0(8,RE),SVUTL                                                    
*                                                                               
RDUNIV2  L     RF,0(R1)                                                         
         USING UTABD,RF                                                         
         MVC   DIDUNIV,UTUNIV                                                   
         B     READUX                                                           
         EJECT                                                                  
* SAVE THE UNIVERSE IN THE MARKET TABLE                                         
         DS    0D                                                               
         DROP  R6                                                               
         USING *,RF                                                             
SVUNIV   NTR1  BASE=READURB                                                     
         L     R2,READUR2                                                       
         LM    RA,RC,READURA                                                    
         L     R6,READUR6                                                       
         DROP  RF                                                               
         USING CP3202+4096,R6                                                   
         L     RF,ADBLOCK          SET TO GET UNIVERSE                          
         L     R9,ACOMFACS                                                      
         USING COMFACSD,R9                                                      
         XC    DIDUNIV,DIDUNIV                                                  
         MVC   DEMO+2(1),HLDDEMO                                                
         GOTO1 CDEMOUT,DMCB,(C'L',DEMO),ADBLOCK,DIDUNIV                         
         DROP  R9                                                               
         LA    RF,WORK                                                          
         USING UTABD,RF                                                         
         MVC   UTDEM,HLDDEMO                                                    
         MVC   UTMKT,MTMKT                                                      
         MVC   UTUNIV,DIDUNIV                                                   
         B     READUX                                                           
*                                                                               
READUX   XMOD1 1                                                                
*                                                                               
READURA  DS    F                                                                
READURB  DS    F                                                                
READURC  DS    F                                                                
READUR2  DS    F                                                                
READUR6  DS    F                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
QCONTAB  DC    C'1ST 2ND 3RD 4TH 1ST 2ND 3RD '                                  
QCONTAB2 DC    C'1/2 2/3 3/4 4/1 1/2 2/3 3/4 4/1 1/2 2/3 '                      
         EJECT                                                                  
CP32WK   CSECT                                                                  
CNH1     DC    C'VS. COMBINED AGENCY CPP REPORT'                                
CNH2     DC    C'CPP INDEX REPORT'                                              
CNH3     DC    C'       ----AGENCY----  ---CLIENT---'                           
CNH4     DC    C'CPP REPORT'                                                    
CNH5     DC    C'TREND REPORT'                                                  
CNH6     DC    C'YR.1    YR.2 IDX'                                              
CNH7     DC    C'----    ---- ---'                                              
CNH8     DC    C'--1ST QUARTER---'                                              
CNH9     DC    C'--2ND QUARTER---'                                              
CNH10    DC    C'--3RD QUARTER---'                                              
CNH11    DC    C'--4TH QUARTER---'                                              
CNH12    DC    C'-QUARTER TO QUARTER TRENDS-'                                   
CNH13    DC    C'1/2 2/3 3/4 4/1 1/2 2/3 3/4'                                   
CNH14    DC    C'--- --- --- --- --- --- ---'                                   
CNH15    DC    C'       ---COMBINED---  ---CLIENT---'                           
         EJECT                                                                  
ATABLES  DC    X'D1',3X'00',X'D2',3X'00',X'D3',3X'00'                           
         DC    X'D4',3X'00',X'D5',3X'00',X'D6',3X'00'                           
         DC    X'D7',3X'00',X'FF'                                               
* TABLE TO DEFINE SORT KEYS                                                     
MKTKY    DC    AL1(POSSRV),AL1(L'SRSRV)   DEMO/DAYPART/MARKET                   
         DC    AL1(POSSSEQ),AL1(L'SRSSEQ)                                       
         DC    AL1(POSDEMO),AL1(L'SRDEMO)                                       
         DC    AL1(POSTGT),AL1(L'SRTGT)                                         
         DC    AL1(POSRPT),AL1(SRPRG-SRRPT+L'SRPRG)                             
         DC    AL1(POSDPT),AL1(SRAGY-SRDPT+L'SRAGY)                             
         DC    AL1(0),AL1(0)                                                    
DPTKY    DC    AL1(POSSRV),AL1(L'SRSRV)                                         
         DC    AL1(POSMSEQ),AL1(L'SRMSEQ)                                       
         DC    AL1(POSREN),AL1(L'SRREN)                                         
         DC    AL1(POSRST),AL1(L'SRRST)                                         
         DC    AL1(POSMKT),AL1(L'SRMKT)                                         
         DC    AL1(POSSSEQ),AL1(L'SRSSEQ)                                       
         DC    AL1(POSTGT),AL1(L'SRTGT)                                         
         DC    AL1(POSDEMO),AL1(L'SRDEMO)                                       
         DC    AL1(POSDPT),AL1(L'SRDPT)                                         
         DC    AL1(POSSLN),AL1(L'SRSLN)                                         
         DC    AL1(POSAFL),AL1(L'SRAFL)                                         
         DC    AL1(POSPRG),AL1(L'SRPRG)                                         
         DC    AL1(POSAGY),AL1(L'SRAGY)                                         
         DC    AL1(POSRPT),AL1(L'SRRPT)                                         
         DC    AL1(0),AL1(0)                                                    
*                                                                               
DPTKYT   DC    AL1(POSSRV),AL1(L'SRSRV)                                         
         DC    AL1(POSMSEQ),AL1(L'SRMSEQ)                                       
         DC    AL1(POSREN),AL1(L'SRREN)                                         
         DC    AL1(POSRST),AL1(L'SRRST)                                         
         DC    AL1(POSSSEQ),AL1(L'SRSSEQ)                                       
         DC    AL1(POSTGT),AL1(L'SRTGT)                                         
         DC    AL1(POSDEMO),AL1(L'SRDEMO)                                       
         DC    AL1(POSDPT),AL1(L'SRDPT)                                         
         DC    AL1(POSSLN),AL1(L'SRSLN)                                         
         DC    AL1(POSAFL),AL1(L'SRAFL)                                         
         DC    AL1(POSPRG),AL1(L'SRPRG)                                         
         DC    AL1(POSAGY),AL1(L'SRAGY)                                         
         DC    AL1(POSRPT),AL1(L'SRRPT)                                         
         DC    AL1(POSMKT),AL1(L'SRMKT)                                         
         DC    AL1(0),AL1(0)                                                    
*                                                                               
*  ******DAYPART WITHIN MARKET WITHIN DEMO*****                                 
QTRKY    DC    AL1(POSSRV),AL1(L'SRSRV)                                         
         DC    AL1(POSTGT),AL1(L'SRTGT)                                         
         DC    AL1(POSDEMO),AL1(L'SRDEMO)                                       
         DC    AL1(POSMSEQ),AL1(L'SRMSEQ)                                       
         DC    AL1(POSREN),AL1(L'SRREN)                                         
         DC    AL1(POSRST),AL1(L'SRRST)                                         
         DC    AL1(POSMKT),AL1(L'SRMKT)                                         
         DC    AL1(POSRPT),AL1(L'SRRPT)                                         
         DC    AL1(POSPRG),AL1(L'SRPRG)                                         
         DC    AL1(POSAFL),AL1(L'SRAFL)                                         
         DC    AL1(POSDPT),AL1(L'SRDPT)                                         
         DC    AL1(POSSLN),AL1(L'SRSLN)                                         
         DC    AL1(POSSDT),AL1(L'SRDTSEQ)                                       
         DC    AL1(0),AL1(0)                                                    
*                                                                               
QTRKYT   DC    AL1(POSSRV),AL1(L'SRSRV)                                         
         DC    AL1(POSTGT),AL1(L'SRTGT)                                         
         DC    AL1(POSDEMO),AL1(L'SRDEMO)                                       
         DC    AL1(POSMSEQ),AL1(L'SRMSEQ)                                       
         DC    AL1(POSREN),AL1(L'SRREN)                                         
         DC    AL1(POSRST),AL1(L'SRRST)                                         
         DC    AL1(POSRPT),AL1(L'SRRPT)                                         
         DC    AL1(POSPRG),AL1(L'SRPRG)                                         
         DC    AL1(POSAFL),AL1(L'SRAFL)                                         
         DC    AL1(POSDPT),AL1(L'SRDPT)                                         
         DC    AL1(POSSLN),AL1(L'SRSLN)                                         
         DC    AL1(POSSDT),AL1(L'SRDTSEQ)                                       
         DC    AL1(POSMKT),AL1(L'SRMKT)                                         
         DC    AL1(0),AL1(0)                                                    
         EJECT                                                                  
* TABLE TO CONTROL REQUIRED SUMMARY GROUPS                                      
SUMCTRL  DC    AL2(001),AL2(010)                                                
         DC    AL2(011),AL2(020)                                                
         DC    AL2(021),AL2(030)                                                
         DC    AL2(031),AL2(040)                                                
         DC    AL2(041),AL2(050)                                                
         DC    AL2(051),AL2(100)                                                
         DC    AL2(101),AL2(250)                                                
         DC    X'FF'                                                            
*                                                                               
* TABLE OF DATA TYPE NAMES                                                      
DTYPETAB DS    0C                                                               
         DC    C'1',CL6'CPP',X'00',X'00',X'02'                                  
         DC    C'2',CL6'CPM',X'00',X'00',X'02'                                  
         DC    C'3',CL6'$(000)',X'00',X'03',X'00'                               
         DC    C'4',CL6'SPOTS',X'00',X'00',X'00'                                
         DC    C'5',CL6'GRPS',X'81',X'00',X'00'                                 
         DC    C'6',CL6'IMPS',X'00',X'00',X'00'                                 
         DC    C'7',CL6'CPS',X'00',X'00',X'00'                                  
         DC    C'8',CL6'PPS',X'00',X'00',X'01'                                  
         DC    C'9',CL6'IPS',X'00',X'00',X'00'                                  
         DC    X'FF'                                                            
LNDTYPTB EQU   10                                                               
*                                                                               
* TABLE OF DAYPART NAMES                                                        
DPTNAM2  DC    C'A',CL24'EARLY MORNING'                                         
         DC    C'C',CL24'DAY'                                                   
         DC    C'E',CL24'WEEKEND MORNING'                                       
         DC    C'G',CL24'WEEKEND AFTERNOON'                                     
         DC    C'J',CL24'EARLY'                                                 
         DC    C'H',CL24'FRINGE'                                                
         DC    C'K',CL24'SUNDAY PRIME'                                          
         DC    C'N',CL24'PRIME'                                                 
         DC    C'L',CL24'PRIME ACCESS'                                          
         DC    C'P',CL24'LATE'                                                  
         DC    C'R',CL24'LATE-LATE'                                             
         DC    C'1',CL24'PRIME AND SUNDAY PRIME'                                
         DC    C'2',CL24'LATE AND LATE-LATE'                                    
         DC    C'7',CL24'NON PRIME'                                             
         DC    C'8',CL24'TOTAL'                                                 
         DC    C'9',CL24'VARIOUS'                                               
         DC    X'FF'                                                            
*                                                                               
* TABLE OF MEDIUM DAYPART NAMES                                                 
DPTNAM3  DC    C'A',CL14'EARLY AM'                                              
         DC    C'C',CL14'DAY'                                                   
         DC    C'E',CL14'WEEKEND AM'                                            
         DC    C'G',CL14'WKND AFTERNOON'                                        
         DC    C'J',CL14'EARLY'                                                 
         DC    C'H',CL14'FRINGE'                                                
         DC    C'K',CL14'SUNDAY PRIME'                                          
         DC    C'N',CL14'PRIME'                                                 
         DC    C'L',CL14'PRIME ACCESS'                                          
         DC    C'P',CL14'LATE'                                                  
         DC    C'R',CL14'LATE-LATE'                                             
         DC    C'1',CL14'PRI+SUNDAY PRI'                                        
         DC    C'2',CL14'LATE+ LTE/LTE'                                         
         DC    C'7',CL14'NON PRIME'                                             
         DC    C'8',CL14'TOTAL'                                                 
         DC    C'9',CL14'VARIOUS'                                               
         DC    X'FF'                                                            
*                                                                               
EAM      EQU   C'A'                                                             
DAY      EQU   C'C'                                                             
WEM      EQU   C'E'                                                             
WEN      EQU   C'F'                                                             
WEA      EQU   C'G'                                                             
ELY      EQU   C'J'                                                             
FRG      EQU   C'H'                BATES SPECIAL                                
SUP      EQU   C'K'                                                             
PRI      EQU   C'N'                                                             
ACC      EQU   C'L'                                                             
LTE      EQU   C'P'                                                             
LLT      EQU   C'R'                                                             
*                                                                               
BANKTGT  DC    AL1(16)             NUMBER OF DEMOS                              
         DC    AL1(V211,HOMES,V1217,V1234)                                      
         DC    AL1(M1834,W1834,V1834)                                           
         DC    AL1(M1849,W1849,V1849)                                           
         DC    AL1(M2554,W2554,V2554)                                           
         DC    AL1(M3564,W3564,V3564)                                           
*                                                                               
BANKTGTC DC    AL1(14)             NUMBER OF DEMOS                              
         DC    AL1(V211,V1217,MO18,WO18,VO18)                                   
         DC    AL1(M1849,W1849,V1849,M1834,W1834,V1834)                         
         DC    AL1(M2554,W2554,V2554)                                           
*                                                                               
* TARGET HEADING CAPTIONS                                                       
TGTTAB   DC    CL13' '                                                          
         DC    CL13'TARGETS'                                                    
         DC    CL13'ALL TARGETS'                                                
         DC    CL13'GROUP TARGETS'                                              
         EJECT                                                                  
RPTCAPS  DS    0CL46                                                            
REGPRG   EQU   1                                                                
         DC    CL36'(REGULAR PROGRAMS ONLY)'                                    
         DC    CL10'REG. PROGS'                                                 
NETREG   EQU   2                                                                
         DC    CL36'(REGULAR NETWORK PROGRAMS ONLY)'                            
         DC    CL10'NET. PROGS'                                                 
INDREG   EQU   3                                                                
         DC    CL36'(REGULAR INDEPENDENT PROGRAMS ONLY)'                        
         DC    CL10'IND. PROGS'                                                 
SPOPRG   EQU   4                                                                
         DC    CL36'(SPORTS PROGRAMS ONLY)'                                     
         DC    CL10'SPO. PROGS'                                                 
KIDPRG   EQU   5                                                                
         DC    CL36'(KIDS PROGRAMS ONLY)'                                       
         DC    CL10'KIDS      '                                                 
NWSPRG   EQU   6                                                                
         DC    CL36'(NEWS PROGRAMS ONLY)'                                       
         DC    CL10'NEWS      '                                                 
SPOT     EQU   7                                                                
         DC    CL36'(SPOT TV ONLY)'                                             
         DC    CL10'SPOT      '                                                 
NETWORK  EQU   8                                                                
         DC    CL36'(NETWORK TV ONLY)'                                          
         DC    CL10'NETWORK   '                                                 
COMBINED EQU   9                                                                
         DC    CL36'(SPOT AND NETWORK TV)'                                      
         DC    CL10'COMBINED  '                                                 
*                                                                               
RPTCAPT  DC    AL1(00)             LINKS TO REPORT CAPTION TABLE                
         DC    AL1(REGPRG)         1                                            
         DC    AL1(REGPRG)         2                                            
         DC    AL1(REGPRG)         3                                            
         DC    AL1(REGPRG)         4                                            
         DC    AL1(REGPRG)         5                                            
         DC    AL1(REGPRG)         6                                            
         DC    AL1(NETREG)         7                                            
         DC    AL1(INDREG)         8                                            
         DC    AL1(REGPRG)         9                                            
         DC    AL1(SPOPRG)         10                                           
         DC    AL1(KIDPRG)         11                                           
         DC    AL1(NWSPRG)         12                                           
         DC    AL1(SPOT)           13                                           
         DC    AL1(NETWORK)        14                                           
         DC    AL1(COMBINED)       15                                           
DEMO     DC    X'00',C'U',X'01',X'FF'                                           
* TABLE OF WHICH SLOTS TO PUT QUARTER DATA IN                                   
TRNSLT   DC    AL1(01)             Q1                                           
         DC    AL1(04)             Q2                                           
         DC    AL1(07)             Q3                                           
         DC    AL1(10)             Q4                                           
         DC    AL1(02)             Q1                                           
         DC    AL1(05)             Q2                                           
         DC    AL1(08)             Q3                                           
         DC    AL1(11)             Q4                                           
*                                                                               
* INDEX SLOTS FOR TREND REPORT                                                  
TRNIDXT  DC    AL1(1,2,3)          1. BASE SLOT                                 
         DC    AL1(4,5,6)          2. PROJECT SLOT                              
         DC    AL1(7,8,9)          3. OUTPUT SLOT                               
         DC    AL1(10,11,12)                                                    
         DC    AL1(01,04,13)                                                    
         DC    AL1(04,07,14)                                                    
         DC    AL1(07,10,15)                                                    
         DC    AL1(10,02,16)                                                    
         DC    AL1(02,05,17)                                                    
         DC    AL1(05,08,18)                                                    
         DC    AL1(08,11,19)                                                    
         DC    AL1(0,0,0)                                                       
         EJECT                                                                  
CP32RA   DS    F                                                                
CP32RB   DS    F                                                                
CP32RC   DS    F                                                                
CP32R2   DS    F                                                                
CP32R6   DS    F                                                                
*                                                                               
ASRTKEY  DC    A(0)                ADDRESS OF CURRENT SORT KEY DEF.             
ARPTKEY  DC    A(0)                ADDRESS OF CURRENT REPORT KEY DEF            
ASRTKEYT DC    A(0)                ADDR OF CURRENT TOT SORT KEY DEF.            
ARPTKEYT DC    A(0)                ADDR OF CURRENT TOT REPORT KEY DEF           
ASRTREC  DC    A(0)                ADDRESS OF CURRENT SORT RECORD               
ADTYPE   DC    A(0)                ADDRESS OF DATA TYPE TABLE                   
ACURDTYP DC    A(0)                POINTER TO CURRENT DATA TYPE                 
*                                                                               
NOACUMS  DC    A(32)                                                            
LHSTART  DS    F                                                                
LHEND    DS    F                                                                
RHSTART  DS    F                                                                
RHEND    DS    F                                                                
RECCNT   DS    F                                                                
RPTCID   DS    C                   CONTROL TABLE ID ( SEE PUTSORT )             
FLDCTRL  DC    C'FFFFFFFP'                                                      
         DC    X'00'                                                            
QYRDATE  DS    CL16                                                             
SVUTL    DS    CL8                                                              
TAPEFRST DC    X'01'               FIRST TIME FOR TAPE                          
PUTTSW   DC    X'00'               FIRST TIME FOR TAPE LOOP                     
REQSENT  DC    C'N'                                                             
TSWITCH  DS    C                                                                
SVQOPT3  DS    C                                                                
SVRDSEQ  DS    C                                                                
CURRRPT  DS    C                                                                
VSOURCE  DS    C                                                                
BANKNUM  DS    C                                                                
DATATYPE DS    C                   CURRENT DATATYPE                             
PLNUM    DS    C                                                                
ONESPTSW DS    C                                                                
FORMATSW DS    C                                                                
DATESW   DS    C                                                                
CURRPER  DS    C                   CURRENT PERIOD COUNTER                       
DPTORDER DS    C                                                                
TEMPDEMO DS    C                                                                
TEMPTGT  DS    C                                                                
ARBCAP   DS    CL3                                                              
SVPERCNT DS    F                   COUNT DOWN PERIOD COUNTER                    
SVPERPTR DS    F                   POINTER TO CPP DATA                          
MTLAST   DS    F                                                                
         EJECT                                                                  
BLDKEY   DS    CL20                                                             
SPOTS    DS    F                                                                
DOLLARS  DS    F                                                                
DOLLARE  DS    F                                                                
POINTS   DS    F                                                                
         DS    0D                                                               
POINTSW  DS    CL8                                                              
IMPS     DS    F                                                                
WEIGHT   DS    F                                                                
CNCPP    DS    F                   STD CPP                                      
CNCPM    DS    F                                                                
CNCPS    DS    F                                                                
CNPPS    DS    F                                                                
CNIPS    DS    F                                                                
EDCPP    DS    CL5                                                              
EDCPM    DS    CL5                                                              
EDCPS    DS    CL5                                                              
EDPPS    DS    CL5                                                              
EDIPS    DS    CL5                                                              
EDCOST   DS    CL5                                                              
EDSPOTS  DS    CL5                                                              
EDPNTS   DS    CL5                                                              
EDIMPS   DS    CL5                                                              
CNMNAM   DS    CL28                MARKET NAME                                  
         ORG   CNMNAM                                                           
         DS    CL4                                                              
CNGCAP   DS    CL7                 GROUP CAPTION                                
         DS    CL1                                                              
CNGST    DS    CL3                 GROUP START                                  
CNGDSH   DS    CL1                                                              
CNGEN    DS    CL3                 GROUP END                                    
         ORG   CNMNAM+28                                                        
CNDTYP   DS    CL6                 DATA TYPE NAME                               
CNDPT    DS    CL15                DAYPART NAME                                 
CNRPT    DS    CL10                REPORT NAME                                  
CNRPTL   DS    CL36                REPORT NAME                                  
CNDNAM   DS    CL7                 DEMO NAME                                    
CNTGT    DS    CL13                                                             
         EJECT                                                                  
         DS    0F                                                               
SRREC    DS    0CL54               SORT RECORD                                  
SRKEY    DS    0CL20                                                            
SRSRV    DS    CL1                 RATING SERVICE                               
SRRPT    DS    CL1                 REPORT NUMBER                                
SRAFL    DS    CL1                 AFFILIATE CODE                               
SRPRG    DS    C                   PROGRAM TYPE                                 
SRTGT    DS    CL1                 TARGET DEMO                                  
SRDEMO   DS    CL1                 DEMO                                         
SRDPT    DS    CL1                 DAYPART                                      
SRSLN    DS    CL1                 SPOT LENGTH                                  
SRMSEQ   DS    CL2                 MARKET SEQUENCE                              
SRREN    DS    CL2                 END RANK   (0 IF SINGLE MARKET)              
SRRST    DS    CL2                 START RANK (COMPLIMENT IF GROUP)             
SRMKT    DS    CL2                 MARKET NUMBER                                
SRAGY    DS    CL2                 AGENCY CODE                                  
SRSSEQ   DS    C                   SORT SEQUENCE                                
SRDTSEQ  DS    C                   DATE SEQUENCE                                
SRDATA   DS    0CL34                                                            
SRSPOT   DS    CL4                 SPOTS                                        
SRDOL    DS    CL4                 DOLLARS                                      
SRPNT    DS    CL4                 POINTS                                       
SRIMP    DS    CL4                 IMPRESSIONS                                  
SRPNTW   DS    CL8                                                              
SRWGHT   DS    CL4                 MARKET WEIGHT                                
SREQUIV  DS    CL4                 MARKET EQUIVALENCE                           
SRMKT2   DS    CL2                                                              
*                                                                               
         DS    0F                                                               
HOLDREC  DS    0CL54                                                            
HLDKEY   DS    0CL20                                                            
HLDSRV   DS    CL1                 RATING SERVICE                               
HLDRPT   DS    CL1                 REPORT NUMBER                                
HLDAFL   DS    CL1                 AFFILIATE CODE                               
HLDPRG   DS    C                                                                
HLDTGT   DS    CL1                 TARGET DEMO                                  
HLDDEMO  DS    CL1                                                              
HLDDPT   DS    CL1                                                              
HLDSLN   DS    CL1                                                              
HLDMSEQ  DS    CL2                                                              
HLDREN   DS    CL2                                                              
HLDRST   DS    CL2                                                              
HLDMKT   DS    CL2                                                              
HLDAGY   DS    CL2                                                              
         DS    CL3                                                              
HLDDATA  DS    0CL34                                                            
HLDSPOT  DS    CL4                                                              
HLDDOL   DS    CL4                                                              
HLDPNT   DS    CL4                                                              
HLDIMP   DS    CL4                                                              
HLDPNTW  DS    CL8                                                              
HLDWGHT  DS    CL4                                                              
HLDEQUIV DS    CL4                                                              
HLDMKT2  DS    CL2                                                              
*                                                                               
         DS    0F                                                               
HLD2REC  DS    0CL54                                                            
HLD2KEY  DS    0CL20                                                            
         DS    C                                                                
HLD2RTYP DS    C                                                                
         DS    CL18                                                             
HLD2DATA DS    0CL34                                                            
         DS    CL20                                                             
HLD2PNTW DS    CL8                                                              
HLD2WGHT DS    CL4                                                              
         DS    CL4                                                              
HLD2MKT2 DS    CL2                                                              
         EJECT                                                                  
CPDGRP   DS    CL5                 DEMO GROUPS FOR CURRENT DEMO                 
CPDPT    DS    CL1                 DAYPART FOR CURRENT DEMO                     
SVINDCPP DS    F                                                                
SVINDCP1 DS    F                                                                
SVAGYCPP DS    F                                                                
SVINDEX  DS    F                                                                
ADBLOCK  DC    A(DEMBLOCK)                                                      
ADEMREC  DC    A(DEMREC)                                                        
DIDUNIV  DS    F                                                                
RELO     DC    A(0)                                                             
MTAB     DC    A(MTABC)                                                         
MTCNT    DC    A(0)                                                             
MTMAX    DC    A(MTABCLEN/MTABLN)                                               
UTAB     DC    A(UTABC)                                                         
UTCNT    DC    A(0)                                                             
UTMAX    DC    A(UTABCLEN/UTLN)                                                 
VSORTER  DS    F                                                                
SAVER8   DS    F                                                                
SAVER9   DS    F                                                                
SAVERE   DS    F                                                                
SAVERF   DS    F                                                                
MSEQ     DC    H'0'                                                             
HIGROUP  DS    H                                                                
FIRST    DC    X'01'                                                            
EOFSW    DC    X'00'                                                            
OK       DC    X'00'                                                            
LIMITSW  DC    X'00'                                                            
REQTGT   DS    C                                                                
ACTSW    DS    C                                                                
MGRPSW   DS    CL1                 MARKET GROUP SWITCH                          
RGROUPSW DS    C                                                                
SAVSKY   DS    CL20                                                             
PREVCOL  DS    C                                                                
MTRECORD DS    (MTKLN+36+36*NUMAGY)C                                            
*                                                                               
SORT     DC    C'SORT FIELDS=(1,20,BI,A),WORK=1 '                               
RECCARD  DC    C'RECORD TYPE=F,LENGTH=54 '                                      
         EJECT                                                                  
* TABLE TO CONTROL VALID AGENCIES AND RATING SERVICES                           
AGYRS    DC    C'BB',C' ',AL1(01)                                               
         DC    C'BD',C' ',AL1(02)  BBDO OLD                                     
         DC    C'BJ',C' ',AL1(03)                                               
         DC    C'BN',C' ',AL1(04)  BBDO IN JAN/91                               
*        DC    C'BS',C' ',AL1(05)                                               
         DC    C'FV',C' ',AL1(05)  FOGARTY 5/22/01                              
         DC    C'CE',C' ',AL1(06)  WAS CA MAY7/90                               
         DC    C'CZ',C' ',AL1(07)                                               
         DC    C'DE',C' ',AL1(08)                                               
         DC    C'DF',C' ',AL1(09)                                               
         DC    C'FM',C' ',AL1(10)                                               
         DC    C'HR',C' ',AL1(11)  HAL RAINEY 9/11/87                           
         DC    C'JW',C' ',AL1(12)                                               
         DC    C'KA',C' ',AL1(13)  IN JAN/91                                    
         DC    C'LC',C' ',AL1(14)                                               
         DC    C'LM',C' ',AL1(15)  LOWE/MARSCHALK 4/12/90                       
         DC    C'NE',C' ',AL1(16)                                               
         DC    C'NW',C' ',AL1(17)                                               
         DC    C'OM',C' ',AL1(18)                                               
         DC    C'QJ',C' ',AL1(19)                                               
         DC    C'RP',C'N',AL1(20)  RUBIN POSTAER (NSI ONLY 5/20/87)             
         DC    C'TR',C' ',AL1(21)                                               
         DC    C'UV',C' ',AL1(22)                                               
         DC    C'UB',C' ',AL1(22)                                               
         DC    C'YN',C' ',AL1(23)  Y&R 9/11/87                                  
         DC    C'99',C' ',AL1(24)                                               
         DC    X'FF'                                                            
NUMAGY   EQU   34                                                               
*                                                                               
*        LIST OF CANADIAN CPPRS AGENCIES                                        
AGYCAN   DC    C'BA',C' ',AL1(01)        BATO                                   
         DC    C'BE',C' ',AL1(02)        BETO                                   
         DC    C'CY',C' ',AL1(03)        CYTO                                   
         DC    C'FT',C'N',AL1(04)        FCTO                                   
         DC    C'GR',C'N',AL1(05)        GRTO                                   
         DC    C'GT',C' ',AL1(06)        GTTO                                   
         DC    C'ME',C'A',AL1(07)        METO                                   
         DC    C'MW',C'A',AL1(08)        MWTO                                   
         DC    C'NT',C'A',AL1(09)        NHTO                                   
         DC    C'PV',C' ',AL1(10)        PVTO                                   
         DC    C'RC',C'A',AL1(11)        RRTO                                   
         DC    C'SA',C' ',AL1(12)        SATO                                   
         DC    C'SM',C' ',AL1(13)        SCTO,LATO                              
         DC    C'VA',C'A',AL1(14)        VATO                                   
         DC    C'VR',C'A',AL1(15)        VRTO                                   
         DC    C'YR',C'A',AL1(16)        YRTO                                   
         DC    C'MH',C'A',AL1(17)        MHTO - BACKER                          
         DC    X'FF'                                                            
         EJECT                                                                  
         EJECT                                                                  
RPTCTRL  DS    0C                                                               
*                                                                               
* DEMO LIST FOR SPORTS PROGRAMS                                                 
SPORTST  DC    AL2(SPORTEN-SPORTST+1),AL1(10)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'S'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(M1849,M2554,M1834,MO18,MO35,HOMES)                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'04'                                  
         DC    X'FF'                                                            
SPORTEN  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR SPORTS PROGRAMS                                                 
SPOTHST  DC    AL2(SPOTHEN-SPOTHST+1),AL1(10)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'S'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(M1849,M2554,M1834,MO18,MO35,HOMES)                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),C'9'                                     
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'04'                                  
         DC    X'FF'                                                            
SPOTHEN  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR KIDS PROGRAMS                                                   
KIDST    DC    AL2(KIDEN-KIDST+1),AL1(11)                                       
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'K'                                     
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(V211,V611,HOMES)                                             
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'03'                                  
         DC    X'FF'                                                            
KIDEN    DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NEWS PROGRAMS                                                   
NEWSST   DC    AL2(NEWSEN-NEWSST+1),AL1(12)                                     
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)   ADDED 97/01/21                
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(DGRPNO)                                                      
         DC    AL1(V25,V611),AL1(V1217,VO12),AL1(V217,VO6)                      
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'02'                                  
         DC    X'FF'                                                            
NEWSEN   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NEWS PROGRAMS                                                   
NEWLST   DC    AL2(NEWLEN-NEWLST+1),AL1(12)                                     
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(DGRPNO)                                                      
         DC    AL1(V25,V611),AL1(V1217,VO12),AL1(V217,VO6)                      
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'02'                                  
         DC    X'FF'                                                            
NEWLEN   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR EAM DAYPART                                                     
EAMST    DC    AL2(EAMEN-EAMST+1),AL1(1)                                        
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENAFL),C'O'                                     
         DC    X'FF'                                                            
EAMEN    DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR DAY DAYPART                                                     
DAYST    DC    AL2(DAYEN-DAYST+1),AL1(2)                                        
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(DGRPNO)                                                      
         DC    AL1(M1824,MO65),AL1(VO2,VO65),AL1(V217,VO6)                      
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENAFL),C'O'                                     
         DC    X'FF'                                                            
DAYEN    DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR WEM DAYPART                                                     
WEMST    DC    AL2(WEMEN-WEMST+1),AL1(3)                                        
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(V211,V25,V611)                                               
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
WEMEN    DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR WEA DAYPART                                                     
WEAST    DC    AL2(WEAEN-WEAST+1),AL1(4)                                        
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
WEAEN    DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR ELY DAYPART                                                     
ELYST    DC    AL2(ELYEN-ELYST+1),AL1(5)                                        
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    X'FF'                                                            
ELYEN    DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR PAC DAYPART                                                     
PACST    DC    AL2(PACEN-PACST+1),AL1(6)                                        
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
*        DC    AL1(POSPRG),AL1(LENPRG),C'N'        OUT 97/01/21                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(DEMNO)                                                       
         DC    AL1(V211,V25,V611)                                               
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
PACEN    DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NETWORK PRIME & SUNDAY PRIME COMB. DAYPART                      
NPRIST   DC    AL2(NPRIEN-NPRIST+1),AL1(7)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'1'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
NPRIEN   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR IND PRIME & SUNDAY PRIME COMB. DAYPART                          
IPRIST   DC    AL2(IPRIEN-IPRIST+1),AL1(8)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'I'                                     
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'1'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
IPRIEN   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR LTE AND LLT COMB. DAYPART                                       
LTEST   DC    AL2(LTEEN-LTEST+1),AL1(9)                                         
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(DEMNO)                                                       
         DC    AL1(V211,V25,V611)                                               
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'2'                                     
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    X'FF'                                                            
LTEEN    DC    X'FF'                                                            
         EJECT                                                                  
***************HOMES DEMO ACROSS TARGETS ************                           
* DEMO LIST FOR SPORTS PROGRAMS                                                 
SPORTSTH DC    AL2(SPORTENH-SPORTSTH+1),AL1(10)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'S'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1849,M2554,M1834,MO18,MO35,HOMES)                           
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'04'                                  
         DC    X'FF'                                                            
SPORTENH DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR SPORTS PROGRAMS ( OTHER DAYPARTS )                              
SPOTHSTH DC    AL2(SPOTHENH-SPOTHSTH+1),AL1(10)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'S'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1849,M2554,M1834,MO18,MO35,HOMES)                           
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    AL1(POSDPT),AL1(LENDPT),C'9'                                     
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'04'                                  
         DC    X'FF'                                                            
SPOTHENH DC    X'FF'                                                            
*                                                                               
* DEMO LIST FOR KIDS PROGRAMS                                                   
KIDSTH   DC    AL2(KIDENH-KIDSTH+1),AL1(11)                                     
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'K'                                     
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(V211,V611,HOMES)                                             
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'03'                                  
         DC    X'FF'                                                            
KIDENH   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NEWS PROGRAMS                                                   
NEWSSTH  DC    AL2(NEWSENH-NEWSSTH+1),AL1(12)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)    97/01/21                     
         DC    X'FF'                                                            
         DC    AL1(TGRPNO)                                                      
         DC    AL1(V25,V611),AL1(V1217,VO12),AL1(V217,VO6)                      
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'02'                                  
         DC    X'FF'                                                            
NEWSENH  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NEWS PROGRAMS (LATE-LATE = LATE)                                
NEWLSTH  DC    AL2(NEWLENH-NEWLSTH+1),AL1(12)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TGRPNO)                                                      
         DC    AL1(V25,V611),AL1(V1217,VO12),AL1(V217,VO6)                      
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'02'                                  
         DC    X'FF'                                                            
NEWLENH  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR EAM DAYPART                                                     
EAMSTH   DC    AL2(EAMENH-EAMSTH+1),AL1(1)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),X'00'                                    
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
EAMENH   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR DAY DAYPART                                                     
DAYSTH   DC    AL2(DAYENH-DAYSTH+1),AL1(2)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    X'FF'                                                            
         DC    AL1(TGRPNO)                                                      
         DC    AL1(M1824,MO65),AL1(VO2,VO65),AL1(V217,VO6)                      
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
DAYENH   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR WEM DAYPART                                                     
WEMSTH   DC    AL2(WEMENH-WEMSTH+1),AL1(3)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(V211,V25,V611)                                               
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
WEMENH   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR WEA DAYPART                                                     
WEASTH   DC    AL2(WEAENH-WEASTH+1),AL1(4)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),X'00'                                    
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
WEAENH   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR ELY DAYPART                                                     
ELYSTH   DC    AL2(ELYENH-ELYSTH+1),AL1(5)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),X'00'                                    
         DC    X'FF'                                                            
ELYENH   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR PAC DAYPART                                                     
PACSTH   DC    AL2(PACENH-PACSTH+1),AL1(6)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
*        DC    AL1(POSPRG),AL1(LENPRG),C'N'    OUT 97/01/21                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTNO)                                                       
         DC    AL1(V211,V25,V611)                                               
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
PACENH   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NETWORK PRIME & SUNDAY PRIME COMB. DAYPART                      
NPRISTH  DC    AL2(NPRIENH-NPRISTH+1),AL1(7)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'1'                                     
         DC    AL1(POSTGT),AL1(LENTGT),X'00'                                    
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
NPRIENH  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR IND PRIME & SUNDAY PRIME COMB. DAYPART                          
IPRISTH  DC    AL2(IPRIENH-IPRISTH+1),AL1(8)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'I'                                     
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'1'                                     
         DC    AL1(POSTGT),AL1(LENTGT),X'00'                                    
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
IPRIENH  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR LTE AND LLT COMB. DAYPART                                       
LTESTH  DC    AL2(LTEENH-LTESTH+1),AL1(9)                                       
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTNO)                                                       
         DC    AL1(V211,V25,V611)                                               
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'2'                                     
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    X'FF'                                                            
LTEENH   DC    X'FF'                                                            
         EJECT                                                                  
***************W1849 DEMO ACROSS TARGETS ************                           
*                                                                               
* DEMO LIST FOR NEWS PROGRAMS                                                   
NEWSSTW  DC    AL2(NEWSENW-NEWSSTW+1),AL1(12)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)   IN 97/01/21                   
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(W1834,W2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(W1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'02'                                  
         DC    X'FF'                                                            
NEWSENW  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NEWS PROGRAMS (LATE-LATE = LATE )                               
NEWLSTW  DC    AL2(NEWLENW-NEWLSTW+1),AL1(12)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(W1834,W2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(W1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'02'                                  
         DC    X'FF'                                                            
NEWLENW  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR EAM DAYPART                                                     
EAMSTW   DC    AL2(EAMENW-EAMSTW+1),AL1(1)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(W1834,W2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(W1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
EAMENW   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR DAY DAYPART                                                     
DAYSTW   DC    AL2(DAYENW-DAYSTW+1),AL1(2)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(W1834,W2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(W1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
DAYENW   DC    X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
* DEMO LIST FOR WEA DAYPART                                                     
WEASTW   DC    AL2(WEAENW-WEASTW+1),AL1(4)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(W1834,W2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(W1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
WEAENW   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR ELY DAYPART                                                     
ELYSTW   DC    AL2(ELYENW-ELYSTW+1),AL1(5)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(W1834,W2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(W1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    X'FF'                                                            
ELYENW   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR PAC DAYPART                                                     
PACSTW   DC    AL2(PACENW-PACSTW+1),AL1(6)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
*        DC    AL1(POSPRG),AL1(LENPRG),C'N'     OUT 97/01/21                    
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(W1834,W2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(W1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
PACENW   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NETWORK PRIME & SUNDAY PRIME COMB. DAYPART                      
NPRISTW  DC    AL2(NPRIENW-NPRISTW+1),AL1(7)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(W1834,W2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(W1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'1'                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
NPRIENW  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR IND PRIME & SUNDAY PRIME COMB. DAYPART                          
IPRISTW  DC    AL2(IPRIENW-IPRISTW+1),AL1(8)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'I'                                     
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(W1834,W2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(W1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'1'                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
IPRIENW  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR LTE AND LLT COMB. DAYPART                                       
LTESTW  DC    AL2(LTEENW-LTESTW+1),AL1(9)                                       
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(W1834,W2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(W1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'2'                                     
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    X'FF'                                                            
LTEENW   DC    X'FF'                                                            
         EJECT                                                                  
***************V1849 DEMO ACROSS TARGETS ************                           
*                                                                               
* DEMO LIST FOR NEWS PROGRAMS                                                   
NEWSSTV  DC    AL2(NEWSENV-NEWSSTV+1),AL1(12)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)      IN 97/01/21                
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(V1834,V2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(V1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'02'                                  
         DC    X'FF'                                                            
NEWSENV  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NEWS PROGRAMS (LATE-LATE = LATE)                                
NEWLSTV  DC    AL2(NEWLENV-NEWLSTV+1),AL1(12)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(V1834,V2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(V1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'02'                                  
         DC    X'FF'                                                            
NEWLENV  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR EAM DAYPART                                                     
EAMSTV   DC    AL2(EAMENV-EAMSTV+1),AL1(1)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(V1834,V2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(V1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
EAMENV   DC    X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
* DEMO LIST FOR WEA DAYPART                                                     
WEASTV   DC    AL2(WEAENV-WEASTV+1),AL1(4)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(V1834,V2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(V1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
WEAENV   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR ELY DAYPART                                                     
ELYSTV   DC    AL2(ELYENV-ELYSTV+1),AL1(5)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(V1834,V2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(V1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    X'FF'                                                            
ELYENV   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR PAC DAYPART                                                     
PACSTV   DC    AL2(PACENV-PACSTV+1),AL1(6)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
*        DC    AL1(POSPRG),AL1(LENPRG),C'N'        OUT 97/01/21                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(V1834,V2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(V1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
PACENV   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NETWORK PRIME & SUNDAY PRIME COMB. DAYPART                      
NPRISTV  DC    AL2(NPRIENV-NPRISTV+1),AL1(7)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(V1834,V2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(V1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'1'                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
NPRIENV  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR IND PRIME & SUNDAY PRIME COMB. DAYPART                          
IPRISTV  DC    AL2(IPRIENV-IPRISTV+1),AL1(8)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'I'                                     
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(V1834,V2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(V1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'1'                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
IPRIENV  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR LTE AND LLT COMB. DAYPART                                       
LTESTV  DC    AL2(LTEENV-LTESTV+1),AL1(9)                                       
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(V1834,V2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(V1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'2'                                     
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    X'FF'                                                            
LTEENV   DC    X'FF'                                                            
         EJECT                                                                  
***************M1849 DEMO ACROSS TARGETS ************                           
* DEMO LIST FOR SPORTS PROGRAMS                                                 
SPORTSTM DC    AL2(SPORTENM-SPORTSTM+1),AL1(10)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'S'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1834,M2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(M1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'04'                                  
         DC    X'FF'                                                            
SPORTENM DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR SPORTS PROGRAMS ( OTHER DAYPARTS )                              
SPOTHSTM DC    AL2(SPOTHENM-SPOTHSTM+1),AL1(10)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'S'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1834,M2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(M1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSDPT),AL1(LENDPT),C'9'                                     
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'04'                                  
         DC    X'FF'                                                            
SPOTHENM DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NEWS PROGRAMS                                                   
NEWSSTM  DC    AL2(NEWSENM-NEWSSTM+1),AL1(12)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)       IN 97/01/21               
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1834,M2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(M1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'02'                                  
         DC    X'FF'                                                            
NEWSENM  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NEWS PROGRAMS                                                   
NEWLSTM  DC    AL2(NEWLENM-NEWLSTM+1),AL1(12)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1834,M2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(M1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'02'                                  
         DC    X'FF'                                                            
NEWLENM  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR EAM DAYPART                                                     
EAMSTM   DC    AL2(EAMENM-EAMSTM+1),AL1(1)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1834,M2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(M1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
EAMENM   DC    X'FF'                                                            
*                                                                               
*                                                                               
*                                                                               
* DEMO LIST FOR WEA DAYPART                                                     
WEASTM   DC    AL2(WEAENM-WEASTM+1),AL1(4)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1834,M2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(M1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
WEAENM   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR ELY DAYPART                                                     
ELYSTM   DC    AL2(ELYENM-ELYSTM+1),AL1(5)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1834,M2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(M1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    X'FF'                                                            
ELYENM   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR PAC DAYPART                                                     
PACSTM   DC    AL2(PACENM-PACSTM+1),AL1(6)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
*        DC    AL1(POSPRG),AL1(LENPRG),C'N'        OUT 97/01/21                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1834,M2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(M1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
PACENM   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NETWORK PRIME & SUNDAY PRIME COMB. DAYPART                      
NPRISTM  DC    AL2(NPRIENM-NPRISTM+1),AL1(7)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1834,M2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(M1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'1'                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
NPRIENM  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR IND PRIME & SUNDAY PRIME COMB. DAYPART                          
IPRISTM  DC    AL2(IPRIENM-IPRISTM+1),AL1(8)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'I'                                     
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1834,M2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(M1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'1'                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
IPRIENM  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR LTE AND LLT COMB. DAYPART                                       
LTESTM  DC    AL2(LTEENM-LTESTM+1),AL1(9)                                       
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1834,M2554)                                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(M1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'2'                                     
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    X'FF'                                                            
LTEENM   DC    X'FF'                                                            
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
RPTCTRL8 DS    0C                  1988 US CPPRS REPORTS                        
*                                                                               
* DEMO LIST FOR SPORTS PROGRAMS                                                 
SPORTST8 DC    AL2(SPORTEN8-SPORTST8+1),AL1(10)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'S'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(M1849,M2554,M1834,MO18,M3564,MO35,HOMES)                     
         DC    AL1(V1849,V2554,V1834,VO18,V3564,VO35)                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'04'                                  
         DC    X'FF'                                                            
SPORTEN8 DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR SPORTS PROGRAMS                                                 
SPOTHST8 DC    AL2(SPOTHEN8-SPOTHST8+1),AL1(10)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'S'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(M1849,M2554,M1834,MO18,M3564,MO35,HOMES)                     
         DC    AL1(V1849,V2554,V1834,VO18,V3564,VO35)                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),C'9'                                     
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'04'                                  
         DC    X'FF'                                                            
SPOTHEN8 DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR KIDS PROGRAMS                                                   
KIDST8   DC    AL2(KIDEN8-KIDST8+1),AL1(11)                                     
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'K'                                     
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(V211,V611,HOMES)                                             
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'03'                                  
         DC    X'FF'                                                            
KIDEN8   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NEWS PROGRAMS                                                   
NEWSST8  DC    AL2(NEWSEN8-NEWSST8+1),AL1(12)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)    IN 97/01/21                  
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(DGRPNO)                                                      
         DC    AL1(V25,V611),AL1(V1217,VO12),AL1(V217,VO6)                      
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'02'                                  
         DC    X'FF'                                                            
NEWSEN8  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NEWS PROGRAMS                                                   
NEWLST8  DC    AL2(NEWLEN8-NEWLST8+1),AL1(12)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(DGRPNO)                                                      
         DC    AL1(V25,V611),AL1(V1217,VO12),AL1(V217,VO6)                      
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'02'                                  
         DC    X'FF'                                                            
NEWLEN8  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR EAM DAYPART                                                     
EAMST8   DC    AL2(EAMEN8-EAMST8+1),AL1(1)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENAFL),C'O'                                     
         DC    X'FF'                                                            
EAMEN8   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR DAY DAYPART                                                     
DAYST8   DC    AL2(DAYEN8-DAYST8+1),AL1(2)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(DGRPNO)                                                      
         DC    AL1(M1824,MO65)                                                  
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENAFL),C'O'                                     
         DC    X'FF'                                                            
DAYEN8   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR WEM DAYPART                                                     
WEMST8   DC    AL2(WEMEN8-WEMST8+1),AL1(3)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(V211,V25,V611)                                               
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
WEMEN8   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR WEA DAYPART                                                     
WEAST8   DC    AL2(WEAEN8-WEAST8+1),AL1(4)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
WEAEN8   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR ELY DAYPART                                                     
ELYST8   DC    AL2(ELYEN8-ELYST8+1),AL1(5)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    X'FF'                                                            
ELYEN8   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR PAC DAYPART                                                     
PACST8   DC    AL2(PACEN8-PACST8+1),AL1(6)                                      
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
*        DC    AL1(POSPRG),AL1(LENPRG),C'N'       OUT 97/01/01                  
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
PACEN8   DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NETWORK PRIME & SUNDAY PRIME COMB. DAYPART                      
NPRIST8  DC    AL2(NPRIEN8-NPRIST8+1),AL1(7)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'1'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
NPRIEN8  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR IND PRIME & SUNDAY PRIME COMB. DAYPART                          
IPRIST8  DC    AL2(IPRIEN8-IPRIST8+1),AL1(8)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'I'                                     
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'1'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
IPRIEN8  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR LTE AND LLT COMB. DAYPART                                       
LTEST8  DC    AL2(LTEEN8-LTEST8+1),AL1(9)                                       
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(DEMNO)                                                       
         DC    AL1(V211,V25,V611)                                               
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'2'                                     
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    X'FF'                                                            
LTEEN8   DC    X'FF'                                                            
         EJECT                                                                  
***************HOMES DEMO ACROSS TARGETS ************                           
* DEMO LIST FOR SPORTS PROGRAMS                                                 
SPOR8STH DC    AL2(SPOR8ENH-SPOR8STH+1),AL1(10)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'S'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1849,M2554,M1834,MO18,M3564,MO35,HOMES)                     
         DC    AL1(V1849,V2554,V1834,VO18,V3564,VO35)                           
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES,M2554,W2554,V2554)                                     
         DC    AL1(M1849,W1849,V1849)                                           
         DC    AL1(M3564,W3564,V3564)                                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'04'                                  
         DC    X'FF'                                                            
SPOR8ENH DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR SPORTS PROGRAMS ( OTHER DAYPARTS )                              
SPO8HSTH DC    AL2(SPO8HENH-SPO8HSTH+1),AL1(10)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'S'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1849,M2554,M1834,MO18,M3564,MO35,HOMES)                     
         DC    AL1(V1849,V2554,V1834,VO18,V3564,VO35)                           
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES,M2554,W2554,V2554)                                     
         DC    AL1(M1849,W1849,V1849)                                           
         DC    AL1(M3564,W3564,V3564)                                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    AL1(POSDPT),AL1(LENDPT),C'9'                                     
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'04'                                  
         DC    X'FF'                                                            
SPO8HENH DC    X'FF'                                                            
*                                                                               
* DEMO LIST FOR KIDS PROGRAMS                                                   
KIDSTH8  DC    AL2(KIDENH8-KIDSTH8+1),AL1(11)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'K'                                     
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(V211,V611,HOMES)                                             
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES,M2554,W2554,V2554)                                     
         DC    AL1(M1849,W1849,V1849)                                           
         DC    AL1(M3564,W3564,V3564)                                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'03'                                  
         DC    X'FF'                                                            
KIDENH8  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NEWS PROGRAMS                                                   
NEWSSTH8 DC    AL2(NEWSENH8-NEWSSTH8+1),AL1(12)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)        IN 97/01/21              
         DC    X'FF'                                                            
         DC    AL1(TGRPNO)                                                      
         DC    AL1(V25,V611),AL1(V1217,VO12),AL1(V217,VO6)                      
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES,M2554,W2554,V2554)                                     
         DC    AL1(M1849,W1849,V1849)                                           
         DC    AL1(M3564,W3564,V3564)                                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'02'                                  
         DC    X'FF'                                                            
NEWSENH8 DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NEWS PROGRAMS (LATE-LATE = LATE)                                
NEWLSTH8 DC    AL2(NEWLENH8-NEWLSTH8+1),AL1(12)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TGRPNO)                                                      
         DC    AL1(V25,V611),AL1(V1217,VO12),AL1(V217,VO6)                      
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES,M2554,W2554,V2554)                                     
         DC    AL1(M1849,W1849,V1849)                                           
         DC    AL1(M3564,W3564,V3564)                                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'02'                                  
         DC    X'FF'                                                            
NEWLENH8 DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR EAM DAYPART                                                     
EAMSTH8  DC    AL2(EAMENH8-EAMSTH8+1),AL1(1)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES,M2554,W2554,V2554)                                     
         DC    AL1(M1849,W1849,V1849)                                           
         DC    AL1(M3564,W3564,V3564)                                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),X'00'                                    
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
EAMENH8  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR DAY DAYPART                                                     
DAYSTH8  DC    AL2(DAYENH8-DAYSTH8+1),AL1(2)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    X'FF'                                                            
         DC    AL1(TGRPNO)                                                      
         DC    AL1(M1824,MO65)                                                  
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES,M2554,W2554,V2554)                                     
         DC    AL1(M1849,W1849,V1849)                                           
         DC    AL1(M3564,W3564,V3564)                                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
DAYENH8  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR WEM DAYPART                                                     
WEMSTH8  DC    AL2(WEMENH8-WEMSTH8+1),AL1(3)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(V211,V25,V611)                                               
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES,M2554,W2554,V2554)                                     
         DC    AL1(M1849,W1849,V1849)                                           
         DC    AL1(M3564,W3564,V3564)                                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
WEMENH8  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR WEA DAYPART                                                     
WEASTH8  DC    AL2(WEAENH8-WEASTH8+1),AL1(4)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES,M2554,W2554,V2554)                                     
         DC    AL1(M1849,W1849,V1849)                                           
         DC    AL1(M3564,W3564,V3564)                                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),X'00'                                    
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
WEAENH8  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR ELY DAYPART                                                     
ELYSTH8  DC    AL2(ELYENH8-ELYSTH8+1),AL1(5)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES,M2554,W2554,V2554)                                     
         DC    AL1(M1849,W1849,V1849)                                           
         DC    AL1(M3564,W3564,V3564)                                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),X'00'                                    
         DC    X'FF'                                                            
ELYENH8  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR PAC DAYPART                                                     
PACSTH8  DC    AL2(PACENH8-PACSTH8+1),AL1(6)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
*        DC    AL1(POSPRG),AL1(LENPRG),C'N'       OUT 97/01/21                  
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES,M2554,W2554,V2554)                                     
         DC    AL1(M1849,W1849,V1849)                                           
         DC    AL1(M3564,W3564,V3564)                                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
PACENH8  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NETWORK PRIME & SUNDAY PRIME COMB. DAYPART                      
NPRISTH8 DC    AL2(NPRIENH8-NPRISTH8+1),AL1(7)                                  
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES,M2554,W2554,V2554)                                     
         DC    AL1(M1849,W1849,V1849)                                           
         DC    AL1(M3564,W3564,V3564)                                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'1'                                     
         DC    AL1(POSTGT),AL1(LENTGT),X'00'                                    
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
NPRIENH8 DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR IND PRIME & SUNDAY PRIME COMB. DAYPART                          
IPRISTH8 DC    AL2(IPRIENH8-IPRISTH8+1),AL1(8)                                  
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'I'                                     
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES,M2554,W2554,V2554)                                     
         DC    AL1(M1849,W1849,V1849)                                           
         DC    AL1(M3564,W3564,V3564)                                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'1'                                     
         DC    AL1(POSTGT),AL1(LENTGT),X'00'                                    
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
IPRIENH8 DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR LTE AND LLT COMB. DAYPART                                       
LTESTH8 DC    AL2(LTEENH8-LTESTH8+1),AL1(9)                                     
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTNO)                                                       
         DC    AL1(V211,V25,V611)                                               
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(HOMES,M2554,W2554,V2554)                                     
         DC    AL1(M1849,W1849,V1849)                                           
         DC    AL1(M3564,W3564,V3564)                                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'2'                                     
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(0)                                   
         DC    X'FF'                                                            
LTEENH8  DC    X'FF'                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
*        *********** CANADIAN CPPRS REPORTS *************                       
RPTCTLC  DS    0C                                                               
DAYSTC   DC    AL2(DAYENC-DAYSTC+1),AL1(13)                                     
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(SELECT)                                                      
         DC    AL1(POSAFL),AL1(LENAFL),C'T'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'01'                                  
         DC    X'FF'                                                            
DAYENC   DC    X'FF'                                                            
*                                                                               
*                                                                               
DAY1STC  DC    AL2(DAY1ENC-DAY1STC+1),AL1(13)                                   
         DC    AL1(DEMYES)                                                      
         DC    AL1(VO18)                                                        
         DC    X'FF'                                                            
         DC    AL1(SELECT)                                                      
         DC    AL1(POSAFL),AL1(LENAFL),C'T'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'01'                                  
         DC    AL1(POSTGT),AL1(LENTGT),X'00'                                    
         DC    X'FF'                                                            
DAY1ENC  DC    X'FF'                                                            
*                                                                               
DAY2STC  DC    AL2(DAY2ENC-DAY2STC+1),AL1(13)                                   
         DC    AL1(DEMYES)                                                      
         DC    AL1(VO18)                                                        
         DC    X'FF'                                                            
         DC    AL1(SELECT)                                                      
         DC    AL1(POSAFL),AL1(LENAFL),C'T'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(MO18,WO18,VO18)                                              
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'03'                                  
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    X'FF'                                                            
DAY2ENC  DC    X'FF'                                                            
*                                                                               
DAY3STC  DC    AL2(DAY3ENC-DAY3STC+1),AL1(13)                                   
         DC    AL1(DEMYES)                                                      
         DC    AL1(V1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(SELECT)                                                      
         DC    AL1(POSAFL),AL1(LENAFL),C'T'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1849,W1849,V1849,M1834,W1834,V1834)                         
         DC    AL1(M2554,W2554,V2554)                                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'04'                                  
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    X'FF'                                                            
DAY3ENC  DC    X'FF'                                                            
*                                                                               
*        CANADIAN NETWORK MEDIA                                                 
DAYSTCN  DC    AL2(DAYENCN-DAYSTCN+1),AL1(14)                                   
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(SELECT)                                                      
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'05'                                  
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    X'FF'                                                            
DAYENCN  DC    X'FF'                                                            
*                                                                               
*                                                                               
DAY1STCN DC    AL2(DAY1ENCN-DAY1STCN+1),AL1(14)                                 
         DC    AL1(DEMYES)                                                      
         DC    AL1(VO18)                                                        
         DC    X'FF'                                                            
         DC    AL1(SELECT)                                                      
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'05'                                  
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),X'00'                                    
         DC    X'FF'                                                            
DAY1ENCN DC    X'FF'                                                            
*                                                                               
DAY2STCN DC    AL2(DAY2ENCN-DAY2STCN+1),AL1(14)                                 
         DC    AL1(DEMYES)                                                      
         DC    AL1(VO18)                                                        
         DC    X'FF'                                                            
         DC    AL1(SELECT)                                                      
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(MO18,WO18,VO18)                                              
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'07'                                  
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    X'FF'                                                            
DAY2ENCN DC    X'FF'                                                            
*                                                                               
DAY3STCN DC    AL2(DAY3ENCN-DAY3STCN+1),AL1(14)                                 
         DC    AL1(DEMYES)                                                      
         DC    AL1(V1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(SELECT)                                                      
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1849,W1849,V1849,M1834,W1834,V1834)                         
         DC    AL1(M2554,W2554,V2554)                                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'08'                                  
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    X'FF'                                                            
DAY3ENCN DC    X'FF'                                                            
*                                                                               
*        CANADIAN COMBINED MEDIA                                                
DAYSTCC  DC    AL2(DAYENCC-DAYSTCC+1),AL1(15)                                   
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'09'                                  
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    X'FF'                                                            
DAYENCC  DC    X'FF'                                                            
*                                                                               
*                                                                               
DAY1STCC DC    AL2(DAY1ENCC-DAY1STCC+1),AL1(15)                                 
         DC    AL1(DEMYES)                                                      
         DC    AL1(VO18)                                                        
         DC    X'FF'                                                            
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'09'                                  
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),X'00'                                    
         DC    X'FF'                                                            
DAY1ENCC DC    X'FF'                                                            
*                                                                               
DAY2STCC DC    AL2(DAY2ENCC-DAY2STCC+1),AL1(15)                                 
         DC    AL1(DEMYES)                                                      
         DC    AL1(VO18)                                                        
         DC    X'FF'                                                            
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(MO18,WO18,VO18)                                              
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'0B'                                  
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    X'FF'                                                            
DAY2ENCC DC    X'FF'                                                            
*                                                                               
DAY3STCC DC    AL2(DAY3ENCC-DAY3STCC+1),AL1(15)                                 
         DC    AL1(DEMYES)                                                      
         DC    AL1(V1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1849,W1849,V1849,M1834,W1834,V1834)                         
         DC    AL1(M2554,W2554,V2554)                                           
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'0C'                                  
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    X'FF'                                                            
DAY3ENCC DC    X'FF'                                                            
         EJECT                                                                  
*   NON PRIME DAYPART TABLES FOR CANADA                                         
DNPSTC   DC    AL2(DNPENC-DNPSTC+1),AL1(13)                                     
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'T'                                     
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'01'                                  
         DC    AL1(POSDPT),AL1(LENDPT),C'7'                                     
         DC    X'FF'                                                            
DNPENC   DC    X'FF'                                                            
*                                                                               
*                                                                               
DNP1STC  DC    AL2(DNP1ENC-DNP1STC+1),AL1(13)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'T'                                     
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(VO18)                                                        
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'01'                                  
         DC    AL1(POSTGT),AL1(LENTGT),X'00'                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'7'                                     
         DC    X'FF'                                                            
DNP1ENC  DC    X'FF'                                                            
*                                                                               
DNP2STC  DC    AL2(DNP2ENC-DNP2STC+1),AL1(13)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'T'                                     
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(MO18,WO18,VO18)                                              
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(VO18)                                                        
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'03'                                  
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSDPT),AL1(LENDPT),C'7'                                     
         DC    X'FF'                                                            
DNP2ENC  DC    X'FF'                                                            
*                                                                               
DNP3STC  DC    AL2(DNP3ENC-DNP3STC+1),AL1(13)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'T'                                     
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1849,W1849,V1849,M1834,W1834,V1834)                         
         DC    AL1(M2554,W2554,V2554)                                           
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(V1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'04'                                  
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSDPT),AL1(LENDPT),C'7'                                     
         DC    X'FF'                                                            
DNP3ENC  DC    X'FF'                                                            
*                                                                               
*        CANADIAN NETWORK MEDIA                                                 
DNPSTCN  DC    AL2(DNPENCN-DNPSTCN+1),AL1(14)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'05'                                  
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),C'7'                                     
         DC    X'FF'                                                            
DNPENCN  DC    X'FF'                                                            
*                                                                               
*                                                                               
DNP1STCN DC    AL2(DNP1ENCN-DNP1STCN+1),AL1(14)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(VO18)                                                        
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'05'                                  
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),X'00'                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'7'                                     
         DC    X'FF'                                                            
DNP1ENCN DC    X'FF'                                                            
*                                                                               
DNP2STCN DC    AL2(DNP2ENCN-DNP2STCN+1),AL1(14)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(MO18,WO18,VO18)                                              
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(VO18)                                                        
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'07'                                  
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSDPT),AL1(LENDPT),C'7'                                     
         DC    X'FF'                                                            
DNP2ENCN DC    X'FF'                                                            
*                                                                               
DNP3STCN DC    AL2(DNP3ENCN-DNP3STCN+1),AL1(14)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1849,W1849,V1849,M1834,W1834,V1834)                         
         DC    AL1(M2554,W2554,V2554)                                           
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(V1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'08'                                  
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSDPT),AL1(LENDPT),C'7'                                     
         DC    X'FF'                                                            
DNP3ENCN DC    X'FF'                                                            
*                                                                               
*        CANADIAN COMBINED MEDIA                                                
DNPSTCC  DC    AL2(DNPENCC-DNPSTCC+1),AL1(15)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(TDYES)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'09'                                  
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),C'7'                                     
         DC    X'FF'                                                            
DNPENCC  DC    X'FF'                                                            
*                                                                               
*                                                                               
DNP1STCC DC    AL2(DNP1ENCC-DNP1STCC+1),AL1(15)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(VO18)                                                        
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'09'                                  
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),X'00'                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'7'                                     
         DC    X'FF'                                                            
DNP1ENCC DC    X'FF'                                                            
*                                                                               
DNP2STCC DC    AL2(DNP2ENCC-DNP2STCC+1),AL1(15)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(MO18,WO18,VO18)                                              
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(VO18)                                                        
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'0B'                                  
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSDPT),AL1(LENDPT),C'7'                                     
         DC    X'FF'                                                            
DNP2ENCC DC    X'FF'                                                            
*                                                                               
DNP3STCC DC    AL2(DNP3ENCC-DNP3STCC+1),AL1(15)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(TGTYES)                                                      
         DC    AL1(M1849,W1849,V1849,M1834,W1834,V1834)                         
         DC    AL1(M2554,W2554,V2554)                                           
         DC    X'FF'                                                            
         DC    AL1(DEMYES)                                                      
         DC    AL1(V1849)                                                       
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'0C'                                  
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSTGT),AL1(LENTGT),AL1(240)                                 
         DC    AL1(POSDPT),AL1(LENDPT),C'7'                                     
         DC    X'FF'                                                            
DNP3ENCC DC    X'FF'                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
*        **************SFM CPP REPORTS***************                           
RPTCTLFM DS    0C                                                               
OVARSTF  DC    AL2(OVARENF-OVARSTF+1),AL1(1)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    X'FF'                                                            
OVARENF  DC    X'FF'                                                            
*                                                                               
OELYSTF  DC    AL2(OELYENF-OELYSTF+1),AL1(1)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    X'FF'                                                            
OELYENF  DC    X'FF'                                                            
*                                                                               
ONPRSTF  DC    AL2(ONPRENF-ONPRSTF+1),AL1(1)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    X'FF'                                                            
ONPRENF  DC    X'FF'                                                            
*                                                                               
OILTSTF  DC    AL2(OILTENF-OILTSTF+1),AL1(1)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSAFL),AL1(LENAFL),C'I'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    X'FF'                                                            
OILTENF  DC    X'FF'                                                            
*                                                                               
NVARSTF  DC    AL2(NVARENF-NVARSTF+1),AL1(12)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    X'FF'                                                            
NVARENF  DC    X'FF'                                                            
*                                                                               
NELYSTF  DC    AL2(NELYENF-NELYSTF+1),AL1(12)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    X'FF'                                                            
NELYENF  DC    X'FF'                                                            
*                                                                               
NNPRSTF  DC    AL2(NNPRENF-NNPRSTF+1),AL1(12)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    X'FF'                                                            
NNPRENF  DC    X'FF'                                                            
*                                                                               
NILTSTF  DC    AL2(NILTENF-NILTSTF+1),AL1(12)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSAFL),AL1(LENAFL),C'I'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    X'FF'                                                            
NILTENF  DC    X'FF'                                                            
*                                                                               
SVARSTF  DC    AL2(SVARENF-SVARSTF+1),AL1(10)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'S'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),C'9'                                     
         DC    X'FF'                                                            
SVARENF  DC    X'FF'                                                            
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*        **************TBC CPP REPORTS***************                           
RPTCTLTB DS    0C                                                               
*                                                                               
OEAMSTT  DC    AL2(OEAMENT-OEAMSTT+1),AL1(1)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENPRG),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    X'FF'                                                            
OEAMENT  DC    X'FF'                                                            
*                                                                               
ODAYSTT  DC    AL2(ODAYENT-ODAYSTT+1),AL1(1)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENPRG),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    X'FF'                                                            
ODAYENT  DC    X'FF'                                                            
*                                                                               
OELYSTT  DC    AL2(OELYENT-OELYSTT+1),AL1(1)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'F'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEN)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENPRG),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    X'FF'                                                            
OELYENT  DC    X'FF'                                                            
*                                                                               
ONPRSTT  DC    AL2(ONPRENT-ONPRSTT+1),AL1(7)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'F'                                     
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSPRG),AL1(LENPRG),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),C'1'                                     
         DC    X'FF'                                                            
ONPRENT  DC    X'FF'                                                            
*                                                                               
OLTESTT  DC    AL2(OLTEENT-OLTESTT+1),AL1(1)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENDPT),C'O'                                     
         DC    AL1(POSPRG),AL1(LENDPT),C'F'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSPRG),AL1(LENPRG),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    X'FF'                                                            
OLTEENT  DC    X'FF'                                                            
*                                                                               
OILTSTT  DC    AL2(OILTENT-OILTSTT+1),AL1(1)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'F'                                     
         DC    AL1(POSAFL),AL1(LENAFL),C'I'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENPRG),C' '                                     
         DC    X'FF'                                                            
OILTENT  DC    X'FF'                                                            
*                                                                               
NELYSTT  DC    AL2(NELYENT-NELYSTT+1),AL1(12)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEN)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    X'FF'                                                            
NELYENT  DC    X'FF'                                                            
*                                                                               
NNPRSTT  DC    AL2(NNPRENT-NNPRSTT+1),AL1(12)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEN)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'8'                                     
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    X'FF'                                                            
NNPRENT  DC    X'FF'                                                            
*                                                                               
NILTSTT  DC    AL2(NILTENT-NILTSTT+1),AL1(12)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    X'FF'                                                            
NILTENT  DC    X'FF'                                                            
*                                                                               
KWKDSTT  DC    AL2(KWKDENT-KWKDSTT+1),AL1(11)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEN)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    X'FF'                                                            
KWKDENT  DC    X'FF'                                                            
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
RPTCTRLX DS    0C                                                               
***************ANY DEMO ACROSS TARGETS ************                             
* DEMO LIST FOR SPORTS PROGRAMS                                                 
SPORXSTH DC    AL2(SPORXENH-SPORXSTH+1),AL1(10)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'S'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'04'                                  
         DC    X'FF'                                                            
SPORXENH DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR SPORTS PROGRAMS ( OTHER DAYPARTS )                              
SPOXHSTH DC    AL2(SPOXHENH-SPOXHSTH+1),AL1(10)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'S'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),C'9'                                     
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'04'                                  
         DC    X'FF'                                                            
SPOXHENH DC    X'FF'                                                            
*                                                                               
* DEMO LIST FOR KIDS PROGRAMS                                                   
KIDSTHX  DC    AL2(KIDENHX-KIDSTHX+1),AL1(11)                                   
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'K'                                     
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'03'                                  
         DC    X'FF'                                                            
KIDENHX  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NEWS PROGRAMS                                                   
NEWSSTHX DC    AL2(NEWSENHX-NEWSSTHX+1),AL1(12)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'02'                                  
         DC    X'FF'                                                            
NEWSENHX DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NEWS PROGRAMS (LATE-LATE = LATE)                                
NEWLSTHX DC    AL2(NEWLENHX-NEWLSTHX+1),AL1(12)                                 
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSSSEQ),AL1(LENSSEQ),X'02'                                  
         DC    X'FF'                                                            
NEWLENHX DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR EAM DAYPART                                                     
EAMSTHX  DC    AL2(EAMENHX-EAMSTHX+1),AL1(1)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(EAM)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
EAMENHX  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR DAY DAYPART                                                     
DAYSTHX  DC    AL2(DAYENHX-DAYSTHX+1),AL1(2)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(DAY)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
DAYENHX  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR WEM DAYPART                                                     
WEMSTHX  DC    AL2(WEMENHX-WEMSTHX+1),AL1(3)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEM)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
WEMENHX  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR WEA DAYPART                                                     
WEASTHX  DC    AL2(WEAENHX-WEASTHX+1),AL1(4)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(WEA)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
WEAENHX  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR ELY DAYPART                                                     
ELYSTHX  DC    AL2(ELYENHX-ELYSTHX+1),AL1(5)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ELY)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    X'FF'                                                            
ELYENHX  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR PAC DAYPART                                                     
PACSTHX  DC    AL2(PACENHX-PACSTHX+1),AL1(6)                                    
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(ACC)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
PACENHX  DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR NETWORK PRIME & SUNDAY PRIME COMB. DAYPART                      
NPRISTHX DC    AL2(NPRIENHX-NPRISTHX+1),AL1(7)                                  
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'N'                                     
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'1'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
NPRIENHX DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR IND PRIME & SUNDAY PRIME COMB. DAYPART                          
IPRISTHX DC    AL2(IPRIENHX-IPRISTHX+1),AL1(8)                                  
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'N'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(PRI)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(SUP)                                 
         DC    AL1(POSAFL),AL1(LENAFL),C'I'                                     
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'1'                                     
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    X'FF'                                                            
IPRIENHX DC    X'FF'                                                            
*                                                                               
*                                                                               
* DEMO LIST FOR LTE AND LLT COMB. DAYPART                                       
LTESTHX DC    AL2(LTEENHX-LTESTHX+1),AL1(9)                                     
         DC    AL1(SELECT)                                                      
         DC    AL1(POSPRG),AL1(LENPRG),C'O'                                     
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LTE)                                 
         DC    AL1(POSDPT),AL1(LENDPT),AL1(LLT)                                 
         DC    X'FF'                                                            
         DC    AL1(OVERRIDE)                                                    
         DC    AL1(POSDPT),AL1(LENDPT),C'2'                                     
         DC    AL1(POSAFL),AL1(LENAFL),C' '                                     
         DC    X'FF'                                                            
LTEENHX  DC    X'FF'                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
MTABC    DS    0F                                                               
MTABCST  DS    0C                                                               
         DS    700000C                                                          
MTABCEN  DS    0C                                                               
MTABCLEN EQU   MTABCEN-MTABCST                                                  
*                                                                               
TRNTAB   DS    10000C                                                           
UTABC    DS    0C                                                               
UTABCST  DS    0C                                                               
         DS    32000C                                                           
UTABCEN  DS    0C                                                               
UTABCLEN EQU   UTABCEN-UTABCST                                                  
*                                                                               
DEMBLOCK DS    CL256                                                            
DEMREC   DS    1000C                                                            
         EJECT                                                                  
OUT      DCB   DDNAME=OUT,DSORG=PS,RECFM=FB,LRECL=CPTCLEN,             X        
               BLKSIZE=CPTCLEN*50,MACRF=PM                                      
IOT      DS    (CPTCLEN)C                                                       
*                                                                               
OUT2     DCB   DDNAME=OUT2,DSORG=PS,RECFM=FB,LRECL=CPDCLEN,            X        
               BLKSIZE=CPDCLEN*50,MACRF=PM                                      
IOT2     DS    (CPDCLEN)C                                                       
*                                                                               
MRKCSV   DCB   DDNAME=MRKCSV,DSORG=PS,RECFM=VB,LRECL=CPDCLEN,          X        
               BLKSIZE=CPDCLEN*50,MACRF=PM                                      
DAYCSV   DCB   DDNAME=DAYCSV,DSORG=PS,RECFM=VB,LRECL=CPDCLEN,          X        
               BLKSIZE=CPDCLEN*50,MACRF=PM                                      
PRGCSV   DCB   DDNAME=PRGCSV,DSORG=PS,RECFM=VB,LRECL=CPDCLEN,          X        
               BLKSIZE=CPDCLEN*50,MACRF=PM                                      
ACTCSV   DCB   DDNAME=ACTCSV,DSORG=PS,RECFM=VB,LRECL=CPDCLEN,          X        
               BLKSIZE=CPDCLEN*50,MACRF=PM                                      
IOTCSV   DS    (CPDCLEN)C                                                       
         EJECT                                                                  
* SAVE AREA FOR MARKETS                                                         
MTABD    DSECT                                                                  
MTABST   DS    0C                                                               
MTMSEQ   DS    CL2                 MARKET SEQUENCE                              
MTREN    DS    CL2                                                              
MTRST    DS    CL2                                                              
MTMKT    DS    CL2                                                              
MTTGT    DS    CL1                                                              
MTDEMO   DS    CL1                                                              
MTRPT    DS    CL1                                                              
MTDPT    DS    CL1                                                              
MTKEN    DS    0C                                                               
MTIND    DS    CL36                INDUSTRY TOTALS                              
MTAGY    DS    CL36                AGENCY TOTALS                                
         DS    (NUMAGY-1)CL36                                                   
MTABEN   DS    0C                                                               
MTABLN   EQU   MTABEN-MTABST                                                    
MTKLN    EQU   MTKEN-MTABST                                                     
         EJECT                                                                  
UTABD    DSECT                                                                  
UTABST   DS    0C                                                               
UTDEM    DS    C                                                                
UTMKT    DS    CL2                                                              
UTUNIV   DS    CL4                                                              
UTABEN   DS    0C                                                               
UTLN     EQU   UTABEN-UTABST                                                    
UTKL     EQU   UTUNIV-UTABST                                                    
         EJECT                                                                  
DPD      DSECT                                                                  
DPDNMKT  DS    F                                                                
DPDWGHT  DS    F                                                                
DPDSPT   DS    F                                                                
DPDDOL   DS    F                                                                
DPDDOLE  DS    F                                                                
DPDPNT   DS    F                                                                
DPDIMP   DS    F                                                                
DPDPNTW  DS    CL8                                                              
DPDLEN   EQU   36                                                               
         EJECT                                                                  
DID      DSECT                                                                  
DIDNMKT  DS    F                                                                
DIDWGHT  DS    F                                                                
DIDSPT   DS    F                                                                
DIDDOL   DS    F                                                                
DIDDOLE  DS    F                                                                
DIDPNT   DS    F                                                                
DIDIMP   DS    F                                                                
DIDPNTW  DS    CL8                                                              
DIDLEN   EQU   36                                                               
         EJECT                                                                  
TRND     DSECT                                                                  
TRNCPP   DS    F                                                                
TRNCPM   DS    F                                                                
TRNDOL   DS    F                                                                
TRNSPT   DS    F                                                                
TRNPNT   DS    F                                                                
TRNIMP   DS    F                                                                
TRNCPS   DS    F                                                                
TRNPPS   DS    F                                                                
TRNIPS   DS    F                                                                
TRNDLEN  EQU   36                                                               
         EJECT                                                                  
PLD      DSECT                                                                  
PLMNUM   DS    CL3                 MARKET NUMBER                                
         DS    CL1                                                              
PLMNAM   DS    CL16                MARKET NAME                                  
         DS    CL1                                                              
PLUNIV   DS    CL5                 UNIVERSE                                     
         DS    C                                                                
PLAVRTG  DS    CL4                 AVERAGE RATING                               
PLCSPOTS DS    CL6                 COMBINED SPOTS                               
         DS    CL1                                                              
PLCCPP   DS    CL5                 COMBINED CPP                                 
         DS    CL1                                                              
PLASPOTS DS    CL5                 AGENCY SPOTS                                 
         DS    CL2                                                              
PLACPP   DS    CL5                 AGENCY CPP                                   
         DS    CL3                                                              
PLCPPIDX DS    CL3                 CPP INDEX                                    
PLEND    DS    0C                                                               
         ORG   PLASPOTS                                                         
PLCTAIDX DS    CL3                                                              
         ORG   PLMNUM                                                           
         DS    CL4                                                              
PLGCAP   DS    CL7                 GROUP CAPTION                                
         DS    CL1                                                              
PLGST    DS    CL3                 GROUP START                                  
PLGDSH   DS    CL1                                                              
PLGEN    DS    CL3                 GROUP END                                    
         ORG   PLEND                                                            
         EJECT                                                                  
*                                  BATES REPORT LAYOUT                          
BA1LINE  DSECT                                                                  
BA1MNAM  DS    CL20                                                             
         DS    CL1                                                              
BA1PCTUS DS    CL5                                                              
         DS    CL1                                                              
BA1CPP   DS    CL5                                                              
         DS    CL1                                                              
BA1IMP   DS    CL7                                                              
         DS    CL1                                                              
BA1CPM   DS    CL6                                                              
         DS    CL1                                                              
BA1SPOTS DS    CL6                                                              
         DS    C                                                                
BA1DOL   DS    CL8                                                              
         EJECT                                                                  
***********************************************************************         
*        TAPE RECORD DSECTS                                           *         
***********************************************************************         
*                                                                               
CPTC     DSECT                     CPPRS DATA RECORD                            
CPTCID   DS    C                   CPP ID                                       
CPTCIDEQ EQU   C'1'                                                             
CPTCRS   DS    CL1                 RATING SERVICE                               
CPTCMNO  DS    CL4                 MARKET NUMBER                                
CPTCREP  DS    CL7                 REPORT DEMO                                  
CPTCTRG  DS    CL7                 TARGET                                       
CPTCDPT  DS    C                   DAYPART CODE                                 
CPTCPTY  DS    C                   PROGRAMMING                                  
CPTCUNIV DS    PL8                 UNIVERSE                                     
*        AGENCY DATA                                                            
CPTCAS   DS    PL8                 SPOTS                                        
CPTCAD   DS    PL8                 DOLLARS                                      
CPTCADE  DS    PL8                 EQUIVALENCED DOLLARS                         
CPTCAP   DS    PL8                 POINTS                                       
CPTCAI   DS    PL8                 IMPS                                         
*        INDUSTRY DATA                                                          
CPTCIS   DS    PL8                 SPOTS                                        
CPTCIDL  DS    PL8                 DOLLARS                                      
CPTCIDE  DS    PL8                 EQUIVALENCED DOLLARS                         
CPTCIP   DS    PL8                 POINTS                                       
CPTCII   DS    PL8                 IMPS                                         
CPTCLEN  EQU   *-CPTCID                                                         
*                                                                               
CPTM     DSECT                     MARKET NAME RECORD                           
CPTMID   DS    C                   ID                                           
CPTMIDEQ EQU   C'M'                                                             
CPTMNO   DS    CL4                 MARKET NUMBER                                
CPTMNAME DS    CL28                MARKET NAME                                  
         DS    CL(CPTCLEN-(*-CPTM)) SPARE                                       
CPTMLEN  EQU   *-CPTMID                                                         
*                                                                               
CPTD     DSECT                     DAYPART NAME RECORD                          
CPTDID   DS    C                   ID                                           
CPTDIDEQ EQU   C'D'                                                             
CPTDCOD  DS    C                   DAYPART CODE                                 
CPTDN24  DS    CL24                DAYPART BIG NAME                             
CPTDN14  DS    CL14                SMALL NAME                                   
         DS    CL(CPTCLEN-(*-CPTD)) SPARE                                       
CPTDLEN  EQU   *-CPTDID                                                         
*                                                                               
CPTP     DSECT                     PROGRAMMING TYPE RECORD                      
CPTPID   DS    C                   ID                                           
CPTPIDEQ EQU   C'P'                                                             
CPTPCOD  DS    C                   PROGRAM TYPE CODE                            
CPTPN36  DS    CL36                BIG DESCRIPTION                              
CPTPN10  DS    CL10                SMALL DESCRIPTION                            
         DS    CL(CPTCLEN-(*-CPTP)) SPARE                                       
CPTPLEN  EQU   *-CPTPID                                                         
         EJECT                                                                  
***********************************************************************         
*        DISK RECORD DSECTS                                           *         
***********************************************************************         
*                                                                               
CPDC     DSECT                     CPPRS DATA RECORD                            
CPDCID   DS    CL1                 CPP ID                                       
CPDCIDEQ EQU   C'1'                                                             
CPDCRS   DS    CL1                 RATING SERVICE                               
CPDCMNO  DS    CL4                 MARKET NUMBER                                
CPDCREP  DS    CL7                 REPORT DEMO                                  
CPDCTRG  DS    CL7                 TARGET                                       
CPDCDPT  DS    CL1                 DAYPART CODE                                 
CPDCPTY  DS    CL3                 PROGRAMMING                                  
CPDCUNIV DS    CL11                UNIVERSE                                     
*        AGENCY DATA                                                            
CPDCAS   DS    CL11                SPOTS                                        
CPDCAD   DS    CL11                DOLLARS                                      
CPDCADE  DS    CL11                EQUIVALENCED DOLLARS                         
CPDCAP   DS    CL11                POINTS                                       
CPDCAI   DS    CL11                IMPS                                         
*        INDUSTRY DATA                                                          
CPDCIS   DS    CL11                SPOTS                                        
CPDCIDL  DS    CL11                DOLLARS                                      
CPDCIDE  DS    CL11                EQUIVALENCED DOLLARS                         
CPDCIP   DS    CL11                POINTS                                       
CPDCII   DS    CL11                IMPS                                         
*                                                                               
CPDCSPR  DS    CL5                 SPARE                                        
CPDCLEN  EQU   *-CPDCID                                                         
*                                                                               
CPDM     DSECT                     MARKET NAME RECORD                           
CPDMID   DS    CL1                 ID                                           
CPDMIDEQ EQU   C'M'                                                             
CPDMNO   DS    CL4                 MARKET NUMBER                                
CPDMNAME DS    CL28                MARKET NAME                                  
         DS    CL(CPDCLEN-(*-CPDM)) SPARE                                       
CPDMLEN  EQU   *-CPDMID                                                         
*                                                                               
CPDD     DSECT                     DAYPART NAME RECORD                          
CPDDID   DS    CL1                 ID                                           
CPDDIDEQ EQU   C'D'                                                             
CPDDCOD  DS    CL1                 DAYPART CODE                                 
CPDDN24  DS    CL24                DAYPART BIG NAME                             
CPDDN14  DS    CL14                SMALL NAME                                   
         DS    CL(CPDCLEN-(*-CPDD)) SPARE                                       
CPDDLEN  EQU   *-CPDDID                                                         
*                                                                               
CPDP     DSECT                     PROGRAMMING TYPE RECORD                      
CPDPID   DS    CL1                 ID                                           
CPDPIDEQ EQU   C'P'                                                             
CPDPCOD  DS    CL3                 PROGRAM TYPE CODE                            
CPDPN36  DS    CL36                BIG DESCRIPTION                              
CPDPN10  DS    CL10                SMALL DESCRIPTION                            
         DS    CL(CPDCLEN-(*-CPDP)) SPARE                                       
CPDPLEN  EQU   *-CPDPID                                                         
         EJECT                                                                  
       ++INCLUDE DEEQUDEMNO                                                     
         EJECT                                                                  
         DS    0F                                                               
EDBLOCK  DSECT                                                                  
       ++INCLUDE DDEBLOCK                                                       
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CPREPWORKD                                                     
       ++INCLUDE CPREPMODES                                                     
       ++INCLUDE CPGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039CPREP3202S05/01/02'                                      
         END                                                                    
