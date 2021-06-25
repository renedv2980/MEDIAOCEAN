*          DATA SET SPRES24    AT LEVEL 179 AS OF 11/12/03                      
*PHASE T20F24A                                                                  
         TITLE 'T20F24- AUDIENCE COMPOSITION REPORT'                            
**********************************************************************          
*                  REGISTERS                                                    
* R0--WORK                         R9--SYSD                                     
* R1--WORK                         RA--SECOND BASE REG                          
* R2--WORK                         RB--FIRST BASE REG                           
* R3--TWA                          RC--GEND                                     
* R4--WORK                         RD--SYSTEM                                   
* R5--WORK                         RE--SYSTEM                                   
* R6--WORK                         RF--SYSTEM                                   
* R7--WORK                                                                      
* R8--SPOOL                                                                     
**********************************************************************          
         EJECT                                                                  
T20F24   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20F24,RA,RR=R2                                                
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R3,ATWA                                                          
         USING CONHEADH-64,R3                                                   
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST TOTALS                                  
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB               SET CC EQ                                    
         B     XIT                                                              
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQ                                
*                                                                               
NO       LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
VK       DS    0H                                                               
*                                                                               
*                                                                               
VK10     LA    R2,CMPSRCH          VALIDATE SOURCE FIELD                        
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     EDTERR                                                           
*                                                                               
         GOTO1 VVALSRC                                                          
         CLI   DBSELMED,C'R'       TEST FOR MEDIA=RADIO                         
         BNE   *+8                                                              
         MVI   RADMED,C'Y'                                                      
*                                                                               
VK20     LA    R2,CMPBOKH          VALIDATE BOOK FIELD                          
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     EDTERR                                                           
*                                                                               
         CLI   RADMED,C'Y'         MEDIA=RADIO                                  
         BE    VK30                                                             
*                                                                               
         GOTO1 VVALBOOK            VALIDATE BOOK FOR THE TELE                   
         B     VK40                                                             
*                                                                               
VK30     GOTO1 VRADBOOK            VALIDATE BOOK FOR RADIO                      
*                                                                               
VK40     MVC   BOOK(2),BOOKS+1     MOVE BINARY BOOK TO STORAGE                  
*                                                                               
         LA    R2,CMPMKTH          VALIDATE MKT/STA                             
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     EDTERR                                                           
         TM    4(R2),X'08'         TEST INPUT ENTIRELY NUMERIC                  
         BZ    VK50                NO,THEN IS STATION                           
         BAS   RE,VALMKT                                                        
         MVI   GIVENMK,C'Y'        MARKET WAS GIVEN                             
         B     VK80                                                             
*                                                                               
VK50     BAS   RE,VALSTA           VAL STATIONS ON DEMO FILE                    
*                                                                               
VK80     LA    R2,CMPDPTH                                                       
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     EDTERR                                                           
*                                                                               
         BAS   RE,VALDYTM          VALIDATE DAY/TIME FIELD                      
         B     VK100                                                            
*                                                                               
VK100    LA    R2,CMPDEMH          VALIDATE DEMO ENTRIES                        
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     EDTERR                                                           
*                                                                               
         CLI   RADMED,C'Y'         TEST IF RADIO                                
         BE    VK120                                                            
*                                                                               
VK110    MVI   NDEMOS,9            MAX #DEMOS                                   
         LA    R4,SPTDEM           POINT TO SPECIAL TELE DEMO                   
         USING DEMSP,R4                                                         
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
*                                                                               
VK115    EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),8(R2)                                                    
         BE    VK117                                                            
         LA    R4,L'DEMENT(R4)     BUMP TO NEXT TABLE ENTRY                     
         CLI   0(R4),X'FF'                                                      
         BNE   VK115                                                            
         B     VK119                                                            
*                                                                               
VK117    LA    R5,DEMOS                                                         
         MVC   0(L'DEMLIST,R5),DEMLIST                                          
         MVC   NDEMOS,DEMNB                                                     
         B     VK130                                                            
         DROP  R4                                                               
*                                                                               
VK119    GOTO1 VVALDEM             VALIDATE AS LIST OF DEMOS                    
         B     VK130                                                            
*                                                                               
VK120    DS    0H                  VALIDATE RADIO DEMOS                         
         BAS   RE,RADDEM                                                        
*                                                                               
VK130    LA    R4,DEMOS            POINT TO DEMO LIST                           
         ZIC   R0,NDEMOS                                                        
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK140                                                            
*                                                                               
         CH    R0,=H'9'            MAX NUMBER DEMOS                             
         BNH   *+12                                                             
         LA    R0,9                                                             
         STCM  R0,1,NDEMOS                                                      
         B     VK150                                                            
*                                                                               
VK140    DS    0H                                                               
         CH    R0,=H'7'            ONLY 7 DEMOS ON LIST                         
         BNH   *+12                                                             
         LA    R0,7                                                             
         STCM  R0,1,NDEMOS                                                      
*                                                                               
VK150    MH    R0,=H'3'                                                         
         AR    R4,R0               INDEX INTO DEMO LIST                         
         MVI   0(R4),X'FF'         INSERT END-OF-LIST                           
*                                                                               
         LA    R4,DEMOS            POINT TO DEMO LIST                           
         ZIC   R0,NDEMOS                                                        
*                                                                               
VK170    CLI   1(R4),C'I'          MAKE SURE IT'S AN IMP                        
         BE    *+12                                                             
         MVI   ERROR,NOTIMP                                                     
         B     EDTERR                                                           
*                                                                               
         LA    R4,3(R4)            BUMP TO NEXT DEMO                            
         BCT   R0,VK170                                                         
*                                                                               
         ZIC   R4,NDEMOS           #DEMOS IN LIST                               
         LA    R4,1(R4)            COMPENSATE FOR TOTAL COLUMN                  
         MH    R4,=Y(DEMPERLN)                                                  
         LA    R4,L'PROG(R4)                                                    
         LA    R4,L'TIME(R4)                                                    
         STH   R4,ENTLEN           STORE TABLE ENTRY LENGTH                     
*                                                                               
         ZIC   R4,NDEMOS                                                        
         MH    R4,=Y(DEMPERLN)                                                  
         STH   R4,LENTOTOT         STORE DISP TO TOTAL COLUMN                   
*                                                                               
VK180    DS    0H                                                               
         LA    R2,CMPOPTH          VALIDATE OPTIONS FIELD                       
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   NOOPT,C'Y'                                                       
         B     VKX                                                              
*                                                                               
         BAS   RE,EDTOPT                                                        
*                                                                               
VKX      DS    0H                                                               
         B     XIT                                                              
*                                                                               
SPTDEM   DS    0C                                                               
         DC    C'MEN  ',X'00C97A00C94B00C95A00C96000C96500C96A00C96E'           
         DC    AL1(07)                                                          
         DC    C'WOMEN',X'00C97A00C91900C92800C92E00C93300C93800C93C'           
         DC    AL1(07)                                                          
         DC    C'TOTAL',X'00C97A00C97D00C98C00C99200C99700C99C00C9A0'           
         DC    AL1(07)                                                          
         DC    C'MIXED',X'00C97A00C97D00C96200C96D00C92800C93000C93B'           
         DC    AL1(07)                                                          
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*              TELE                LIST PERIOD TOTALS                           
*              RADIO               LIST MARKET IMPS                             
*                                                                               
*                                                                               
LR       DS    0H                                                               
         MVI   NLISTS,10           SET #LINES FOR LISTMON                       
         LA    R5,POSTLINE                                                      
         USING LSTLINE,R5                                                       
         MVC   LSTLINE(4),=C'STAT'             INSERT TABLE HEADLINES           
         MVC   LSTDATI+2(8),=C'DAY/TIME'                                        
*                                                                               
         L     R6,AIO2                                                          
         LA    R6,500(R6)                                                       
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         LA    R2,CMPSRCH                                                       
         GOTO1 VVALSRC             REINITIALIZE DBLOCK                          
*                                                                               
         GOTO1 DEMOCON,DMCB,(NDEMOS,DEMOS),(5,(R6)),DBLOCK                      
*                                                                               
         ZIC   R0,NDEMOS                                                        
         LA    R7,LSTDAT                                                        
         USING LSTDAT,R7                                                        
*                                                                               
LR05     MVC   LSTDAT(L'LSTDAT),0(R6)                                           
         LA    R7,LSTDLEN(R7)                                                   
         LA    R6,10(R6)                                                        
         BCT   R0,LR05                                                          
*                                                                               
         MVC   LSTDAT(5),=C'TOTAL'                                              
*                                                                               
         MVC   CMPHDLN(L'CMPHDLN),POSTLINE                                      
         OI    CMPHDLNH+6,X'80'    TRANSMIT HEADLINE                            
*                                                                               
         DROP  R5,R7                                                            
*                                                                               
         CLI   RADMED,C'Y'         TEST IF RADIO                                
         BNE   LR70                                                             
*                                                                               
*                                  CODE EXECUTED FOR RADIO                      
         OC    KEY,KEY             TEST FIRST TIME THROUGH                      
         BZ    LR10                                                             
         BAS   RE,GETSAVE          RETRIEVE TABLE FROM SAVE TWA                 
         B     LR60                                                             
*                                                                               
LR10     DS    0H                  BUILD BUFFER TABLE                           
         XC    SVTBLNDX,SVTBLNDX                                                
         XC    LSTCNT,LSTCNT                                                    
         AI    LSTCNT,1                                                         
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         LA    R2,CMPSRCH          POINT TO SOURCE FIELD                        
         GOTO1 VVALSRC             REINITIALIZE DBLOCK                          
*                                                                               
         CLI   GIVENMK,C'Y'                                                     
         BNE   LR20                                                             
         XC    COUNTER,COUNTER                                                  
         MVI   DBFUNCT,DBGETMS     DEMAND GETS STATIONS IN MKT                  
         MVC   DBAREC,AIO1                                                      
         MVC   DBSELRMK,SVMKT                                                   
         MVC   DBSELBK,BOOK                                                     
         GOTO1 DEMAND,DMCB,DBLOCK,STAHOOK,0                                     
*                                                                               
LR20     L     R4,AIO2             POINT TO LIST OF STATIONS                    
         ZIC   R5,COUNTER          R5=#STATIONS                                 
         XC    COUNTER,COUNTER     CLEAR COUNTER                                
*                                                                               
LR30     DS    0H                                                               
         MVC   THISSTA,0(R4)                                                    
         XC    DBLOCK,DBLOCK                                                    
         LA    R2,CMPSRCH          POINT TO SOURCE FIELD                        
         GOTO1 VVALSRC             REINITIALIZE DBLOCK                          
*                                                                               
         MVC   DBFILE,=C'RDP'      SET DBFILE                                   
         MVC   DBAREC,AIO1                                                      
         MVC   DBSELBK,BOOK                                                     
         MVC   DBSELSTA,THISSTA                                                 
         MVI   DBFUNCT,DBGETDEM    GET DEMO RECORDS                             
         MVC   DBSELDAY,DYTMLST                                                 
         XC    DBSELTIM,DBSELTIM          DOESN'T NEED TIME                     
         MVC   DBSELPRG+1(1),PROGCD       SINCE STANDARD DPT                    
         GOTO1 DEMAND,DMCB,DBLOCK,RDEMHOOK,0                                    
*                                                                               
LR40     BAS   RE,PERCENT          CALCULATE PERCENT FOR IMPS                   
         LA    R4,5(R4)            BUMP TO NEXT STATION                         
         BCT   R5,LR30             R5=#STATIONS IN MARKET                       
*                                                                               
         LA    R6,BUFF             SET UP REGISTERS FOR SORT ROUTINE            
         ZIC   R0,COUNTER                                                       
         LH    R5,ENTLEN                                                        
         OC    RANKN,RANKN                                                      
         BNZ   *+8                                                              
         MVI   RANKN,1             DEFAULT IS RANK ON FIRST DEMO                
         ZIC   R2,RANKN                                                         
         BCTR  R2,0                                                             
         MH    R2,=Y(DEMPERLN)                                                  
         AH    R2,=Y(L'PROG+L'TIME)      DISP TO KEY                            
         GOTO1 XSORT,DMCB,(1,(R6)),(R0),(R5),4,(R2)                             
*                                                                               
         MH    R0,ENTLEN                                                        
         AR    R6,R0                                                            
         MVI   0(R6),X'FF'         REPLACE END-OF-TABLE                         
*                                                                               
         OC    NUMST,NUMST         TEST#STA OPTION                              
         BZ    LR50                                                             
         LA    R6,BUFF                                                          
         XR    R5,R5                                                            
         LH    R5,NUMST                                                         
         MH    R5,ENTLEN                                                        
         AR    R6,R5                                                            
         MVI   0(R6),X'FF'         SET MARKER AFTER DESIRED#STA                 
*                                                                               
LR50     BAS   RE,PUTSAVE                                                       
*                                                                               
LR60     DS    0H                                                               
         BAS   RE,LIST                                                          
*                                                                               
         CLI   ENDTAB,C'Y'         TEST END OF BUFFTAB                          
         BE    LR150                                                            
         B     LR60                                                             
         EJECT                                                                  
*                                                                               
*                                  CODE EXECUTED FOR THE TELE LIST              
*                                                                               
*                                                                               
LR70     OC    KEY,KEY                                                          
         BZ    LR80                                                             
         BAS   RE,GETSAVE                                                       
         B     LR110                                                            
*                                                                               
LR80     DS    0H                                                               
         XC    SVTBLNDX,SVTBLNDX                                                
         XC    LSTCNT,LSTCNT                                                    
         AI    LSTCNT,1                                                         
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         LA    R2,CMPSRCH          POINT TO SOURCE FIELD                        
         GOTO1 VVALSRC             REINITIALIZE DBLOCK                          
*                                                                               
         CLI   GIVENMK,C'Y'                                                     
         BNE   LR90                                                             
         XC    COUNTER,COUNTER                                                  
         MVI   DBFUNCT,DBGETMS     DEMAND GETS STATIONS IN MKT                  
         MVC   DBAREC,AIO1                                                      
         MVC   DBSELRMK,SVMKT                                                   
         MVC   DBSELBK,BOOK                                                     
         GOTO1 DEMAND,DMCB,DBLOCK,STAHOOK,0                                     
*                                                                               
LR90     L     R4,AIO2             POINT TO LIST OF STATIONS                    
         ZIC   R5,COUNTER          R5=#STATIONS                                 
         XC    COUNTER,COUNTER     CLEAR COUNTER                                
         MVI   TOTOPT,C'Y'         MUST WANT TOTAL FOR LEGAL LIST               
         XC    NOOPT,NOOPT                                                      
*                                                                               
LR100    DS    0H                                                               
         LA    R6,TIMETAB                                                       
         MVC   0(4,R6),TIMSAVE                                                  
         MVC   THISSTA,0(R4)                                                    
*                                                                               
         GOTO1 DEMROUT,DMCB,(R6)                                                
*                                                                               
         BAS   RE,PERCENT                                                       
*                                                                               
         LA    R4,5(R4)            BUMP TO NEXT STATION                         
         XC    PREVPROG,PREVPROG                                                
         BCT   R5,LR100                                                         
*                                                                               
         BAS   RE,PUTSAVE                                                       
*                                                                               
LR110    DS    0H                                                               
         BAS   RE,LIST                                                          
*                                                                               
         CLI   ENDTAB,C'Y'                                                      
         BE    LR150                                                            
         B     LR110                                                            
*                                                                               
LR150    DS    0H                                                               
         XC    ENDTAB,ENDTAB                                                    
         XC    KEY,KEY                                                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
*              FORMAT OFFLINE REPORT                                            
*                                                                               
*                                                                               
PR       DS    0H                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVI   HDHOOKOK,C'Y'                                                    
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         LA    R2,CMPSRCH          POINT TO SOURCE FIELD                        
         GOTO1 VVALSRC             REINITIALIZE DBLOCK                          
*                                                                               
         CLI   GIVENMK,C'Y'                                                     
         BNE   PR20                                                             
         XC    COUNTER,COUNTER                                                  
         MVI   DBFUNCT,DBGETMS     DEMAND GETS STATIONS IN MKT                  
         MVC   DBAREC,AIO1                                                      
         MVC   DBSELRMK,SVMKT                                                   
         MVC   DBSELBK,BOOK                                                     
         GOTO1 DEMAND,DMCB,DBLOCK,STAHOOK,0                                     
*                                                                               
PR20     CLI   RADMED,C'Y'         TEST RADIO MEDIA                             
         BE    PR200                                                            
*                                                                               
*                                  THIS CODE FOR TELE REPORT                    
*                                                                               
PR30     L     R4,AIO2             POINT TO LIST OF STATIONS                    
         ZIC   R5,COUNTER          R5=#STATIONS                                 
         XC    COUNTER,COUNTER     CLEAR COUNTER                                
         CLI   NOOPT,C'Y'          TEST ANY OPTIONS                             
         BE    PR40                NO,THEN BRANCH                               
         BAS   RE,CALC             SET TIMETAB VALUES                           
         LA    R6,TIMETAB                                                       
         ZIC   R0,HRUNITS          LOOP COUNTER                                 
         B     PR50                                                             
*                                                                               
PR40     LA    R6,TIMETAB                                                       
         MVC   0(4,R6),TIMSAVE                                                  
         LA    R0,1                SET LOOP COUNTER                             
         STCM  R0,1,HRUNITS                                                     
*                                                                               
PR50     MVC   THISSTA,0(R4)                                                    
         CLI   TOTOPT,C'Y'         TEST TOTAL OPTION                            
         BE    PR70                                                             
*                                                                               
PR60     GOTO1 DEMROUT,DMCB,(R6)                                                
*                                                                               
         LA    R6,4(R6)            BUMP TO NEXT TIMETAB ENTRY                   
         BCT   R0,PR60                                                          
*                                                                               
         CLI   NOOPT,C'Y'          TEST ANY OPTIONS                             
         BE    PR80                                                             
*                                                                               
PR70     LA    R6,TIMETAB                                                       
         MVC   SAVTIME,0(R6)                                                    
         MVC   0(4,R6),TIMSAVE     MOVE FOR STATION TOTALS                      
         GOTO1 DEMROUT,DMCB,(R6)   GET STATION TOTALS                           
         MVC   0(4,R6),SAVTIME     RESET START TIME                             
*                                                                               
PR80     BAS   RE,PERCENT          CALCULATE PERCENT FOR IMPS                   
         BAS   RE,PRINT            PRINT THE TABLE                              
         LA    R4,5(R4)            BUMP TO NEXT STATION                         
         ZIC   R0,HRUNITS                                                       
         XC    COUNTER,COUNTER                                                  
         XC    PREVPROG,PREVPROG   ALWAYS PRINT PROG ON NEW STA                 
         LA    R6,TIMETAB                                                       
         BCT   R5,PR50             R5=#STATIONS REQUESTED                       
*                                                                               
PR100    DS    0H                                                               
         B     PRX                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*                                  CODE EXECUTED FOR RADIO REPORT               
*                                                                               
*                                                                               
PR200    L     R4,AIO2             POINT TO LIST OF STATIONS                    
         ZIC   R5,COUNTER          R5=#STATIONS                                 
         XC    COUNTER,COUNTER     CLEAR COUNTER                                
*                                                                               
PR240    DS    0H                                                               
         MVC   THISSTA,0(R4)                                                    
         XC    DBLOCK,DBLOCK                                                    
         LA    R2,CMPSRCH          POINT TO SOURCE FIELD                        
         GOTO1 VVALSRC             REINITIALIZE DBLOCK                          
*                                                                               
         MVC   DBFILE,=C'RDP'      SET DBFILE                                   
         MVC   DBAREC,AIO1                                                      
         MVC   DBSELBK,BOOK                                                     
         MVC   DBSELSTA,THISSTA                                                 
         MVI   DBFUNCT,DBGETDEM    GET DEMO RECORDS                             
         MVC   DBSELDAY,DYTMLST                                                 
         XC    DBSELTIM,DBSELTIM          DOESN'T NEED TIME                     
         MVC   DBSELPRG+1(1),PROGCD       SINCE STANDARD DPT                    
         GOTO1 DEMAND,DMCB,DBLOCK,RDEMHOOK,0                                    
*                                                                               
PR250    BAS   RE,PERCENT          CALCULATE PERCENT FOR IMPS                   
         LA    R4,5(R4)            BUMP TO NEXT STATION                         
         BCT   R5,PR240            R5=#STATIONS IN MARKET                       
*                                                                               
         LA    R6,BUFF             SET UP REGISTERS FOR SORT ROUTINE            
         ZIC   R0,COUNTER                                                       
         LH    R5,ENTLEN                                                        
         OC    RANKN,RANKN                                                      
         BNZ   *+8                                                              
         MVI   RANKN,1             DEFAULT IS RANK ON FIRST DEMO                
         ZIC   R2,RANKN                                                         
         BCTR  R2,0                                                             
         MH    R2,=Y(DEMPERLN)                                                  
         AH    R2,=Y(L'PROG+L'TIME)       DISP TO KEY                           
         GOTO1 XSORT,DMCB,(1,(R6)),(R0),(R5),4,(R2)                             
*                                                                               
         MH    R0,ENTLEN                                                        
         AR    R6,R0                                                            
         MVI   0(R6),X'FF'         REPLACE END-OF-TABLE                         
*                                                                               
         BAS   RE,RPRINT           PRINT THE TABLE FOR RADIO                    
*                                                                               
PRX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*              ROUTINE TO CALL SPDEMLK FOR TELE DEMO LOOKUP                     
*                                                                               
DEMROUT  NTR1                                                                   
         L     R6,0(R1)                                                         
         XC    DBLOCK,DBLOCK                                                    
         LA    R2,CMPSRCH          POINT TO SOURCE FIELD                        
         GOTO1 VVALSRC             REINITIALIZE DBLOCK                          
*                                                                               
         L     R4,AIO2                                                          
         LA    R4,1000(R4)         SET LOCATION FOR SPDEMLKD                    
         USING SPDEMLKD,R4                                                      
         XC    SPDEMLK(SPDEMLKL),SPDEMLK                                        
         L     R5,AIO1                                                          
         ST    R5,SPLKAREC                                                      
         MVC   SPLKAFAC,ACOMFACS                                                
         LA    R0,DEMOS            DEMO LIST                                    
         ST    R0,SPLKALST                                                      
         LA    R0,DEMVAL           DEMO VALUES                                  
         ST    R0,SPLKAVAL                                                      
         LA    R0,DEMHOOK          ADDRESS OF HOOK ROUTINE                      
         ST    R0,SPLKHOOK                                                      
         MVC   SPLKFIL,DBFILE                                                   
         MVC   SPLKMED,DBSELMED                                                 
         MVC   SPLKSRC,DBSELSRC                                                 
         MVC   SPLKAGY,DBSELAGY                                                 
         MVC   SPLKDBK,BOOK                                                     
         MVC   SPLKSTA,THISSTA                                                  
         MVC   SPLKDAY,DYTMLST                                                  
         MVC   SPLKTIM,0(R6)                                                    
         MVI   SPLKSVI,X'FF'       NO ADJUSTMENTS                               
*                                                                               
*                                  CALL SPDEMLK TO GET DEMOS                    
         GOTO1 GETDEMO,DMCB,(X'FF',SPDEMLKD)                                    
*                                                                               
         MVC   TIMEPD,SPLKTIM      SET TIME PERIOD FOR UNTIME                   
         MVI   ENDCALL,C'Y'        PERIOD AVG CALL TO POST                      
         BAS   RE,POST             POST TO BUFFER TABLE                         
*                                                                               
         LA    R6,BUFF                                                          
         ZIC   R7,COUNTER                                                       
         MH    R7,ENTLEN                                                        
         AR    R6,R7               BUMP TO NEXT AVAILABLE LOCATION              
         MVI   0(R6),X'FF'         END OF TABLE MARKER                          
*                                                                               
         BAS   RE,TOT              TOTAL ON LINE                                
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              HOOK ROUTINE CALLED FROM SPDEMLK                                 
*                                                                               
DEMHOOK  DS    0H                                                               
         ST    RE,SAVERE                                                        
*                                                                               
         CLI   NOOPT,C'Y'          TEST ANY OPTIONS                             
         BNE   DEM50               IF ARE OPTIONS,THEN EXIT                     
*                                                                               
         L     RE,SPLKDBLK         RE=A(GETDEM DBLOCK)                          
         LA    RE,DBFACTOR-DBLOCK(RE)     RE=A(DBFACTOR)                        
         LH    R1,0(RE)                                                         
         CH    R1,=H'1'            TEST FOR WEIGHTED RECORD                     
         BE    DEM40                                                            
*                                  UNWEIGHT WEIGHTED DEMOS                      
         ZIC   R0,NDEMOS           R0=N'DEMOS                                   
         LA    R2,DEMVAL           R2=A(DEMO VALUES)                            
*                                                                               
DEM20    L     RF,0(R2)            GET DEMO VALUES                              
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         DR    RE,R1               WEIGHTED DEMO/WEIGHT                         
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,0(R2)                                                         
         LA    R2,8(R2)                                                         
         BCT   R0,DEM20                                                         
*                                                                               
DEM40    BAS   RE,POST             POST TO BUFFER TABLE                         
*                                                                               
DEM50    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*              HOOK TO EXTRACT DEMO VALUES AND POST TO BUFFER                   
*                                                                               
*                                                                               
RDEMHOOK DS    0H                                                               
         ST    RE,SAVERE                                                        
*                                  GET DEMO VALUES FROM RECORD                  
*                                                                               
         GOTO1 DEMOUT,DMCB,(C'L',DEMOS),DBLOCK,DEMVAL                           
*                                                                               
         BAS   RE,POST             POST TO BUFFER                               
*                                                                               
         LA    R6,BUFF                                                          
         ZIC   R7,COUNTER                                                       
         MH    R7,ENTLEN                                                        
         AR    R6,R7               BUMP TO NEXT AVAILABLE LOCATION              
         MVI   0(R6),X'FF'         END OF TABLE MARKER                          
*                                                                               
         BAS   RE,TOT              TOTAL ON LINE                                
*                                                                               
RDMHK50  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                  POSTS ONE LINE IN BUFFER                     
*                                                                               
POST     NTR1                                                                   
         LA    R6,BUFF                                                          
         USING BUFTABD,R6                                                       
*                                                                               
         ZIC   R7,COUNTER                                                       
         MH    R7,ENTLEN                                                        
*                                                                               
         L     R1,=AL4(BUFFEND-BUFF)                                            
         SR    R1,R7               LENGTH TO END OF BUFF                        
         CH    R1,ENTLEN           SPACE LEFT FOR ANOTHER ENTRY                 
         BNL   *+12                YES,THEN PROCEED                             
         MVI   ERROR,TABFUL                                                     
         B     EDTERR                                                           
*                                                                               
         AR    R6,R7               INDEX INTO TABLE                             
*                                                                               
         CLI   RADMED,C'Y'         TEST RADIO MEDIA                             
         BE    POST5                                                            
*                                                                               
*                                  CODE EXECUTED FOR THE TELE                   
         XC    PROG,PROG                                                        
         CLC   PREVPROG(L'PREVPROG),SPLKPRG                                     
         BE    *+10                                                             
         MVC   PROG(L'PROG),SPLKPRG    MOVE PROG NAME TO TABLE                  
         MVC   PREVPROG(L'PREVPROG),SPLKPRG      REPLACE PREVPROG               
*                                                                               
         CLI   ENDCALL,C'Y'        TEST PERIOD AVERAGES                         
         BNE   *+12                                                             
         BAS   RE,POSTEND                                                       
         B     POSTX                                                            
*                                                                               
         GOTO1 DEFINE,DMCB,=C'DAY',SPLKDBLK,WORK                                
         MVC   TIME(3),WORK+2                                                   
*                                                                               
         GOTO1 DEFINE,DMCB,=C'TIME',SPLKDBLK,WORK                               
         GOTO1 UNTIME,DMCB,WORK+2,TIME+3                                        
*                                                                               
         ZIC   R7,DBACTSQC                                                      
         B     POST10                                                           
*                                                                               
*                                  CODE EXECUTED FOR RADIO MEDIA                
POST5    DS    0H                                                               
         MVC   TIME(5),THISSTA     MOVE RADIO STATION TO STORAGE                
*                                                                               
         LA    R7,STANDPTS                                                      
         USING STANDENT,R7                                                      
*                                                                               
POST6    CLI   0(R7),X'FF'         TEST LIST END                                
         BNE   *+6                                                              
         DC    H'00'               DIE IF NO PROG CODE                          
*                                                                               
         CLC   STANDPRG,PROGCD     FIND CODE IN TABLE                           
         BE    POST7                                                            
*                                                                               
         LA    R7,L'STANDENT(R7)                                                
         B     POST6                                                            
*                                                                               
POST7    XC    WORK,WORK                                                        
         MVC   WORK(L'STANDESC),STANDESC                                        
*                                                                               
         DROP  R7                                                               
         LA    R1,WORK                                                          
POST8    CLI   0(R1),C' '          TEST FOR SPACE                               
         BE    POST9                                                            
         LA    R1,1(R1)                                                         
         B     POST8                                                            
*                                                                               
POST9    MVC   PROG(L'PROG),0(R1)  MOVE TIME TO TABLE                           
         LA    R7,WORK                                                          
         SR    R1,R7                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TIME+5(0),0(R7)       MOVE DAY TO TABLE                          
*                                                                               
*                                                                               
POST10   LA    R6,DEMO                                                          
         LA    R5,DEMVAL                                                        
         ZIC   R0,NDEMOS                                                        
         B     POST30                                                           
*                                                                               
POST20   LA    R6,DEMPERLN(R6)                                                  
         LA    R5,4(R5)            BUMP TO NEXT DEMO                            
         CLI   RADMED,C'Y'         TEST IF RADIO                                
         BE    *+8                                                              
         LA    R5,4(R5)            BUMP PAST SVI INDEX FOR TELE                 
*                                                                               
POST30   MVC   0(4,R6),0(R5)      INSERT DEMO IN TABLE                          
         BCT   R0,POST20                                                        
*                                                                               
         CLI   RADMED,C'Y'                                                      
         BE    POST40                                                           
*                                                                               
         LA    R7,1(R7)            ONLY EXECUTED FOR THE TELE                   
         CLM   R7,1,DBACTEQC                                                    
         BNH   POST20                                                           
*                                                                               
POST40   AI    COUNTER,1           INCREMENT LINE COUNTER                       
*                                                                               
POSTX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                  POSTS PERIOD AVERAGES TO BUFF TABLE          
*                                  ONLY EXECUTED FOR THE TELE                   
*                                                                               
POSTEND  NTR1                                                                   
         USING BUFTABD,R6                                                       
*                                                                               
         GOTO1 UNDAY,DMCB,DYTMLST,TIME                                          
*                                                                               
         GOTO1 UNTIME,DMCB,TIMEPD,TIME+3                                        
*                                                                               
         MVC   TIME+10(4),THISSTA          SAVE STATION FOR LIST                
*                                                                               
PEND10   LA    R6,DEMO                                                          
         LA    R5,DEMVAL                                                        
         ZIC   R0,NDEMOS                                                        
         B     PEND30                                                           
*                                                                               
PEND20   LA    R6,DEMPERLN(R6)                                                  
         LA    R5,8(R5)            BUMP TO NEXT DEMO                            
*                                                                               
PEND30   MVC   0(4,R6),0(R5)       INSERT DEMO IN TABLE                         
         BCT   R0,PEND20                                                        
*                                                                               
         AI    COUNTER,1           INCREMENT LINE COUNTER                       
         MVI   ENDCALL,C'N'                                                     
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                  ROUTINE TO LIST RECORDS IN BUFF              
*                                  ON SCREEN                                    
*                                                                               
*                                                                               
LIST     NTR1                                                                   
         DROP  R4                                                               
*                                                                               
         LA    R6,BUFF                                                          
         CLI   0(R6),X'FF'         ANYTHING IN BUFF?                            
         BNE   LS05                                                             
         XC    CMPHDLN,CMPHDLN                                                  
         MVC   CMPHDLN(40),=C'NO RECORDS EXIST ON DEMO FILE FOR FIELDS'         
         OI    CMPHDLNH+6,X'80'    TRANSMIT                                     
         MVI   ENDTAB,C'Y'                                                      
         B     LISTX                                                            
*                                                                               
LS05     LA    R6,BUFF                                                          
         USING BUFTABD,R6                                                       
         L     R0,SVTBLNDX                                                      
         MH    R0,ENTLEN                                                        
         AR    R6,R0               INDEX INTO TABLE                             
         CLI   0(R6),X'FF'         TEST TABLE END                               
         BNE   LS10                                                             
         MVI   ENDTAB,C'Y'                                                      
         B     LISTX                                                            
*                                                                               
LS10     LA    R2,LISTAR                                                        
         MVC   LISTAR,SPACES                                                    
         USING LSTLINE,R2                                                       
*                                                                               
         XR    R5,R5                                                            
         ZIC   R4,LSTCNT           #TIMES THROUGH LIST CODE                     
         SRDL  R4,1                                                             
         LTR   R5,R5               TEST EVEN/ODD                                
         BZ    LS40                IF EVEN, DO PCT                              
*                                                                               
LS20     DS    0H                                                               
*                                                                               
         CLI   RADMED,C'Y'                                                      
         BE    *+20                                                             
*                                                                               
         MVC   LSTSTA(4),TIME+10   STATION TO PRINT(TELE)                       
         MVC   LSTDATI(10),TIME     DAY/TIME TO PRINT(TELE)                     
         B     *+16                                                             
*                                                                               
         MVC   LSTDATI(9),TIME+5   DAY TO PRINT(RADIO)                          
         MVC   LSTSTA(5),TIME       STATION TO PRINT (RADIO)                    
         MVC   LSTWRD(3),=C'IMP'                                                
*                                                                               
         ZIC   R1,NDEMOS                                                        
         LA    R1,1(R1)            R0=TOTAL #ENTRIES ACROSS THE PAGE            
         LA    R4,DEMO             POINT TO DEMOS IN TABLE                      
         USING DEMO,R4                                                          
         LA    R5,LSTDAT           POINT TO DEMOS IN LIST                       
         USING LSTDAT,R5                                                        
*                                                                               
LS30     L     RE,DEMO                   MOVE DEMOS TO SCREEN                   
         EDIT  (RE),(5,LSTDAT),ALIGN=LEFT                                       
         LA    R5,LSTDLEN(R5)                                                   
         LA    R4,DEMPERLN(R4)                                                  
         BCT   R1,LS30                                                          
         DROP  R4,R5                                                            
*                                                                               
         B     LS70                                                             
*                                                                               
LS40     DS    0H                  MOVE PERCENTS TO SCREEN                      
*                                                                               
         MVC   LSTDATI(L'LSTDATI),PROG       PROGRAM NAME                       
         MVC   LSTWRD(3),=C'PCT'                                                
*                                                                               
         ZIC   R1,NDEMOS                                                        
         LA    R1,1(R1)                                                         
         LA    R5,LSTDAT                                                        
         USING LSTDAT,R5                                                        
         LA    R4,DEMO                                                          
         USING DEMO,R4                                                          
*                                                                               
LS50     XR    RE,RE                                                            
         LH    RE,PER                                                           
         EDIT  (RE),(3,LSTDAT),ALIGN=LEFT                                       
         LA    R5,LSTDLEN(R5)                                                   
         LA    R4,DEMPERLN(R4)                                                  
         BCT   R1,LS50                                                          
         DROP  R4,R5                                                            
*                                                                               
LS60     L     R1,SVTBLNDX         TABLE INDEX FROM SAVED STORAGE               
         LA    R1,1(R1)            INCREMENT                                    
         ST    R1,SVTBLNDX                                                      
*                                                                               
LS70     MVC   KEY(9),=C'NOT FIRST'                                             
         XC    KEY+14(2),KEY+14                                                 
         MVC   KEY+16(2),=2X'01'                                                
         MVC   DMDSKADD,KEY+14     FUDGE DISC ADDRESS FOR GENCON                
         AI    LSTCNT,1            #TIMES THROUGH LIST CODE                     
*                                                                               
         DROP  R2,R6                                                            
*                                                                               
LS80     GOTO1 LISTMON                                                          
*                                                                               
LISTX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                      ROUTINE TO CALCULATE PERCENTAGES OF IMPS                 
*                                                                               
*                                                                               
*                                                                               
PERCENT  NTR1                                                                   
         XR    R0,R0                                                            
         LA    R6,BUFF                                                          
         USING BUFTABD,R6                                                       
*                                                                               
PER10    CLI   0(R6),X'FF'         TEST FOR END OF LIST                         
         BE    PERX                                                             
*                                                                               
         LA    R7,DEMO             POINT TO LIST OF DEMOS                       
         USING DEMO,R7                                                          
*                                                                               
         LA    R1,0(R7)                                                         
         LH    R0,LENTOTOT                                                      
         AR    R1,R0               POINT TO TOTAL COLUMN                        
*                                                                               
PER20    L     R4,0(R7)                                                         
         MH    R4,=H'100'                                                       
         XR    R5,R5                                                            
*                                  DEFEND AGAINST ZERO DIVIDE                   
         OC    0(4,R1),0(R1)       TEST TOTAL IS ZERO                           
         BNZ   PER30                                                            
         LTR   R4,R4               TEST IF DIVIDEND ZERO                        
         BZ    PER40               YES, THEN HUNKY-DORY                         
         DC    H'0'                NO,THEN SERIOUS PROBLEMS                     
*                                                                               
PER30    SRDA  R4,31                                                            
         D     R4,0(R1)            CALCULATE PERCENT                            
         LTR   R5,R5               ROUNDED DIVIDE                               
         BM    *+8                                                              
         AH    R5,=H'1'                                                         
         SRA   R5,1                                                             
*                                                                               
PER40    STH   R5,PER              INSERT INTO LINE                             
         LA    R7,DEMPERLN(R7)     BUMP TO NEXT DEMO                            
         CR    R7,R1               TEST FOR END OF LIST                         
         BNE   PER20                                                            
         LH    R0,ENTLEN                                                        
         AR    R6,R0                                                            
         DROP  R7                                                               
         B     PER10                                                            
*                                                                               
PERX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*              CALCULATES VALUE OF TOTAL COLUMN ENTRY                           
*                                                                               
TOT      NTR1                                                                   
         XR    R0,R0                                                            
         XR    R5,R5                                                            
*                                                                               
         LA    R6,BUFF                                                          
         USING BUFTABD,R6                                                       
*                                                                               
TOT10    XR    R1,R1                                                            
         LA    R7,DEMO             BEGINNING ADDRESS                            
         LA    R4,6                INCREMENT                                    
         LH    R0,LENTOTOT                                                      
         LA    R5,0(R7)                                                         
         AR    R5,R0                                                            
         BCTR  R5,0                LIMIT ADDRESS                                
*                                                                               
TOT20    A     R1,0(R7)                                                         
         BXLE  R7,R4,TOT20                                                      
*                                                                               
         LA    R5,1(R5)            POINT TO END OF DEMO LIST                    
         ST    R1,0(R5)            MOVE TOTAL TO TABLE                          
         LH    R0,ENTLEN                                                        
         AR    R6,R0               BUMP TO NEXT LINE                            
         CLI   0(R6),X'FF'         TEST END OF TABLE                            
         BNE   TOT10                                                            
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*              TELE                MOVES BUFF TABLE TO PRINT                    
*                                  CALLED ONCE PER STATION                      
*                                                                               
PRINT    NTR1                                                                   
*                                                                               
         LA    R6,BUFF                                                          
         CLI   0(R6),X'FF'         ANYTHING IN BUFF?                            
         BNE   PRT                                                              
         MVC   P1(40),=C'NO RECORDS EXIST ON DEMO FILE FOR FIELDS'              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
PRT      LA    R6,BUFF             POINT TO TABLE                               
         USING BUFTABD,R6                                                       
*                                                                               
PRNT     CLI   0(R6),X'FF'         TEST END OF TABLE                            
         BE    XIT                                                              
*                                                                               
         LA    R1,P1               R1 POINTS TO PRINT LINE                      
         USING PRLNE,R1                                                         
*                                                                               
         MVC   PRSTA(4),THISSTA    STATION TO PRINT IF TELE                     
         MVC   PRDATI(10),TIME     DAY/TIME TO PRINT                            
         MVC   PRIMP(3),=C'IMP'                                                 
*                                                                               
         ZIC   R2,NDEMOS                                                        
         LA    R2,1(R2)            R2=TOTAL #ENTRIES ACROSS THE PAGE            
         LA    R4,DEMO             POINT TO DEMOS IN TABLE                      
         USING DEMO,R4                                                          
         LA    R5,PRDEM            POINT TO DEMOS IN PRINT                      
         USING PRDEM,R5                                                         
*                                                                               
PRNT20   L     R7,DEMO                   MOVE DEMOS TO PRINT                    
         EDIT  (R7),(5,PRDEM),ALIGN=LEFT                                        
         LA    R5,PRDLEN(R5)                                                    
         LA    R4,DEMPERLN(R4)                                                  
         BCT   R2,PRNT20                                                        
         DROP  R4,R5,R1                                                         
*                                                                               
         LA    R1,P2                                                            
         USING PRLINE,R1                                                        
*                                                                               
         MVC   PRPROG(L'PROG),PROG                                              
         MVC   PRPERN(3),=C'PCT'                                                
*                                                                               
         ZIC   R2,NDEMOS                                                        
         LA    R2,1(R2)                                                         
         LA    R5,PRPER                                                         
         USING PRPER,R5                                                         
         LA    R4,DEMO                                                          
         USING DEMO,R4                                                          
*                                                                               
PRNT30   XR    R7,R7                                                            
         LH    R7,PER                  MOVE PERCENTS TO PRINT                   
         EDIT  (R7),(3,PRPER),ALIGN=LEFT                                        
         LA    R5,PRPLEN(R5)                                                    
         LA    R4,DEMPERLN(R4)                                                  
         BCT   R2,PRNT30                                                        
         DROP  R4,R5,R1                                                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LH    R0,ENTLEN                                                        
         AR    R6,R0               POINT TO NEXT LINE IN TABLE                  
         B     PRNT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*              RADIO               MOVES BUFF TABLE TO PRINT                    
*                                  CALLED AFTER SORT ON FIRST IMP               
*                                  PRINTS ALL STATIONS(OR #REQ)IN MKT           
*                                                                               
RPRINT   NTR1                                                                   
*                                                                               
         LA    R6,BUFF                                                          
         CLI   0(R6),X'FF'         ANYTHING IN BUFF?                            
         BNE   RPRT                                                             
         MVC   P1(40),=C'NO RECORDS EXIST ON DEMO FILE FOR FIELDS'              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     RPRNTX                                                           
*                                                                               
RPRT     LA    R6,BUFF             POINT TO TABLE                               
         USING BUFTABD,R6                                                       
*                                                                               
         XR    R7,R7                                                            
         OC    NUMST,NUMST         TEST #STATIONS OPTION                        
         BZ    RPRNT                                                            
         LH    R7,NUMST                                                         
         CLM   R7,1,COUNTER        TEST #REQ L.T. #IN MKT                       
         BH    RPRNT                                                            
         B     RPRNT10                                                          
*                                                                               
RPRNT    ZIC   R7,COUNTER          NO OPT,THEN PRINT ALL                        
*                                                                               
RPRNT10  LA    R1,P1               R1 POINTS TO PRINT LINE                      
         USING PRLNE,R1                                                         
*                                                                               
         MVC   PRSTA(5),TIME       STATION TO PRINT                             
         MVC   PRDATI(9),TIME+5    DAYTIME TO PRINT                             
         MVC   PRIMP(3),=C'IMP'                                                 
*                                                                               
         ZIC   R2,NDEMOS                                                        
         LA    R2,1(R2)            R2=TOTAL #ENTRIES ACROSS THE PAGE            
         LA    R4,DEMO             POINT TO DEMOS IN TABLE                      
         USING DEMO,R4                                                          
         LA    R5,PRDEM            POINT TO DEMOS IN PRINT                      
         USING PRDEM,R5                                                         
*                                                                               
RPRNT20  L     RE,DEMO                   MOVE DEMOS TO PRINT                    
         EDIT  (RE),(5,PRDEM),ALIGN=LEFT                                        
         LA    R5,PRDLEN(R5)                                                    
         LA    R4,DEMPERLN(R4)                                                  
         BCT   R2,RPRNT20                                                       
         DROP  R4,R5,R1                                                         
*                                                                               
         LA    R1,P2                                                            
         USING PRLINE,R1                                                        
*                                                                               
         MVC   PRPROG(L'PROG),PROG                                              
         MVC   PRPERN(3),=C'PCT'                                                
*                                                                               
         ZIC   R2,NDEMOS                                                        
         LA    R2,1(R2)                                                         
         LA    R5,PRPER                                                         
         USING PRPER,R5                                                         
         LA    R4,DEMO                                                          
         USING DEMO,R4                                                          
*                                                                               
RPRNT30  XR    RE,RE                                                            
         LH    RE,PER                  MOVE PERCENTS TO PRINT                   
         EDIT  (RE),(3,PRPER),ALIGN=LEFT                                        
         LA    R5,PRPLEN(R5)                                                    
         LA    R4,DEMPERLN(R4)                                                  
         BCT   R2,RPRNT30                                                       
         DROP  R4,R5,R1                                                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LH    R0,ENTLEN                                                        
         AR    R6,R0               POINT TO NEXT LINE IN TABLE                  
         BCT   R7,RPRNT10                                                       
*                                                                               
RPRNTX   B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              HOOK FOR LIST OF STATIONS IN MARKET                              
*                                                                               
STAHOOK  DS    0H                                                               
         ST    RE,SAVERE                                                        
*                                                                               
         L     R5,DBAREC           R5 POINTS TO RECORD FROM DEMAND              
         USING MLKEY,R5                                                         
*                                                                               
         L     R6,AIO2             BUILD STATION LIST HERE                      
         ZIC   R0,COUNTER                                                       
         MH    R0,=H'5'                                                         
         AR    R6,R0               INDEX INTO IO AREA                           
*                                                                               
         OC    MLKMKT,MLKMKT       IGNORE SPILL MKTS                            
         BNZ   STAHKX                                                           
*                                                                               
         TM    MLSTAT,X'F0'        TEST NUMERIC                                 
         BO    STAHKX              YES, THEN EXIT                               
*                                                                               
         MVC   0(5,R6),MLSTAT      MOVE STATION TO LIST                         
         LA    R6,5(R6)            BUMP PIONTER                                 
         AI    COUNTER,1           INCREMENT STATION COUNTER                    
*                                                                               
STAHKX   L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*        CALC                     -CALCULATES TIME TABLE FOR DEMAND             
*                                 -SETS LOOP COUNTER FOR DEMROUT CALL           
*                                                                               
CALC     NTR1                                                                   
         CLI   TOTOPT,C'Y'         TEST TOTAL OPTION                            
         BE    XIT                                                              
*                                                                               
         MVC   DBSELTIM,TIMSAVE                                                 
*                                                                               
         LA    R6,WORK                                                          
         XR    R4,R4                                                            
         ICM   R4,3,DBSELTIM       LOAD START TIME                              
         BAS   RE,GETMIN                                                        
         MVC   HRNUMS,0(R6)        #OF HOURS,START                              
         MVC   MINNUMS,2(R6)       #OF MIN,START                                
         MVC   MINTIMS,4(R6)       START TIME IN MINUTES                        
*                                                                               
         ICM   R4,3,DBSELTIM+2     LOAD END TIME                                
         BAS   RE,GETMIN                                                        
         MVC   HRNUME,0(R6)        #OF HOURS,END                                
         MVC   MINNUME,2(R6)       #OF MIN,END                                  
         MVC   MINTIME,4(R6)       END TIME IN MINUTES                          
*                                                                               
         LH    R4,MINTIMS                                                       
         LH    R5,MINTIME                                                       
         SR    R5,R4                                                            
*                                                                               
         LTR   R5,R5               TEST FOR WRAP-AROUND                         
         BNM   CALC15                                                           
         LH    R5,MINTIME          FUDGE # FOR LOOP COUNTER                     
         XR    R6,R6                                                            
         LH    R6,=Y(24*60)                                                     
         SR    R6,R4                                                            
         AR    R5,R6                                                            
*                                                                               
CALC15   CLI   HLFINC,C'Y'                                                      
         BNE   CALC20                                                           
         AH    R5,=H'29'           ROUND TO NEAREST HALF HR                     
         SR    R4,R4                                                            
         D     R4,=F'30'                                                        
         B     CALC30              R5=#HALF HR UNITS                            
*                                                                               
CALC20   AH    R5,=H'59'           ROUND TO NEAREST HR                          
         SR    R4,R4                                                            
         D     R4,=F'60'           R5=#HOUR UNITS                               
*                                                                               
CALC30   STCM  R5,1,HRUNITS        LOOP COUNTER FOR DEMROUT                     
*                                                                               
         LH    R4,HRNUMS           CALC 48 UNIT DAY                             
         MH    R4,=H'2'                                                         
         LH    R5,MINNUMS                                                       
         CH    R5,=H'30'                                                        
         BL    *+8                                                              
         LA    R4,1(R4)                                                         
         STH   R4,STUNIT           START UNIT                                   
*                                                                               
         LH    R4,HRNUME                                                        
         MH    R4,=H'2'                                                         
         LH    R5,MINNUME                                                       
         CH    R5,=H'30'                                                        
         BL    *+8                                                              
         LA    R4,1(R4)                                                         
         STH   R4,ENUNIT           END UNIT                                     
*                                                                               
         LH    R4,STUNIT           BUILD TIME PERIOD TABLE                      
         LA    R6,TIMETAB                                                       
         STH   R4,0(R6)                                                         
         LA    R6,2(R6)                                                         
*                                                                               
CALC40   LA    R4,1(R4)                                                         
         CLI   HLFINC,C'Y'                                                      
         BE    *+8                                                              
         LA    R4,1(R4)                                                         
         CH    R4,ENUNIT                                                        
         BNL   CALC60                                                           
*                                                                               
         STH   R4,0(R6)                                                         
         STH   R4,2(R6)                                                         
         LA    R6,4(R6)                                                         
         B     CALC40                                                           
*                                                                               
CALC60   MVC   0(2,R6),ENUNIT                                                   
         BAS   RE,FIXTIME                                                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE CONVERTS START/END TIMES TO MINUTES                      
*              STORES CONVERTED NUMBERS IN WORK                                 
*                                                                               
GETMIN   NTR1                                                                   
         SRDL  R4,32                                                            
         D     R4,=F'100'                                                       
         STH   R5,0(R6)            STORE HOUR#                                  
         STH   R4,2(R6)            STORE REMAIDER=MINUTES                       
         MH    R5,=H'60'                                                        
         AR    R5,R4                                                            
         STH   R5,4(R6)            STORE #MINUTES                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              CONVERTS 48 UNITS/DAY TO MILITARY TIME                           
*              STORES CONVERTED TIMES IN TIMETAB                                
*                                                                               
FIXTIME  NTR1                                                                   
         ZIC   R0,HRUNITS                                                       
         MH    R0,=H'2'            SET LOOP COUNTER                             
*                                                                               
         LA    R6,TIMETAB                                                       
*                                                                               
FIX10    LH    R5,0(R6)            ENTER FIRST TIMETAB ENTRY                    
         XR    R4,R4                                                            
         D     R4,=F'2'                                                         
         MH    R5,=H'100'                                                       
         LTR   R4,R4               TEST FOR HALF HOUR                           
         BZ    *+8                                                              
         AH    R5,=H'30'                                                        
         STH   R5,0(R6)            REPLACE IN TIMETAB                           
         LA    R6,2(R6)            NEXT TIMETAB ENTRY                           
         BCT   R0,FIX10                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*ROUTINE TO WRITE TABLE TO SAVE STORAGE IN BETWEEN TRANSACTIONS                 
*                                                                               
PUTSAVE  NTR1                                                                   
         LA    R0,2                LOOP COUNTER                                 
         LA    R2,2                R2=TWA START PAGE NUMBER                     
         SR    R3,R3                                                            
         ICM   R3,3,TERM           R3=TERMINAL NUMBER                           
         LA    R4,BUFF             R4=I/O ADDRESS                               
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=Y(4000)                                              
PUTSAVE2 GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',((R2),(R3)),(R4),,            
         LA    R4,4000(R4)         BUMP TO NEXT RECORD ADDRESS                  
         LA    R2,1(R2)            BUMP PAGE NUMBER                             
         BCT   R0,PUTSAVE2         DO FOR NUMBER OF TWA'S                       
         B     XIT                                                              
*                                                                               
*ROUTINE TO READ TABLE FROM SAVE STORAGE AFTER TRANSACTIONS                     
*                                                                               
*                                                                               
GETSAVE  NTR1                                                                   
         LA    R0,2                LOOP COUNTER                                 
         LA    R2,2                R2=TWA START PAGE NUMBER                     
         SR    R3,R3                                                            
         ICM   R3,3,TERM           R3=TERMINAL NUMBER                           
         LA    R4,BUFF             R4=I/O ADDRESS                               
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=Y(4000)                                              
GETSAVE2 GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',((R2),(R3)),(R4),,           
         LA    R4,4000(R4)         BUMP TO NEXT RECORD ADDRESS                  
         LA    R2,1(R2)            BUMP PAGE NUMBER                             
         BCT   R0,GETSAVE2         DO FOR LEN OF BUFF (8K)                      
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*                               VALIDATE STATION EXPRESSION ON SCREEN           
VALSTA   NTR1                                                                   
         MVI   ERROR,INVALID                                                    
         XC    ACTSTAT,ACTSTAT                                                  
*                                                                               
         XC    BLOCK(100),BLOCK                                                 
         LA    R4,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(10,(R4)),C',=,-'                              
*                                                                               
STA10    DS    0H                                                               
         CLI   0(R4),0             TEST END OF LIST                             
         BE    XIT                                                              
         CLI   0(R4),3                                                          
         BL    EDTERR                                                           
         CLI   0(R4),4                                                          
         BH    EDTERR                                                           
         TM    2(R4),X'40'         TEST ALPHA (MSPACK REQUIREMENT)              
         BZ    EDTERR                                                           
         MVC   ACTSTAT(4),12(R4)                                                
*                                                                               
         CLI   1(R4),2                                                          
         BH    EDTERR                                                           
         CLI   22(R4),C' '                                                      
         BNE   *+14                                                             
         MVC   22(2,R4),=C'T '     SET FOR THE TELE                             
         MVI   1(R4),X'01'         FUDGE THE SCANNER BLOCK                      
         MVC   ACTSTAT+4(1),22(R4)                                              
         ZIC   R5,1(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,STAAM                                                         
         BE    STA20                                                            
         EX    R5,STAFM                                                         
         BE    STA20                                                            
         EX    R5,STACO                                                         
         BE    STA20                                                            
         EX    R5,STAT                                                          
         BE    STA20                                                            
         B     EDTERR                                                           
STAAM    CLC   22(0,R4),=C'AM'                                                  
STAFM    CLC   22(0,R4),=C'FM'                                                  
STACO    CLC   22(0,R4),=C'CO'                                                  
STAT     CLC   22(0,R4),=C'T '                                                  
         SPACE 1                                                                
* VALIDATE STATION CALL LETTERS EXIST ON DEMO FILE *                            
*                                                                               
STA20    DS    0H                  VAL STATIONS ON DEMO FILE                    
         MVI   DBFUNCT,DBVLST                                                   
         MVC   DBAREC,AIO1                                                      
         MVC   DBSELSTA,ACTSTAT    VALIDATE CALL LETTERS                        
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         CLI   DBERROR,0                                                        
         BNE   STA40                                                            
*                                                                               
         L     R6,AIO2             BUILD LIST IN SECOND IO                      
         ZIC   R0,COUNTER                                                       
         MH    R0,=H'5'                                                         
         AR    R6,R0               INDEX INTO LIST                              
         MVC   0(5,R6),ACTSTAT                                                  
         AI    COUNTER,1           STATION COUNTER                              
         LA    R6,5(R6)            BUMP LIST POINTER                            
         LA    R4,32(R4)           BUMP SCANNER TABLE                           
         B     STA10                                                            
*                                                                               
STA40    MVI   ERROR,INVSTAT                                                    
         B     EDTERR                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
*              VALIDATES MARKET ON DEMO FILE                                    
VALMKT   NTR1                                                                   
         GOTO1 ANY                                                              
         MVI   ERROR,NOTNUM                                                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   WORK(6),=6X'F0'                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)                                                    
         CLC   WORK(6),=6X'F0'                                                  
         BNE   EDTERR                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BNP   EDTERR                                                           
         ST    R1,FULL                                                          
         MVC   SVMKT,FULL+2        SAVE MKT NUMBER                              
         SPACE 1                                                                
* READ FOR MARKET NUMBER ON DEMO FILE *                                         
         SPACE 1                                                                
VAL5     DS    0H                                                               
         MVI   DBFUNCT,DBGETMK     VALIDATES MARKET                             
         MVC   DBAREC,AIO2         SET ADDRESS OF OUTPUT                        
         MVC   DBSELRMK,SVMKT                                                   
         MVC   DBSELBK,BOOK                                                     
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         CLI   DBERROR,0           IS VALID MARKET                              
         BNE   VAL10               YES,THEN EXIT                                
         GOTO1 DEFINE,DMCB,=C'MNAME',DBLOCK,WORK                                
         CLC   WORK+2(9),=C'**UNKNOWN'            IS MKT VALID                  
         BE    VAL10                                                            
         B     XIT                                                              
*                                                                               
VAL10    MVI   ERROR,INVMKT                                                     
         B     EDTERR                                                           
         EJECT                                                                  
*                                                                               
*                                                                               
*                                  VALIDATES DAY/TIME ENTRIES                   
*                                  FOR TELEVISION                               
*                                                                               
*                                                                               
VALDYTM  NTR1                                                                   
*                                                                               
         XC    BLOCK(100),BLOCK                                                 
         XC    DYTMLST,DYTMLST                                                  
         LA    R7,DYTMLST                                                       
         MVI   SCANLEN,32                                                       
         GOTO1 SCANNER,DMCB,(R2),(15,BLOCK),0                                   
*                                                                               
         LA    R4,BLOCK                                                         
         XC    BYTE,BYTE                                                        
*                                                                               
DYTM10   DS    0H                                                               
         ZIC   R0,0(R4)                                                         
         GOTO1 DAYVAL,DMCB,((R0),12(R4)),BYTE,WORK                              
         CLI   RADMED,C'Y'         TEST RADIO MEDIA                             
         BNE   *+12                                                             
         CLI   BYTE,0              NON-ZERO IS VALID INPUT                      
         BNE   DYTM40              RADIO MUST BRANCH HERE                       
*                                                                               
         CLI   BYTE,0                                                           
         BNE   DYTM60              NON-ZERO MEANS VALID INPUT                   
         SPACE 1                                                                
* CODE HERE TO HANDLE SPECIAL SITUATIONS                                        
         SPACE 1                                                                
         CLC   12(4,R4),=CL4'+W/E'   TEST SPECIAL WEEKEND CODE                  
         BNE   DYTM15                                                           
         MVC   0(6,R7),=X'020618010618'   SLOPPY TECHNIQUE,I KNOW.              
         LA    R7,6(R7)                   ANY SUGGESTIONS...                    
         LA    R4,32(R4)                                                        
         B     DYTM100                                                          
*                                                                               
DYTM15   LA    R1,SPCLDAY                                                       
         LA    R0,SPCLDAYL                                                      
*                                                                               
DYTM20   CLC   12(0,R4),1(R1)                                                   
         BNE   DYTM30                                                           
         MVC   BYTE,0(R1)          SET VALID INPUT                              
         CLI   RADMED,C'Y'         TEST RADIO MEDIA                             
         BE    DYTM40              RADIO MUST BRANCH HERE                       
         B     DYTM60                                                           
*                                                                               
DYTM30   LA    R1,1(R1)                                                         
         BCT   R0,DYTM20                                                        
         B     SCANERR                                                          
*                                                                               
SPCLDAY  DS    0CL6                                                             
         DC    X'03',CL5'S-S  '    DAY VALUE/INPUT STRING                       
         DC    X'03',CL5'SA-S '                                                 
         DC    X'03',CL5'S-SU '                                                 
         DC    X'7F',CL5'M-S  '                                                 
SPCLDAYL EQU   *-SPCLDAY                                                        
         EJECT                                                                  
*                                                                               
*              SPECIAL CODE TO VALIDATE RADIO DAYPARTS                          
*              ONLY CERTAIN DAYS VALID AS STANDARD RADIO DPTS                   
*                                                                               
*                                                                               
*                                                                               
DYTM40   LA    R1,DAYLIST                                                       
         LA    R0,DAYLISTL                                                      
*                                                                               
DYTM50   CLC   BYTE,0(R1)                                                       
         BE    DYTM60                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,DYTM50                                                        
         B     SCANERR                                                          
*                                                                               
DAYLIST  DC    X'7C'               MO-FR                                        
         DC    X'7E'               MO-SA                                        
         DC    X'7F'               MO-SU                                        
         DC    X'03'               SA-SU                                        
         DC    X'02'               SAT                                          
         DC    X'01'               SUN                                          
DAYLISTL EQU   *-DAYLIST                                                        
         EJECT                                                                  
*                                                                               
*                                                                               
DYTM60   MVC   0(1,R7),BYTE        MOVE DAY TO LIST                             
*                                                                               
         LA    R4,32(R4)           NEXT SCANNER ENTRY                           
*                                                                               
         CLI   0(R4),0                                                          
         BNE   DYTM70                                                           
         MVI   ERROR,NOTIME        TELL USER TIME IS MISSING                    
         B     EDTERR                                                           
*                                                                               
DYTM70   DS    0H                                                               
         BAS   RE,VALTIME                                                       
         BE    DYTM80                                                           
         CLI   RADMED,C'Y'         TEST RADIO                                   
         BNE   SCANERR                                                          
         CLC   12(5,R4),=CL5'DRIVE'         TEST SPECIAL RADIO DAYPART          
         BNE   SCANERR                                                          
         LA    R7,DYTMLST                                                       
         MVC   0(6,R7),=X'7C060A7C0F13'       TSK,TSK(AGAIN)                    
         LA    R7,6(R7)                                                         
         LA    R4,32(R4)           TEST NEXT SCAN AREA                          
*                                                                               
DYTM80   MVC   1(2,R7),HALF        MOVE TIME TO LIST                            
*                                                                               
         LA    R7,3(R7)            ADVANCE DAY/TIME LIST POINTER                
*                                                                               
         LA    R4,32(R4)                                                        
         CLI   RADMED,C'Y'         IS RADIO THE MEDIA                           
         BE    DYTM100             YES, THEN BRANCH TO CONTINUE LIST            
*                                                                               
         CLI   0(R4),0             TEST ANY MORE INPUT THIS FIELD               
         BE    DYTMX                                                            
*                                                                               
         MVI   ERROR,TOOMANY                                                    
         BH    EDTERR                                                           
*                                                                               
DYTM100  CLI   0(R4),0             SEE ANY INPUT                                
         BE    DYTM120                                                          
*                                                                               
         BAS   RE,VALTIME          VALIDATE AS ANOTHER TIME                     
         BNE   DYTM10              IF INVALID, TRY FOR ANOTHER DAY              
*                                                                               
         LR    RE,R7                                                            
         SH    RE,=H'3'                                                         
         MVC   0(1,R7),0(RE)       COPY DAY FROM PREVIOUS                       
         B     DYTM80                                                           
*                                                                               
DYTM120  DS    0H                                                               
*              ROUTINE FIGURES OUT WHETHER DAYPART IS STANDARD                  
         LA    R7,DYTMLST                                                       
*                                                                               
DYTM140  DS    0H                  FOR EACH DAYPART IN LIST                     
         LA    R2,STANDPTS         LOOK UP STANDARD DAYPARTS                    
         USING STANDENT,R2                                                      
*                                                                               
DYTM150  CLI   0(R2),X'FF'         IF NOT MATCH IS FOUND                        
         BNE   *+16                                                             
         MVI   ERROR,INVDPT                                                     
         LA    R2,CMPDPTH                                                       
         B     EDTERR                                                           
*                                                                               
         CLC   STANDDTS(15),0(R7)  LOOK FOR A MATCH ON DAY/TIMES                
         BNE   DYTM160                                                          
         MVC   PROGCD,STANDPRG    ALL WE NEED IS THE PROGRAM CODE               
         B     DYTMX                                                            
*                                                                               
DYTM160  LA    R2,L'STANDENT(R2)                                                
         B     DYTM150                                                          
         DROP  R2                                                               
*                                                                               
DYTMX    B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                  VALIDATES TIME IN DAY/TIME ENTRY             
*                                                                               
VALTIME  NTR1                                                                   
         ZIC   R0,0(R4)                                                         
         GOTO1 TIMVAL,DMCB,((R0),12(R4)),FULL                                   
         CLI   0(R1),X'FF'         TEST TIME INVALID                            
         BNE   VALT10                                                           
         MVI   ERROR,INVTIME                                                    
         B     NEQXIT                                                           
*                                                                               
VALT10   CLI   RADMED,C'Y'         SEE IF RADIO MEDIA                           
         BNE   VALT30              TELE NEEDN'T BE ON THE HOUR                  
*                                                                               
VALT20   MVI   ERROR,BADSTTIM                                                   
         LH    R0,FULL                                                          
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LTR   R0,R0               TIME MUST BE HOUR ONLY                       
         BNZ   EDTERR                                                           
         STC   R1,HALF             SET START HOUR                               
*                                                                               
         MVI   ERROR,BADNDTIM                                                   
         SR    R0,R0                                                            
         ICM   R0,3,FULL+2                                                      
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LTR   R0,R0                                                            
         BNZ   EDTERR                                                           
         STC   R1,HALF+1           SET END HOUR                                 
*                                                                               
VALT30   MVC   TIMSAVE,FULL       SET MILITARY B/E TIMES                        
*                                                                               
         B     EQXIT                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
* VALIDATE RADIO DEMO EXPRESSIONS                                               
         SPACE 1                                                                
*              INPUT COMBINATIONS: AREA, DEMO, DEMO, DEMO, ETC                  
*                                                                               
*              OUTPUT IS 3-BYTE DEMO CODE : AREA-MODIFIER-DEMO#                 
*                                                                               
         SPACE 1                                                                
RADDEM   NTR1                                                                   
         MVI   SCANLEN,32                                                       
         GOTO1 SCANNER,PARAS,(R2),(10,BLOCK),0                                  
         LA    R4,BLOCK                                                         
         LA    R5,DEMOS                                                         
*                                                                               
RDEM10   LA    R6,CATLIST                                                       
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
*                                                                               
RDEM20   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R6),12(R4)                                                   
         BE    RDEM30                                                           
         LA    R6,12(R6)                                                        
         CLI   0(R6),X'FF'                                                      
         BNE   RDEM20                                                           
         B     BADCAT                                                           
*                                                                               
RDEM30   DS    0H                                                               
         MVC   AREANM,6(R6)        AREA NUMBER CODE                             
*                                  AREA CODE FOUND,REST MUST BE DEMOS           
         LA    R4,32(R4)           VALIDATE DEMOS FOLLOWING AREA NAME           
*                                                                               
         ZIC   R0,PARAS+4                                                       
         BCTR  R0,R0               COMPENSATE FOR AREA FIELD                    
         LTR   R0,R0                                                            
         BZ    BADDEM              IF ZERO, THEN NO DEMOS INPUT                 
         STC   R0,NDEMOS                                                        
*                                                                               
         CLC   12(2,R4),=C'M '     MENU OPTION                                  
         BE    RDEM110                                                          
         CLC   12(2,R4),=C'L '     LIST OPTION                                  
         BE    RDEM69                                                           
*                                                                               
RDEM40   DS    0H                                                               
         LA    R6,SPRDEM           POINT TO SPECIAL RADIO DEMO                  
         USING DEMSP,R6                                                         
*                                                                               
         ZIC   R1,0(R4)            LENGTH OF SCANNER ENTRY                      
         BCTR  R1,0                                                             
*                                                                               
RDEM50   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R6),12(R4)                                                   
         BE    RDEM60                                                           
         LA    R6,L'DEMENT(R6)     BUMP TO NEXT TABLE ENTRY                     
         CLI   0(R6),X'FF'                                                      
         BNE   RDEM50                                                           
         B     RDEM90                                                           
*                                                                               
RDEM60   LA    R5,DEMOS                                                         
         MVC   0(L'DEMLIST,R5),DEMLIST                                          
         MVC   NDEMOS,DEMNB                                                     
         B     RDEM200                                                          
         DROP  R6                                                               
*                                                                               
RDEM69   XC    WORK,WORK                                                        
         ZIC   R1,1(R4)                                                         
         STC   R1,WORK+5                                                        
         MVC   WORK+8(10),22(R4)                                                
         BAS   RE,VONEDEM                                                       
         BNE   BADDEM                                                           
         LA    R1,TYPLIST                                                       
         LA    R6,DEMOS                                                         
         LA    R0,6                                                             
         MVI   NDEMOS,6                                                         
         CLI   DBSELMED,C'C'       CANADIAN LIST                                
         BNE   RDEM70                                                           
         LA    R1,CANLIST                                                       
         LA    R0,3                                                             
         MVI   NDEMOS,3                                                         
         SPACE 1                                                                
RDEM70   MVC   0(3,R6),WORK                                                     
         MVC   1(1,R6),0(R1)                                                    
         LA    R1,1(R1)                                                         
         LA    R6,3(R6)                                                         
         BCT   R0,RDEM70                                                        
         CLI   BLOCK+32,0          SECOND CANADIAN LIST                         
         BE    RDEM200                                                          
         CLI   DBSELMED,C'C'                                                    
         BNE   BADDEM                                                           
         XC    WORK,WORK                                                        
         LA    R6,BLOCK+32                                                      
         ZIC   R1,0(R6)                                                         
         STC   R1,WORK+5                                                        
         MVC   WORK+8(10),12(R6)                                                
         BAS   RE,VONEDEM                                                       
         BNE   BADDEM                                                           
         LA    R6,DEMOS+9                                                       
         MVI   NDEMOS,6                                                         
         LA    R1,CANLIST                                                       
         LA    R0,3                                                             
         SPACE 1                                                                
RDEM80   MVC   0(3,R6),WORK                                                     
         MVC   1(1,R6),0(R1)                                                    
         LA    R1,1(R1)                                                         
         LA    R6,3(R6)                                                         
         BCT   R0,RDEM80                                                        
         B     RDEM200                                                          
         SPACE 1                                                                
TYPLIST  DC    C'RTPXSQ'                                                        
CANLIST  DC    C'REI'                                                           
         SPACE 1                                                                
RDEM90   MVC   0(3,R5),=X'20C9FF'  SPECIAL COST HANDLING                        
         CLC   12(4,R4),=C'COST'                                                
         BE    RDEMEND                                                          
         CLC   12(2,R4),=C'$ '                                                  
         BE    RDEMEND                                                          
         XC    WORK,WORK           BUILD A SINGLE HEADER                        
         ZIC   R1,0(R4)                                                         
         MVC   WORK+8(10),12(R4)                                                
         CLI   12(R4),C'$'                                                      
         BNE   RDEM100                                                          
         BCTR  R1,0                                                             
         MVC   WORK+8(9),WORK+9                                                 
*                                                                               
RDEM100  STC   R1,WORK+5                                                        
         BAS   RE,VONEDEM                                                       
         BNE   BADDEM                                                           
         MVC   0(3,R5),WORK                                                     
         CLI   12(R4),C'$'                                                      
         BNE   *+8                                                              
         OI    0(R5),X'20'                                                      
*                                                                               
RDEMEND  LA    R5,3(R5)                                                         
         LA    R4,32(R4)                                                        
         BCT   R0,RDEM90                                                        
         B     RDEM200                                                          
         SPACE 1                                                                
VONEDEM  NTR1                                                                   
         MVI   WORK,18             SET HEADER LENGTH                            
         LA    R5,IO                  PHONY EST HEADER                          
         GOTO1 DEMOVAL,PARAS,WORK,(1,WORK+20),(C'S',DBLOCK),(R5)                
         MVC   WORK(3),WORK+20                                                  
         CLI   4(R1),0                                                          
         BE    NO                                                               
         B     EQXIT                                                            
*                                                                               
SPRDEM   DS    0C                                                               
         DC    C'MEN  ',X'00C9BA00C95A00C96000C96500C96900C96C00C96E'           
         DC    AL1(07)                                                          
         DC    C'WOMEN',X'00C91900C92800C92E00C93300C93700C93A00C93C'           
         DC    AL1(07)                                                          
         DC    C'TOTAL',X'00C97D00C98C00C99200C99700C99B00C99E00C9A0'           
         DC    AL1(07)                                                          
         DC    C'MIXED',X'00C97D00C95B00C96600C96D00C92900C93400C93B'           
         DC    AL1(07)                                                          
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
* VALIDATE DEMO LIST NAME *                                                     
         SPACE 1                                                                
RDEM110  DS    0H                                                               
         MVI   ERROR,BADMENU                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D26'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(4),22(R4)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EDTERR                                                           
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,5                                                         
         LA    R7,10               SET MAX COUNTER                              
         BAS   RE,GETEL                                                         
         BNE   EDTERR                                                           
         B     RDEM130                                                          
*                                                                               
RDEM120  BAS   RE,NEXTEL                                                        
         BNE   RDEM150                                                          
RDEM130  MVC   0(3,R5),2(R6)       MOVE DEMO VALUE                              
         LA    R5,3(R5)                                                         
         BCT   R7,RDEM120                                                       
*                                                                               
RDEM150  LA    R0,10                                                            
         SR    R0,R7                                                            
         STC   R0,NDEMOS                                                        
         B     RDEM200                                                          
*                                                                               
BADDEM   MVI   ERROR,INVDEMO                                                    
         B     EDTERR                                                           
*                                                                               
BADCAT   MVI   ERROR,INVCAT                                                     
         B     EDTERR                                                           
*                                                                               
RDEM200  ZIC   R0,NDEMOS                                                        
         LA    R5,DEMOS            POINT TO DEMOS                               
         MVC   0(1,R5),AREANM      INSERT AREA CODE                             
         LA    R5,3(R5)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         B     XIT                                                              
*                                  AREAS                                        
CATLIST  DC    C'MSA   ',AL1(1),C'     '                                        
         DC    C'TSA   ',AL1(2),C'     '                                        
         DC    C'ADI   ',AL1(3),C'     '                                        
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                  ROUTINE TO EDIT OPTIONS FIELD                
EDTOPT   NTR1                                                                   
         LA    R0,2                MAX IS TWO OPTIONS                           
         XC    BLOCK(100),BLOCK                                                 
         LA    R4,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(R4),0                                         
*                                                                               
EDOP10   DS    0H                                                               
         CLI   0(R4),0             NO INPUT                                     
         BNE   *+8                                                              
         B     EDTOPTX                                                          
*                                                                               
         CLI   RADMED,C'Y'         TEST IF OPTIONS FOR THE TELE                 
         BE    EDOP30                                                           
*                                  TELE OPTIONS                                 
         TM    2(R4),X'40'         AVERAGE OPTION                               
         BZ    EDOP20                                                           
         CLC   12(3,R4),=C'TOT'     TEST USER WANTS AVG                         
         BNE   EDOP50                                                           
         MVI   TOTOPT,C'Y'         SET TOTAL SWITCH                             
         B     EDOP40                                                           
*                                                                               
EDOP20   TM    2(R4),X'80'         DATA INCREMENT OPTION                        
         BZ    EDOP50                                                           
         CLC   12(2,R4),=C'30'                                                  
         BNE   *+12                                                             
         MVI   HLFINC,C'Y'                                                      
         B     EDOP40                                                           
         CLC   12(2,R4),=C'60'                                                  
         BNE   EDOP50                                                           
         MVI   HRINC,C'Y'          DATA IN HOUR INCREMENTS                      
         B     EDOP40                                                           
*                                  VALIDATE RADIO OPTIONS                       
EDOP30   CLI   12(R4),C'N'                                                      
         BNE   EDOP35                                                           
         TM    3(R4),X'80'                                                      
         BZ    EDOP50                                                           
         ZIC   RE,1(R4)            LENGTH IN RE                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,22(0,R4)        PACK # STATIONS REQUESTED                    
         CVB   RE,DUB                                                           
         STH   RE,NUMST                                                         
         B     EDOP40                                                           
*                                                                               
EDOP35   DS    0H                                                               
         CLI   12(R4),C'R'         TEST RANK OPTION                             
         BNE   EDOP50                                                           
         TM    3(R4),X'80'         TEST 2ND HALF NUMERIC                        
         BZ    EDOP50                                                           
         ZIC   RE,1(R4)            LENGTH IN RE                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,22(0,R4)        PACK RANKING DEMO#                           
         CVB   RE,DUB                                                           
         STCM  RE,1,RANKN                                                       
         CLC   RANKN(1),NDEMOS                                                  
         BH    EDOP50              RANK# L.T. #DEMOS                            
*                                                                               
EDOP40   LA    R4,32(R4)                                                        
         BCT   R0,EDOP10                                                        
         B     EDTOPTX                                                          
*                                                                               
EDOP50   MVI   ERROR,INVOPT                                                     
         B     EDTERR                                                           
*                                                                               
EDTOPTX  B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
HOOK     NTR1                      HEADLINE ROUTINES                            
         CLI   HDHOOKOK,C'N'       TEST OK TO DO HEADHOOK                       
         BE    HOOKX                                                            
*                                                                               
         MVI   H3,0                PRINT BLANK LINES                            
         MVI   H4,0                                                             
         MVI   H5,0                                                             
*                                                                               
         LA    R5,H6                                                            
         USING PRLINE,R5                                                        
         MVC   PRLINE+1(4),=C'STAT'             INSERT TABLE HEADLINES          
         MVC   PRPROG+2(8),=C'DAY/TIME'                                         
*                                                                               
         L     R6,AIO2                                                          
         LA    R6,500(R6)                                                       
*                                                                               
         XC    DBLOCK,DBLOCK                                                    
         LA    R2,CMPSRCH                                                       
         GOTO1 VVALSRC             REINITIALIZE DBLOCK                          
*                                                                               
         GOTO1 DEMOCON,DMCB,(NDEMOS,DEMOS),(5,(R6)),DBLOCK                      
*                                                                               
         ZIC   R0,NDEMOS                                                        
         LA    R7,PRPER                                                         
         USING PRPER,R7                                                         
*                                                                               
HOOK10   MVC   PRPER(L'PRPER),0(R6)                                             
         LA    R7,PRPLEN(R7)                                                    
         LA    R6,10(R6)                                                        
         BCT   R0,HOOK10                                                        
*                                                                               
         MVC   PRPER(5),=C'TOTAL'                                               
*                                                                               
         OC    ABOX,ABOX                                                        
         BZ    HOOKX                                                            
*                                                                               
         L     R4,ABOX             ADDRESS OF BOX DSECT                         
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+6,C'M'                                                   
         MVI   BOXROWS+59,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+6,C'C'                                                   
         MVI   BOXCOLS+21,C'C'                                                  
         MVI   BOXCOLS+25,C'C'                                                  
*                                                                               
         ZIC   R0,NDEMOS                                                        
         LA    R1,BOXCOLS+25                                                    
         LA    R1,PRPLEN(R1)            BUMP TO NEXT COLUMN                     
         MVI   0(R1),C'C'                                                       
         BCT   R0,*-8                                                           
*                                                                               
         LA    R1,PRPLEN(R1)            BUMP TO NEXT COLUMN                     
         MVI   0(R1),C'R'          TOTAL COLUMN                                 
*                                                                               
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  R4,R5,R7                                                         
*                                                                               
HOOKX    B     XIT                                                              
*                                                                               
HEDSPECS SSPEC H1,2,C'MEDIA      SPOT RESEARCH'                                 
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,53,C'AUDIENCE COMPOSITION REPORT'                             
         SSPEC H2,53,C'---------------------------'                             
         SSPEC H1,102,RUN                                                       
         SSPEC H2,102,REPORT                                                    
         SSPEC H2,120,PAGE                                                      
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 VGETERR                                                          
EDTERR   EQU   TRAPERR                                                          
*                                                                               
SCANERR  MVI   ERROR,INVALID                                                    
         GOTO1 VCURSERR                                                         
*                                                                               
*                                                                               
BUMP     SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             TEST E-O-S                                   
         BR    RE                                                               
         SPACE 1                                                                
         DS    0H                                                               
PATCH    DC    XL32'00'                                                         
         LTORG                                                                  
       ++INCLUDE RADSDPL                                                        
         EJECT                                                                  
*              DSECT TO COVER ENTRY IN STANDARD DAYPART TABLE                   
         SPACE 1                                                                
STAND    DSECT                                                                  
STANDENT DS    0CL36                                                            
STANDDTS DS    XL15                UP TO 15 DAY/HOUR/HOUR                       
STANDPRG DS    CL1                 'PROGRAM' CODE                               
STANDESC DS    CL20                DESCRIPTION                                  
         EJECT                                                                  
       ++INCLUDE SPRESWORKD                                                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESC4D                                                       
* SPDEMLK                                                                       
         PRINT OFF                                                              
SPDEMLKD DSECT                                                                  
       ++INCLUDE SPDEMLK                                                        
         PRINT ON                                                               
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* WORK AREA                                                                     
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK                                                           
BOOK     DS    XL2                                                              
*                                                                               
POSTLINE DS    CL80                                                             
DEMVAL   DS    XL80                DEMO VALUES                                  
TIMETAB  DS    XL100               TIME PERIODS FOR DEMAND                      
DYTMLST  DS    CL30                LIST OF DAY/TIMES                            
ENDDYTM  EQU   *                                                                
*                                                                               
TOTOPT   DS    C                   TOTAL OPTION FLAG                            
HDHOOKOK DS    C                                                                
HRINC    DS    C                   HOUR INCREMENT FLAG                          
HLFINC   DS    C                   HALF HOUR FLAG                               
RADMED   DS    C                   MEDIA=RADIO FLAG                             
NOOPT    DS    C                   FLAG FOR OPTIONS                             
GIVENMK  DS    C                   FLAG IF MARKET REQUESTED                     
ENDCALL  DS    C                   FLAG FOR PERIOD AVG                          
ENDTAB   DS    C                   FLAG FOR TABLE END                           
COUNTER  DS    CL1                                                              
PROGCD   DS    CL1                 DAYPART CODE FOR DEMAND                      
AREANM   DS    CL1                 RADIO DEMO AREA NUMBER                       
RANKN    DS    CL1                 RANKING DEMO NUMBER                          
HRUNITS  DS    CL1                                                              
THISSTA  DS    CL5                 CURRENT STATION                              
ENTLEN   DS    H                   ENTRY LENGTH IN BUFFER TAB                   
NUMST    DS    H                                                                
LENTOTOT DS    H                   DISP TO TOTAL COLUMN                         
SVMKT    DS    H                   SAVE MKT NUMBER                              
HRNUMS   DS    H                   NUMBER OF HOURS(START)                       
MINNUMS  DS    H                   NUMBER OF MIN(START)                         
MINTIMS  DS    H                   START TIME IN MINUTES                        
HRNUME   DS    H                   NUMBER OF HOURS(END)                         
MINNUME  DS    H                   NUMBER OF MIN(END)                           
MINTIME  DS    H                   END TIME IN MINUTES                          
STUNIT   DS    H                   START UNIT                                   
ENUNIT   DS    H                   END UNIT                                     
TIMSAVE  DS    F                   SAVED DBSELTIM VALUE                         
TIMEPD   DS    F                                                                
SAVTIME  DS    F                                                                
PREVPROG DS    CL14                PREV PROGRAM NAME                            
         DS    CL(L'OVWORK-(*-OVWORK))           SPARE                          
*                                                                               
T20FFFD  DSECT                                                                  
         ORG   CONHEAD+3520-256                                                 
*                                                                               
SVTBLNDX DS    F                                                                
SVTBLSIZ DS    F                                                                
LISTN    DS    CL1                                                              
LSTCNT   DS    CL1                                                              
         DS    CL246               SPARE                                        
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*DSECT FOR DEMO TABLE                                                           
*                                                                               
DEMSP    DSECT                                                                  
DEMENT   DS    0CL27               LENGTH OF TABLE ENTRY                        
DEMWRD   DS    CL5                 KEYWORD FOR DEMO LIST                        
DEMLIST  DS    XL21                DEMO LIST                                    
DEMNB    DS    CL1                 NUMBER OF DEMOS                              
*                                                                               
*                                                                               
*DSECT FOR BUFFER TABLE                                                         
*                                                                               
BUFTABD  DSECT                                                                  
         ORG                                                                    
PROG     DS    CL14                PROGRAM NAME                                 
TIME     DS    CL14                DAY/TIME DATA                                
DEMO     DS    CL4                 IMP                                          
PER      DS    CL2                 PERCENT OF IMP                               
DEMPERLN EQU   *-DEMO                                                           
*                                                                               
*                                                                               
*DSECT TO COVER PRINT LINE                                                      
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PRLNE    DS    CL1                                                              
PRSTA    DS    CL5                 STATION                                      
         DS    CL1                                                              
PRDATI   DS    CL14                DAY/TIME                                     
         DS    CL1                                                              
PRIMP    DS    CL3                 IMPRESSION                                   
         DS    CL1                                                              
PRDEM    DS    CL5                 DEMO                                         
         DS    CL1                                                              
PRDLEN   EQU   *-PRDEM             LENGTH OF DEMO ELEMENT                       
*                                                                               
         ORG   P2                                                               
PRLINE   DS    CL7                                                              
PRPROG   DS    CL14                PROGRAM NAME                                 
         DS    CL1                                                              
PRPERN   DS    CL3                 PERCENT NAME                                 
         DS    CL1                                                              
PRPER    DS    CL5                 PERCENT NUMBER                               
         DS    CL1                                                              
PRPLEN   EQU   *-PRPER             LENGTH OF PERCENT                            
*                                                                               
*                                                                               
*DSECT TO COVER LIST LINE                                                       
*                                                                               
LSTLINE  DSECT                                                                  
         DS    CL1                                                              
LSTSTA   DS    CL5                 STATION                                      
         DS    CL1                                                              
LSTDATI  DS    CL14                DAY/TIME                                     
         DS    CL1                                                              
LSTWRD   DS    CL3                 WORD                                         
         DS    CL1                                                              
LSTDAT   DS    CL5                 DATA                                         
         DS    CL1                                                              
LSTDLEN  EQU   *-LSTDAT            LENGTH OF DATA ELEMENT                       
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'179SPRES24   11/12/03'                                      
         END                                                                    
