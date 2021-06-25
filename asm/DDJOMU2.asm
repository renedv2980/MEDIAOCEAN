*          DATA SET DDJOMU2    AT LEVEL 061 AS OF 05/01/02                      
*PHASE EDICTEZR                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        EDICTEZR -- RECEIVE AT&T EASYLINK TRANSMISSIONS      *         
*                                                                     *         
*  CALLED BY:    DDEDICTEZ                                            *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- WORK                                           *         
*                R7 -- S P A R E                                      *         
*                R8 -- CPRINT                                         *         
*                R9 -- PARAMETERS FROM EDICTEZ (VIA R1)               *         
*                RA -- 2ND BASE                                       *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- LOCAL WORKING STORAGE                          *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDEDICTEZR -- RECEIVE AT&&T EASYLINK TRANSMISSIONS'             
         MACRO                                                                  
&NAME    PRNT  &A                                                               
&NAME    MVC   P(18),=CL18'&A'                                                  
         TIME  DEC                                                              
         ST    R0,DUB                                                           
         UNPK  WORK(7),DUB(4)                                                   
         MVC   P+20(2),WORK                                                     
         MVI   P+22,C':'                                                        
         MVC   P+23(2),WORK+2                                                   
         MVI   P+25,C':'                                                        
         MVC   P+26(2),WORK+4                                                   
         GOTO1 =V(PRINTER)                                                      
         MEND                                                                   
         EJECT                                                                  
EDICTEZR CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,EDICTEZR,=A(R13CHAIN),RA                                       
*                                                                               
         ENTRY UTL                 FOR DATAMGR                                  
         ENTRY SSB                                                              
*                                                                               
         LR    R9,RC                                                            
         SH    R9,=H'4'                                                         
         L     R9,0(R9)                                                         
         L     R9,0(R9)            A(R1 PARAMETERS FROM ATTACH)                 
         USING EZPARMD,R9                                                       
*                                                                               
         LA    R0,4                                                             
         LNR   R0,R0                                                            
         SVC   247                 SO EDICTEZR IS NOT SWAPPABLE                 
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=C'EZRTRACE'  DDNAME=EZRTRACE              
         LA    R1,ESTOPECB                                                      
         STCM  R1,7,EZLNKPRM+21    SET A(STOPPING ECB)                          
*                                                                               
         OPEN  (APPCLOGR,OUTPUT)                                                
*                                                                               
         MVC   APPCACTN,APPCACOI   OPEN FOR INPUT                               
         L     RF,EAMYLUID         RF = A(LUID OF RECEIVER FROM AT&T)           
         MVC   APPCDATA(8),0(RF)                                                
*                                                                               
         LA    R1,EZLNKPRM         PARAMETERS TO LU6.2 LINK                     
         LINK  EP=DDSAPPC                                                       
*                                                                               
         MVC   P+30(9),=C'REG 15 = '                                            
         EDIT  (RF),(5,P+39),ALIGN=LEFT,ZERO=NOBLANK                            
         PRNT  OPENEZRECEIVE                                                    
         EJECT                                                                  
         GOTO1 =V(DATCON),DMCB,(5,0),TODAY                                      
*                                                                               
         MVC   WORK(2),TODAY       EBCDIC YEAR                                  
         MVC   WORK+2(4),=C'0331'  START LOOKING AT MARCH 31                    
*                                                                               
DAYLITE5 GOTO1 =V(ADDAY),DMCB,WORK,WORK,F'1'                                    
         GOTO1 =V(GETDAY),DMCB,WORK,WORK+6                                      
         CLI   DMCB,7              IS IT SUNDAY?                                
         BNE   DAYLITE5            NO                                           
*                                                                               
         CLC   TODAY,WORK                                                       
         BL    NEXTMSG             TODAY IS PRIOR TO START OF DST               
*                                                                               
         MVC   WORK(2),TODAY       EBCDIC YEAR                                  
         MVC   WORK+2(4),=C'1101'  START LOOKING AT NOVEMBER 1                  
*                                                                               
DAYLITE7 GOTO1 =V(ADDAY),DMCB,WORK,WORK,F'-1'                                   
         GOTO1 =V(GETDAY),DMCB,WORK,WORK+6                                      
         CLI   DMCB,7              IS IT SUNDAY?                                
         BNE   DAYLITE7            NO                                           
         GOTO1 =V(ADDAY),DMCB,WORK,WORK,F'-1'  SATURDAY IS END OF DST           
*                                                                               
         CLC   TODAY,WORK                                                       
         BH    NEXTMSG             TODAY IS AFTER THE END OF DST                
*                                                                               
         MVI   DSTFLAG,C'Y'        IT'S DAYLIGHT SAVINGS TIME!                  
         PRNT  DAYLIGHTSAVINGS                                                  
         EJECT                                                                  
NEXTMSG  BAS   RE,EZLINRCV         FILL UP AN INPUT BUFFER                      
         BE    CHKSTOP                                                          
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
CHKSTOP  CLI   STOPNOW,C'Y'        DO WE STOP NOW?                              
         BE    GOODBYE             YES                                          
*                                                                               
         LA    R3,EASYBUF                                                       
*                                                                               
         CLC   =C'DLN',0(R3)       DELIVERY NOTIFICATION?                       
         BNE   *+12                NO                                           
         BAS   RE,EZDLN            YES -- POST MESSAGE DELIVERED                
         B     CONFIRM                                                          
*                                                                               
         CLC   =C'CAN',0(R3)       CANCELLATION NOTIFICATION?                   
         BNE   *+12                NO                                           
         BAS   RE,EZCAN            YES  -- POST MESSAGE CANCELLED               
         B     CONFIRM                                                          
*                                                                               
         CLC   =C'INB',0(R3)       INBOUND MESSAGE?                             
         BNE   *+12                NO -- IGNORE THIS MESSAGE                    
         BAS   RE,EZINB            YES -- WRITE IT TO PRTQUE                    
         B     CONFIRM             NO MORE TO RECEIVE                           
*                                                                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(34),=C'UNKNOWN MESSAGE TYPE FROM EASYLINK'                  
         GOTO1 =V(PRINTER)                                                      
         CLI   RQSTCONF,C'Y'       DID WE GET THE WHOLE MESSAGE?                
         BE    CONFIRM                                                          
         BAS   RE,EZLINRCV         NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    *-12                                                             
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
CONFIRM  CLI   RQSTCONF,C'Y'       CONFIRMATION REQUESTED?                      
         BE    *+6                                                              
         DC    H'0'                NO -- HOW DID WE GET HERE?                   
         MVC   APPCACTN,APPCACRC   SEND RECEIVE/CONFIRMATION                    
         LINK  EPLOC=APPCEPLC,                                         +        
               PARAM=(APPCACTN,APPCTOKN,APPCDATA,APPCEPLC,APPCLOGR),   +        
               VL=1                                                             
         PRNT  CONFIRMATIONSENT                                                 
         B     NEXTMSG                                                          
*                                                                               
GOODBYE  PRNT  EZRCVRCLOSEPREPARE                                               
         MVC   APPCACTN,APPCACCP   ISSUE CLOSE/PREPARE                          
         LINK  EPLOC=APPCEPLC,                                         +        
               PARAM=(APPCACTN,APPCTOKN,APPCDATA,APPCEPLC,APPCLOGR),   +        
               VL=1                                                             
*                                                                               
         DS    0H                                                               
         PRNT  EZRCVRCLOSE                                                      
         MVC   APPCACTN,APPCACC    REQUEST CLOSE OF CONVERSATION                
         LINK  EPLOC=APPCEPLC,                                         +        
               PARAM=(APPCACTN,APPCTOKN,APPCDATA,APPCEPLC,APPCLOGR),   +        
               VL=1                                                             
         PRNT  EZRECEIVERCLOSED                                                 
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
* THIS ROUTINE HANDLES AN EASYLINK DELIVERY NOTIFICATION.                       
*                                                                               
EZDLN    NTR1                                                                   
*                                                                               
         MVC   CUSTREF,SPACES                                                   
         LA    R3,5(R3)            BUMP PAST 'DLN(CR)(LF)' TO CUSTREF           
*                                                                               
         LA    RF,CUSTREF                                                       
EZDLN10  CLC   CRLF,0(R3)                                                       
         BE    EZDLN20                                                          
         MVC   0(1,RF),0(R3)                                                    
         LA    R3,1(R3)                                                         
         LA    RF,1(RF)                                                         
         B     EZDLN10                                                          
*                                                                               
EZDLN20  LA    R3,2(R3)            R3 NOW POINTS TO DATE                        
         GOTO1 =V(HEXIN),DMCB,0(R3),DELDAY,2                                    
         CLC   =F'1',DMCB+12                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK(2),8(R3)       HH                                           
         MVC   WORK+2(2),11(R3)    MM (SKIP OVER COLON)                         
         GOTO1 =V(HEXIN),DMCB,WORK,DELTIME,4                                    
         CLC   =F'2',DMCB+12                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   DSTFLAG,C'Y'        IS IT DAYLIGHT SAVINGS TIME?                 
         BNE   EZDLN30                                                          
*                                                                               
         XC    HALF,HALF                                                        
         ZIC   R1,DELTIME          PWOS HOUR                                    
         SLL   R1,4                                                             
         O     R1,=X'0000000C'                                                  
         STH   R1,HALF             PACKED DECIMAL HOUR                          
         AP    HALF,=P'1'          ADD 1 HOUR ADJUSTMENT                        
         CP    HALF,=P'24'         DID WE GO PAST MIDNIGHT?                     
         BNE   *+14                                                             
         ZAP   HALF,=P'0'          YES -- MILITARY HOUR = 0, NOT 24             
         MVI   DELDAY,X'FF'        SPECIAL - MEANS I BUMPED TO NEXT DAY         
*                                                                               
         LH    R1,HALF             ADJUSTED PACKED DECIMAL HOUR                 
         SRL   R1,4                                                             
         STC   R1,DELTIME          PWOS HOUR                                    
*                                                                               
EZDLN30  MVC   EZLEDGER,15(R3)                                                  
*                                                                               
         GOTO1 =V(HEXIN),DMCB,CUSTREF,FULL,8  EDICT FILE DISK ADDRESS           
         CLC   =F'4',DMCB+12                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,EASENTBL         A(TRANSMIT TABLE)                            
         USING XMTTABLD,R3                                                      
*                                                                               
EZDLN40  CLI   0(R3),0             END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                THIS MESSAGE NOT FOUND IN TABLE              
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   EZDLN40                                                          
         CLC   FULL,XMTDSKAD       FIND MATCH ON DISK ADDRESS                   
         BNE   EZDLN40                                                          
*                                                                               
         MVI   4(R1),EACTDLVQ      POST DELIVERED                               
         MVC   5(1,R1),DELDAY      DAY DELIVERED                                
         MVC   6(2,R1),DELTIME     TIME DELIVERED                               
         GOTO1 EAPOSTSN,DMCB,(R3),,EZLEDGER                                     
         DROP  R3                                                               
*                                                                               
         MVC   P+30(21),CUSTREF    CUSTOMER REFERENCE NUMBER                    
         PRNT  EZDELIVERYNOT                                                    
*                                                                               
EZDLN50  CLI   RQSTCONF,C'Y'       DID WE GET THE WHOLE MESSAGE?                
         BE    EZDLNX                                                           
         BAS   RE,EZLINRCV         NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    EZDLN50                                                          
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
EZDLNX   XIT1                                                                   
         EJECT                                                                  
* THIS ROUTINE HANDLES AN EASYLINK CANCELLATION NOTIFICATION.                   
*                                                                               
EZCAN    NTR1                                                                   
*                                                                               
         MVC   CUSTREF,SPACES                                                   
         LA    R3,5(R3)            BUMP PAST 'CAN(CR)(LF)'                      
*                                                                               
         LA    RF,CUSTREF                                                       
EZCAN10  CLC   CRLF,0(R3)                                                       
         BE    EZCAN20                                                          
         MVC   0(1,RF),0(R3)                                                    
         LA    R3,1(R3)                                                         
         LA    RF,1(RF)                                                         
         B     EZCAN10                                                          
*                                                                               
EZCAN20  LA    R3,2(R3)            R3 NOW POINTS TO DATE                        
         GOTO1 =V(HEXIN),DMCB,0(R3),DELDAY,2                                    
         CLC   =F'1',DMCB+12                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK(2),8(R3)       HH                                           
         MVC   WORK+2(2),11(R3)    MM (SKIP OVER COLON)                         
         GOTO1 =V(HEXIN),DMCB,WORK,DELTIME,4                                    
         CLC   =F'2',DMCB+12                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   DSTFLAG,C'Y'        IS IT DAYLIGHT SAVINGS TIME?                 
         BNE   EZCAN30                                                          
*                                                                               
         XC    HALF,HALF                                                        
         ZIC   R1,DELTIME          PWOS HOUR                                    
         SLL   R1,4                                                             
         O     R1,=X'0000000C'                                                  
         STH   R1,HALF             PACKED DECIMAL HOUR                          
         AP    HALF,=P'1'          ADD 1 HOUR ADJUSTMENT                        
         CP    HALF,=P'24'         DID WE GO PAST MIDNIGHT?                     
         BNE   *+14                                                             
         ZAP   HALF,=P'0'          YES -- MILITARY HOUR = 0, NOT 24             
         MVI   DELDAY,X'FF'        SPECIAL - MEANS I BUMPED TO NEXT DAY         
*                                                                               
         LH    R1,HALF             ADJUSTED PACKED DECIMAL HOUR                 
         SRL   R1,4                                                             
         STC   R1,DELTIME          PWOS HOUR                                    
*                                                                               
EZCAN30  MVC   EZLEDGER,15(R3)                                                  
*                                                                               
         GOTO1 =V(HEXIN),DMCB,CUSTREF,FULL,8  EDICT FILE DISK ADDRESS           
         CLC   =F'4',DMCB+12                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,EASENTBL         A(TRANSMIT TABLE)                            
         USING XMTTABLD,R3                                                      
*                                                                               
EZCAN40  CLI   0(R3),0             END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                THIS MESSAGE NOT FOUND IN TABLE              
         CLI   XMTSOURC,XMTSRCPQ   PRINT QUEUE ENTRY?                           
         BNE   EZCAN40                                                          
         CLC   FULL,XMTDSKAD       FIND MATCH ON DISK ADDRESS                   
         BNE   EZCAN40                                                          
*                                                                               
         MVI   4(R1),EACTCANQ      POST CANCELLED                               
         MVC   5(1,R1),DELDAY      DAY CANCELLED                                
         MVC   6(2,R1),DELTIME     TIME CANCELLED                               
         GOTO1 EAPOSTSN,DMCB,(R3),,EZLEDGER                                     
         DROP  R3                                                               
*                                                                               
         MVC   P+30(21),CUSTREF    CUSTOMER REFERENCE NUMBER                    
         PRNT  EZCANCELLATION                                                   
*                                                                               
EZCAN50  CLI   RQSTCONF,C'Y'       DID WE GET THE WHOLE MESSAGE?                
         BE    EZCANX                                                           
         BAS   RE,EZLINRCV         NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    EZCAN50                                                          
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
EZCANX   XIT1                                                                   
         EJECT                                                                  
* THIS ROUTINE READS AN EASYLINK INBOUND MESSAGE AND WRITES IT                  
* TO THE RECEIVER'S PRINT QUEUE.                                                
*                                                                               
EZINB    NTR1                                                                   
*                                                                               
         PRNT  EASYLINKINBOUND                                                  
*                                                                               
         XC    R,R                 PRINT LINE FOR PRTQUE CALLS                  
         LA    R5,R                                                             
         USING PQPLD,R5                                                         
*                                                                               
         LA    R3,5(R3)            BUMP PAST 'INB(CR)(LF)'                      
*                                                                               
         LR    R1,R3               MATCH ON MAILBOX NUMBER                      
         ICM   R1,B'1000',EZAPPRTQ SPECIAL PARAMETER FOR SUBROUTINE             
         GOTO1 EAFINDDS                                                         
         BE    EZINB20             GOT IT                                       
*                                                                               
         L     R2,EADESTBL         DESTINATION TABLE SENTINEL                   
         SH    R2,=H'4'            BACK UP TO SENTINEL                          
         XC    0(4,R2),0(R2)       FREE THE TABLE                               
         MVC   P+11(13),=C'*** ERROR ***'                                       
         MVC   P+30(27),=C'MAILBOX XXXXXXXX IS UNKNOWN'                         
         MVC   P+38(8),0(R3)       PUT MAILBOX NO. IN PRINT LINE                
         GOTO1 =V(PRINTER)                                                      
         B     EZRCVALL            DON'T POST THIS MESSAGE                      
*                                                                               
EZINB20  MVC   QLSRCID,DESTUIDN-DESTTABD(R2)  NUMERIC AGENCY CODE               
         MVI   QLEXTRA,X'FF'       OPEN PRINT QUEUE REPORT                      
         MVC   QLSUBID,=C'WU '                                                  
         MVI   QLCLASS,C'K'                                                     
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,132                                                      
         MVC   QLRETNL,=H'36'                                                   
         MVC   QLRETND,=H'2'                                                    
         MVC   QLDESC,=C'EASYLINK   '                                           
*                                                                               
         L     RE,=A(CIREC)                                                     
         XCEFL (RE),14336          CLEAR PQ C/I BUFFER                          
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)         
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         MVC   P+30(28),=C'COULD NOT OPEN PRTQUE REPORT'                        
         B     EZCLOPUR                                                         
*                                                                               
         MVI   R,X'89'             FORM FEED                                    
         MVC   R+1(132),SPACES                                                  
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)         
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         MVC   P+30(29),=C'COULD NOT WRITE FIRST PQ LINE'                       
         B     EZCLOPUR            ERROR ON FIRST LINE - PROBABLY DISK          
*                                                                               
         LA    R3,23(R3)           MAILBOX+(CRLF)+LEDGER+(CRLF)                 
         XC    HALF,HALF           CLEAR LINE COUNTER                           
*                                                                               
EZINB30  LH    R6,HALF             CURRENT NUMBER OF LINES                      
         CH    R6,=H'56'                                                        
         BNH   EZINB40                                                          
         MVI   R,X'89'             EJECT PAGE EVERY 56 LINES                    
         SR    R6,R6                                                            
         MVC   R+1(132),SPACES                                                  
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)         
         CLI   DMCB+8,0                                                         
         BE    EZINB40                                                          
         MVC   P+30(26),=C'COULD NOT WRITE PAGE EJECT'                          
         B     EZCLOPUR            ERROR ON FIRST LINE - PROBABLY DISK          
*                                                                               
EZINB40  LA    R6,1(R6)                                                         
         STH   R6,HALF             INCREMENT NO. OF LINES                       
         MVI   R,X'09'                                                          
         MVC   R+1(132),SPACES                                                  
         ST    R3,FULL             START OF TEXT TO BE PRINTED                  
         LA    R4,R+16             START OF PRINT LINE                          
*                                                                               
EZINB50  TRT   0(256,R3),TRINBND   LOOK FOR NON-PRINTING CHARACTERS             
         LR    R6,R1                                                            
         CLI   0(R6),ETX                                                        
         BE    EZINB100            END OF MESSAGE                               
         CLI   0(R6),ETB                                                        
         BE    EZINB80             GET THE NEXT BUFFER                          
         CLC   LF,0(R6)            LINE FEED?                                   
         BNE   *+12                                                             
         LA    R3,1(R3)            YES - BUMP PAST IT                           
         B     EZINB60                                                          
*                                                                               
         CLC   CRLF,0(R6)                                                       
         BE    *+16                                                             
         MVI   0(R6),C' '          REPLACE INVALID CHARACTER WITH BLANK         
         LA    R3,1(R6)                                                         
         B     EZINB50                                                          
*                                                                               
         L     R3,FULL                                                          
         SR    R6,R3               LENGTH OF STRING TO BE PRINTED               
         LTR   R6,R6               ANY DATA TO PRINT?                           
         BNZ   *+12                YES                                          
         LA    R3,2(R3)            NO - BUMP PAST CRLF                          
         B     EZINB60                                                          
*                                                                               
         LA    R2,0(R6,R4)                                                      
         LA    R0,R+133                                                         
         CR    R2,R0               LINE IS TOO LONG?                            
         BNH   *+14                                                             
         MVC   P+30(22),=C'PRINT LINE IS TOO LONG'                              
         B     EZCLOPUR            YES                                          
*                                                                               
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)       MOVE STRING INTO PRINT LINE                  
         LA    R3,3(R6,R3)         LENGTH OF STRING + BCTR + CRLF               
*                                                                               
EZINB60  CLC   =C'..NP             ',R+16     PROCESS NEW PAGE REQUEST          
         BNE   EZINB70                                                          
         MVC   R+1(132),SPACES                                                  
         XC    HALF,HALF                                                        
         MVI   R,X'89'                                                          
*                                                                               
EZINB70  GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)         
         CLI   DMCB+8,0                                                         
         BE    EZINB30                                                          
         MVC   P+30(27),=C'COULD NOT WRITE PRTQUE LINE'                         
         B     EZCLOPUR                                                         
*                                                                               
EZINB80  L     R3,FULL                                                          
         SR    R6,R3               LENGTH OF STRING TO BE PRINTED               
         LTR   R6,R6               ANYTHING TO SAVE?                            
         BZ    EZINB90             NO                                           
*                                                                               
         LA    R2,0(R6,R4)                                                      
         LA    R0,R+133                                                         
         CR    R2,R0               LINE IS TOO LONG?                            
         BNH   *+14                                                             
         MVC   P+30(22),=C'PRINT LINE IS TOO LONG'                              
         B     EZCLOPUR            YES                                          
*                                                                               
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)       MOVE STRING INTO PRINT LINE                  
         LA    R4,1(R6,R4)         LENGTH OF STRING + BCTR                      
*                                                                               
EZINB90  BAS   RE,EZLINRCV         GET NEXT BUFFER                              
         BNE   *+16                ERROR                                        
         LA    R3,EASYBUF          CONTINUATION OF MESSAGE                      
         ST    R3,FULL                                                          
         B     EZINB50                                                          
*                                                                               
         XC    R,R                                                              
         MVI   R,X'FE'             ERASE PARTIALLY GENERATED REPORT             
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)         
         MVC   P+30(25),=C'MUST ERASE PARTIAL REPORT'                           
         B     EZCLOPUR                                                         
*                                                                               
EZINB100 L     R3,FULL             WRAP UP                                      
         SR    R6,R3               LENGTH OF FINAL LINE                         
         LTR   R6,R6               ANYTHING TO PRINT?                           
         BZ    EZINB110            NO                                           
*                                                                               
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   R+16(0),0(R3)       MOVE STRING INTO PRINT LINE                  
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)         
         CLI   DMCB+8,0                                                         
         BE    EZINB110                                                         
         MVC   P+30(27),=C'COULD NOT WRITE PRTQUE LINE'                         
         B     EZCLOPUR                                                         
*                                                                               
EZINB110 PRNT  ENDINBOUND                                                       
         MVI   R,X'FF'             CLOSE REPORT                                 
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),=C'PRTQUE',0,R,A(CIREC)         
         CLI   DMCB+8,0                                                         
         BE    EZINBX                                                           
         DROP  R5                                                               
*                                                                               
EZCLOPUR MVC   P+11(13),=C'*** ERROR ***'                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'CLO/PUR'),=C'PRTQUE',0,R,A(CIREC)         
*                                                                               
EZRCVALL CLI   RQSTCONF,C'Y'       DID WE GET THE WHOLE MESSAGE?                
         BE    EZINBX                                                           
         BAS   RE,EZLINRCV         NO -- RECEIVE IT (AND IGNORE IT!)            
         BE    EZRCVALL                                                         
         ABEND 701,DUMP            BAD RETURN CODE FROM SUBROUTINE              
*                                                                               
EZINBX   XIT1                                                                   
         EJECT                                                                  
TRINBND  DS    0XL256              CHECK OUT ANY NON-PRINTING CHAR              
*                                                                               
*                0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                               
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 00-0F                        
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 10-1F                        
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 20-2F                        
         DC    X'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' 30-3F                        
         DC    X'00FFFFFFFFFFFFFFFFFF000000000000' 40-4F                        
         DC    X'00FFFFFFFFFFFFFFFFFF000000000000' 50-5F                        
         DC    X'0000FFFFFFFFFFFFFFFF000000000000' 60-6F                        
         DC    X'FFFFFFFFFFFFFFFFFFFF000000000000' 70-7F                        
         DC    X'FF000000000000000000FFFFFFFFFFFF' 80-8F                        
         DC    X'FF000000000000000000FFFFFFFFFFFF' 90-9F                        
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' A0-AF                        
         DC    X'00000000000000000000FFFFFFFFFFFF' B0-BF                        
         DC    X'FF000000000000000000FFFFFFFFFFFF' C0-CF                        
         DC    X'FF000000000000000000FFFFFFFFFFFF' D0-DF                        
         DC    X'FFFF0000000000000000FFFFFFFFFFFF' E0-EF                        
         DC    X'00000000000000000000FFFFFFFFFFFF' F0-FF                        
         EJECT                                                                  
EZLINRCV NTR1                                                                   
*                                                                               
* RECEIVE RECORDS FROM EASYLINK, AND FILL UP A BUFFER                           
*                                                                               
         MVI   RQSTCONF,C'N'       NO REQUEST FOR CONFIRMATION YET              
         LA    R3,EASYBUF                                                       
         LR    RE,R3                                                            
         LA    RF,EASYBUFL         CLEAR BUFFER                                 
         XCEFL                                                                  
         LA    R2,10               10 RECORDS PER BUFFER MAXIMUM                
*                                                                               
EZRCV10  MVC   APPCACTN,APPCACR    WAIT TO RECEIVE                              
         MVC   APPCMXLN,=Y(APPCMXLQ)                                            
*                                                                               
         LA    R1,EZLNKPRM         PARAMETERS TO LU6.2 LINK                     
         LINK  EPLOC=APPCEPLC                                                   
*                                                                               
         TM    ESTOPECB,X'40'      STOP NOW?                                    
         BZ    EZRCV20                                                          
         MVI   STOPNOW,C'Y'        YES                                          
         PRNT  EZRCVSTOP                                                        
         B     EZLINXOK                                                         
*                                                                               
EZRCV20  XC    ESTOPECB,ESTOPECB                                                
         LTR   RF,RF               CHECK RETURN CODE                            
         BZ    EZRCV40             OK                                           
         LR    R4,RF               SAVE RETURN CODE                             
         MVC   P+30(9),=C'REG 15 = '                                            
         EDIT  (R4),(5,P+39),ALIGN=LEFT,ZERO=NOBLANK                            
         MVC   P+50(8),=C'FLAGS = '                                             
         GOTO1 =V(HEXOUT),DMCB,APPCHDR,P+58,2,=C'TOG'                           
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         C     R4,=F'4'            ANY STATUS BITS SET?                         
         BNE   EZRCV30             NO                                           
         TM    APPCHDR,X'40'       CONFIRMATION REQUESTED?                      
         BZ    EZRCV30             NO                                           
         MVI   RQSTCONF,C'Y'                                                    
         PRNT  CONFIRMREQUESTED                                                 
         B     EZRCV40                                                          
*                                                                               
EZRCV30  PRNT  BADRETURNCODE                                                    
         B     EZLINBAD                                                         
*                                                                               
EZRCV40  SR    R4,R4                                                            
         ICM   R4,3,APPCLEN        LENGTH OF RETURNED DATA                      
         SH    R4,=H'2'            MINUS 2 BYTES FOR LENGTH ITSELF              
         LTR   R1,R4                                                            
         BZ    EZRCV50             NO DATA RETURNED                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),APPCDTA     PLACE IN BUFFER                              
         AR    R3,R4               POINT TO NEXT POSITION IN BUFFER             
         GOTO1 =V(PRNTBL),DMCB,0,APPCDTA,C'DUMP',(R4),=C'1D'                    
*                                                                               
EZRCV50  CLI   RQSTCONF,C'Y'       CONFIRMATION REQUESTED?                      
         BNE   *+12                NO                                           
         MVI   0(R3),ETX           END OF MESSAGE MARKER                        
         B     EZLINXOK                                                         
*                                                                               
         BCT   R2,EZRCV10          GET NEXT LINE                                
*                                                                               
         MVI   0(R3),ETB           END OF BUFFER MARKER                         
         B     EZLINXOK                                                         
*                                                                               
EZLINBAD LTR   RB,RB               SET CC NOT EQUAL                             
         B     EZLINXX                                                          
*                                                                               
EZLINXOK GOTO1 =V(PRNTBL),DMCB,=C'EASYLINK INPUT BUFFER',EASYBUF,      +        
               C'DUMP',2000,=C'2D'                                              
*                                                                               
         CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EZLINXX  XIT1                                                                   
         EJECT                                                                  
SSB      DC    F'0'                FOR DATAMGR (OFFLINE)                        
UTL      DC    F'0',X'0A'          FOR DATAMGR (CONTROL SYSTEM)                 
         SPACE 2                                                                
CRLF     DS    0XL2                                                             
CR       DC    X'0D'                                                            
LF       DC    X'25'                                                            
ETB      EQU   X'26'                                                            
ETX      EQU   X'03'                                                            
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
DMCB     DS    6F                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
WORK     DS    CL256                                                            
         SPACE 2                                                                
TODAY    DS    CL6                 EBCDIC DATE TODAY (YYMMDD)                   
STOPNOW  DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
DSTFLAG  DC    C'N'                'Y' = DAYLIGHT SAVINGS TIME                  
CUSTREF  DS    CL21                TTTTBBRRNNNNNUUUUUUUU                        
EZLEDGER DS    CL11                EASYLINK LEDGER NUMBER                       
DELDAY   DS    X                   MESSAGE DELIVERY DAY -- PWOS                 
DELTIME  DS    XL2                 MESSAGE DELIVERY TIME (HM) -- PWOS           
RQSTCONF DS    C                   'Y' = WE GOT REQUEST FOR CONFIRM             
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL210               PQ RECORD DATA                               
         SPACE 2                                                                
EZLNKPRM DS    0D                                                               
         DC    A(APPCACTN)                                                      
         DC    A(APPCTOKN)                                                      
         DC    A(APPCDATA)                                                      
         DC    A(APPCEPLC)                                                      
         DC    A(APPCLOGR)                                                      
         DC    X'80000000'         A(ECB) GETS STORED HERE                      
         SPACE 2                                                                
APPCACTN DS    CL4                 ACTION CODE                                  
APPCACOA DC    C'OA  '             ACTION OPEN SEND TO AT&T EASYLINK            
APPCACOS DC    C'OS  '             ACTION OPEN SEND                             
APPCACOR DC    C'OR  '             ACTION OPEN RECEIVE                          
APPCACOI DC    C'OI  '             ACTION OPEN FOR INPUT                        
APPCACSD DC    C'SD  '             ACTION SEND DATA                             
APPCACSF DC    C'SF  '             ACTION SEND FLUSH                            
APPCACSC DC    C'SC  '             ACTION SEND CONFIRMATION                     
APPCACSR DC    C'SR  '             ACTION SEND/RECEIVE                          
APPCACR  DC    C'R   '             ACTION RECEIVE                               
APPCACRC DC    C'RC  '             ACTION RECEIVE/CONFIRM                       
APPCACCP DC    C'CP  '             ACTION CLOSE/PREPARE                         
APPCACC  DC    C'C   '             ACTION CLOSE                                 
APPCTOKN DC    F'0'                TOKEN FOR EASYLINK                           
APPCEPLC DS    CL8                 PROVIDED BY APPC OPEN CALL                   
         DS    0D                                                               
         DC    CL8'APPCDATA'                                                    
APPCDATA DS    0X                  DATA AREA                                    
APPCHDR  DC    XL6'00'             HEADER INFO                                  
APPCMXLN DC    XL2'00'             MAXIMUM LENGTH OF DATA                       
APPCLEN  DC    XL2'00'             LENGTH OF DATA                               
APPCDTA  DC    XL248'00'           ACTUAL DATA                                  
APPCMAXL EQU   *-APPCLEN           MAXIMUM LENGTH OF DATA                       
APPCMXLQ EQU   *-APPCMXLN          MAXIMUM LENGTH OF DATA FOR RECEIVE           
         SPACE 2                                                                
APPCLOGR DCB   DDNAME=EAPPCLGR,RECFM=FBA,LRECL=133,MACRF=PM,DSORG=PS            
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*EASYBUF'                                                      
EASYBUF  DS    (EASYBUFL)X'00'     EASYLINK LINE BUFFER                         
EASYBUFL EQU   2000                                                             
*                                                                               
         DS    0D                                                               
         DC    C'*CIREC**'                                                      
CIREC    DC    14336X'00'          PRINT QUEUE C/I BUFFER                       
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
         SPACE 3                                                                
       ++INCLUDE DDEDICTWRK                                                     
         EJECT                                                                  
       ++INCLUDE DDEDICTFIL                                                     
         EJECT                                                                  
* DMGREQUS                                                                      
* DMPRTQL                                                                       
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061DDJOMU2   05/01/02'                                      
         END                                                                    
