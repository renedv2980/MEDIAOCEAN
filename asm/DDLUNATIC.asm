*          DATA SET DDLUNATIC  AT LEVEL 052 AS OF 05/01/02                      
*PHASE LUNATICA                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        LUNATIC -- LOGICAL UNIT NOTIFICATION AND             *         
*                            TRANSMISSION -- INTER-CONTINENTAL        *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK (AND GETEL REGISTER)                      *         
*                R5 -- WORK                                           *         
*                R6 -- PRINT QUEUE TABLE                              *         
*                R7 -- S P A R E                                      *         
*                R8 -- CPRINT                                         *         
*                R9 -- 3RD BASE                                       *         
*                RA -- 2ND BASE                                       *         
*                RB -- 1ST BASE                                       *         
*                RC -- LOCAL WORKING STORAGE                          *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'LUNATIC - LOCATE AND XMIT INTER-CONTINENTAL REPORTS'            
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
LUNATIC  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*LUNATIC,=A(R13CHAIN),RA,R9                                    
*                                                                               
         ENTRY UTL                 FOR DATAMGR                                  
         ENTRY SSB                                                              
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
         USING CIDATAD,R6                                                       
*                                                                               
         LA    R0,4                                                             
         LNR   R0,R0               R0 = -4                                      
         SVC   247                 SO LUNATIC IS NOT SWAPPABLE                  
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM            SET UP OPERATOR COMMUNICATIONS               
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         MVC   ECBLST,AOPERECB     A(ECB)                                       
         L     R2,COMCIBPT         GET A(CIB)                                   
         LA    R3,COMCIBPT         GET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTART    WAS LUNATIC BROUGHT UP WITH 'START'?         
         BNE   NOSTART                                                          
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)  YES -- FREE THE CIB                      
NOSTART  QEDIT ORIGIN=(R3),CIBCTR=1    NOW ALLOW MODIFIES                       
*                                                                               
         EXTRACT FULL,'S',FIELDS=(ASID)                                         
         L     R3,FULL             MVS JOB ACCOUNTING INFORMATION               
         LOCASCB ASID=(R3)         PUTS A(ASCB) INTO R1                         
         ST    R1,AASCB            SAVE A(ASCB)                                 
*                                                                               
         BAS   RE,INITIAL          INITIALIZE                                   
*                                                                               
         CLI   MODE,C'S'           SENDING LUNATIC?                             
         BE    SEND                                                             
         CLI   MODE,C'R'           RECEIVING LUNATIC?                           
         BE    *+6                                                              
         DC    H'0'                NO MODE PARAMETER CARD GIVEN                 
         EJECT                                                                  
* THIS IS FOR THE RECEIVING LUNATIC                                             
*                                                                               
         MVC   APPCACTN,APPCACOR   OPEN TO RECEIVE                              
         MVC   APPCDATA(8),MYNAME  LUID OF THIS PROGRAM                         
         MVC   APPCDATA+8(8),TALKTO LUID OF PARTNER                             
         LINK  EP=DDSAPPC,                                             +        
               PARAM=(APPCACTN,APPCTOKN,APPCDATA,APPCEPLC,APPCLOG),VL=1         
*                                                                               
RCV10    MVC   APPCACTN,APPCACR    WAIT TO RECEIVE                              
*                                                                               
RCV20    MVC   APPCMXLN,=Y(APPCMXLQ)                                            
*                                                                               
         LINK  EPLOC=APPCEPLC,                                         +        
               PARAM=(APPCACTN,APPCTOKN,APPCDATA,APPCEPLC,APPCLOG)              
*                                                                               
* EXAMINE DATA AREA AND TAKE APPROPRIATE ACTION                                 
*                                                                               
         LH    R1,=Y(APPCM1LQ)                                                  
         SH    R1,=H'3'            TWO FOR LENGTH, ONE FOR EX                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   APPCM1(0),APPCDTA   'HERE COMES A REPORT'?                       
         BNE   RCV30               NO                                           
*                                                                               
         XC    APPCDTA,APPCDTA     YES -- CLEAR DATA AREA                       
         MVC   APPCLEN,=Y(APPCM2LQ) LENGTH OF MESSAGE                           
         LH    R1,=Y(APPCM2LQ)                                                  
         SH    R1,=H'3'            TWO FOR LENGTH, ONE FOR EX                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APPCDTA(0),APPCM2   'GIVE ME THE REPORT'                         
         MVC   APPCACTN,APPCACSR   ISSUE SEND/RECEIVE                           
         B     RCV20                                                            
*                                                                               
RCV30    CLC   =C'*HDR*',APPCDTA   REPORT HEADER DETAILS?                       
         BNE   RCV60               NO                                           
*                                                                               
         PACK  DUB,APPCDTA+5(5)    CONTROL INTERVAL ADDRESS                     
         CVB   R3,DUB                                                           
*                                                                               
         LA    RE,APPCDTA+10                                                    
         XC    P,P                 PRINT LINE FOR PRTQUE CALLS                  
         LA    R2,P                                                             
         USING PQPLD,R2                                                         
         MVI   QLEXTRA,X'FF'       OPEN PRINT QUEUE REPORT                      
         OI    QLFLAG,QLFLRALL                                                  
         MVC   QLINDEX,QLINDEX-PQPLD(RE)                                        
         MVC   QLBATTR,QLBATTR-PQPLD(RE)                                        
         NI    QLTYPE,X'FF'-PQTYRSND  TURN OFF 'SEND PENDING' FLAG              
         MVC   QLFATTR,QLFATTR-PQPLD(RE)                                        
         MVI   RPTTYPE,C' '                                                     
*                                                                               
         TM    QLATTB,QLATJOBI     TRANSMITTING JCL?                            
         BZ    *+18                NO                                           
         MVI   RPTTYPE,C'J'                                                     
         MVC   QLRNUM,QLREPNO-PQPLD(RE)                                         
         B     RCV40                                                            
*                                                                               
         TM    QLATTB,QLATJOBO     TRANSMITTING SOON JOB OUTPUT?                
         BZ    RCV40                                                            
         MVI   RPTTYPE,C'O'        YES                                          
         MVI   FRSTLINE,C'Y'                                                    
         OI    QLFLAG,QLFLCIDA+QLFLRDT+QLFLRRE                                  
         STCM  R3,3,QLREPRCI       CONTROL INTERVAL NUMBER                      
         MVC   RPTCLASS,QLCLASS    REPORT CLASS                                 
         MVC   RPTLPP,QLLPP        REPORT LINES PER PAGE                        
         MVC   RPTFORMS,QLFORMS    REPORT FORMS CODE                            
         MVC   RPTCHARS,QLCHARS    REPORT CHARACTER SET                         
         MVC   RPTCOPYS,QLCOPIES   REPORT NUMBER OF COPIES                      
         MVC   RPTDESC,QLDESC      REPORT DESCRIPTION                           
         MVC   RPTMAKER,QLMAKER    REPORT MAKER                                 
*                                                                               
RCV40    GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),PRTQUE,0,P,CXREC                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                COULD NOT OPEN PQ REPORT                     
*                                                                               
         XC    APPCDTA,APPCDTA                                                  
         SR    R0,R0                                                            
         TM    QLATTB,QLATJOBI     TRANSMITTING JCL?                            
         BZ    RCV50               NO                                           
         MVC   SAVERCI,QLREPRCI    NEW CONTROL INTERVAL NUMBER                  
         MVC   SAVEREF,QLREPRNO    NEW REPORT REFERENCE NUMBER                  
         LH    R0,SAVEREF                                                       
*                                                                               
RCV50    MVC   APPCLEN,=Y(APPCM3LQ) LENGTH OF MESSAGE                           
         LH    R1,=Y(APPCM3LQ)                                                  
         SH    R1,=H'3'            TWO FOR LENGTH, ONE FOR EX                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APPCDTA(0),APPCM3   '*DDS*REPORT IS OPENED XXXXX'                
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  APPCDTA+22(5),DUB                                                
         MVC   APPCACTN,APPCACSR   ISSUE SEND/RECEIVE                           
         B     RCV20                                                            
         DROP  R2                                                               
*                                                                               
RCV60    CLC   =C'*REC*',APPCDTA   REPORT LINE?                                 
         BNE   RCV80               NO                                           
*                                                                               
         XC    P,P                 PRINT LINE FOR PRTQUE CALLS                  
         MVC   P,APPCDTA+5                                                      
         CLC   =C'DIRECT=',P+1                                                  
         BNE   RCV65                                                            
         GOTO1 =V(HEXOUT),DMCB,SAVEREF,P+48,2,=C'TOG'                           
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(HEXOUT),DMCB,SAVERCI,P+52,2,=C'TOG'                           
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RCV65    CLI   RPTTYPE,C'O'        IS THIS SOON JOB OUTPUT?                     
         BNE   RCV70                                                            
         CLI   FRSTLINE,C'Y'       YES -- IS THIS THE 1ST REPORT LINE?          
         BNE   RCV70                                                            
         MVI   FRSTLINE,C'N'                                                    
         LA    R2,CXREC            UPDATE SOME REPORT HEADER FIELDS             
         USING PQRECD,R2                                                        
         MVC   PQCLASS,RPTCLASS    REPORT CLASS                                 
         MVC   PQLPP,RPTLPP        REPORT LINES PER PAGE                        
         MVC   PQFORMS,RPTFORMS    REPORT FORMS CODE                            
         MVC   PQCHARS,RPTCHARS    REPORT CHARACTER SET                         
         MVC   PQCOPIES,RPTCOPYS   REPORT NUMBER OF COPIES                      
         MVC   PQDESC,RPTDESC      REPORT DESCRIPTION                           
         MVC   PQMAKER,RPTMAKER    REPORT MAKER                                 
         OI    PQLINET,X'C0'       REPORT FLAG -- CONVERT TO WIDE RPT           
         MVI   PQLINEW,198         REPORT WIDTH                                 
         DROP  R2                                                               
*                                                                               
RCV70    GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),PRTQUE,0,P,CXREC                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                COULD NOT WRITE PQ LINE                      
         XC    APPCDTA,APPCDTA                                                  
         B     RCV10               ISSUE RECEIVE                                
*                                                                               
RCV80    LH    R1,=Y(APPCM4LQ)                                                  
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   APPCM4(0),APPCDTA   'END OF REPORT'?                             
         BE    *+6                                                              
         DC    H'0'                NO -- WHAT MESSAGE IS THIS?                  
*                                                                               
         CLI   RPTTYPE,C'J'        DID WE JUST CREATE JCL?                      
         BNE   RCV90               NO                                           
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'CLO/JOB'),PRTQUE,0,P,CXREC                
         CLI   DMCB+8,0                                                         
         BE    RCV100                                                           
         DC    H'0'                COULD NOT CLOSE PQ REPORT                    
*                                                                               
RCV90    MVI   P,X'FF'             CLOSE REPORT -- NORMAL                       
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMPRINT'),PRTQUE,0,P,CXREC                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                COULD NOT CLOSE PQ REPORT                    
*                                                                               
RCV100   XC    APPCDTA,APPCDTA                                                  
         MVC   APPCLEN,=Y(APPCM5LQ) LENGTH OF MESSAGE                           
         LH    R1,=Y(APPCM5LQ)                                                  
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APPCDTA(0),APPCM5   'REPORT CLOSED'                              
         MVC   APPCACTN,APPCACSR   ISSUE SEND/RECEIVE                           
         B     RCV20                                                            
         EJECT                                                                  
* THIS IS FOR THE SENDING LUNATIC                                               
*                                                                               
* FIND JCL ON ALL PRINT QUEUES, AND UPDATE THE REPORT TABLE WITH THEM.          
* ATTACH ANY JCL WHICH IS FOR US, WAIT (IF NECESSARY), THEN GO BACK             
* AND UPDATE THE TABLE AGAIN.  THIS PROGRAM RUNS FOREVER UNTIL STOPPED          
* BY THE OPERATOR.                                                              
*                                                                               
SEND     MVC   APPCACTN,APPCACOS   OPEN TO SEND                                 
         MVC   APPCDATA(8),MYNAME  LUID OF THIS PROGRAM                         
         LINK  EP=DDSAPPC,                                             +        
               PARAM=(APPCACTN,APPCTOKN,APPCDATA,APPCEPLC,APPCLOG),VL=1         
*                                                                               
SENDLOOP BAS   RE,UPDTABLE         UPDATE THE REPORT TABLE                      
*                                                                               
NEXT     CLI   OPERSTOP,C'Y'       DOES OPERATOR WANT TO STOP LUNATIC?          
         BNE   ANYRPTS             NO                                           
*                                                                               
         BAS   RE,SHUTDOWN         WRAP UP                                      
         XBASE                                                                  
*                                                                               
ANYRPTS  OC    ANEXTRPT,ANEXTRPT   ANY REPORTS FOUND TO TRANSMIT?               
         BZ    WAIT                NO                                           
         BAS   RE,TRANSMIT         TRANSMIT THE REPORT                          
         B     NEXT                LOOK FOR MORE REPORTS                        
*                                                                               
WAIT     BAS   RE,WAITABIT         WE MIGHT WAIT A LITTLE WHILE                 
         B     SENDLOOP            NOW LOOK FOR MORE REPORTS                    
         SPACE 5                                                                
SHUTDOWN NTR1                                                                   
*                                                                               
* WRAP UP.                                                                      
*                                                                               
         GOTO1 =V(DMENQDEQ),DMCB,(C'D',=C'ALL')                                 
*                                                                               
*========                                                                       
* APPC CLOSE MACROS GO HERE                                                     
*========                                                                       
*                                                                               
         PRNT  TERMINATE                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
INITIAL  NTR1                                                                   
*                                                                               
* INITIALIZE                                                                    
*                                                                               
         MVC   TITLE(7),=C'LUNATIC'                                             
         PRNT  INITIALIZE                                                       
*                                                                               
         BAS   RE,READCRDS         READ PARAMETER CARDS                         
*                                                                               
         BAS   RE,OPENCTFL         OPEN CTFILE (IF NOT OPENED ALREADY)          
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'GLIST'),PRTQUE,AIO,0,CXREC                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO                                                           
         L     R4,32(R4)           SAVE A(PRINT QUEUE LIST)                     
*                                                                               
         ZIC   R2,0(R4)            NUMBER OF PRINT QUEUES                       
         CH    R2,=Y(NUMPQS)                                                    
         BE    *+6                                                              
         DC    H'0'                EQUATED SYMBOL 'NUMPQS' IS INCORRECT         
*                                                                               
         L     R6,ACITABLE         A(PRINT QUEUE TABLE)                         
INIT10   LA    R4,8(R4)            BUMP TO NEXT PRINT QUEUE                     
         MVC   CFPQENUM,4(R4)      PRINT QUEUE EXTERNAL FILE NUMBER             
         MVC   PRTQUE+4(1),1(R4)   CONSTRUCT C'PRTQ#'                           
         GOTO1 =V(DATAMGR),DMCB,(0,=C'BUFFER'),PRTQUE,0,0,CXREC                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CIDATA,CXREC+12     CIDATA IS HERE                               
         L     R1,=V(DMENQDEQ)     V(DMENQDEQ)                                  
         ST    R1,CIENQDEQ                                                      
         LA    R1,L'PQINDEX        LENGTH OF PQ KEY                             
         STH   R1,CINDXLN                                                       
         L     R1,CXREC+8          DISPLACEMENT TO PQ SAVE AREA                 
         LA    R1,CXREC(R1)                                                     
         ST    R1,APQSAVE          A(PQ SAVE AREA)                              
         LA    R6,CITBLLNQ(R6)                                                  
         BCT   R2,INIT10           LOOK AT ALL PRINT QUEUES                     
*                                                                               
         MVC   OPMS1,NORPTMSG                                                   
*                                                                               
         OPEN  (APPCLOG,OUTPUT)                                                 
*                                                                               
         ANSR                                                                   
         EJECT                                                                  
READCRDS NTR1                                                                   
*                                                                               
* READ PARAMETER CARDS                                                          
*                                                                               
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD                        
*   MODE=SEND/RECEIVE  MANDATORY!                                               
*   MYNAME=LUID      MANDATORY -- LUID OF THIS LUNATIC                          
*   TALKTO=LUID      MANDATORY -- LUID OF OTHER LUNATIC                         
*   WAITSECS=N       WAIT N SECONDS BETWEEN PQ SEARCHES (DEFAULT = 120)         
*   JESMSG=YES       GIVE CONSOLE MSGS AT XMIT START/END (DEFAULT = NO)         
*                                                                               
*   THE FOLLOWING PARAMETERS ARE REALLY FOR TESTING/DEBUGGING ONLY              
*                                                                               
*   DDSIO=DDSIOX     ALTERNATE DDSIO (FOR LUNATIC AND ATTACHED TASKS)           
*   USERID=CCCCCC    TRANSMIT REPORTS FOR THIS USERID ONLY                      
*   SUBID=CCC        TRANSMIT REPORTS FOR THIS SUB-ID ONLY                      
*                                                                               
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    RCX                                                              
*                                                                               
         MVC   P(80),CARD          PRINT ALL PARAMETER CARDS                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    RC10                YES                                          
*                                                                               
         CLC   =C'MODE=',CARD      MODE=                                        
         BNE   RC20                                                             
         MVC   MODE,CARD+5                                                      
         CLC   =C'SEND',CARD+5                                                  
         BE    RC10                                                             
         CLC   =C'RECEIVE',CARD+5                                               
         BE    RC10                                                             
         B     INVCARD                                                          
*                                                                               
RC20     CLC   =C'DDSIO=',CARD     DDSIO=                                       
         BNE   RC30                                                             
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6      THIS ONE IS FOR LUNATIC ONLY                 
         B     RC10                                                             
*                                                                               
RC30     CLC   =C'WAITSECS=',CARD  WAITSECS=N                                   
         BNE   RC40                                                             
         GOTO1 =V(NUMVAL),DMCB,CARD+9,(2,0)                                     
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         L     R1,DMCB+4                                                        
         MH    R1,=H'100'          SCALE THE TIME INTERVAL FOR STIMER           
         ST    R1,WAITSECS                                                      
         B     RC10                                                             
*                                                                               
RC40     CLC   =C'USERID=',CARD    USERID=XXX                                   
         BNE   RC50                                                             
         BAS   RE,OPENCTFL         OPEN CTFILE                                  
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),CARD+7   USER-ID                                      
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,AIO,0             
         CLI   DMCB+8,0                                                         
         BNE   INVCARD             ID RECORD NOT FOUND                          
         L     R4,AIO                                                           
         BAS   RE,GETEL            FIND ID ELEMENT (X'02')                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   USERFILT,2(R4)      HEX USERID FILTER                            
         B     RC10                                                             
*                                                                               
RC50     CLC   =C'SUBID=',CARD     SUBID=                                       
         BNE   RC60                                                             
         MVC   SUBFILT,CARD+6                                                   
         B     RC10                                                             
*                                                                               
RC60     CLC   =C'JESMSG=',CARD    JESMSG=                                      
         BNE   RC70                                                             
         CLC   =C'NO',CARD+7                                                    
         BE    RC10                JESMSG=NO IS THE DEFAULT                     
         CLC   =C'YES',CARD+7                                                   
         BNE   INVCARD                                                          
         MVI   JESMSG,C'Y'         JESMSG=YES                                   
         B     RC10                                                             
*                                                                               
RC70     CLC   =C'MYNAME=',CARD    MYNAME=                                      
         BNE   RC80                                                             
         MVC   MYNAME,CARD+7                                                    
         B     RC10                                                             
*                                                                               
RC80     CLC   =C'TALKTO=',CARD    TALKTO=                                      
         BNE   INVCARD                                                          
         MVC   TALKTO,CARD+7                                                    
         B     RC10                                                             
*                                                                               
INVCARD  MVC   P+20(22),=C'- INVALID CONTROL CARD'                              
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
RCX      CLC   MYNAME,SPACES                                                    
         BNE   *+6                                                              
         DC    H'0'                MUST IDENTIFY OURSELF                        
         CLC   TALKTO,SPACES                                                    
         BNE   *+6                                                              
         DC    H'0'                MUST IDENTIFY PARTNER                        
*                                                                               
         GOTO1 =V(PRINTER)         SKIP A LINE                                  
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
UPDTABLE NTR1                                                                   
*                                                                               
* READ PRTQUES TO FIND REPORTS TO TRANSMIT.                                     
*                                                                               
         PRNT  UPDATETABLE                                                      
*                                                                               
         SR    R0,R0                                                            
UP10     TIME  BIN                 GET THE CURRENT TIME                         
         LTR   R0,R0                                                            
         BZ    UP10                BAD RETURN FROM MACRO                        
         ST    R0,PQSRCHTM         TIME WE BEGAN SEARCHING PRINT QUEUES         
*                                                                               
         L     R6,ACITABLE         PRINT QUEUE INFO                             
         XC    RPTTBLSZ,RPTTBLSZ   NO REPORTS FOUND YET                         
         L     R5,ARPTTAB                                                       
         LA    R0,MAXRPTS          FILL REPORT TABLE WITH X'FF'S                
         MVC   0(RPTTABLQ,R5),HIGHRPT                                           
         LA    R5,RPTTABLQ(R5)                                                  
         BCT   R0,*-10                                                          
*                                                                               
UP20     XC    KEY,KEY                                                          
         GOTO1 =V(DATAMGR),DMCB,(0,=C'SEQ'),CFPQID,KEY,R,CXREC                  
         TM    DMCB+8,X'80'        END OF FILE?                                 
         BO    UP40                YES                                          
*                                                                               
         LA    R3,R                                                             
         USING PQPLD,R3                                                         
         TM    QLTYPE,PQTYRSND+PQTYRRCV  LUNATIC REPORT FOR SENDING?            
         BNO   UP20                NO                                           
         TM    QLSTAT,PQSTHO       IS REPORT IN 'HOLD' STATUS?                  
         BO    UP20                YES                                          
*                                                                               
         OC    USERFILT,USERFILT   IS THERE A USERID FILTER?                    
         BZ    *+14                NO                                           
         CLC   QLSRCID,USERFILT    YES - DOES THIS REPORT MATCH FILTER?         
         BNE   UP20                NO                                           
         CLC   SUBFILT,=C'   '     IS THERE A SUBID FILTER?                     
         BE    *+14                NO                                           
         CLC   QLSUBID,SUBFILT     YES - DOES THIS REPORT MATCH FILTER?         
         BNE   UP20                NO                                           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,QLREPNO                                                     
         LA    R1,1(R1)                                                         
         MH    R1,CITRKS                                                        
         LH    R0,CITRKS                                                        
         BCTR  R0,0                                                             
         SR    R1,R0                                                            
*                                                                               
         LA    R5,WORK                                                          
         USING RPTTABLD,R5                                                      
         STCM  R1,3,RPTTRKNO                                                    
         MVC   RPTKEY,QLKEY        SAVE PRINT QUEUE KEY                         
         MVC   RPTTIME,QLAGELT     TIME CREATED (4/3 SEC UNITS)                 
         MVC   RPTPQNUM,CFPQENUM   PRINT QUEUE NUMBER                           
         DROP  R3                                                               
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),RPTREPUI  USER-ID                                      
         L     RF,AIO                                                           
         CLC   KEY(25),0(RF)                                                    
         BE    UP30                NO NEED TO RE-READ SAME RECORD               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,AIO,0             
         CLI   DMCB+8,0                                                         
         BNE   INVCARD             ID RECORD NOT FOUND                          
UP30     L     R4,AIO                                                           
         BAS   RE,GETEL            FIND ID ELEMENT (X'02')                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RPTREPUA,2(R4)      ALPHA USERID                                 
         DROP  R5                                                               
*                                                                               
         LA    R2,MAXRPTS                                                       
         GOTO1 =V(BINSRCH),DMCB,(1,WORK),ARPTTAB,RPTTBLSZ,RPTTABLQ,    +        
               RPTTABLQ,(R2)                                                    
         TM    DMCB,X'01'          RECORD BETTER NOT BE THERE ALREADY           
         BO    *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,DMCB          A(RECORD)                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   RPTTBLSZ,DMCB+8                                                  
         B     UP20                                                             
*                                                                               
UP40     LA    R6,CITBLLNQ(R6)     BUMP TO NEXT PQ                              
         CLI   0(R6),X'FF'         END-OF-LIST?                                 
         BNE   UP20                NO                                           
*                                                                               
         ICM   R3,15,RPTTBLSZ                                                   
         BNZ   UP50                YES                                          
         MVC   P(28),=C'NO REPORTS FOUND TO TRANSMIT'                           
         GOTO1 =V(PRINTER)                                                      
         XC    ANEXTRPT,ANEXTRPT                                                
         B     UPX                                                              
*                                                                               
UP50     MVC   P(41),=C'PRTQ#   TTTT   REPORT KEY            TIME'              
         GOTO1 =V(PRINTER)                                                      
         MVC   P(41),=C'-----   ----   ----------            ----'              
         GOTO1 =V(PRINTER)                                                      
         L     R5,ARPTTAB                                                       
         USING RPTTABLD,R5                                                      
         ST    R5,ANEXTRPT                                                      
*                                                                               
UP60     L     R6,ACITABLE         FIND OUT WHICH PQ THE JCL IS ON              
UP70     CLC   RPTPQNUM,CFPQENUM                                                
         BE    UP80                                                             
         LA    R6,CITBLLNQ(R6)                                                  
         CLI   0(R6),X'FF'         END OF TABLE?                                
         BNE   UP70                YES                                          
         DC    H'0'                                                             
*                                                                               
UP80     MVC   P(5),CFPQID         C'PRTQN'                                     
         GOTO1 =V(HEXOUT),DMCB,RPTTRKNO,P+8,2,=C'TOG'                           
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P+15(8),RPTREPUA                                                 
         LA    RF,P+22             END OF USERNAME                              
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                BACK UP TO LAST CHAR OF USERNAME             
         B     *-10                                                             
         MVI   1(RF),C','                                                       
         MVC   2(3,RF),RPTSUBID                                                 
         MVI   5(RF),C','                                                       
         EDIT  RPTREFNO,(5,6(RF)),ALIGN=LEFT                                    
*                                                                               
         MVC   THREE(2),RPTTIME                                                 
         BAS   RE,CONVTHMS         CONVERT TIME TO X'HHMMSS'                    
         ZIC   R0,THREE            SUBMIT TIME (HOURS)                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+37(2),DUB                                                      
         MVI   P+39,C':'                                                        
         ZIC   R0,THREE+1          SUBMIT TIME (MINUTES)                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+40(2),DUB                                                      
         MVI   P+42,C':'                                                        
         ZIC   R0,THREE+2          SUBMIT TIME (SECONDS)                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+43(2),DUB                                                      
         GOTO1 =V(PRINTER)         PRINT INFO FOR THIS REPORT                   
*                                                                               
         LA    R5,RPTTABLQ(R5)     BUMP TO NEXT TABLE ENTRY                     
         BCT   R3,UP60                                                          
*                                                                               
         ZAP   LINE,=PL2'75'       EJECT PAGE NEXT TIME                         
*                                                                               
UPX      B     XIT                                                              
         EJECT                                                                  
TRANSMIT NTR1                                                                   
*                                                                               
* READ THE PRINT QUEUE FOR THE FIRST REPORT IN THE TABLE AND XMIT IT.           
*                                                                               
         L     R5,ANEXTRPT                                                      
         USING RPTTABLD,R5                                                      
*                                                                               
         L     R6,ACITABLE         FIND OUT WHICH PQ THE JCL IS ON              
XMIT10   CLC   RPTPQNUM,CFPQENUM                                                
         BE    XMIT20                                                           
         LA    R6,CITBLLNQ(R6)                                                  
         CLI   0(R6),X'FF'         END OF TABLE?                                
         BNE   XMIT10              YES                                          
         DC    H'0'                                                             
*                                                                               
XMIT20   MVC   OPMS1,SPACES        FORMAT THE OPERATOR MESSAGE                  
         MVC   OPMS1+13(8),RPTREPUA                                             
         MVI   OPMS1+21,C','                                                    
         MVC   OPMS1+22(3),RPTSUBID                                             
         MVI   OPMS1+25,C','                                                    
         EDIT  RPTREFNO,(5,OPMS1+26)                                            
*                                                                               
         L     RF,APQSAVE                                                       
         USING SKBUFFD,RF                                                       
         XC    SKBCTRL,SKBCTRL     START READING FROM SCRATCH                   
         XC    SKFCTRL,SKFCTRL                                                  
         XC    SKXCTRL,SKXCTRL                                                  
         MVC   SKLABEL,=C'*PQSAVE*'                                             
         MVC   SKINTNO,CFPQINUM    INTERNAL FILE NUMBER                         
         MVC   SKEXTNO,CFPQENUM    EXTERNAL FILE NUMBER                         
         MVC   SKFSTCI(2),RPTTRKNO TRACK NUMBER                                 
         MVI   SKFSTCI+2,1         ALWAYS START WITH BLOCK 1                    
         DROP  RF                                                               
*                                                                               
         XC    R,R                                                              
         MVI   R+4,C'L'            SILLY PARAMETER FOR RANDOM READ              
         GOTO1 =V(DATAMGR),DMCB,(0,=C'RANDOM'),CFPQID,0,R,CXREC                 
         CLI   DMCB+8,0            READ PQ HEADER INFORMATION                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,R                                                             
         USING PQPLD,R4                                                         
         CLC   RPTKEY,QLKEY        IS IT THE SAME REPORT?                       
         BNE   XMIT60                                                           
         CLC   RPTTIME,QLAGELT                                                  
         BNE   XMIT60                                                           
*                                                                               
         MVC   P+30(12),=C'REPORT ID:  '                                        
         MVC   P+42(18),OPMS1+13   UUUUUUUU,XXX,NNNNN                           
         PRNT  TRANSMIT                                                         
*                                                                               
         MVC   OPMS1(12),=C'TRANSMITTING'                                       
         CLI   JESMSG,C'Y'         ARE WE DISPLAYING CONSOLE MESSAGES?          
         BNE   XMIT30              NO                                           
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
XMIT30   XC    APPCDTA,APPCDTA                                                  
         MVC   APPCLEN,=Y(APPCM1LQ) LENGTH OF MESSAGE                           
         LH    R1,=Y(APPCM1LQ)                                                  
         SH    R1,=H'3'            TWO FOR LENGTH, ONE FOR EX                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APPCDTA(0),APPCM1   'HERE COMES A REPORT'                        
         MVC   APPCMXLN,=Y(APPCMXLQ)                                            
         MVC   APPCACTN,APPCACSR   ISSUE SEND/RECEIVE                           
*                                                                               
         LINK  EPLOC=APPCEPLC,                                         +        
               PARAM=(APPCACTN,APPCTOKN,APPCDATA,APPCEPLC,APPCLOG)              
*                                                                               
         LH    R1,=Y(APPCM2LQ)                                                  
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   APPCM2(0),APPCDTA   'GIVE ME THE REPORT'?                        
         BE    *+6                                                              
         DC    H'0'                INVALID RESPONSE TO LAST SEND                
*                                                                               
         XC    APPCDTA,APPCDTA                                                  
         MVC   APPCDTA(5),=C'*HDR*'                                             
         MVC   APPCDTA+10(L'R),R                                                
         MVC   APPCLEN,=Y(APPCMAXL)                                             
         MVC   APPCMXLN,=Y(APPCMXLQ)                                            
*                                                                               
         LH    R1,QLRNUM                                                        
         LA    R1,1(R1)                                                         
         MH    R1,CITRKS                                                        
         LH    R0,CITRKS                                                        
         BCTR  R0,0                                                             
         SR    R1,R0               R1 = TTTT                                    
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  APPCDTA+5(5),DUB    PASS ALONG C/I ADDRESS                       
*                                                                               
         MVC   APPCACTN,APPCACSR   ISSUE SEND/RECEIVE                           
         LINK  EPLOC=APPCEPLC,                                         +        
               PARAM=(APPCACTN,APPCTOKN,APPCDATA,APPCEPLC,APPCLOG)              
*                                                                               
         LH    R1,=Y(APPCM3LQ)                                                  
         SH    R1,=H'3'            TWO FOR LENGTH, ONE FOR EX                   
         SH    R1,=H'6'            EXTRA LENGTH FOR ' XXXXX'                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   APPCM3(0),APPCDTA   '*DDS*REPORT IS OPENED XXXXX'?               
         BE    *+6                                                              
         DC    H'0'                INVALID RESPONSE TO LAST SEND                
         PACK  DUB,APPCDTA+22(5)   CONTROL INTERVAL ADDRESS                     
         CVB   R1,DUB                                                           
         STH   R1,SAVEREF          REFERENCE NUMBER IN OTHER COUNTRY            
         DROP  R4                                                               
*                                                                               
XMIT40   GOTO1 =V(DATAMGR),DMCB,(0,=C'READ'),CFPQID,0,R,CXREC                   
         CLI   DMCB+8,0            READ A CARD                                  
         BNE   XMIT50                                                           
*                                                                               
         MVC   P,R                                                              
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   APPCDTA(5),=C'*REC*'                                             
         MVC   APPCDTA+5(L'R),R                                                 
         MVC   APPCLEN,=Y(APPCMAXL)                                             
         MVC   APPCMXLN,=Y(APPCMXLQ)                                            
         MVC   APPCACTN,APPCACSD   ISSUE SEND DATA                              
*                                                                               
         LINK  EPLOC=APPCEPLC,                                         +        
               PARAM=(APPCACTN,APPCTOKN,APPCDATA,APPCEPLC,APPCLOG)              
         B     XMIT40                                                           
*                                                                               
XMIT50   MVC   OPMS1(12),=C'END TRANSMIT'                                       
*                                                                               
         XC    APPCDTA,APPCDTA                                                  
         MVC   APPCLEN,=Y(APPCM4LQ) LENGTH OF MESSAGE                           
         LH    R1,=Y(APPCM4LQ)                                                  
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APPCDTA(0),APPCM4   '*DDS*END OF REPORT'                         
         MVC   APPCMXLN,=Y(APPCMXLQ)                                            
         MVC   APPCACTN,APPCACSR   ISSUE SEND/RECIEVE                           
*                                                                               
         LINK  EPLOC=APPCEPLC,                                         +        
               PARAM=(APPCACTN,APPCTOKN,APPCDATA,APPCEPLC,APPCLOG)              
*                                                                               
         LH    R1,=Y(APPCM5LQ)                                                  
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   APPCM5(0),APPCDTA   'REPORT CLOSED'?                             
         BE    *+6                                                              
         DC    H'0'                INVALID RESPONSE TO LAST SEND                
*                                                                               
* ENQUEUE THE APPROPRIATE PRINT QUEUE                                           
*                                                                               
         BAS   RE,PQLOCK           ENQUEUE THE PRINT QUEUE                      
*                                                                               
         MVC   CIADDR(2),RPTTRKNO  TRACK NUMBER                                 
         MVI   CIADDR+2,1          ALWAYS START WITH BLOCK 1                    
         MVI   CIADDR+3,0          RECORD 0                                     
         BAS   RE,GETXPE           FIND INDEX PAGE/ENTRY                        
         BAS   RE,GETXAD           FIND INDEX DISK ADDRESS                      
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),CFPQID,CXADDR,CXREC              
         CLI   DMCB+8,0            ANY DMGR ERRORS?                             
         BE    *+10                                                             
         BAS   RE,PQUNLK           YES -- DEQUEUE THE PRINT QUEUE               
         DC    H'0'                                                             
*                                                                               
         LH    RE,CXENTRY          INDEX PAGE ENTRY                             
         MH    RE,CINDXLN          LENGTH OF PQ KEY                             
         LA    R4,CXREC(RE)        R4 = A(INDEX ENTRY)                          
         USING PQRECD,R4                                                        
         NI    PQTYPE,X'FF'-PQTYRSND TURN OFF 'SEND PENDING' BIT                
         TM    PQATTB,PQATJOBI     DID WE JUST SEND JCL?                        
         BZ    *+8                                                              
         OI    PQSTAT,PQSTTE       YES -- SET 'CREATION IN PROGRESS'            
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMWRT'),CFPQID,CXADDR,CXREC               
*                                                                               
         L     RF,APQSAVE                                                       
         USING SKBUFFD,RF                                                       
         XC    SKFCTRL,SKFCTRL     START READING FROM SCRATCH                   
         XC    SKXCTRL,SKXCTRL                                                  
         MVC   SKLABEL,=C'*PQSAVE*'                                             
         MVC   SKINTNO,CFPQINUM    INTERNAL FILE NUMBER                         
         MVC   SKEXTNO,CFPQENUM    EXTERNAL FILE NUMBER                         
         MVC   SKFSTCI(2),RPTTRKNO CONTROL INTERVAL                             
         MVI   SKFSTCI+2,1         ALWAYS START WITH BLOCK 1                    
         DROP  RF                                                               
*                                                                               
         XC    R,R                                                              
         MVI   R+4,C'L'            SILLY PARAMETER FOR RANDOM READ              
         GOTO1 =V(DATAMGR),DMCB,(0,=C'RANDOM'),CFPQID,0,R,CXREC                 
         CLI   DMCB+8,0            ANY DMGR ERRORS?                             
         BE    *+10                                                             
         BAS   RE,PQUNLK           YES -- DEQUEUE THE PRINT QUEUE               
         DC    H'0'                                                             
*                                                                               
         LA    R4,CXREC            A(FIRST CONTROL INTERVAL)                    
         NI    PQTYPE,X'FF'-PQTYRSND TURN OFF 'SEND PENDING' BIT                
         TM    PQATTB,PQATJOBI     DID WE JUST SEND JCL?                        
         BZ    *+8                                                              
         OI    PQSTAT,PQSTTE       YES -- SET 'CREATION IN PROGRESS'            
         MVC   PQRNUM,SAVEREF      SAVE REFERENCE NUM IN OTHER COUNTRY          
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMWRT'),CFPQID,CIADDR,CXREC               
*                                                                               
         BAS   RE,PQUNLK           DEQUEUE THE PRINT QUEUE                      
         CLI   DMCB+8,0            ANY ERROR ON THE DMWRT?                      
         BE    *+6                 NO                                           
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
* BY THIS POINT, THE PRINT QUEUE HAS BEEN DEQUEUED                              
*                                                                               
         GOTO1 =V(DMENQDEQ),DMCB,(C'D',=C'ALL')                                 
*                                                                               
         CLI   JESMSG,C'Y'         ARE WE DISPLAYING CONSOLE MESSAGES?          
         BNE   XMIT60              NO                                           
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
XMIT60   MVC   OPMS1,NORPTMSG                                                   
         LA    R5,RPTTABLQ(R5)     FORWARD POINTER BECOMES NEW START            
         CLC   HIGHRPT(RPTTABLQ),0(R5)  END OF THE TABLE?                       
         BE    *+12                                                             
         ST    R5,ANEXTRPT                                                      
         B     XMITX                                                            
         XC    ANEXTRPT,ANEXTRPT   YES - NO MORE REPORTS TO TRANSMIT            
         DROP  R5                                                               
*                                                                               
XMITX    B     XIT                                                              
         EJECT                                                                  
CONVTIME NTR1  ,                   R2 = A(NUMBER)                               
*                                                                               
         XC    DUB,DUB                                                          
         LA    RF,DUB                                                           
CT10     CLI   0(R2),C'0'          IS CHARACTER NUMERIC?                        
         BL    CT20                                                             
         CLI   0(R2),C'9'                                                       
         BH    CT20                                                             
         MVC   0(1,RF),0(R2)       YES -- SAVE IT                               
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         B     CT10                                                             
*                                                                               
CT20     GOTO1 =V(NUMVAL),DMCB,DUB,(2,0)                                        
         CLI   DMCB,0              DID WE GET VALUE?                            
         BE    *+10                                                             
         XC    DMCB+4(4),DMCB+4    NO -- RETURN ZERO                            
         ST    R2,DMCB             A(STRING END)                                
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
WAITABIT NTR1                                                                   
*                                                                               
* IF LESS THAN THE REQUESTED TIME INTERVAL HAS ELAPSED SINCE THE                
* LAST SEARCH OF THE PRINT QUEUES, THEN WAIT UNTIL THE FULL INTERVAL            
* HAS EXPIRED BEFORE SEARCHING AGAIN.  ALSO, CHECK FOR AN OPERATOR              
* INTERRUPT.                                                                    
*                                                                               
         SR    R0,R0                                                            
WB10     TIME  BIN                 GET THE CURRENT TIME                         
         LTR   R0,R0                                                            
         BZ    WB10                BAD RETURN FROM MACRO                        
*                                                                               
         S     R0,PQSRCHTM         R0 = ELASPED TIME SINCE PQ SEARCH            
         L     R2,WAITSECS         R2 = THE WAIT INTERVAL                       
         CR    R0,R2               HAVE WE WAITED THE FULL INTERVAL?            
         BNL   WB20                YES                                          
*                                                                               
         SR    R2,R0               R2 = REMAINING TIME TO WAIT                  
         ST    R2,FULL                                                          
         STIMERM SET,ID=STIMER2,BINTVL=FULL,WAIT=YES                            
         LTR   RF,RF               WAIT THE REQUESTED INTERVAL                  
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WB20     L     RF,AOPERECB         A(OPERATOR ECB)                              
         TM    0(RF),X'40'         DID THE OPERATOR INTERRUPT?                  
         BZ    *+8                 NO                                           
         BAS   RE,CHKOPER          EXAMINE THE OPERATOR INTERRUPT               
*                                                                               
         B     XIT                                                              
         SPACE 2                                                                
WAITSECS DC    F'12000'            DEFAULT WAIT TIME = 120 SECONDS              
         EJECT                                                                  
* THE TIMER EXIT ROUTINE.  IT POSTS THE TIMERECB.                               
*                                                                               
         DROP  RB,RA,R9                                                         
         SPACE 3                                                                
TIMERXIT SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING TIMERXIT,RB                                                      
         POST  TIMERECB                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         DROP  RB                                                               
         SPACE 2                                                                
TIMERECB DC    F'0'                ECB OF ATTACHED TASK TIMER                   
         SPACE 3                                                                
         USING LUNATIC,RB,RA,R9                                                 
         EJECT                                                                  
* CANCEL THE TIMER                                                              
*                                                                               
CANTIMER STIMERM CANCEL,ID=STIMER1                                              
         LTR   RF,RF                                                            
         BZR   RE                                                               
         DC    H'0'                UNSUCCESSFUL TIMER CANCEL                    
         SPACE 5                                                                
OPENCTFL NTR1                                                                   
*                                                                               
         BC    0,OPENCTX           FIRST TIME THROUGH, DON'T EXIT               
         MVI   *-3,X'F0'           ONLY OPEN CONTROL SYSTEM ONCE                
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',AIO,0                                              
*                                                                               
OPENCTX  B     XIT                                                              
         EJECT                                                                  
* CONVERT THE BINARY 4/3 SECOND UNIT TIME IN THE FIRST TWO BYTES OF             
* 'THREE' INTO FORMAT X'HHMMSS', AND PUT THE RESULT BACK INTO THREE.            
*                                                                               
CONVTHMS SR    R0,R0                                                            
         ICM   R0,3,THREE          BINARY UNIT TIME IN FIRST TWO BYTES          
         SLL   R0,2                TIMES 4                                      
         SRDL  R0,32               PREPARE FOR DIVIDE                           
         D     R0,=F'3'            R1 = TIME IN SECONDS ONLY                    
         SR    R0,R0                                                            
         D     R0,=F'60'           R1 = TIME IN MINUTES ONLY                    
         STC   R0,THREE+2          NUMBER OF SECONDS                            
         SR    R0,R0                                                            
         D     R0,=F'60'           R1 = TIME IN MINUTES ONLY                    
         STC   R0,THREE+1          NUMBER OF MINUTES                            
         STC   R1,THREE            NUMBER OF HOURS                              
*                                                                               
         BR    RE                                                               
         EJECT                                                                  
CHKOPER  NTR1                                                                   
*                                                                               
* THE OPERATOR HAS INTERRUPTED WITH EITHER A 'STOP' OR 'MODIFY'                 
* COMMAND.  EXAMINE THE COMMAND AND TAKE THE APPROPRIATE ACTION.                
*                                                                               
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BNE   CH10                                                             
         MVI   OPERSTOP,C'Y'       YES -- SET STOP FLAG                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'STOP COMMAND ACCEPTED'             
         B     CHX                                                              
*                                                                               
CH10     CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    *+6                 YES                                          
         DC    H'0'                WHAT DID THE OPERATOR DO?                    
*                                                                               
         CLC   CIBDATLN,=H'6'      LENGTH OF 'MODIFY' STRING                    
         BNE   CHBAD                                                            
         CLC   =C'JESMSG',CIBDATA  TOGGLE JESMSG SWITCH?                        
         BNE   CHBAD                                                            
         CLI   JESMSG,C'Y'         YES                                          
         BNE   *+18                                                             
         MVI   JESMSG,C'N'                                                      
         MVC   OPMS3+33(3),=C'OFF'                                              
         B     *+14                                                             
         MVI   JESMSG,C'Y'                                                      
         MVC   OPMS3+33(3),=C'ON '                                              
         L     R1,AASCB            A(ASCB)                                      
         MVC   HALF,ASCBASID-ASCB(R1)                                           
         GOTO1 =V(HEXOUT),DMCB,HALF,OPMS3+7,2,=C'TOG'                           
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS3,OPMS3)                       
         B     CHX                                                              
*                                                                               
CHBAD    GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'*INVALID LUNATIC COMMAND*'         
*                                                                               
CHX      L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         DROP  RF                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
*                                                                               
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* ++INCLUDE DMPRTQR                                                             
         PRINT OFF                                                              
       ++INCLUDE DMPRTQR                                                        
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 2                                                                
         GETEL R4,28,ELCODE                                                     
         SPACE 2                                                                
ECBLST   DC    A(0)                A(OPERATOR ECB IS STORED HERE)               
         DC    X'80',AL3(TIMERECB) A(TASK TIMER ECB)                            
         SPACE 2                                                                
ELCODE   DC    X'02'               ID ELEMENT                                   
HIGHRPT  DC    (RPTTABLQ)X'FF'     HIGH RECORD FOR REPORT TABLE                 
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DMWORK   DS    12D                                                              
DMCB     DS    6F                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
THREE    DS    XL3                                                              
WORK     DS    CL64                                                             
         SPACE 2                                                                
SSB      DC    F'0'                FOR DATAMGR (OFFLINE)                        
UTL      DC    F'0',X'0A'          FOR DATAMGR (CONTROL SYSTEM)                 
         SPACE 2                                                                
AIO      DC    A(IO)               A(IO AREA)                                   
ARPTTAB  DC    A(RPTTABLE)         A(REPORT TABLE)                              
ACITABLE DC    A(CITABLE)          A(PRINT QUEUE TABLE)                         
AOPERECB DC    A(0)                A(ECB OF OPERATOR INTERRUPT)                 
APQSAVE  DS    A                   A(PQ BUFFER SAVE AREA)                       
ACOMM    DS    A                   A(COMMUNICATIONS PARAMETER LIST)             
AASCB    DS    A                   A(ASCB) FROM EXTRACT                         
         SPACE 2                                                                
PQSRCHTM DS    F                   TIME WE LAST SEARCHED PRINT QUEUES           
RPTTBLSZ DS    A                   NUMBER OF ENTRIES IN REPORT TABLE            
ANEXTRPT DS    A                   A(NEXT REPORT IN TABLE TO TRANSMIT)          
USERFILT DC    H'0'                OPTIONAL HEX USERID FILTER                   
SUBFILT  DC    CL3'   '            OPTIONAL SUBID FILTER                        
SAVERCI  DS    H                   REPORT CONTROL INTERVAL NUMBER               
SAVEREF  DS    H                   REPORT REFERENCE NUMBER                      
         SPACE 2                                                                
RPTCLASS DS    C                   REPORT CLASS                                 
RPTLPP   DS    X                   REPORT LINES PER PAGE                        
RPTFORMS DS    CL4                 REPORT FORMS CODE                            
RPTCHARS DS    CL4                 REPORT CHARACTER SET                         
RPTCOPYS DS    X                   REPORT NUMBER OF COPIES                      
RPTDESC  DS    CL11                REPORT DESCRIPTION                           
RPTMAKER DS    CL5                 REPORT MAKER                                 
         SPACE 2                                                                
RPTTYPE  DS    C                   'J' = JCL, 'O' = JOB OUTPUT                  
FRSTLINE DS    C                   'Y' = HAVEN'T WRITTEN ANYTHING YET           
MODE     DC    C' '                'S' = SEND, 'R' = RECEIVE                    
OPERSTOP DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
JESMSG   DC    C'N'                'Y' = DO CONSOLE MSGS FOR START/END          
STIMER1  DS    XL4                 FOR TIMER POPS                               
STIMER2  DS    XL4                 FOR TIMER POPS                               
PRTQUE   DC    C'PRTQU'            PRTQ NAME                                    
NORPTMSG DC    C'NO REPORT TRANSMITTING NOW     '                               
MYNAME   DC    CL8' '              LUID OF THIS PROGRAM                         
TALKTO   DC    CL8' '              LUID WE ARE TALKING TO                       
OPMS1    DS    CL31                                                             
OPMS3    DC    C'ASID = XXXX:  START/END MESSAGES XXX'                          
CARD     DS    CL80                FOR INPUT/OUTPUT OF CONTROL CARDS            
KEY      DS    XL40                FOR PQ INDEX AND CTFILE READS                
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL200               PQ RECORD                                    
         SPACE 2                                                                
APPCACTN DS    CL4                 ACTION CODE                                  
APPCACOS DC    C'OS  '             ACTION OPEN SEND                             
APPCACOR DC    C'OR  '             ACTION OPEN RECEIVE                          
APPCACSD DC    C'SD  '             ACTION SEND DATA                             
APPCACSF DC    C'SF  '             ACTION SEND FLUSH                            
APPCACSC DC    C'SC  '             ACTION SEND CONFIRMATION                     
APPCACSR DC    C'SR  '             ACTION SEND/RECEIVE                          
APPCACR  DC    C'R   '             ACTION RECEIVE                               
APPCACPC DC    C'PC  '             ACTION PREPARE TO CLOSE                      
APPCACC  DC    C'C   '             ACTION CLOSE                                 
APPCTOKN DC    F'0'                                                             
APPCEPLC DS    CL8                 PROVIDED BY APPC OPEN CALL                   
APPCLOG  DCB   DDNAME=APPCLOG,RECFM=FBA,LRECL=133,MACRF=PM,DSORG=PS             
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
APPCM1   DC    C'*DDS*HERE COMES A REPORT'                                      
APPCM1LQ EQU   (*-APPCM1)+2                                                     
*                                                                               
APPCM2   DC    C'*DDS*GIVE ME THE REPORT'                                       
APPCM2LQ EQU   (*-APPCM2)+2                                                     
*                                                                               
APPCM3   DC    C'*DDS*REPORT IS OPENED XXXXX'                                   
APPCM3LQ EQU   (*-APPCM3)+2                                                     
*                                                                               
APPCM4   DC    C'*DDS*END OF REPORT'                                            
APPCM4LQ EQU   (*-APPCM4)+2                                                     
*                                                                               
APPCM5   DC    C'*DDS*REPORT CLOSED'                                            
APPCM5LQ EQU   (*-APPCM5)+2                                                     
         EJECT                                                                  
         PRINT OFF                                                              
         IEZCOM                                                                 
         PRINT ON                                                               
         DS    0D                                                               
         DC    C'*CXREC**'                                                      
CXREC    DC    14336X'00'          PRINT QUEUE BUFFER                           
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
*                                                                               
         DS    0D                                                               
         DC    C'***IO***'                                                      
IO       DC    1000X'00'           CTFILE I/O AREA (FOR ID RECORDS)             
         EJECT                                                                  
         DS    0D                                                               
         DC    C'CITABLE*'                                                      
CITABLE  DS    (NUMPQS)XL(CITBLLNQ)                                             
         DC    X'FF'               END OF TABLE MARKER                          
*&&US                                                                           
NUMPQS   EQU   8                   8 PRINT QUEUES                               
*&&                                                                             
*&&UK                                                                           
NUMPQS   EQU   2                   2 PRINT QUEUES                               
*&&                                                                             
         SPACE 2                                                                
CIDATAD  DSECT                                                                  
CFPQENUM DS    X                   PRINT QUEUE EXTERNAL FILE NUMBER             
         DS    XL7                 SPARE                                        
       ++INCLUDE DMPRTQW                                                        
CITBLLNQ EQU   *-CIDATAD                                                        
         EJECT                                                                  
LUNATIC  CSECT                                                                  
         DS    0D                                                               
         DC    C'RPTTABLE'                                                      
RPTTABLE DS    (MAXRPTS)XL(RPTTABLQ)                                            
MAXRPTS  EQU   1000                                                             
         SPACE 2                                                                
RPTTABLD DSECT                                                                  
RPTTIME  DS    XL2                 REPORT CREATION TIME (SORT KEY)              
RPTPQNUM DS    X                   REPORT PRINT QUEUE NUMBER                    
RPTKEY   DS    0XL7                REPORT PRINT QUEUE KEY                       
RPTREPUI DS    XL2                 REPORT USER-ID                               
RPTSUBID DS    CL3                 REPORT SUB-ID                                
RPTREFNO DS    XL2                 REPORT REFERENCE NUMBER                      
RPTTRKNO DS    XL2                 REPORT TRACK NUMBER                          
RPTREPUA DS    CL8                 ALPHA USERID                                 
RPTTABLQ EQU   *-RPTTABLD                                                       
         EJECT                                                                  
       ++INCLUDE DMPRTQD                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQS                                                        
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         IHAASCB                                                                
         EJECT                                                                  
         DSECT                                                                  
         IEZCIB                                                                 
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052DDLUNATIC 05/01/02'                                      
         END                                                                    
