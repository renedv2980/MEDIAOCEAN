*          DATA SET NEWRI85    AT LEVEL 035 AS OF 04/10/14                      
*PHASE T32085A,+0                                                               
*                                                                               
         TITLE 'T32085 - NETWORK UNIT HISTORY UPDATE'                           
************************************************************                    
*                                                                               
* THIS REPORT READS RECOVERY FILE AND UPDATES NETWORK UNIT                      
* HISTORY RECORDS                                                               
*                                                                               
* HISTORY RECORD KEY IS BASED ON X'84' KEY                                      
* IN CASE OF A TIME CHANGE, SINCE QUARTER HOUR IS NOT ON X'84'                  
* KEY, THE HISTORY REC IS NOT DELETED AND THE NEW TIME IS SIMPLY                
* ADDED TO THE HISTORY REC AS A CHANGE ELEMENT                                  
* IN CASE OF A DATE CHANGE, SINCE THAT IS ON X'84' KEY, PROGRAM                 
* DELETES THE HISTORY REC AND ADDS A NEW ONE.                                   
*************************************************************                   
         SPACE 2                                                                
T32085   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T32085*,RA,R8                                                 
         USING T32085,RB,RA,R8                                                  
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
                                                                                
         L     R6,ATWA                                                          
         USING T320FFD,R6                                                       
                                                                                
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
*                                                                               
         L     R1,ANETWS1        ANETWS1 AND 2  FOR CLIENT RECORD               
         ST    R1,NBACLI                                                        
*                                                                               
         L     R7,ANETWS3                                                       
         USING MYWORKD,R7                                                       
                                                                                
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
***      LA    RE,HEDSPECS                                                      
***      ST    RE,SPECS                                                         
***      LA    R1,HDRTN                                                         
***      ST    R1,HEADHOOK                                                      
                                                                                
         DROP  R5                                                               
*                                                                               
         L     R5,=A(MYIO)                                                      
         USING RECD,R5                                                          
         LA    RE,28(R5)           POINT PAST RECV HEADER                       
         ST    RE,AUNITREC         UNIT RECORD POINTER                          
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    MAINLINE                                                         
EXIT     XIT1                                                                   
                                                                                
         DROP  R6                                                               
                                                                                
         EJECT                                                                  
*                                                                               
MAINLINE DS    0H                                                               
         SPACE                                                                  
         BAS   RE,INIT         INITIALIZE                                       
         BAS   RE,NET1         READ RECOVERY                                    
         CLOSE (RECVIN)        CLOSE RECOVERY                                   
         FREEPOOL RECVIN                                                        
*                                                                               
         B     EXIT                                                             
*                                                                               
COPY     EQU   1                                                                
CHANGE   EQU   2                                                                
ADDS     EQU   3                                                                
*                                                                               
         EJECT                                                                  
                                                                                
INIT     NTR1                                                                   
* - RUN DATE                                                                    
         GOTO1 DATCON,DMCB,(5,0),(2,TODAYC)     COMPRESSED                      
                                                                                
****-->  TESTING                                                                
* FOR RERUNS/TESTS SET RUN DATE AND RECOVERY HEADER                             
***8     MVC   TODAYC,=X'D0D7'          JUN23/04                                
***      MVC   RERUNREC(10),=X'2A03EB9900013CA84042'                            
***      MVC   RERUNREC+10(14),=X'739C533350002818680617D00EE8'                 
***      MVI   RERUNTST,C'Y'   SET RERUN FLAG                                   
****************************************************************                
                                                                                
INIT00   DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,TODAYC),(3,TODAYB)     BINARY                     
         GOTO1 DATCON,DMCB,(2,TODAYC),(X'20',WORK)   YYMMDD PRINTABLE           
                                                                                
         MVC   CHANGDAT(2),WORK+2                                               
         MVI   CHANGDAT+2,C'/'                                                  
         MVC   CHANGDAT+3(2),WORK+4                                             
         MVI   CHANGDAT+5,C'/'                                                  
         MVC   CHANGDAT+6(2),WORK                                               
                                                                                
                                                                                
         MVI   MYSPACES,X'40'                                                   
         MVC   MYSPACES+1(L'MYSPACES-1),MYSPACES                                
         MVC   MYP,MYSPACES                                                     
         LA    RE,WHOTBL                                                        
         LA    RF,L'WHOTBL                                                      
         XCEF                                                                   
         MVI   WHOTBLX,X'FF'                                                    
         OPEN  (RECVIN,(INPUT))                                                 
INITX    B     EXIT                                                             
*                                                                               
         EJECT                                                                  
********************************************************                        
* GETS RECORDS FROM THE RECOVERY FILE                                           
* IF SIGNIFICANT CHANGE TO REC UPDATE HISTORY RECORD                            
*                                                                               
********************************************************                        
         SPACE 2                                                                
NET1     NTR1                                                                   
*                                                                               
*                                                                               
GET      L     R5,=A(MYIO)         REESTABLISH R5                               
         USING RECD,R5                                                          
*                                                                               
         LA    R1,RECVIN                                                        
         L     R0,=A(MYIO)                                                      
         PRINT GEN                                                              
         GET   (1),(0)                                                          
         PRINT NOGEN                                                            
         SPACE                                                                  
         CLI   RFILTY,X'2A'       TEST UNTFILE                                  
         BNE   GET                                                              
*  RECOVERY FILE CAN HAVE EARLY MORNING OFF-LINE CHANGES ON IT                  
*  WITH DATE OF PREVIOUS DAY,  ONN-LINE CHANGES HAVE TODAY'S DATE.              
*  SO BEST TO SKIP CHECK BELOW SINCE WE NEED OFF-LINE CHANGES ALSO.             
*****    CLC   RDATE,TODAYB        ONLY TODAY'S REC                             
*****    BNE   GET                                                              
         CLI   RTASKID,X'FF'       BACKED OUT AFTER A DUMP                      
         BE    GET                 YES/SKIP IT                                  
                                                                                
         CLI   RPRG,X'01'          PFM ?                                        
         BE    GET                 SKIP IT                                      
                                                                                
         L     R6,AUNITREC         POINT TO UNIT RECORD - R6                    
         USING NURECD,R6                                                        
         CLI   NUKTYPE,X'04'       TEST UNIT RECORD                             
         BNE   GET                                                              
         CLI   NUKSUB,X'C1'        SKIP TRAFFIC RECS                            
         BNL   GET                                                              
                                                                                
                                                                                
* TEST **********************************************                           
         B     SKIPTST                                                          
         CLC   NUKDATE,=X'C651'     FEB17/98                                    
         BNE   GET                                                              
         CLI   NUKSUB,1            LINE - 5                                     
         BNE   GET                                                              
         CLC   =C'ABC',NUKNET                                                   
         BNE   GET                                                              
         CLC   =X'BEF9',NUKCLT                                                  
         BNE   GET                                                              
         CLI   NUKEST,19                                                        
         BNE   GET                                                              
SKIPTST  DS    0H                                                               
* *****************************************************                         
                                                                                
                                                                                
* SET ZERO ELEMENT CODE AT E-O-R                                                
         LH    R1,REC-4                                                         
         LA    R1,REC-4(R1)                                                     
         XC    0(2,R1),0(R1)                                                    
                                                                                
* - TEST IF AGENCY                                                              
         CLC   NUKAM,AMSAVE        IS IT REQUESTED AGENCY ?                     
         BNE   GET                                                              
* - TEST IF CLIENT                                                              
         CLI   ONECLT,0            IS IT REQUESTED CLIENT ?                     
         BE    NET10                                                            
         CLC   NUKCLT,ONECLT                                                    
         BNE   GET                                                              
                                                                                
* - TEST IF RERUN                                                               
NET10    CLI   RERUNTST,C'Y'       ARE WE DOING A RERUN?                        
         BNE   NET13                                                            
         CLC   RECVHDR,RERUNREC     HAVE WE REACHED KEY FOR RERUN ?             
         BNE   GET                 NO                                           
         MVI   RERUNTST,0          YES, SO HANDLE ALLRECS FROM HERE ON          
         B     NET30B              TREAT THIS AS AN ADD                         
                                                                                
                                                                                
         EJECT                                                                  
NET13    L     R6,AUNITREC                                                      
         USING NURECD,R6                                                        
         MVC   RECDAT,RDATE        SAVE RECOVERY DATE                           
         MVC   RECTIME,RTIME       SAVE RECOVERY TIME                           
         MVC   RECFILTY,RECVHDR                                                 
                                                                                
         CLI   RRECTY,COPY         TEST IF COPY                                 
         BNE   NET13B                                                           
         MVC   RERUNREC,RECVHDR    FOR RERUN CHECKING IF DUMP                   
         B     NET15                                                            
                                                                                
NET13B   CLI   RRECTY,CHANGE       TEST IF CHA                                  
         BE    NET20                                                            
                                                                                
         MVC   RERUNREC,RECVHDR    FOR RERUN CHECKING IF DUMP                   
         CLI   RRECTY,ADDS         TEST IF ADD                                  
         BE    NET30                                                            
         B     GET                 SKIP ALL ELSE                                
****     DC    H'0'                                                             
         DROP  R5                                                               
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************                     
* - COPY                                                                        
***********************************************************                     
NET15    DS    0H                                                               
                                                                                
         MVI   HISTFLG,C'Y'                                                     
         TM    NUPACKST,X'02'      HISTORY FLAG TURNED ON ?                     
         BO    NET15B              YES                                          
         MVI   HISTFLG,C'N'        NO-SET FLAG IN CASE CHANGE TURNS             
         MVI   PREVREC,COPY           ON AUDIT BIT AND SKIP THIS REC            
         B     GET                                                              
                                                                                
                                                                                
NET15B   DS    0H                                                               
         TM    NURSTAT,X'80'       IF IT'S DELETED                              
         BNO   SKPTHS                                                           
         MVI   HISTFLG,C'D'        SET HISTORY FLAG                             
SKPTHS   MVC   PREVIOUS,RECFILTY   SAVE KEY OF CURRENT COPY                     
         MVI   PREVREC,COPY        SET FLAG                                     
*                                                                               
NET17    DS    0H                  PROCESS CURRENT COPY                         
         L     R4,=A(COPYTBL)                                                   
         BAS   RE,DOHISTRY                                                      
         B     GET                                                              
                                                                                
         EJECT                                                                  
***********************************************************                     
* - CHANGE                                                                      
*                                                                               
* - NOTE SPECIAL CASE CHECK UP FRONT TO CATCH CHANGE TO UNIT                    
* - WHEN HISTORY BIT IS TURNED ON - THESE RECS WILL BE TREATED                  
* - AS ADDS                                                                     
***********************************************************                     
NET20    DS    0H                                                               
         CLI   HISTFLG,C'D'        WAS 'COPY' DELETED?                          
         BNE   NET20A                                                           
         MVI   HISTFLG,0           YES/CLEAR FLAG                               
         TM    NURSTAT,X'80'           IS CHANGE ALSO DELETED?                  
         BO    GET                     ..YES SKIP THIS                          
*                                      ..COULD BE TRAFFIC READS                 
         B     NET22                   ,,NO-DO NORMAL PROCESSING                
*                                      ,,   COULD BE TIME CHANGED AND           
*                                      ,,   THEN CHANGED BACK                   
*                                                                               
NET20A   CLI   HISTFLG,C'Y'        IS IT ALREADY IN HIST STREAM?                
         BE    NET20B              YES - CONTINUE NORMAL PROCESSING             
*                                                                               
         TM    NUPACKST,X'02'      NO       IS HIST FLAG TURNED ON?             
         BO    NET20AA                      YES                                 
         MVI   PREVREC,0                    NO-CLEAR PREVREC TYPE               
         B     GET                          SKIP IT                             
                                                                                
                                                                                
* IF UNIT HIST BYTE IS ON, BUT HISTFLG=N, USUSALLY INDICATES TREATING           
* THIS CHANGE AS AN ADD.                                                        
* HOWEVER OFFLINE CHANGES CAN END UP HERE IF PROGRAM IS NOT WRITING             
* COPY TO RECOVERY FILE.  IN THAT CASE SKIP THE CHANGE.                         
*                                                                               
NET20AA  CLI   PREVREC,COPY        IF PREV WAS A COPY                           
         BE    NET30B              TREAT AS ADD                                 
         L     R1,=A(MYIO)              ELSE- CHECK IF OFFLINE CHANGE           
         USING RECD,R1                                                          
         CLI   RPRG,0                    OFFLINE CHANGE?                        
         BE    GET                       SKIP                                   
         B     NET30B                    NO-TREAT AS ADDED REC                  
         DROP  R1                                                               
*                                                                               
NET20B   CLI   PREVREC,COPY       PREVIOUS REC MUST BE A COPY                   
         BE    *+12                                                             
         BAS   RE,CHANGERR         ELSE PRINT THIS REC AS ERROR                 
         B     GET                 AND GET NEXT REC                             
                                                                                
         TM    NUPACKST,X'02'      HISTORY FLAG TURNED ON ?                     
         BO    NET20C              YES                                          
         MVI   PREVREC,0           NO                                           
         B     GET                 IGNORE THIS REC                              
                                                                                
NET20C   TM    NURSTAT,X'80'       DELETED ?                                    
         BNO   NET22               NO                                           
         MVI   PREVREC,0           CLEAR PREV REC BECAUSE                       
*                                  IF KEY CHANGE OR NOT WE GO OFF               
*                                  TO GET NEXT REC AND NOT CLEARING             
*                                  CAUSES ERRORNEOUS ERROR MESSAGE              
*                                  OF 'COPY NO CHANGE'                          
         L     R6,NBAIO            YES - IS THERE A X'90' ELEMENT?              
         MVI   ELCODE,X'90'                                                     
         BAS   RE,GETEL                                                         
         BE    NET20D                    YES                                    
*****    MVI   PREVREC,0                                                        
***      B     GET                       NO - ASSUME THIS IS A DELETE           
*                                             WITH NO DATE/TIME CHANGE          
*                                             CURRENTLY NOT ALLOWED             
*                                             IN NBUY                           
         DC    H'0'                LET'S TAKE A HIT HERE                        
*                                  AND TRAP POTENTIAL PROBLEMS                  
                                                                                
NET20D   BAS   RE,PROCDEL                YES/PROCESS DELETED UNIT REC           
         BE    NET21               NO KEY CHANGE                                
         CLI   PREVREC,X'FF'       HIST ERROR                                   
         BE    NET30B              ADD CHANGE AS NEW ADDED REC                  
         B     GET                 KEY CHANGED NEW REC ADDED IN PROCDEL         
                                                                                
* - SPECIAL PROCESSING IN CASE THIS DELETED CHANGE                              
* - WAS A TIME CHANGE                                                           
NET21    L     R4,=A(COPYTBL)       SET THIS DELETED CHANGE TO COPY TBL         
         BAS   RE,DOHISTRY                                                      
         B     GET                 AND GET UNIT                                 
                                                                                
* BE CAREFUL HERE - 'ADD' IN CASE OF TIME CHANGE COMES IN HERE                  
*                   FROM 'ADD' LOGIC                                            
NET22    L     R4,=A(CHANGTBL)                                                  
         BAS   RE,DOHISTRY         PUT HISTRY DATA TO CHANGTBL                  
************************************************************                    
* DO WE HAVE HISTORY REC FOR THESE COPY/CHG                                     
* IF NOT ADD HISTORY REC FOR THE CHANGE AND IGNORE ALL ELSE                     
         USING HISTDATD,R4                                                      
                                                                                
         L     R6,NBAIO            POINT R4 TO UNITKEY (X'04' KEY)              
                                                                                
         LA    R5,KEY             CREATE HISTORY KEY FROM X'04' KEY             
         USING NHRECD,R5                                                        
         XC    KEY,KEY                                                          
         MVI   NHKTYPE,X'40'                                                    
         MVC   NHKPAM,NUKAM                                                     
         MVC   NHKPCLT,NUKCLT                                                   
         MVC   NHKNET,NUKNET                                                    
         MVC   NHKPROG,NUKPROG                                                  
         MVC   NHKDATE,NUKDATE                                                  
         MVC   NHKEST,NUKEST                                                    
         MVC   NHKSUB,NUKSUB                                                    
         MVC   NHKDP,NUKDP                                                      
         BAS   RE,MYHIGH                                                        
         CLC   KEY(20),KEYSAVE                                                  
         BE    HISTOK                                                           
*                                                                               
         L     R6,NBAIO                                                         
         USING NURECD,R6                                                        
****     GOTO1 =V(PRNTBL),DMCB,=C'HIST',0(R6),C'DUMP',30,=C'1D'                 
         MVC   MYP(4),=C'HIST'                                                  
         GOTO1 HEXOUT,DMCB,0(R6),MYP+5,40                                       
         BAS   RE,PRINTIT                                                       
         B     NET30B            NO - HISTORY TREAT AS ADD                      
         DROP  R4,R5                                                            
HISTOK   EQU   *                                                                
************************************************************                    
         BAS   RE,COMPARCT         COMPARE COPY TO CHANGE                       
         CLI   UPDTFLG,C'Y'        UPDATED?                                     
         BNE   NET24                                                            
         BAS   RE,ACTIVDAT         UPDATE ACTIVITY DATE                         
         BAS   RE,MYPUT            YES                                          
NET24    MVI   PREVREC,0           CLEAR PREVREC FLAG                           
                                                                                
         B     GET                 GET NEXT RECORD                              
                                                                                
         EJECT                                                                  
                                                                                
*********************************************************                       
* ADD                                                                           
*                                                                               
* NOTE SPECIAL CASE OF 'CHANGE' TREATED AS 'ADD' WHEN HISTORY BIT               
* IS JUST TURNED ON                                                             
*                                                                               
* NOTE SPECIAL CASE WHEN HISTORY REC ALREADY EXISTS FOR                         
* NEWLY ADDED UNIT -                                                            
*                                                                               
*                                                                               
*   **** NOTE **** R1 ->->->-> HISTORY RECORD   BECAREFUL!!!                    
*                                                                               
*********************************************************                       
NET30    DS    0H                                                               
         TM    NUPACKST,X'02'      HISTORY FLAG TURNED ON ?                     
         BNO   GET                 NO -  IGNORE THIS REC                        
*                                                                               
         CLI   PREVREC,COPY       PREVIOUS REC A COPY ?                         
         BNE   *+8                NO                                            
         BAS   RE,COPYERR         YES/PRINT OUT PREVIOUS COPY AS ERROR          
*                                                                               
NET30B   MVI   PREVREC,0           CLEAR PREVREC FLAG                           
*                                                                               
* - CREATE/ADD NEW HISTORY RECORD                                               
         BAS   RE,FILLDBLK                                                      
         CLC   SAVCLT2,NBACTCLI    SAME CLIENT ?                                
         BE    NET30D                                                           
                                                                                
*******************************************************************             
* - GET CLIENT RECORD FOR PRINTABLE CLIENT CODE                                 
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),NBACTAM                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY,0                     
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,MYDMWORK                                                      
         L     R4,=A(MYIO2)                                                     
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,(R4),(R3),0           
         USING CLTHDR,R4                                                        
         GOTO1 CLUNPK,DMCB,(CPROF+6,NBACTCLI),SAVECLT                           
         MVC   SAVCLT2,NBACTCLI                                                 
         B     NET30D                                                           
         DROP  R4                                                               
**************************************************************                  
                                                                                
NET30D   L     R1,=A(MYIO2)          !!!!! R1 -> HIST REC !!!!!!                
         XC    0(255,R1),0(R1)                                                  
                                                                                
         USING NHRECD,R1                                                        
         MVI   NHKTYPE,X'40'                                                    
         MVC   NHKPAM,NBACTAM                                                   
         MVC   NHKPCLT,NBACTCLI                                                 
         MVC   NHKNET,NBACTNET                                                  
         MVC   NHKPROG,NBACTPRG                                                 
         MVC   NHKDATE,NBACTDAT                                                 
         MVC   NHKEST,NBACTEST                                                  
         MVC   NHKSUB,NBACTSUB                                                  
         MVC   NHKDP,NBACTDP                                                    
*                                                                               
*****    CLC   =X'38',NBACTSQH      TEST TEST TEST                              
*****    BE    GET                                                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(20),0(R1)       CREATE KEY FOR ADDREC                        
*                                                                               
         BAS   RE,MYHIGH           CHECK IF HISTORY REC ALREADY EXISTS?         
         CLC   KEY(20),KEYSAVE                                                  
         BNE   NET40               NO - REGULAR 'ADD' PROCESSING                
                                                                                
*********************************************************************           
*********************************************************************           
* HISTORY REC ALREADY EXISTS FOR THIS 'ADD' UNIT                                
* MEANS THIS UNIT IS AN ADD FROM A DAY/DATE CHANGE                              
* HISTORY REC WAS CREATED IN 'PROCDEL' ROUTINE                                  
* HERE WE NEED TO GET USER ID FROM UNIT REC AND SET IT INTO                     
* PROPER X'05' ELEM OF HISTORY REC                                              
*                                                                               
* NOTE - COULD ALSO MEAN THERE WAS A TIME CHANGE/ THIS DOES NOT                 
*        AFFECT KEY OF HISTORY REC BUT WE NEED TO KEEP TRACK                    
*        OF CHANGES                                                             
                                                                                
                                                                                
                                                                                
         BAS   RE,MYGET            YES - GET THE HISTORY RECORD                 
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUSDRD,R6                                                        
         TM    NUACT2WY,X'40'      DATE CHANGE?                                 
         BO    NET33                                                            
         TM    NUACT2WY,X'20'      TIME CHANGE?                                 
         BO    NET22               YES/TREAT THIS AS A CHANGE                   
***      DC    H'0'                   INSTEAD OF TAKING A HIT WE WILL           
**       REPLACE HISTROY REC OF 'DELETED' UNIT WITH                             
**       THE NEW HISTORY REC                                                    
         MVI   PREVREC,X'FF'       USE PREVREC AS FLAG                          
         B     NET40                                                            
*                                                                               
NET33    L     R6,NBAIO            PULL USER/REASON DATA FROM NEW UNIT          
         MVI   ELCODE,X'99'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST EXIST                                   
         USING NUACTD,R6                                                        
         XC    WORK,WORK                                                        
         CLI   NUACTLEN,12           IF OLD 99 ELEM                             
         BH    NET33B                NO                                         
         MVC   WORK(8),=C'????????'  YES - SET UNKNOWN                          
         B     NET34                 AND SKIP SEARCH FOR CODES                  
                                                                                
NET33B   MVC   WORK+20(3),NUACTRSN   SAVE REASON CODE  IN WORK+20               
         MVC   WORK(2),NUACTAGD    AGENCY                                       
         MVC   WORK+2(2),NUACTCID  CHANGE PERSONAL ID                           
         OC    NUACTCID,NUACTCID                                                
         BNZ   *+10                                                             
         MVC   WORK+2(2),NUACTAID  CREATION PERSONAL ID                         
         BAS   RE,OWHO             OUTUSER RETURNS USER IN WORK                 
         DROP  R6                                                               
*                                                                               
NET34    L     R6,=A(MYIO2)        POINT TO HISTORY REC                         
         MVI   ELCODE,X'05'        HISTORY ELEMENTS                             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NCHAEL,R6                                                        
NET35    CLI   NCHGFCOD,C'D'       GET DATE CHANGE TYPE                         
         BE    NET37                                                            
NET36    BAS   RE,NEXTEL                                                        
         BE    NET35                                                            
         DC    H'0'                MUST BE HERE !                               
                                                                                
NET37    OC    NCHGUSER,NCHGUSER   IS THERE A USER ?                            
         BNZ   NET36               HAS USER-NOT THE ELEMENT WE WANT             
         DS    0H                  NO USER/ THIS IS DATE CHANGE ELEM            
         MVC   NCHGREAS,WORK+20     SET REASON/USER FROM WORK ABOVE             
         MVC   NCHGUSER,WORK                                                    
         DROP  R6                                                               
         BAS   RE,MYPUT            PUT THE REC BACK TO FILE                     
         B     GET                 AND GET THE NEXT RECORD                      
                                                                                
*************************************************************                   
*************************************************************                   
         EJECT                                                                  
* - NORMAL 'ADD' PROCESSING (I.E. NOT A DAY OR TIME CHANGE )                    
NET40    MVC   NHKRLEN,=H'204'         SET RECORD LENGTH                        
*                                                                               
         MVI   NHMAINEL,X'01'      DO X'01' ELEMENT                             
         MVI   NHMAINLN,177        FIXED ELEMENT LENGTH                         
         MVC   NHCLIENT,SAVECLT    PRINTABLE CLIENT CODE                        
         MVC   NHOLEN,NBLEN                                                     
         MVC   NNHOPRD(2),NBPRD       PRODUCTS (PRD1 AND PRD2)                  
         CLI   NBPRDNO,0              IS IT MULTIPLE PRODUCTS?                  
         BE    *+10                                                             
         MVC   NNHOPRD,NBPRDLST       MULTIPLE PRODUCTS                         
         MVC   NHOACT,NBACTUAL        ACTUAL COST                               
         TM    NBUNITST,X'20'      ACTUAL COST INPUT?                           
         BNO   *+8                                                              
         OI    NHSTAT,X'02'        YES                                          
         MVC   NHODATE,NBACTDAT       DATE                                      
         MVC   NHOTIME,NBTIME         TIME                                      
         MVC   NHOPRNME,NBPROGNM      PROGRAM NAME                              
         MVC   NHOROT,NBSDROT         ROTATION                                  
*                                                                               
         MVC   NHADDAT,RECDAT      DATE UNIT ADDED                              
         MVC   NHADDTM,RECTIME     TIME UNIT ADDED                              
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'02'           UNIT STATUS = NEW?                        
         USING NUSDREL,R6                                                       
         BAS   RE,GETEL               I.E ADDED AFTER HIST FLAG SET             
         BNE   *+16                                                             
         TM    NUSDST4,X'20'          NEW ?                                     
         BNO   *+8                                                              
         OI    NHSTAT,X'01'           YES                                       
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'04'           COMMENTS ?                                
         BAS   RE,GETEL                                                         
         BNE   NET42                                                            
         ZIC   R2,1(R6)            LENGTH OF ELEMENT                            
         S     R2,=F'4'            MINUS ELEM ID/LEN ETC                        
         BCTR  R2,0                MINUS 1 FOR MVC                              
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   NHOCMMT(0),4(R6)                                                 
*                                                                               
NET42    L     R6,NBAIO                                                         
         MVI   ELCODE,X'09'           AUDIT GROUP                               
         USING NUAUDD,R6                                                        
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   NHADTGRP,NUAUDGRP                                                
         DROP  R6                                                               
*                                                                               
         TM    NBUNITST,X'40'      PREEMPT ?                                    
         BNO   *+8                                                              
         MVI   NHOPRE,C'Y'                                                      
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'99'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE 99 ELEMENT                         
         USING NUACTD,R6                                                        
                                                                                
         XC    WORK,WORK                                                        
         CLI   NUACTLEN,12           IF OLD 99 ELEM                             
         BH    NET43                 NO                                         
         MVC   WORK(8),=C'????????'  YES - SET UNKNOWN                          
         B     NET44                 AND SKIP SEARCH FOR CODES                  
                                                                                
NET43    DS    0H                                                               
         MVC   NHREASN,NUACTRSN    REASON CODE                                  
         MVC   WORK(2),NUACTAGD    AGENCY                                       
         MVC   WORK+2(2),NUACTCID  CHANGE PERSONAL ID                           
         OC    NUACTCID,NUACTCID                                                
         BNZ   *+10                                                             
         MVC   WORK+2(2),NUACTAID  CREATION PERSONAL ID                         
         BAS   RE,OWHO             OUTWHOUSER                                   
         MVC   NHUSER,WORK                                                      
* - SAVE REASON USER FOR X'05' MISSED/MKGD ELEM                                 
         MVC   WORK+8(3),NUACTRSN    REASON                                     
*                                                                               
NET44    L     R6,NBAIO            MISSED/MAKEGOOD DATA                         
         MVI   ELCODE,6            MSS/MKGD DETAILS                             
         BAS   RE,GETEL                                                         
         BNE   NET50                                                            
         DROP  R1                                                               
NET45    LA    R2,ELEM                                                          
         USING NCHAEL,R2                                                        
         MVI   ELEM,5              MSS/MKGD DETAILS                             
         MVI   ELEM+1,53           LENGTH 32 + 21(ELEM DATA)                    
         MVC   NCHGREAS,WORK+8      REASON                                      
         MVC   NCHGUSER,WORK        USER FROM ABOVE                             
         MVC   NCHGTIME,RECTIME     TIME                                        
         GOTO1 DATCON,DMCB,(3,RECDAT),(2,NCHGDATE)  DATE                        
         MVI   NCHGFCOD,C'M'                                                    
         CLI   ELCODE,6                                                         
         BE    *+8                                                              
         MVI   NCHGFCOD,C'G'                                                    
         MVC   NCHGFLD(32),0(R6)    MISSED/MAKEGOOD DATA                        
         DROP  R2                                                               
         L     R2,=A(MYIO2)                                                     
         XC    DMCB(16),DMCB                                                    
         GOTO1 HELLO,DMCB,(C'P',=CL8'UNTFILE'),(R2),ELEM,0                      
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
NET48    BAS   RE,NEXTEL           ANY MORE MISSED ELEMENTS                     
         BE    NET45                                                            
         CLI   ELCODE,7            IF WE ARE IN MAKEGOOD LOOP?                  
         BE    NET60              GETOUT                                        
*                                                                               
NET50    L     R6,NBAIO            MAKEGOOD DETAILS?                            
         MVI   ELCODE,7                                                         
         BAS   RE,GETEL                                                         
         BNE   NET60                                                            
         B     NET45                                                            
*                                                                               
NET60    DS    0H                                                               
         BAS   RE,ACTIVDAT          SET ACTIVITY DATE                           
*                                                                               
         CLI   PREVREC,X'FF'       SPECIAL CASE OF ADD BUT HIST                 
         BNE   NET66               RECORD ALREADY EXISTS ?                      
         BAS   RE,MYPUT            YES/PUT IT                                   
         MVI   PREVREC,0           CLEAR FLAG                                   
         B     NET70                                                            
*                                                                               
NET66    BAS   RE,MYADD             NO/ADD RECORD                               
*                                                                               
NET70    B     GET                                                              
                                                                                
         DROP  R6                                                               
               EJECT                                                            
                                                                                
**************************************************                              
* - COMPARE COPY TO CHANGE                                                      
* - IF NOT =, ADD CHANGE TO UNIT HISTORY RECORD                                 
* - R5 -> NEW DATA,  BYTE=FIELD TYPE                                            
***************************************************                             
                                                                                
COMPARCT NTR1                                                                   
*                                                                               
         MVI   UPDTFLG,0                                                        
         L     RE,=A(MYIO2)        CLEAR I/O AREA FOR HIST REC                  
         LA    RF,HMYIOLNE                                                      
         XCEF                                                                   
         L     RE,=A(MYIO2+HMYIOLNE)    CLEAR I/O AREA FOR HIST REC             
         LA    RF,HMYIOLNE                                                      
         XCEF                                                                   
*                                                                               
         L     R4,=A(COPYTBL)      R4 -> COPY                                   
         L     R5,=A(CHANGTBL)      R5 -> CHANGE                                
         LA    R5,HISTPRD-HIUNTKEY(R5)   POINT R5 TO START OF DATA              
         USING HISTDATD,R4                                                      
         CLC   HISTPRD,0(R5)                                                    
         BE    *+12                                                             
         MVI   BYTE,C'B'           BYTE = TYPE OF DATA                          
         BAS   RE,ADDIT           ADD TO HISTORY RECORD                         
         LA    R5,L'HISTPRD(R5)                                                 
*                                                                               
         CLC   HISTLEN,0(R5)       LENGTH                                       
         BE    *+12                                                             
         MVI   BYTE,C'L'                                                        
         BAS   RE,ADDIT                                                         
         LA    R5,L'HISTLEN(R5)                                                 
*                                                                               
         CLC   HISTCOST,0(R5)      ACTUAL COST                                  
         BE    *+12                                                             
         MVI   BYTE,C'A'                                                        
         BAS   RE,ADDIT                                                         
         LA    R5,L'HISTCOST(R5)                                                
*                                                                               
         CLC   HISTDATE,0(R5)      UNIT DATE                                    
         BE    *+12                                                             
         MVI   BYTE,C'D'                                                        
         BAS   RE,ADDIT                                                         
         LA    R5,L'HISTDATE(R5)                                                
*                                                                               
         CLC   HISTIME,0(R5)       START-END TIME                               
         BE    *+12                                                             
         MVI   BYTE,C'T'                                                        
         BAS   RE,ADDIT                                                         
         LA    R5,L'HISTIME(R5)                                                 
*                                                                               
         OC    HISTPNAM,MYSPACES                                                
         OC    0(L'HISTPNAM,R5),MYSPACES                                        
         CLC   HISTPNAM,0(R5)      PROGRAM NAME                                 
         BE    *+12                                                             
         MVI   BYTE,C'N'                                                        
         BAS   RE,ADDIT                                                         
         LA    R5,L'HISTPNAM(R5)                                                
*                                                                               
         CLC   HISTROT,0(R5)        ROTATION                                    
         BE    *+12                                                             
         MVI   BYTE,C'R'                                                        
         BAS   RE,ADDIT                                                         
         LA    R5,L'HISTROT(R5)                                                 
         B     CMP25                                                            
                                                                                
         EJECT                                                                  
                                                                                
* MATCH CHANGE AGAINST COPY MSD                                                 
CMP25    DS    0H                                                               
         LA    R3,8                MAX MISSED = 8                               
         LR    R1,R5               SAVE POINTER TO CHANGE MISSSED               
         LA    R4,HISTMIS                                                       
CMP30    DS    0H                                                               
         CLI   0(R5),0             END OF LIST                                  
         BE    CMP30D                                                           
         BAS   RE,ISIT             IS CHANGE MSD ON COPY MSD                    
         BE    CMP30B              YES                                          
         MVI   BYTE,C'M'                                                        
         BAS   RE,ADDIT            NO / ADD MSD TO HIST REC                     
CMP30B   LA    R5,L'HISTMIS(R5)    BUMP CHANGE POINTER                          
         BCT   R3,CMP30                                                         
                                                                                
* MATCH COPY  AGAINST  MSD                                                      
* IF NO MATCH MEANS A COPY MSD WAS REMOVED                                      
* R5 -> START OF CHANGE MSD AREA                                                
CMP30D   LR    R5,R1               RESET POINTER TO CHANGE MSD                  
         LA    R3,8                MAX MISSED = 8                               
         L     R4,=A(COPYTBL)                                                   
         LA    R4,HISTMIS                                                       
CMP30E   CLI   0(R4),0             END OF LIST FOR COPY MSD                     
         BE    CMP38                                                            
         BAS   RE,NOTIT            IS COPY MSD ON CHANGE MSD                    
         BE    CMP30EE                                                          
         MVI   BYTE,C'M'           SET TYPE                                     
         MVI   WORK,C'-'           SET REMOVED FLAG                             
         LR    R1,R5               SAVE CHANGE POINTER                          
         LR    R5,R4               ( ADDIT EXPECTS R5-> DATA )                  
         BAS   RE,ADDIT                                                         
         LR    R5,R1               RETURN SAVED POINTER TO R5                   
CMP30EE  LA    R4,L'HISTMIS(R4)    BUMP COPY MSD                                
         BCT   R3,CMP30E                                                        
                                                                                
         EJECT                                                                  
                                                                                
* NOW DEAL WITH MKGD                                                            
CMP38    L     R4,=A(COPYTBL)     RESET R4-> COPY TABLE ADDRESS                 
         L     R5,=A(CHANGTBL)           R5 -> CHANGE                           
         LA    R5,HISTMKD-HIUNTKEY(R5)   POINT R5 TO START OF MKGD              
                                                                                
* MATCH CHANGE AGAINST COPY                                                     
         LA    R3,8                MAX MISSED = 8                               
         LR    R1,R5               SAVE POINTER TO CHANGE MISSSED               
         LA    R4,HISTMKD                                                       
CMP39    DS    0H                                                               
         CLI   0(R5),0             END OF LIST                                  
         BE    CMP39D                                                           
         BAS   RE,ISIT             IS CHANGE MSD ON COPY MSD                    
         BE    CMP39B              YES                                          
         MVI   BYTE,C'G'                                                        
         BAS   RE,ADDIT            NO / ADD MSD TO HIST REC                     
CMP39B   LA    R5,L'HISTMKD(R5)    BUMP CHANGE POINTER                          
         BCT   R3,CMP39                                                         
                                                                                
* MATCH COPY  AGAINST  MSD                                                      
* IF NO MATCH MEANS A COPY MSD WAS REMOVED                                      
* R5 -> START OF CHANGE MSD AREA                                                
CMP39D   LR    R5,R1               RESET POINTER TO CHANGE MKD                  
         LA    R3,8                MAX MISSED = 8                               
         L     R4,=A(COPYTBL)                                                   
         LA    R4,HISTMKD                                                       
CMP39E   CLI   0(R4),0             END OF LIST FOR COPY MKD                     
         BE    CMP45                                                            
         BAS   RE,NOTIT            IS COPY MSD ON CHANGE MKD                    
         BE    CMP39EE                                                          
         MVI   BYTE,C'G'           SET TYPE                                     
         MVI   WORK,C'-'           SET REMOVED FLAG                             
         LR    R1,R5               SAVE CHANGE POINTER                          
         LR    R5,R4               ( ADDIT EXPECTS R5-> DATA )                  
         BAS   RE,ADDIT                                                         
         LR    R5,R1               RETURN SAVED POINTER TO R5                   
CMP39EE  LA    R4,L'HISTMKD(R4)    BUMP COPY MSD                                
         BCT   R3,CMP39E                                                        
         B     CMP45                                                            
                                                                                
                                                                                
*                                                                               
CMP45    L     R4,=A(COPYTBL)     RESET R4 -> COPY TABLE ADDRESS                
         L     R5,=A(CHANGTBL)           R5 -> CHANGE                           
         LA    R5,HISTPRMT-HIUNTKEY(R5)   POINT R5 TO START OF PRMT             
                                                                                
*                                                                               
         CLC   HISTPRMT,0(R5)      PREEMPT                                      
         BE    *+12                                                             
         MVI   BYTE,C'P'                                                        
         BAS   RE,ADDIT                                                         
         LA    R5,L'HISTPRMT(R5)                                                
*                                                                               
         OC    HISTCOMN,MYSPACES          COMMENT                               
         OC    0(L'HISTCOMN,R5),MYSPACES                                        
         CLC   HISTCOMN,0(R5)                                                   
         BE    *+12                                                             
         MVI   BYTE,C'C'                                                        
         BAS   RE,ADDIT                                                         
                                                                                
*                                                                               
CMP100   B     EXIT                                                             
         EJECT                                                                  
*******************************************                                     
* MATCHING CHANGE REC (R5) AGAINST COPY (R4)                                    
ISIT     NTR1                                                                   
         LA    R3,8                                                             
         LR    R2,R4                                                            
IS10     CLC   0(L'HISTMIS,R2),0(R5)                                            
         BE    ISYES                                                            
         LA    R2,L'HISTMIS(R2)                                                 
         BCT   R3,IS10                                                          
         B     ISNO                                                             
ISYES    SR    RE,RE                                                            
ISNO     LTR   RE,RE                                                            
         B     EXIT                                                             
                                                                                
                                                                                
                                                                                
                                                                                
*******************************************                                     
* MATCHING COPY REC (R4) AGAINST CHANGE (R5)                                    
NOTIT    NTR1                                                                   
         LA    R3,8                                                             
NT10     CLC   0(L'HISTMIS,R5),0(R4)                                            
         BE    NTYES                                                            
         LA    R5,L'HISTMIS(R5)                                                 
         BCT   R3,NT10                                                          
         B     NTNO                                                             
NTYES    SR    RE,RE                                                            
NTNO     LTR   RE,RE                                                            
         B     EXIT                                                             
                                                                                
                                                                                
         EJECT                                                                  
*******************************************************************             
* WE HAVE 3 SITUATIONS WITH DELETED UNITS                                       
*                                                                               
* 1) SIMPLE DELETE OF UNIT  (NOT ALLOWED FOR NOW !!! )                          
*                                                                               
* 2) CHANGE OF START QUARTER HOUR ON UNIT                                       
*    THIS CAUSES NET SYSTEM TO DELETE OLD QUARTER HOUR UNIT                     
*                           AND ADD SAME UNIT WITH NEW QUARTER HOUR             
*    WE TREAT THIS AS A SIMPLE COPY/CHANGE SITUATION SINCE                      
*    SINCE QUARTER HOUR DOES NOT AFFECT KEY OF HISTORY RECORD                   
*                                                                               
* 3) CHANGE OF DAY/DATE OF UNIT                                                 
*    THIS CAUSES NET SYSTEM TO DELETE OLD UNIT                                  
*                    AND ADD SAME UNIT WITH NEW DAY/DATE                        
*                                                                               
*    THIS IS A MORE COMPLICATED SITUATION FOR HISTORY RECS SINCE                
*                    DATE IS PART OF HISTORY REC KEY                            
*                                                                               
*                                                                               
* THE X'90'ELEMENT ON THE UNIT TELLS US WHICH SITUATION WE HAVE                 
* THIS ELEMENT CARRIES THE KEY OF THE NEW UNIT                                  
*                                                                               
*     IF THERE IS NO X'90' ELEMENT, WE HAVE SITUATION  (1)                      
*                                                                               
*     IF THERE IS A X'90', IF DATE OF NEW UNIT AND DELETED                      
*                          UNIT ARE THE SAME, WE HAVE SITUATION (2)             
*                                                                               
*     IF THERE IS A X'90', BUT DATE OF NEW AND DELETED UNIT ARE                 
*                          DIFFERENT, WE HAVE SITUATION (3)                     
*     IN THIS CASE WE DELETE HISTORY REC OF DELETED UNIT                        
*     WE ADD THIS SAME HISTORY REC WITH NEW DATE IN KEY TO                      
*     THE FILE.  WE ALSO ADD AN X'05' HISTORY TRAIL ELEM WITH THE NEW           
*     DATE TO THIS RECORD.  THE X'90'ELEM DOES NOT CARRY ID OF USER             
*     WHO MADE THIS CHANGE. THIS INFORMATION WILL BE PICKED UP IN               
*     'NET30', THE 'NEW REC HAS BEEN ADDED' ROUTINE.                            
*                                                                               
*                                                                               
*                                                                               
*                                                                               
********************************************************************            
PROCDEL  NTR1                                                                   
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'90'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,2(R6)            R6 -> NEW X'04' KEY                          
         L     R1,NBAIO                                                         
                                                                                
* CHECK DATE                                                                    
         CLC   4(2,R6),4(R1)       DAY CHANGE ?                                 
         BE    EXIT                NO/SET CC TO =                               
                                                                                
* - CHANGE OF DAY THUS KEY                                                      
* DELETE CURRENT HISTORY RECORD                                                 
* ADD NEW HISTORY REC                                                           
         L     R4,=A(COPYTBL)      SET R4 -> COPY TABLE ADDRESS                 
         BAS   RE,RDHIST           READ HIST REC                                
***                                                                             
         CLI   PREVREC,X'FF'       HIST ERROR                                   
         BNE   PROCD10                                                          
         LTR   RE,RE                                                            
         B     EXIT                                                             
***                                                                             
PROCD10  L     R4,=A(MYIO2)        OLD HISTORY REC                              
         USING NHRECD,R4                                                        
         OI    22(R4),X'80'        DELETE OLD HIST REC                          
         BAS   RE,MYPUT                                                         
                                                                                
         OI    KEY+20,X'80'        DELETE OLD HIST KEY                          
         BAS   RE,MYDWRT                                                        
                                                                                
* - SET UP NEW HISTORY KEY                                                      
         NI    22(R4),X'FF'-X'80'  CLEAR DELETE BIT                             
         MVC   NHKDATE,4(R6)       CHANGE KEY TO DATE OF NEW HIST REC           
         MVC   NHKSUB,18(R6)       SET NEW SUB LINE                             
         XC    KEY,KEY             SET NEW KEY TO KEY                           
         MVC   KEY(20),0(R4)                                                    
                                                                                
         LA    R5,NHKDATE          POINT R5 AT NEW DATE                         
         MVI   BYTE,C'D'           SET TYPE OF DATA                             
         MVI   HALF,X'FF'          SET SPECIAL CASE FLAG                        
         BAS   RE,ADDIT            ADD ELEM TO HIST REC                         
                                                                                
         BAS   RE,MYADD            ADD HISTORY RECORD                           
         LTR   RE,RE               SET CC TO NOT =                              
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
********************************************************                        
*                                                                               
* UNIT CHANGED -  ADD OLD DATA TO HISTORY RECORD TRAIL                          
*                                                                               
* BYTE = TYPE OF DATA                                                           
* R5  -> POINTS TO DATA FROM CHANGTBL TO BE ADDED INTO ELEM                     
*                                                                               
*                                                                               
*********************************************************                       
                                                                                
ADDIT    NTR1                                                                   
                                                                                
         MVI   UPDTFLG,C'Y'        TURN ON UPDATE HIST REC FLAG                 
         L     R2,=A(MYIO2)                                                     
         CLI   0(R2),X'40'         DO I ALREADY HAVE HISTORY RECORD?            
         BE    ADD10               YES                                          
                                                                                
         L     R4,=A(CHANGTBL)     HISTORY READ EXPECTS R4->HISDSECT            
         BAS   RE,RDHIST           NO READ HISTORY RECORD INTO MYIO2            
         CLI   PREVREC,X'FF'       ERROR                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
ADD10    DS    0H                                                               
         LA    R3,ELEM             BUILD HISTORY DATA IN ELEM                   
         USING NCHAEL,R3                                                        
         XC    ELEM,ELEM                                                        
                                                                                
****************************************************                            
* -> R5 -> DATA TO BE ADDED   BYTE-> TYPE OF DATA                               
*                                                                               
         LA    R2,DATALNTB         DATA TYPE TABLE                              
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                TYPE NOT RECOGNIZED !                        
                                                                                
         CLC   BYTE,0(R2)                                                       
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-14                                                             
         ZIC   R4,1(R2)            LENGTH OF DATA                               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   NCHGFLD(0),0(R5)     PUT DATA TO ELEM                            
                                                                                
* - LENGTH OF NEW ELEM = 21 + LENGTH OF HIST DATA                               
         LA    R4,1(R4)            LENGTH OF HIST DATA                          
         LA    R4,21(R4)           LENGTH OF REQUIRED FIELDS                    
         STC   R4,NCHGLEN                                                       
*                                                                               
                                                                                
         MVI   NCHGEL,X'05'                                                     
         MVC   NCHGFCOD,BYTE                         TYPE CODE                  
*                                                                               
         CLI   BYTE,C'A'           ACTUAL COST ELEMENT?                         
         BNE   SKIPCOST            NO -                                         
         TM    NBUNITST,X'20'      YES - ACTUAL COST INPUT?                     
         BNO   SKIPCOST                                                         
         OI    NCHGSTAT,X'02'            YES                                    
SKIPCOST DS    0H                                                               
*                                                                               
         CLI   BYTE,C'G'           MISSED AND MAKEGOOD ELEMS                    
         BE    *+12                CAN HAVE 'REMOVED' STATUS                    
         CLI   BYTE,C'M'                                                        
         BNE   ADD12                                                            
         CLI   WORK,C'-'           IS IT A REMOVED MKGD/MSD ?                   
         BNE   ADD12                                                            
         OI    NCHGSTAT,X'01'      YES                                          
         MVI   WORK,0              CLEAR 'REMOVED' FLAG                         
                                                                                
ADD12    GOTO1 DATCON,DMCB,(3,RECDAT),(2,NCHGDATE)  DATE OF CHANGE              
         MVC   NCHGTIME,RECTIME                    TIME OF CHANGE               
                                                                                
         CLI   HALF,X'FF'          SPECIAL CASE OF DAY/DATE CHANGE ?            
         BE    ADD20               YES / USER ID NOT AROUND                     
                                                                                
* GET REASON AND USER CODE FROM UNIT IN NBAIO                                   
* UNIT IN NBAIO SHOULD BE CHANGED (THUS NEW) UNIT                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'99'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE 99 ELEMENT                         
         USING NUACTD,R6                                                        
                                                                                
         XC    WORK,WORK                                                        
         CLI   NUACTLEN,12           IF OLD 99 ELEM                             
         BH    ADD15                 NO                                         
         MVC   WORK(8),=C'????????'  YES - SET UNKNOWN                          
         B     ADD16                 AND SKIP SEARCH FOR CODES                  
                                                                                
ADD15    MVC   NCHGREAS,NUACTRSN   REASON CODE                                  
         MVC   WORK(2),NUACTAGD    AGENCY                                       
         MVC   WORK+2(2),NUACTCID  CHANGE PERSONAL ID                           
         OC    NUACTCID,NUACTCID                                                
         BNZ   *+10                                                             
         MVC   WORK+2(2),NUACTAID  CREATION PERSONAL ID                         
         BAS   RE,OWHO             OUTWHOUSER                                   
ADD16    MVC   NCHGUSER,WORK                                                    
                                                                                
                                                                                
* - ADD NEW HISTORY ELEMENT - HIST REC SITS IN MYIO2                            
ADD20    L     R2,=A(MYIO2)                                                     
***      LA    R1,ELEM                                                          
***      LA    R1,2(R1)           POINT TO OPTIONAL SEARCH ARGUMENT             
***      ST    R1,DMCB+8                                                        
***      ZIC   R1,ELEM+1           GET LENGTH OF ELEM TO BE ADDED               
***      BCTR  R1,0               LENGTH OF OPTIONAL SEARCH ARGUMENT            
***      BCTR  R1,0               LENGTH OF OPTIONAL SEARCH ARGUMENT            
***      STC   R1,DMCB+8           STORE IT                                     
* HAVE I ADDED SAME ELEMENT PREVIOUSLY?  (IN CASE OF RERUN)                     
***      GOTO1 HELLO,DMCB,(C'G',=CL8'UNTFILE'),(X'05',(R2)),,0                  
***      CLI   DMCB+12,0           DID WE FIND IT?                              
***      BE    ADD30               YES/SO DON'T ADD AGAIN                       
         XC    DMCB(16),DMCB                                                    
         GOTO1 HELLO,DMCB,(C'P',=CL8'UNTFILE'),(R2),ELEM,0                      
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
ADD30    MVI   HALF,0              CLEAR SPECIAL CASE FLAG                      
         MVI   BYTE,0              CLEAR DATA TYPE FLAG                         
*                                                                               
         B     EXIT                                                             
                                                                                
         DROP  R6,R3                                                            
                                                                                
*                                                                               
DATALNTB DS    0H                                                               
         DC    C'B',AL1(6)         BRAND                                        
         DC    C'L',AL1(1)         LENGTH                                       
         DC    C'A',AL1(4)         ACTUAL COST                                  
         DC    C'D',AL1(2)         DATE                                         
         DC    C'T',AL1(4)         MILITARY START-END TIME                      
         DC    C'N',AL1(16)        PROGRAM NAME                                 
         DC    C'R',AL1(1)         ROTATION                                     
         DC    C'G',AL1(32)        MAKEGOOD                                     
         DC    C'M',AL1(32)        MISSED                                       
         DC    C'P',AL1(1)         PREEMPT                                      
         DC    C'C',AL1(60)        COMMENT                                      
         DC    X'00'                                                            
         EJECT                                                                  
*****************************************************                           
* PULLS DATA FROM NETBLOCK AND UNIT RECORD          *                           
* R4 ->  AREA FILLED WITH HISTORY DATA              *                           
*****************************************************                           
                                                                                
DOHISTRY NTR1                                                                   
         LR    R5,R4               NOTE R5 KEEPS POINTER TO START               
*                                  OF TABLE                                     
         LR    RE,R4               CLEAR TABLE                                  
         LA    RF,HISTLENE                                                      
         XCEF                                                                   
                                                                                
         USING HISTDATD,R4                                                      
                                                                                
* - CALL NETVALUE TO FILL NETBLOCK                                              
         BAS   RE,FILLDBLK                                                      
                                                                                
* - SAVE HISTORY FIELDS                                                         
                                                                                
         L     R1,NBAIO                                                         
         MVC   HIUNTKEY,0(R1)         SAVE UNIT KEY                             
         MVC   HIRECTM,RECTIME        TIME OF CHANGE                            
         MVC   HIRECDAT,RECDAT        DATE OF CHANGE                            
*                                                                               
         MVC   HISTPRD(2),NBPRD       PRODUCTS (PRD1 AND PRD2)                  
         CLI   NBPRDNO,0              IS IT MULTIPLE PRODUCTS?                  
         BE    *+10                                                             
         MVC   HISTPRD,NBPRDLST       MULTIPLE PRODUCTS                         
*                                                                               
         MVC   HISTLEN,NBLEN          LENGTH                                    
*                                                                               
         MVC   HISTCOST,NBACTUAL      ACTUAL COST                               
*                                                                               
         MVC   HISTDATE,NBACTDAT      DATE                                      
*                                                                               
         MVC   HISTIME,NBTIME         TIME                                      
*                                                                               
         MVC   HISTPNAM,NBPROGNM      PROGRAM NAME                              
*                                                                               
         MVC   HISTROT,NBSDROT        ROTATION                                  
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'04'           COMMENTS ?                                
         BAS   RE,GETEL                                                         
         BNE   DH7                                                              
         ZIC   R1,1(R6)                                                         
         S     R1,=F'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HISTCOMN(0),4(R6)         YES                                    
                                                                                
* - MISSED                                                                      
DH7      LA    R3,8                    SAVE MAX OF 8                            
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'06'            MISSED DETAILS                           
         BAS   RE,GETEL                                                         
         BNE   DH10                                                             
DH8      MVC   HISTMIS,0(R6)           MISSED DATA                              
         LA    R4,L'HISTMIS(R4)    BUMP TO NEXT MISSED AREA                     
         BAS   RE,NEXTEL           BUMP TO NEXT MISSED ELEM                     
         BNE   DH10                                                             
         BCT   R3,DH8                                                           
                                                                                
* - MAKE-GOOD                                                                   
DH10     LR    R4,R5               RESET R4 POINTER                             
         LA    R3,8                SAVE MAX OF 8                                
         L     R6,NBAIO            RESET R6 TO START OF UNIT RECORD             
         MVI   ELCODE,X'07'        MAKE GOOD DETAILS                            
         BAS   RE,GETEL                                                         
         BNE   DH20                                                             
DH12     MVC   HISTMKD,0(R6)        MAKE GOOD DATA                              
         LA    R4,L'HISTMKD(R4)     BUMP TO NEXT MKGD AREA                      
         BAS   RE,NEXTEL            BUMP TO NEXT MKGD ELEM                      
         BNE   DH20                                                             
         BCT   R3,DH12                                                          
*                                                                               
DH20     LR    R4,R5               RESET POINTER TO START OF TABLE              
         TM    NBUNITST,X'40'      PREEMPT ?                                    
         BNO   *+8                                                              
         MVI   HISTPRMT,C'Y'                                                    
*                                                                               
DHX      XIT1                                                                   
                                                                                
         EJECT                                                                  
*                                                                               
FILLDBLK NTR1                                                                   
         MVC   NBAIO,AUNITREC       SET NBAIO TO START OF UNIT REC              
         MVI   NBESTOPT,0           NO ESTIMATED DEMOS                          
         MVI   NBACTOPT,0           NO ACTUAL DEMOS                             
         GOTO1 NBNETVAL,DMCB,NETBLOCK                                           
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
FILLX    B     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
*****************************************                                       
*                                                                               
* SET TODAY'S DATE TO HISTORY RECORD                                            
*                                                                               
ACTIVDAT NTR1                                                                   
         L     R2,=A(MYIO2)        GET HISTORY RECORD                           
         USING NHRECD,R2                                                        
         MVC   NHCHGDAT,TODAYC     TODAY'S COMPRESSED DATE                      
         MVC   NHCHGTIM,RECTIME    TIME OF CHANGE                               
         MVC   NHPKG,NBPACK        PACKAGE                                      
         B     EXIT                                                             
         DROP  R2                                                               
*******************************************                                     
         EJECT                                                                  
***********************************************************                     
*                                                                               
* READS HISTORY RECORD INTO MYIO2                                               
* EXPECTS R4 -> HISTDSECT                                                       
*                                                                               
*************************************************************                   
                                                                                
RDHIST   NTR1                                                                   
         USING HISTDATD,R4                                                      
                                                                                
         LA    R4,HIUNTKEY         POINT R4 TO UNITKEY (X'04' KEY)              
         USING NURECD,R4                                                        
                                                                                
         LA    R5,KEY             CREATE HISTORY KEY FROM X'04' KEY             
         USING NHRECD,R5                                                        
         XC    KEY,KEY                                                          
         MVI   NHKTYPE,X'40'                                                    
         MVC   NHKPAM,NUKAM                                                     
         MVC   NHKPCLT,NUKCLT                                                   
         MVC   NHKNET,NUKNET                                                    
         MVC   NHKPROG,NUKPROG                                                  
         MVC   NHKDATE,NUKDATE                                                  
         MVC   NHKEST,NUKEST                                                    
         MVC   NHKSUB,NUKSUB                                                    
         MVC   NHKDP,NUKDP                                                      
         BAS   RE,MYHIGH                                                        
         CLC   KEY(20),KEYSAVE                                                  
***      BE    *+6                                                              
***      DC    H'0'                 MUST BE HISTORY RECORD!                     
         BE    RDHST10                                                          
         MVI   PREVREC,X'FF'                                                    
         B     RDHSTX                                                           
RDHST10  MVI   UPDTBIT,X'80'        READ FOR UPDATE                             
         BAS   RE,MYGET                                                         
RDHSTX   XIT1                                                                   
         DROP  R4,R5                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
                                                                                
         EJECT                                                                  
MYHIGH   NTR1                                                                   
         MVC   COMMAND,=CL8'DMRDHI'                                             
         MVC   KEYSAVE,KEY                                                      
         B     DIRALL                                                           
                                                                                
MYSEQ    NTR1                                                                   
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         B     DIRALL                                                           
                                                                                
MYDWRT   NTR1                                                                   
         MVC   COMMAND,=CL8'DMWRT'                                              
         B     DIRALL                                                           
                                                                                
DIRALL   DS    0H                                                               
         CLI   FILE,0              WAS REP DIR OVERRIDEN ?                      
         BE    *+6                 NO                                           
         DC    H'0'                SHOULD NOT BE HERE                           
         MVC   FILE,=CL8'UNTDIR'   DEFAULT                                      
         ZIC   R4,UPDTBIT          SET OPTIONAL BIT                             
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),FILE,KEY,KEY,0                       
         MVI   FILE,0              CLEAR FILE FOR REP DEFAULT                   
         B     DDRECX                                                           
*                                                                               
MYGET    NTR1                                                                   
         LA    RF,=C'GETREC'                                                    
         ZIC   R4,UPDTBIT          SET OPTIONAL READ FOR UPDATE                 
         B     DDREC5                                                           
MYADD    NTR1                                                                   
         LA    RF,=C'ADDREC'                                                    
         B     DDREC5                                                           
MYPUT    NTR1                                                                   
         LA    RF,=C'PUTREC'                                                    
         B     DDREC5                                                           
*                                                                               
DDREC5   ST    RF,DMCB                                                          
         CLI   FILE,0              OVERIDE DEFAULT UNT FILE?                    
         BNE   DDREC7                                                           
         MVC   FILE,=CL8'UNTFILE'  NO                                           
         L     R3,=A(MYIO2)        HIST RECS READ INTO MYIO2                    
         LA    R2,KEY+21                                                        
         B     DDREC10                                                          
                                                                                
DDREC7   MVC   FILE,=CL8'SPTFILE'                                               
         LA    R2,KEY+14                                                        
         L     R3,=A(MYIO2)         SPOT RECS READ INTO MYIO2                   
         B     DDREC10                                                          
                                                                                
DDREC10  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,,FILE,(R2),(R3),MYDMWORK,0                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   UPDTBIT,0           DEFAULT NOT READ FOR UPDATE                  
         MVI   FILE,0              DEFAULT UNT FILE                             
*                                                                               
DDRECX   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         DS    0F                                                               
FILE     DS    CL8                                                              
MYDMWORK DS    CL96                                                             
UPDTBIT  DS    CL1                                                              
*                                                                               
         EJECT                                                                  
* - PRINTD ONLY HAS ONE PRINT LINE/USE MY OWN                                   
PRINTIT  NTR1                                                                   
         L     R5,ASPOOLD          RA - DPRINT                                  
         USING SPOOLD,R5                                                        
         MVC   P,MYP                                                            
         GOTO1 SPOOL,DMCB,(R5)                                                  
         B     EXIT                                                             
         DROP  R5                                                               
                                                                                
* - GET NAME TO MATCH PERSONAL ID                                               
* - EXPECTS - WORK(2) HAS AGENCY  WORK+2(2) HAS PERSONAL ID                     
* - RETURNS - NAME(8) IN WORK                                                   
OWHO     NTR1                                                                   
*                                                                               
         LA    R3,WHOTBL           IS NAME IN TABLE ?                           
         CLI   0(R3),0             FIRST TIME?                                  
         BE    OWHO05              YES                                          
                                                                                
OWHO00   CLC   WORK(4),0(R3)                                                    
         BNE   OWHO01                                                           
         MVC   WORK(8),4(R3)       YES                                          
         B     OWHOX                                                            
                                                                                
OWHO01   LA    R3,12(R3)           NO - BUMP TO NEXT ENTRY                      
         CLI   0(R3),0             ROOM IN TABLE                                
         BE    OWHO05              YES - ADD NEW ID/NAME HERE                   
         CLI   0(R3),X'FF'         NO MORE ROOM IN TABLE                        
         BNE   OWHO00                                                           
                                                                                
* - TABLE IS FULL - MOVE TBL DOWN AND DELETE LAST ENTRY                         
         LA    RF,WHOTBL               POINT RF TO LAST ENTRY                   
         LA    RF,(L'WHOTBL-12)(RF)                                             
         LA    R1,(L'WHOTBL-12)        R1 = LENGTH OF WHOTBL-12                 
         LA    RE,WHOTBL                                                        
         LA    RE,(L'WHOTBL-24)(RE)    POINT RE TO PENULTIMATE ENTRY            
         MOVE  ((RF),(R1)),(RE)                                                 
         LA    R3,WHOTBL            POINT R3 TO TBL START FOR NEW ENTRY         
*                                                                               
OWHO05   MVC   MYKEY,KEY           SAVE KEY                                     
                                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CT0REC,R6                                                        
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,WORK                                                     
         MVC   CT0KNUM,WORK+2                                                   
         L     R6,=A(MYIO3)                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,(R6)                      
         CLC   CT0KEY,KEY                                                       
         BNE   OWHO30                                                           
*                                                                               
         LA    RE,28(R6)                                                        
         SR    R0,R0                                                            
OWHO10   CLI   0(RE),0                                                          
         BE    OWHO30                                                           
         CLI   0(RE),X'C3'                                                      
         BE    *+14                                                             
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     OWHO10                                                           
         MVC   0(4,R3),WORK      ADD TO TABLE AGY/ID                            
         MVC   4(8,R3),2(RE)                  NAME                              
         MVC   WORK(8),2(RE)       PASS BACK TO CALLER                          
         B     OWHO40                                                           
                                                                                
OWHO30   MVC   WORK(8),=C'????????'                                             
                                                                                
OWHO40   DS    0H                                                               
         MVC   KEY,MYKEY           RESET KEY                                    
OWHOX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=8096,                                             X        
               MACRF=GM,                                               X        
               EODAD=EXIT                                                       
               SPACE                                                            
         SPACE                                                                  
UTL      DC    F'0',X'00'                                                       
SSB      DC    F'2'                                                             
                                                                                
* - COPY WITH NO CHANGE                                                         
COPYERR  NTR1                                                                   
         MVC   MYP(16),=C'COPY - NO CHANGE'                                     
         GOTO1 HEXOUT,DMCB,PREVIOUS,MYP+20,44                                   
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
                                                                                
* - CHANGE WITH NO PREVIOUS COPY                                                
CHANGERR NTR1                                                                   
         MVC   MYP(16),=C'CHANGE - NO COPY'                                     
         GOTO1 HEXOUT,DMCB,RECFILTY,MYP+20,100                                  
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
                                                                                
* - UNIT REC WITH HIST FLAG DELETED - SHOULD NOT BE !!!                         
DELETERR NTR1                                                                   
         MVC   MYP(16),=C'HIST DELETE ERR '                                     
         GOTO1 HEXOUT,DMCB,RECFILTY,MYP+20,100                                  
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
HEDSPECS SPROG 1,2                                                              
         SSPEC H1,1,C'CLIENT NAME'                                              
         SSPEC H2,1,C'AUDIT GROUP'                                              
         SSPEC H3,1,C'NETWORK'                                                  
         SSPEC H4,1,C'CHANGE DATE'                                              
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,120,PAGE                                                      
         SSPEC H1,50,C'NOTICE OF CHANGE'                                        
         DC    X'00'                                                            
                                                                                
HDRTN    NTR1                                                                   
         L     R1,ASPOOLD                                                       
         USING SPOOLD,R1                                                        
         MVC   H1+12(3),SRTKCLT                                                 
         MVC   H2+12(4),SRTKGRP                                                 
         MVC   H3+12(4),SRTKNET                                                 
         MVC   H4+12(8),CHANGDAT                                                
         CLI   RCSUBPRG,2                                                       
         BE    HDRTNX                                                           
         LA    R1,H5                                                            
         USING PLINED,R1                                                        
         MVC   PLACTDAT,=C'ACTIVITY'                                            
         MVC   PLACTDAT+134(4),=C'DATE'                                         
         MVC   PLSTATUS,=C'STATUS'                                              
         MVC   PLUNTDAT+2(4),=C'UNIT'                                           
         MVC   PLUNTDAT+134(4),=C'DATE'                                         
         MVC   PLPROG+3(7),=C'PROGRAM'                                          
         MVC   PLDAY(3),=C'DAY'                                                 
         MVC   PLTIME+3(4),=C'TIME'                                             
         MVC   PLLEN(3),=C'LEN'                                                 
         MVC   PLCOST+1(5),=C'$COST'                                            
         MVC   PLBRAND(5),=C'BRAND'                                             
         MVC   PLREASON+1(3),=C'RSN'                                            
         MVC   PLTRAIL+1(5),=C'TRAIL'                                           
         MVC   PLCOMM+1(8),=C'COMMENTS'                                         
         MVC   PLBUYER(5),=C'BUYER'                                             
HDRTNX   B     EXIT                                                             
         DROP  R1                                                               
                                                                                
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
                                                                                
         EJECT                                                                  
*                                                                               
         DC    CL8'**COPY**'                                                    
COPYTBL  DS    CL(HISTLENE)        COPY UNIT HIST DATA                          
*                                                                               
         DC    CL8'**CHANG*'                                                    
CHANGTBL DS    CL(HISTLENE)        CHANGED UNIT HIST DATA                       
*                                                                               
*                                                                               
         DC    CL8'**MYIO**'                                                    
MYIO     DS    0D                                                               
         DS    4048C                                                            
HMYIOLNE EQU   *-MYIO                                                           
         DS    4048C                                                            
MYIOLNE  EQU   *-MYIO                                                           
                                                                                
         DC    CL8'**MYIO2*'                                                    
MYIO2    DS    0D                                                               
         DS    8096C                                                            
MYIO2LNE EQU   *-MYIO2                                                          
                                                                                
         DC    CL8'**MYIO3*'                                                    
MYIO3    DS    0D                                                               
         DS    8096C                                                            
MYIO3LNE EQU   *-MYIO3                                                          
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
HISTDATD DSECT                     HISTORY DATA DSECT                           
*                                                                               
HISTARTD DS    0C                                                               
HIUNTKEY DS    CL20                KEY OF COPY/CHANGE UNIT                      
HIRECTM  DS    XL4                 RECOVERY HEADER TIME                         
HIRECDAT DS    XL3                 RECOVERY HEADER DATE                         
HIREAS   DS    CL4                 REASON CODE                                  
HIUSER   DS    CL8                 USER CODE                                    
*                                                                               
HISTPRD  DS    CL6                 PRODUCT (MAX OF 6)                           
HISTLEN  DS    CL1                 LENGTH                                       
HISTCOST DS    CL4                 ACTUAL COST                                  
HISTDATE DS    CL2                 UNIT DATE                                    
HISTIME  DS    CL4                 START-END TIME                               
HISTPNAM DS    CL16                PROGRAM NAME                                 
HISTROT  DS    CL1                 ROTATION                                     
*                                                                               
HISTMIS  DS    CL32                MISSED DETAILS                               
         DS    CL(7*32)            LEAVE ROOM FOR 7 MORE MISSED                 
*                                                                               
HISTMKD  DS    CL32                MAKE-GOOD DETAILS                            
         DS    CL(7*32)            LEAVE ROOM FOR 7 MORE MAKE-GOOD              
*                                  0-5    PROGRAM CODE                          
*                                  6-21   PROGRAM NAME                          
*                                  22-23  DATE                                  
*                                  24     SUB-LINE NUMBER                       
*                                                                               
HISTPRMT DS    CL1                 PREEMPT                                      
*                                                                               
HISTCOMN DS    CL60                COMMENT                                      
HISTLENE EQU   *-HISTARTD                                                       
         EJECT                                                                  
*                                                                               
                                                                                
MYWORKD  DSECT                                                                  
AMSAVE   DS    CL1    *** FROM EDIT MODULE - DO NOT MOVE                        
ONECLT   DS    CL2    ***                                                       
*                                                                               
AUNITREC DS    A                   UNIT RECORD POINTER                          
*                                                                               
PREVREC  DS    CL1                                                              
SAVECLT  DS    CL3                                                              
SAVCLT2  DS    CL2                                                              
SAVEPROD DS    CL7                                                              
SAVEEST  DS    CL1                                                              
SAVENET  DS    CL4                                                              
SAVEDEM  DS    CL6                                                              
USERNUM  DS    CL2                                                              
PWORK    DS    PL16                                                             
PREVIOUS DS    CL44                                                             
RECFILTY DS    CL100               RECOVERY HEADER+PART OF RECORD               
RECDAT   DS    CL3                 RECOVERY DATE                                
RECTIME  DS    CL4                 RECOVERY TIME                                
                                                                                
* FIELDS BELOW USED TO RERUN FILE IF HISTORY DIES                               
RERUNTST DS    CL1                 Y=RERUN                                      
RERUNREC DS    CL24                RECOVERY HEADER SAVE                         
*                                                                               
*                                                                               
*                                                                               
AGYNAMSV DS    CL33                                                             
AGYADRSV DS    CL33                                                             
PKCLISV  DS    CL2                 CLIENT FILTER                                
CLIENTNM DS    CL20                                                             
PRODNAME DS    CL20                                                             
ESTNAME  DS    CL20                                                             
*                                                                               
RCVRYSV  DS    CL60                                                             
TODAYB   DS    CL3                 BINARY                                       
TODAYC   DS    CL2                 COMPRESSED                                   
PREVFLG  DS    CL1                                                              
UPDTFLG  DS    CL1                                                              
HISTFLG  DS    CL1                                                              
WRITEFLG DS    CL1                                                              
SVSRTCLT DS    CL3                                                              
CHANGDAT DS    CL8                                                              
SAVAMC   DS    CL3                 SAVE AGY/MED/CLT OF HIST REC                 
MYKEY    DS    CL40                                                             
                                                                                
MYP      DS    CL132                                                            
MYSPACES DS    CL132                                                            
*                                                                               
WHOTBL   DS    CL(12*50)           ROOM FOR 50 ID(2)/AGY(2)/NAME(8)             
WHOTBLX  DS    CL1                 END OF TABLE = X'FF'                         
*                                                                               
*********************************************************                       
*                                                                               
SRTTBL   DS    0CL200    *** NOTE HARD CODED                                    
SRTKCLT  DS    CL3               * CLIENT           HIST KEY                    
SRTKGRP  DS    CL4               * AUDIT GROUP                                  
SRTKNET  DS    CL4               * NETWORK          HIST KEY                    
SRTKPRG  DS    CL6               * PROGRAM          HIST KEY                    
SRTKDAT  DS    CL2               * AIR DATE         HIST KEY                    
SRTKEST  DS    CL1               * ESTIMATE         HIST KEY                    
SRTKSUB  DS    CL1               * SUB LINE         HIST KEY                    
SRTKCHD  DS    CL2               * CHANGEDATE                                   
SRTKTIM  DS    CL4               * CHANGETIME                                   
         DS    CL3               * SPARE                                        
* SORT KEY = 30                                                                 
*                                                                               
SRDATA   DS    0CL2                DATA FIELDS START HERE                       
SRUNTDAT DS    CL2                 DATE                                         
SRPRGNM  DS    CL16                PROGRAM NAME                                 
SRROT    DS    CL1                 ROTATION                                     
SRTIM    DS    CL4                 TIME                                         
SRLEN    DS    CL1                 LENGTH                                       
SRCOST   DS    CL4                 ACTUAL                                       
SRPROD   DS    CL6                 PRODUCT                                      
SRREASON DS    CL3                 REASON                                       
SRSTATUS DS    CL1                 STATUS BYTE                                  
*                                  X'01' = REMOVED MKGD/MSD ELEM                
SRCOMN   DS    CL60                COMMENT                                      
SRBUYER  DS    CL8                 USER CODE                                    
SRMKGMSD DS    CL35                MAKEGOOD/MISSED                              
SRPRMT   DS    CL1                 PREEMPT C'Y'                                 
SRTAMC   DS    CL3                 AGY/MEDIA/CLI                                
         DS    CL24                SPARE                                        
SRTYPE   DS    CL1                 A=ADD/D=DELETE                               
SRTBLEN  EQU   *-SRTTBL            TOTAL SORT REC LENGTH                        
SRTDLEN  EQU   *-SRDATA            DATA LENGTH                                  
*                                                                               
*                                                                               
*                                                                               
         PRINT ON                                                               
MYWRKDLE EQU   *-MYWORKD                                                        
*                                                                               
                                                                                
*                                                                               
PLINED   DSECT                                                                  
         DS    CL1                                                              
PLACTDAT DS    CL8                                                              
         DS    CL1                                                              
PLSTATUS DS    CL6                                                              
         DS    CL1                                                              
PLUNTDAT DS    CL8                                                              
         DS    CL1                                                              
PLPROG   DS    CL16                                                             
         DS    CL1                                                              
PLDAY    DS    CL3                                                              
         DS    CL1                                                              
PLTIME   DS    CL10                                                             
         DS    CL1                                                              
PLLEN    DS    CL3                                                              
         DS    CL1                                                              
PLCOST   DS    CL7                                                              
         DS    CL1                                                              
PLBRAND  DS    CL4                                                              
         DS    CL1                                                              
PLREASON DS    CL4                                                              
         DS    CL1                                                              
PLTRAIL  DS    CL23                                                             
         DS    CL1                                                              
PLCOMM   DS    CL15                                                             
         DS    CL1                                                              
PLBUYER  DS    CL8                                                              
PLLENE   EQU   *-PLINED                                                         
*                                                                               
RECD     DSECT                                                                  
RECLN    DS    XL2                                                              
         DS    CL2                                                              
REC      DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
         EJECT                                                                  
       ++INCLUDE NEGENHIST                                                      
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRICFD                                                       
*                                                                               
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE DDGENTWA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035NEWRI85   04/10/14'                                      
         END                                                                    
