*          DATA SET RESFM28    AT LEVEL 005 AS OF 05/01/02                      
*PHASE T81828A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T81828 - DIRECT RESPONSE COPY                         *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T81800 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS PCOPY ACTION ONLY                            *         
*                                                                     *         
*  INPUTS       SCREEN T818CC (COPY SCREEN)                           *         
*                                                                     *         
*  OUTPUTS      NEW DR AND DRN DRN RECORDS                            *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- SECOND BASE                                     *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
* 19MAY93 (SKU) CREATION DATE                                         *         
*                                                                     *         
* 19NOV97 (JRD) YR2000 PWOS DATE                                      *         
*                                                                     *         
***********************************************************************         
         TITLE 'T81828 - DIRECT REPONSE COPY'                                   
T81828   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81828**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
*                                                                               
         MVI   IOOPT,C'N'          DO MY OWN IO'S                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
*                                                                               
         CLI   MODE,PRINTREP       COPY RECORDS OFFLINE                         
         BE    OFFCOPY                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY ROUTINE                                                          
***********************************************************************         
VK       DS    0H                  VALIDATE FROM DETAILS                        
         LA    R2,DIRFSTAH         FROM-STATION                                 
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         MVC   FROMSTA(3),=C'ALL'  DEFAULT                                      
         CLC   =C'ALL',8(R2)                                                    
         BNE   VK05                                                             
         XC    DIRTSTA,DIRTSTA     IF FROM-STATION = 'ALL'                      
         MVC   DIRTSTA(3),=C'ALL'  TO-STATION MUST BE 'ALL', TOO                
         MVI   DIRTSTAH+5,3                                                     
         OI    DIRTSTAH+6,X'80'    XMIT IT                                      
         B     VK10                                                             
*                                                                               
VK05     DS    0H                                                               
         GOTO1 VALISTA                                                          
         MVC   FROMSTA,WORK                                                     
*                                                                               
VK10     DS    0H                                                               
         LA    R2,DIRFPERH         FROM-PERIOD                                  
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         BAS   RE,VALIPERI                                                      
         MVC   FSTARTDT,STARTDT                                                 
         MVC   FENDDT,ENDDT                                                     
*                                                                               
         LA    R2,DIRFSTAH                                                      
         CLC   =C'ALL',8(R2)       IS FROM-STATION ALL?                         
         BE    VK100                                                            
         LA    R6,KEY              CHECK IF DR RECORD EXISTS                    
         USING RDIRKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RDIRTYP,RDIRTYPQ                                                 
         MVC   RDIRREP,AGENCY                                                   
         MVC   RDIRSTA,FROMSTA                                                  
         MVC   RDIRENDT,FENDDT                                                  
         MVC   RDIRSTDT,FSTARTDT                                                
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDIRKEY),KEYSAVE                                           
         BNE   RECNOTFD            FROM DR RECORD DOES NOT EXISTS               
*                                                                               
VK100    DS    0H                  VALIDATE TO-DETAILS                          
         CLC   =C'ALL',FROMSTA                                                  
         BE    VK200                                                            
*                                                                               
VK110    DS    0H                                                               
         LA    R2,DIRTSTAH         TO-STATION                                   
         CLI   5(R2),0             MUST HAVE AT LEAST STATION AND/OR            
         BNE   VK120               PERIOD                                       
         CLI   DIRTPERH+5,0                                                     
         BE    MISSFLD                                                          
         MVC   TOSTA,FROMSTA                                                    
         B     VK130                                                            
*                                                                               
VK120    DS    0H                                                               
         GOTO1 VALISTA                                                          
         MVC   TOSTA,WORK                                                       
*                                                                               
VK130    DS    0H                                                               
         LA    R2,DIRTPERH         TO-PERIOD                                    
         CLI   5(R2),0                                                          
         BNE   VK140                                                            
         CLI   DIRTSTAH+5,0                                                     
         BE    MISSFLD                                                          
         MVC   TSTARTDT,FSTARTDT                                                
         MVC   TENDDT,FENDDT                                                    
         B     VK150                                                            
*                                                                               
VK140    DS    0H                                                               
         BAS   RE,VALIPERI                                                      
         MVC   TSTARTDT,STARTDT                                                 
         MVC   TENDDT,ENDDT                                                     
*                                                                               
VK150    DS    0H                  CHECK FOR PERIOD OVERLAPPING                 
         GOTO1 VALOVL,DMCB,TOSTA,TSTARTDT,TENDDT                                
         BE    VK300                                                            
         LA    R2,DIRTSTAH         POINT TO TO-STATION                          
         B     OVLEXIST            DR RECORD OVERLAPS EXISTING RECS             
*                                                                               
VK200    DS    0H                  CASE FROM-STATION=ALL                        
         LA    R2,DIRTSTAH         IF FROM-STATION IS ALL                       
         CLI   5(R2),0             TO-STATION MUST BE ALL, ALSO                 
         BE    INVLFLD                                                          
         CLC   =C'ALL',DIRTSTA                                                  
         BNE   INVLFLD                                                          
*                                                                               
         LA    R2,DIRTPERH         IF FROM-STATION=ALL, ONLY TO-PERIOD          
         CLI   5(R2),0             IS NEEDED                                    
         BE    MISSFLD                                                          
*                                                                               
         BAS   RE,VALIPERI                                                      
         MVC   TSTARTDT,STARTDT                                                 
         MVC   TENDDT,ENDDT                                                     
*                                                                               
         CLC   TENDDT,FSTARTDT     CHECK FOR DATE OVERLAPPING                   
         BL    VK300                                                            
         CLC   TSTARTDT,FENDDT                                                  
         BH    VK300                                                            
         B     INVLDOVL                                                         
*                                                                               
VK300    DS    0H                                                               
         LA    R2,DIRCTIDH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
VKX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* OFFLINE COPY                                                                  
* WILL COPY AS MUCH AS IT CAN AND REPORTS SUCCESS/FAILURE IN THE REPORT         
***********************************************************************         
OFFCOPY  DS    0H                                                               
         CLI   ACTNUM,ACTPCOPY     PROCESS PCOPY ACTION ONLY                    
         BNE   OFFCX                                                            
         CLI   OFFLINE,C'Y'        MUST BE OFFLINE                              
         BNE   OFFCX                                                            
*                                                                               
         LA    R1,HEDSPECS         INITIALIZE                                   
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         CLC   =C'ALL',FROMSTA                                                  
         BE    OFFC100                                                          
*                                                                               
* CASE 1: SINGLE COPY INWHICH BOTH FROM-STATION AND FROM-PERIOD ARE             
*         SPECIFIED                                                             
*                                                                               
         BAS   RE,COPYREC                                                       
         B     OFFCX                                                            
*                                                                               
* CASE 2: ALL COPY INWHICH FROM-STATION IS ALL AND FROM-PERIOD IS               
*         SPECIFIED                                                             
*                                                                               
OFFC100  DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RDIRKEY,R6                                                       
         MVI   RDIRTYP,RDIRTYPQ                                                 
         MVC   RDIRREP,AGENCY                                                   
*                                                                               
OFFC110  DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   RDIRENDT,FENDDT     MUST MATCH FROM-PERIOD                       
         BNE   OFFC150                                                          
         CLC   RDIRSTDT,FSTARTDT                                                
         BNE   OFFC150                                                          
*                                                                               
         CLI   RDIRMAST,0          MAKE SURE IT'S NOT A DRN REC                 
         BNE   OFFC150                                                          
*                                                                               
         MVC   FROMSTA,RDIRSTA     SETUP FOR COPYREC ROUTINE                    
         MVC   TOSTA,RDIRSTA                                                    
         MVC   SEQKEY,KEY                                                       
         BAS   RE,COPYREC                                                       
         MVC   KEY,SEQKEY                                                       
         GOTO1 HIGH                RE-ESTABLISH SEQUENTIAL READ                 
*                                                                               
OFFC150  DS    0H                                                               
         GOTO1 SEQ                                                              
         CLC   KEY(15),KEYSAVE     SAME REC TYPE AND REP?                       
         BE    OFFC110                                                          
         DROP  R6                                                               
*                                                                               
OFFCX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO COPY DR/DRN RECORDS AND PRINT ACTIVITY                             
***********************************************************************         
COPYREC  NTR1                                                                   
         XC    COPYSTAT,COPYSTAT                                                
         LA    R5,P                                                             
         USING PRTLINE,R5                                                       
*                                                                               
         MVC   PSTATION(4),TOSTA   PRINT TO-STATION LINE                        
         MVC   PSTATION+4(2),=C'-T'                                             
         CLI   TOSTA+4,C' '                                                     
         BE    *+10                                                             
         MVC   PSTATION+5(1),TOSTA+4                                            
*                                                                               
         GOTO1 EXPPERD,DMCB,TSTARTDT,TENDDT,PPERIOD                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RDIRKEY,R6                                                       
         MVI   RDIRTYP,RDIRTYPQ                                                 
         MVC   RDIRREP,AGENCY                                                   
         MVC   RDIRSTA,FROMSTA                                                  
         MVC   RDIRENDT,FENDDT                                                  
         MVC   RDIRSTDT,FSTARTDT                                                
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDIRKEY),KEYSAVE                                           
         BE    COPYR10                                                          
         OI    COPYSTAT,FNOTFOND   FROM RECORD NOT FOUND                        
         B     COPYRNO                                                          
*                                                                               
COPYR10  DS    0H                                                               
         GOTO1 GETREC              DR RECORD                                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* CHECK NEW KEY AGAINST EXISTING RECORDS FOR OVERLAPPING PERIOD                 
*                                                                               
         GOTO1 VALOVL,DMCB,TOSTA,TSTARTDT,TENDDT                                
         BE    COPYR20                                                          
*                                                                               
         OI    COPYSTAT,NEWKEYOV   SET ERROR TO NEW KEY OVERLAPS                
         B     COPYRNO                                                          
*                                                                               
COPYR20  DS    0H                                                               
         XC    KEY,KEY             CHECK IF NEW KEY EXISTS                      
         LA    R6,KEY                                                           
         USING RDIRKEY,R6                                                       
         MVI   RDIRTYP,RDIRTYPQ                                                 
         MVC   RDIRREP,AGENCY                                                   
         MVC   RDIRSTA,TOSTA                                                    
         MVC   RDIRENDT,TENDDT                                                  
         MVC   RDIRSTDT,TSTARTDT   CHECK IF NEW NOTES KEY EXISTS                
         MVI   RDIRMAST,1          DR NOTES RECORD                              
         DROP  R6                                                               
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDIRKEY),KEYSAVE                                           
         BNE   COPYR30             ERROR, NEW NOTES KEY EXISTS                  
         OI    COPYSTAT,NEWNKEYX                                                
         B     COPYRNO                                                          
*                                                                               
COPYR30  DS    0H                                                               
         L     R6,AIO                                                           
         USING RDIRREC,R6                                                       
         MVC   RDIRSTA,TOSTA                                                    
         MVC   RDIRENDT,TENDDT                                                  
         MVC   RDIRSTDT,TSTARTDT                                                
*                                                                               
         MVC   CTRLID,RDIRDCID     SAVE OFF CONTROL ID                          
         DROP  R6                                                               
*                                                                               
         LA    R2,DIRDESCH         NEW DESCRIPTION?                             
         CLI   5(R2),0                                                          
         BE    COPYR50                                                          
         L     R6,AIO              YES, DELETE DESC ELEMENT SINCE               
         MVI   ELCODE,RDIRDCDQ     IT HAS TO BE RECONSTRUCTED                   
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM                                                          
         USING RDIRDESD,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   RDIRDCDE,RDIRDCDQ                                                
         GOTO1 DATCON,DMCB,(5,0),(3,RDIRDLUP)  UPDATE DATE=TODAY                
         MVC   RDIRDCID,CTRLID                                                  
         CLI   DIRCTIDH+5,0                                                     
         BE    COPYR40                                                          
         MVC   RDIRDCID,DIRCTID    NEW CONTROL ID                               
*                                                                               
COPYR40  DS    0H                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RDIRDESC(0),DIRDESC                                              
*                                                                               
         LA    RF,RDIRDOV          ELEMENT OVERHEAD LENGTH                      
         LA    RF,1(R1,RF)         PLUS LENGTH OF DESCRIPTION FIELD             
         STC   RF,RDIRDELN         IS THE TOTAL ELEMENT LENGTH                  
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    COPYR60                                                          
         DC    H'0'                                                             
*                                                                               
COPYR50  DS    0H                                                               
         L     R6,AIO                                                           
         USING RDIRDESD,R6                                                      
         MVI   ELCODE,RDIRDCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                BETTER BE THERE                              
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,RDIRDLUP)  UPDATE DATE=TODAY                
         CLI   DIRCTIDH+5,0                                                     
         BE    COPYR60                                                          
         MVC   RDIRDCID,DIRCTID    NEW CONTROL ID                               
         DROP  R6                                                               
*                                                                               
COPYR60  DS    0H                                                               
         GOTO1 ADDREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   DIRTNOT,C'N'        COPY DRN, TOO?                               
         BE    COPYR100                                                         
         XC    KEY,KEY             CHECK IF COPY FROM KEY HAS DR NOTES          
         LA    R6,KEY              RECORD.  COPY DR NOTE REC IF FOUND           
         USING RDIRKEY,R6                                                       
         MVI   RDIRTYP,RDIRTYPQ                                                 
         MVC   RDIRREP,AGENCY                                                   
         MVC   RDIRSTA,FROMSTA                                                  
         MVC   RDIRENDT,FENDDT                                                  
         MVC   RDIRSTDT,FSTARTDT                                                
         MVI   RDIRMAST,X'01'      DR NOTES RECORD                              
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'N'       IF DR NOTES RECORD EXISTS,                   
         GOTO1 HIGH                COPY IT, TOO                                 
         CLC   KEY(L'RDIRKEY),KEYSAVE                                           
         BNE   COPYRYES                                                         
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         USING RDIRREC,R6                                                       
         MVC   RDIRSTA,TOSTA                                                    
         MVC   RDIRENDT,TENDDT                                                  
         MVC   RDIRSTDT,TSTARTDT                                                
         MVI   RDIRMAST,X'01'      DR NOTES RECORD                              
         DROP  R6                                                               
*                                                                               
         GOTO1 ADDREC              ADD NEW DR NOTES RECORD                      
         CLI   DMCB+8,0                                                         
         BE    COPYR100                                                         
         DC    H'0'                                                             
*                                                                               
COPYR100 DS    0H                                                               
         B     COPYRYES                                                         
*                                                                               
COPYRNO  DS    0H                                                               
         MVC   PSTATUS(13),=C'NOT COPIED - '                                    
*                                                                               
         TM    COPYSTAT,FNOTFOND                                                
         BZ    COPYRNO3                                                         
         MVC   PSTATUS+14(21),=C'FROM-RECORD NOT FOUND'                         
         B     COPYRXIT                                                         
*                                                                               
COPYRNO3 TM    COPYSTAT,NEWKEYEX                                                
         BZ    COPYRNO5                                                         
         MVC   PSTATUS+14(24),=C'DR RECORD ALREADY EXISTS'                      
         B     COPYRXIT                                                         
*                                                                               
COPYRNO5 TM    COPYSTAT,NEWNKEYX                                                
         BZ    COPYRNO6                                                         
         MVC   PSTATUS+14(25),=C'DRN RECORD ALREADY EXISTS'                     
         B     COPYRXIT                                                         
*                                                                               
COPYRNO6 TM    COPYSTAT,NEWKEYOV                                                
         BZ    COPYRXIT                                                         
         MVC   PSTATUS+14(32),=C'PERIOD OVERLAPS EXISTING RECORDS'              
         B     COPYRXIT                                                         
*                                                                               
COPYRYES DS    0H                                                               
         MVC   PSTATUS(13),=C'RECORD COPIED'                                    
*                                                                               
COPYRXIT DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* EXPAND PERIOD FOR PRINTING                                                    
* P1=START DATE                                                                 
* P2=END DATE                                                                   
* P3=OUTPUT AREA                                                                
***********************************************************************         
EXPPERD  NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),0(3,R2)      CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(8,WORK),(5,0(R4))                                   
*                                                                               
         MVI   8(R4),C'-'                                                       
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),0(3,R3)      CHANGE TO PACK WITH SIGN                  
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         GOTO1 DATCON,DMCB,(8,WORK),(5,9(R4))                                   
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* REPORT SPECS                                                                  
***********************************************************************         
HEDSPECS SSPEC H1,1,C'DIRECT RESPONSE'                                          
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C'PCOPY REPORT'                                            
         SSPEC H2,52,C'------------'                                            
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,1,C'STATION'                                                  
         SSPEC H5,1,C'PERIOD'                                                   
         SSPEC H6,1,C'NOTES'                                                    
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H5,112,PAGE                                                      
         SSPEC H9,1,C'STATION'                                                  
         SSPEC H10,1,C'-------'                                                 
         SSPEC H9,12,C'PERIOD'                                                  
         SSPEC H10,12,C'------'                                                 
         SSPEC H9,34,C'STATUS'                                                  
         SSPEC H10,34,C'------'                                                 
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* HEADLINE ROUTINE                                                              
***********************************************************************         
HOOK     NTR1                                                                   
         MVC   H4+8(6),DIRFSTA                                                  
         MVC   H5+8(17),DIRFPER                                                 
         MVC   H6+8(1),DIRTNOT                                                  
HOOKX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD                                                               
* INPUT - R2 POINTS TO FIELD HEADER                                             
* OUTPUT - STARTDT HAS START DATE                                               
*          ENDDT HAS END DATE                                                   
***********************************************************************         
VALIPERI NTR1                                                                   
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),BLOCK,C',=,-'                                  
         CLI   DMCB+4,0                                                         
         BE    INVLPER             ERROR ENCOUNTERED                            
*                                                                               
* VALIDATE START DATE                                                           
*                                                                               
         LA    R5,BLOCK                                                         
         GOTO1 DATVAL,DMCB,12(R5),WORK                                          
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    INVLPER                                                          
         GOTO1 DATCON,DMCB,WORK,(19,STARTDT)     START DATE                     
*                                                                               
* VALIDATE END DATE                                                             
* END DATE IS SECOND IN SCANNER BLOCK                                           
*                                                                               
         GOTO1 DATVAL,DMCB,22(R5),WORK+6                                        
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    INVLPER                                                          
*                                                                               
         CLC   WORK+6(6),WORK      END V START DATE                             
         BL    INVLDAT             ERR - END DATE BEFORE START DATE             
*                                                                               
         GOTO1 DATCON,DMCB,WORK+6,(19,ENDDT)     END DATE                       
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),ENDDT(3)  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         MVC   ENDDT,WORK                                                       
*                                                                               
         ZAP   WORK+8(4),=P'0'                                                  
         MVO   WORK+8(4),STARTDT(3) CHANGE TO PACK WITH SIGN                    
         ZAP   WORK+4(4),=P'999999'                                             
         SP    WORK+4(4),WORK+8(4) GET 9'S COMPLEMENT                           
         MVO   WORK(4),WORK+4(4)   CHANGE TO PWOS                               
         MVC   STARTDT,WORK                                                     
*                                                                               
VALPERX  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK NEW KEY AGAINST EXISTING RECORDS FOR OVERLAPPING PERIOD                 
* P1=STATION CALL                                                               
* P2=START DATE                                                                 
* P3=END DATE                                                                   
* SETS CONDITION CODE ON EXIT                                                   
***********************************************************************         
VALOVL   NTR1                                                                   
         L     R3,0(R1)                                                         
         L     R4,4(R1)                                                         
         L     R5,8(R1)                                                         
*                                                                               
         XC    KEY,KEY             CHECK IF NEW KEY EXISTS                      
         LA    R6,KEY                                                           
         USING RDIRKEY,R6                                                       
         MVI   RDIRTYP,RDIRTYPQ                                                 
         MVC   RDIRREP,AGENCY                                                   
         MVC   RDIRSTA,0(R3)                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
         GOTO1 HIGH                                                             
*                                                                               
VALOV10  CLC   KEY(20),KEYSAVE     OK IF DIFFERENT STATION                      
         BNE   VALOVYES                                                         
         CLI   RDIRMAST,0          MUST BE MASTER REC                           
         BNE   VALOV30                                                          
         CLC   RDIRSTDT,0(R4)      CAN'T OVERLAP                                
         BE    VALOVNO                                                          
         CLC   RDIRENDT,0(R5)                                                   
         BE    VALOVNO                                                          
*                                                                               
         CLC   0(3,R5),RDIRENDT    IS NEW END DATE EARLIER THAN                 
         BH    VALOV20             EXISTING END DATE?                           
*                                                                               
         CLC   0(3,R4),RDIRENDT    NO, NEW START DATE HAS TO BE                 
         BL    VALOV30             LATER THAN EXISTING END DATE                 
         B     VALOVNO                                                          
*                                                                               
VALOV20  CLC   0(3,R5),RDIRSTDT    YES, NEW END DATE HAS TO BE                  
         BNH   VALOVNO             EARLIER THAN EXISTING START DATE             
*                                                                               
VALOV30  DS    0H                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
         GOTO1 SEQ                                                              
         B     VALOV10                                                          
*                                                                               
VALOVNO  LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
VALOVYES SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
*                                                                               
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
*                                                                               
INVLPER  MVC   RERROR,=AL2(INVPER)                                              
         B     ERREND                                                           
*                                                                               
INVLDAT  MVC   RERROR,=AL2(INVDAT)                                              
         B     ERREND                                                           
*                                                                               
RECNOTFD MVC   RERROR,=AL2(NOTFOUND)                                            
         B     ERREND                                                           
*                                                                               
ISINFILE MVC   RERROR,=AL2(RECEXIST)                                            
         B     ERREND                                                           
*                                                                               
INVLDOVL MVC   RERROR,=AL2(384)    DATE RANGES CANNOT OVERLAP                   
         B     ERREND                                                           
*                                                                               
OVLEXIST MVC   RERROR,=AL2(366)    RECORD OVERLAPS EXISTING RECORD              
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 MYERROR                                                          
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
ACTPCOPY EQU   16                                                               
RELO     DS    F                                                                
SEQKEY   DS    CL(L'KEY)                                                        
FROMSTA  DS    CL5                 FROM                                         
FSTARTDT DS    XL3                   START DATE                                 
FENDDT   DS    XL3                   END DATE                                   
*                                                                               
TOSTA    DS    CL5                 TO                                           
TSTARTDT DS    XL3                   START DATE                                 
TENDDT   DS    XL3                   END DATE                                   
*                                                                               
STARTDT  DS    XL3                                                              
ENDDT    DS    XL3                                                              
*                                                                               
CTRLID   DS    CL(L'RDIRDCID)      CONTROL ID                                   
*                                                                               
COPYSTAT DS    X                   COPY ACTION STATUS                           
FNOTFOND EQU   X'80'               FROM DR RECORD NOT FOUND                     
NEWKEYEX EQU   X'40'               NEW DR KEY ALREADY EXISTS                    
NEWNKEYX EQU   X'20'               NEW NOTE KEY ALREADY EXISTS                  
NEWKEYOV EQU   X'10'               NEW KEY OVERLAPS EXISTING RECORDS            
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENDIR                                                       
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMCCD          (OUR COPY SCREEN OVERLAY)                    
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         PRINT ON                                                               
PRTLINE  DSECT                                                                  
PSTATION DS    CL6                                                              
         DS    CL5                                                              
PPERIOD  DS    CL17                                                             
         DS    CL5                                                              
PSTATUS  DS    0C                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005RESFM28   05/01/02'                                      
         END                                                                    
