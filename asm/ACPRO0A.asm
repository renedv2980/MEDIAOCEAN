*          DATA SET ACPRO0A    AT LEVEL 004 AS OF 05/20/11                      
*PHASE T60B0AA                                                                  
         TITLE 'T60B0A - JOB NUMBERING (JNUM)'                                  
T60B0A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B0A**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         BAS   RE,VREC                                                          
         BAS   RE,DKEY                                                          
         BAS   RE,DREC                                                          
         B     XIT                                                              
*                                                                               
MODE4    CLI   MODE,DISPREC                                                     
         BNE   XIT                                                              
         BAS   RE,DKEY                                                          
         BAS   RE,DREC                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
*                                                                               
         USING AJNRECD,R6                                                       
VKEY     NTR1                                                                   
         LA    R6,USERKEY                                                       
         XC    USERKEY,USERKEY                                                  
         MVI   AJNKTYP,AJNKTYPQ                                                 
         MVI   AJNKSUB,AJNKSUBQ                                                 
         MVC   AJNKCUL,CUL                                                      
*                                                                               
         LA    R2,PROOGRH          OFFICE GROUP                                 
         CLI   5(R2),0                                                          
         BE    VKEY2                                                            
         CLC   =C'ALL',8(R2)       TEST FOR ALL                                 
         BE    VKEY2                                                            
         GOTO1 VALOG                                                            
         MVC   AJNKOG,8(R2)                                                     
*                                                                               
VKEY2    LA    R2,PROOFFH          OFFICE                                       
         CLI   5(R2),0                                                          
         BE    VKEY4                                                            
         MVI   ERROR,NOTOFNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PROOGRH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALOFF                                                           
         MVC   AJNKOFC,EFFOFFC                                                  
*                                                                               
VKEY4    LA    R2,PROCLIH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    VKEY6                                                            
         MVI   ERROR,NOTCLNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PROOGRH+5,0                                                      
         BNE   ERREND                                                           
         MVI   ERROR,NOTCLNOF      NOT COMPATIBLE WITH OFFICE                   
         CLI   PROOFFH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALCLI                                                           
         MVC   AJNKCLI,CLICODE                                                  
*                                                                               
VKEY6    LA    R2,PROPROH          PRODUCT                                      
         CLI   5(R2),0                                                          
         BE    VKEY8                                                            
         MVI   ERROR,NEEDCLI       IF INPUT, NEED CLIENT AS WELL                
         CLI   PROCLIH+5,0                                                      
         BE    ERREND                                                           
         GOTO1 VALPROD                                                          
         MVC   AJNKPRO,PRODCODE                                                 
*                                                                               
VKEY8    LA    R2,PROMGRH          MEDIA GROUP                                  
         CLI   5(R2),0                                                          
         BE    VKEY10                                                           
         GOTO1 VALMG                                                            
         MVC   AJNKMG,8(R2)                                                     
*                                                                               
VKEY10   LA    R2,PROMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BE    VKEY11                                                           
         MVI   ERROR,NOTMENMG      NOT COMPATIBLE WITH MEDIA GROUP              
         CLI   PROMGRH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALMED                                                           
         MVC   AJNKMED,8(R2)                                                    
*                                                                               
VKEY11   LA    R2,PROTYPH          TYPE                                         
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),0                                                          
         BH    VKEY11A                                                          
         CLI   ACTNUM,ACTADD                                                    
         BE    ERREND                                                           
         MVI   PROTYP,C'L'                                                      
         OI    PROTYPH+6,X'80'                                                  
         MVI   5(R2),1                                                          
*                                                                               
VKEY11A  GOTO1 ANY                                                              
         MVI   AJNKTYPE,AJNKLIVQ                                                
         CLI   PROTYP,C'L'                                                      
         BE    VKEY12                                                           
         MVI   AJNKTYPE,AJNKDRFQ                                                
         CLI   PROTYP,C'D'                                                      
         BNE   ERREND                                                           
*                                                                               
VKEY12   LA    R2,PROEFFH                                                       
         GOTO1 ANY                                                              
         GOTO1 DATCON,DMCB,(5,0),(1,TODAY)                                      
         SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),=CL5'TODAY'                                              
         BNE   VKEY14                                                           
         MVC   EFFDATE,TODAY                                                    
         B     VKEY20                                                           
*                                                                               
VKEY14   CLI   ACTNUM,ACTADD       ARE WE ADDING ?                              
         BE    VKEY18              YES                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),=CL4'LAST'                                               
         BNE   VKEY16                                                           
         MVC   EFFDATE,EFFS                                                     
         B     VKEY20                                                           
*                                                                               
VKEY16   EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),=CL8'PREVIOUS'                                           
         BNE   VKEY18                                                           
         ICM   RE,7,EFFDATE                                                     
         BCTR  RE,0                                                             
         STCM  RE,7,EFFDATE                                                     
         B     VKEY20                                                           
*                                                                               
VKEY18   MVI   ERROR,INVDATE                                                    
         GOTO1 DATVAL,DMCB,WORK,DUB                                             
         OC    DMCB,DMCB                                                        
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,DUB,(1,EFFDATE)                                      
*                                                                               
VKEY20   CLI   ACTNUM,ACTADD       ARE WE ADDING ?                              
         BNE   VKEY22              NO                                           
         MVI   ERROR,DATNOTGT      YES, MUST BE TODAY OR GREATER                
         CLC   EFFDATE,TODAY                                                    
         BL    ERREND                                                           
*                                                                               
VKEY22   MVC   AJNKEFF,EFFDATE                                                  
         XC    AJNKEFF,EFFS                                                     
*                                                                               
VKEYEND  MVC   KEY,USERKEY                                                      
         CLI   ACTNUM,ACTADD                                                    
         BE    VKEYX                                                            
         GOTO1 HIGH                                                             
         L     R6,AIO                                                           
         MVI   ERROR,NOTFOUND                                                   
         CLC   AJNKEY(AJNKEFF-AJNKEY),KEYSAVE                                   
         BNE   ERREND                                                           
         MVC   EFFDATE,AJNKEFF                                                  
         XC    EFFDATE,EFFS                                                     
         XC    PROEFF,PROEFF                                                    
         GOTO1 DATCON,DMCB,(1,EFFDATE),(8,PROEFF)                               
         OI    PROEFFH+6,X'80'                                                  
*                                                                               
         CLI   ACTNUM,ACTDEL       ARE WE DELETING ?                            
         BE    DELLOGIC            YES                                          
         CLI   ACTNUM,ACTREST      NO, ARE WE RESTORING ?                       
         BE    RESLOGIC            YES                                          
*                                                                               
VKEYX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              VALIDATE RECORD                                                  
*                                                                               
         USING AJNRECD,R6                                                       
VREC     NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,JNAELQ       GET EXISTING B5 ELEMENT                      
         GOTO1 GETELIO                                                          
         BE    VREC4                                                            
*                                                                               
         USING JNAELD,R6                                                        
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   JNAEL,JNAELQ                                                     
         MVI   JNALN,JNALNQ                                                     
*                                                                               
VREC4    ST    R6,SAVER6           SAVE THIS SPOT                               
         CLI   ACTNUM,ACTCHA       ARE WE CHANGING ?                            
         BNE   VREC6               NO, CONTINUE                                 
         OC    JNALNUM,JNALNUM     HAVE WE USED THIS RECORD BEFORE ?            
         BNZ   NOCHANGE            YES                                          
*                                                                               
VREC6    LA    R2,PROTITH                                                       
         LA    R0,5                MAXIMUM NUMBER OF LINES                      
         XC    NUMSN,NUMSN         NUMBER OF SEQUENTIAL FIELDS                  
         XC    NUMNA,NUMNA         NUMBER OF FIELDS NOT IN USE                  
         XC    NUMOTHR,NUMOTHR     NUMBER OF ALL FIELDS EXCEPT US               
*                                                                               
VREC8    LR    R3,R2               SAVE THIS SPOT                               
         CLI   5(R2),0             DO WE HAVE A TITLE ?                         
         BZ    VREC10              NO                                           
         GOTO1 ANY                                                              
         MVC   JNAP2N,WORK         TITLE FIELD                                  
*                                                                               
VREC10   BAS   RE,BUMP                                                          
         GOTO1 ANY                                                              
         LA    R5,TITLES                                                        
*                                                                               
VREC12   CLC   0(L'JNAP2,R5),WORK                                               
         BE    VREC16                                                           
         CLI   0(R5),X'FF'                                                      
         BE    VREC14                                                           
         LA    R5,L'TITLES(R5)                                                  
         B     VREC12                                                           
*                                                                               
VREC14   MVI   ERROR,INP2LONG                                                   
         CLI   5(R2),1             MUST BE ONLY 1 CHARACTER                     
         BNE   ERREND                                                           
*                                                                               
VREC16   CLC   WORK(2),=C'US'      IS THIS A USER SUPPLIED FIELD ?              
         BNE   VREC18              NO                                           
         MVI   ERROR,NOUS                                                       
         CLI   NUMOTHR,X'00'       DO WE HAVE AN OTHER FIELDS ?                 
         BNE   ERREND              YES, ERROR                                   
         B     VREC20                                                           
*                                                                               
VREC18   SR    R4,R4                                                            
         IC    R4,NUMOTHR                                                       
         LA    R4,1(R4)                                                         
         STC   R4,NUMOTHR                                                       
*                                                                               
VREC20   CLI   5(R3),0             DID WE HAVE A TITLE ?                        
         BNE   VREC22              YES                                          
         MVC   JNAP2N,2(R5)                                                     
         MVC   8(L'JNAP2N,R3),JNAP2N                                            
         MVC   5(1,R3),=AL1(L'JNAP2N)                                           
         OI    6(R3),X'80'                                                      
*                                                                               
VREC22   CLC   WORK(2),=C'SN'      IS THIS A SEQUENTIAL NUMBER ?                
         BNE   VREC24              NO                                           
         CLI   NUMNA,X'00'         YES, ARE ANY FIELDS NOT USED ?               
         BNE   INVEND              YES, CAN'T HAVE SEQUENTIAL THEN              
         SR    R4,R4               NO, INCREASE NUMBER OF 'SN'                  
         IC    R4,NUMSN                                                         
         LA    R4,1(R4)                                                         
         STC   R4,NUMSN                                                         
         B     VREC30                                                           
*                                                                               
VREC24   CLC   WORK(2),=C'NA'      IS THIS FIELD IN USE ?                       
         BNE   VREC26              YES                                          
         SR    R4,R4               NO, INCREASE NUMBER OF 'NA'.                 
         IC    R4,NUMNA                                                         
         LA    R4,1(R4)                                                         
         STC   R4,NUMNA                                                         
         B     VREC30                                                           
*                                                                               
VREC26   CLC   WORK(2),=C'O2'      IS THIS FIELD POSITION 2 OF OFFICE ?         
         BNE   VREC28              NO                                           
         TM    COMPSTA4,X'01'      YES, ARE THEY ON NEW OFFICES ?               
         BZ    INVEND              NO, ERROR                                    
*                                                                               
VREC28   CLI   NUMSN,X'00'         ARE THERE ANY SEQUENTIAL FIELDS ?            
         BNE   INVEND              YES, CAN'T HAVE DATA AFTER IT                
         CLI   NUMNA,X'00'         NO, ANY FIELDS NOT USED ?                    
         BNE   INVEND              YES, CAN'T HAVE DATA AFTER                   
*                                                                               
VREC30   MVC   JNAP2,WORK          VALUE FIELD                                  
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMPTOUN         GET TO NEXT TITLE                            
         LA    R6,L'JNAP2+L'JNAP2N(R6)                                          
         BCT   R0,VREC8                                                         
*                                                                               
         LA    R2,PROTITH                                                       
         CLI   NUMSN,X'00'         NEED AT LEAST 1 SEQUENTIAL FIELD             
         BE    SEQERR                                                           
*                                                                               
         CLI   NUMNA,X'03'         NEED AT LEAST 3 VALID CHARACTERS             
         BH    VALERR                                                           
*                                                                               
         L     R6,SAVER6           GET BACK TO START FOR CORRECT DISP           
         LA    R2,PROSTRTH                                                      
         MVC   JNASTRT,SPACES                                                   
         GOTO1 NUMERIC                                                          
         GOTO1 ANY                                                              
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         CLM   R4,1,NUMSN                                                       
         BNE   STRTERR                                                          
*                                                                               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   JNASTRT(0),WORK                                                  
*                                                                               
VREC32   LA    R2,PROMANRH                                                      
         MVC   JNAMANS,SPACES                                                   
         MVC   JNAMANE,SPACES                                                   
         CLI   5(R2),0                                                          
         BE    VREC40                                                           
         SR    R4,R4                                                            
         IC    R4,NUMSN                                                         
*                                                                               
         GOTO1 SCANNER,DMCB,PROMANRH,(13,BLOCK),C',=-,'                         
         SR    R0,R0                                                            
         ICM   R0,1,DMCB+4                                                      
         BZ    INVEND                                                           
*                                                                               
         LA    R3,BLOCK                                                         
         CH    R0,=H'1'            DO WE HAVE 1 ENTRY ?                         
         BNE   VREC34              NO, MUST BE 2                                
         CLM   R4,1,0(R3)                                                       
         BNE   MANERR                                                           
         OC    4(4,R3),4(R3)                                                    
         BZ    INVEND              NOT NUMERIC                                  
*                                                                               
         SR    R7,R7                                                            
         IC    R7,0(R3)                                                         
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   JNAMANS(0),12(R3)                                                
*                                                                               
         CLI   1(R3),X'00'         DO WE HAVE A SECOND FIELD ?                  
         BZ    VREC38              NO                                           
         CLM   R4,1,1(R3)          YES                                          
         BNE   MANERR                                                           
         OC    8(4,R3),8(R3)                                                    
         BZ    INVEND              NOT NUMERIC                                  
*                                                                               
         SR    R7,R7                                                            
         IC    R7,1(R3)                                                         
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     VREC36                                                           
         MVC   JNAMANE(0),22(R3)                                                
*                                                                               
VREC34   CH    R0,=H'2'                                                         
         BH    INVEND                                                           
         CLM   R4,1,0(R3)                                                       
         BNE   MANERR                                                           
         OC    4(4,R3),4(R3)                                                    
         BZ    INVEND                                                           
*                                                                               
         SR    R7,R7                                                            
         IC    R7,0(R3)                                                         
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   JNAMANS(0),12(R3)                                                
*                                                                               
         LA    R3,32(R3)                                                        
         CLM   R4,1,0(R3)                                                       
         BNE   MANERR                                                           
         OC    4(4,R3),4(R3)                                                    
         BZ    INVEND                                                           
*                                                                               
         SR    R7,R7                                                            
         IC    R7,0(R3)                                                         
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   JNAMANE(0),12(R3)                                                
*                                                                               
VREC36   CLC   JNAMANE,JNAMANS                                                  
         BNH   ASCERR                                                           
*                                                                               
VREC38   LA    R2,PROSTRTH                                                      
         CLC   JNASTRT,JNAMANS                                                  
         BL    VREC40                                                           
         BE    SMANERR                                                          
         CLC   JNASTRT,JNAMANE                                                  
         BNH   SMANERR                                                          
         BE    SMANERR                                                          
*                                                                               
VREC40   GOTO1 ADDELEM                                                          
         GOTO1 PERSIN                                                           
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              DISPLAY KEY                                                      
*                                                                               
         USING AJNRECD,R6                                                       
DKEY     NTR1                                                                   
         L     R6,AIO                                                           
         MVC   PROOGR(L'AJNKOG),AJNKOG                                          
         OI    PROOGRH+6,X'80'                                                  
         MVC   PROOFF,AJNKOFC                                                   
         OI    PROOFFH+6,X'80'                                                  
         MVC   PROCLI,AJNKCLI                                                   
         OI    PROCLIH+6,X'80'                                                  
         MVC   PROPRO,AJNKPRO                                                   
         OI    PROPROH+6,X'80'                                                  
         MVC   PROMGR,AJNKMG                                                    
         OI    PROMGRH+6,X'80'                                                  
         MVC   PROMED,AJNKMED                                                   
         OI    PROMEDH+6,X'80'                                                  
         OC    AJNKOG(AJNKEFF-AJNKOG),AJNKOG                                    
         BNZ   *+10                                                             
         MVC   PROOGR(L'PROOGR),=C'ALL'                                         
         OI    PROTYPH+6,X'80'                                                  
         MVI   PROTYP,C'L'                                                      
         CLI   AJNKTYPE,AJNKLIVQ                                                
         BE    XIT                                                              
         MVI   PROTYP,C'D'                                                      
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              DISPLAY RECORD                                                   
*                                                                               
         USING AJNRECD,R6                                                       
DREC     NTR1                                                                   
         L     R6,AIO                                                           
         GOTO1 PERSOUT                                                          
         MVC   PROLACT,SPACES                                                   
         MVC   PROLACT(14),=C'LAST ACTIVITY:'                                   
         MVC   PROLACT+15(20),WORK+20                                           
         OI    PROLACTH+6,X'80'                                                 
         GOTO1 VCLEARF,DMCB,PROTITH,PROLAST                                     
*                                                                               
         LA    R2,PROSTATH                                                      
         MVI   8(R2),C'A'                                                       
         TM    AJNRSTA,AJNNDACT   IS RECORD ACTIVE ?                            
         BZ    *+8                 YES                                          
         MVI   8(R2),C'I'          NO                                           
         OI    6(R2),X'80'                                                      
*                                                                               
         USING JNAELD,R6                                                        
         LA    R2,PROTITH                                                       
         MVI   ELCODE,JNAELQ                                                    
         GOTO1 GETELIO                                                          
         BNE   XIT                                                              
         LA    R0,5                NUMBER OF LINES TO PRINT                     
         LR    R5,R6               SAVE ELEMENT START                           
*                                                                               
DREC2    MVC   8(L'JNAP2N,R2),JNAP2N                                            
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         MVC   8(L'JNAP2,R2),JNAP2                                              
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMPTOUN                                                      
         LA    R6,L'JNAP2+L'JNAP2N(R6)                                          
         BCT   R0,DREC2                                                         
*                                                                               
         LR    R6,R5               GET BACK TO START FOR CORRECT DISP           
         LA    R2,PROSTRTH                                                      
         MVC   8(L'JNASTRT,R2),JNASTRT                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,PROLNUMH                                                      
         MVC   8(L'JNALNUM,R2),JNALNUM                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,PROMANRH                                                      
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'JNAMANS),JNAMANS                                          
         MVC   WORK+L'JNAMANS+1(L'JNAMANE),JNAMANE                              
         GOTO1 SQUASHER,DMCB,WORK,(C'-',L'JNAMANS+L'JNAMANE+1)                  
         SR    R1,R1                                                            
         ICM   R1,1,DMCB+7                                                      
         BZ    DREC4                                                            
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK                                                     
         OI    6(R2),X'80'                                                      
*                                                                               
DREC4    CLI   ACTNUM,ACTCHA       ARE WE CHANGING ?                            
         BNE   DREC8               NO, CONTINUE                                 
         OC    JNALNUM,JNALNUM     HAVE WE USED THIS RECORD BEFORE ?            
         BNZ   NOCHANGE            YES                                          
*                                                                               
         USING AJNRECD,R6                                                       
         L     R6,AIO                                                           
         TM    AJNRSTA,AJNNDACT    IS RECORD ACTIVE ?                           
         BZ    DREC8               YES                                          
*                                                                               
         MVI   ERROR,X'FE'                                                      
         LA    R1,CONACTH          YES, CHANGE ACTION                           
         MVC   8(7,R1),=C'DISPLAY'                                              
         MVI   5(R1),7                                                          
         MVI   6(R1),X'80'                                                      
         MVI   ACTNUM,ACTDIS                                                    
         LA    R2,PROOGRH                                                       
         OI    GENSTAT2,USMYOK                                                  
         OI    CONHEADH+1,X'08'                                                 
         MVC   CONHEAD(L'DACTMSG),DACTMSG                                       
         GOTO1 ERREX2                                                           
*                                                                               
DREC8    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO DELETE AND RESTORE TEXT RECORDS.                        *         
***********************************************************************         
*                                                                               
         USING AJNRECD,R6                                                       
DELLOGIC GOTO1 READ                READ THE RECORD                              
         L     R6,AIO                                                           
         OI    AJNRSTA,AJNNDACT    DEACTIVATE THE RECORD                        
         BAS   RE,WRITEIT          REWRITE THE RECORD                           
         BAS   RE,DREC             REDISPLAY THE RECORD                         
         MVI   ERROR,X'FE'                                                      
         LA    R2,PROOGRH                                                       
         OI    GENSTAT2,USMYOK                                                  
         OI    CONHEADH+1,X'08'                                                 
         MVC   CONHEAD(L'DELMSG),DELMSG                                         
         GOTO1 ERREX2                                                           
*                                                                               
RESLOGIC GOTO1 READ                READ THE RECORD                              
         L     R6,AIO                                                           
         NI    AJNRSTA,X'FF'-AJNNDACT REACTIVATE THE RECORD                     
         BAS   RE,WRITEIT          REWRITE THE RECORD                           
         BAS   RE,DREC             REDISPLAY THE RECORD                         
         MVI   ERROR,X'FE'                                                      
         LA    R2,PROOGRH                                                       
         OI    GENSTAT2,USMYOK                                                  
         OI    CONHEADH+1,X'08'                                                 
         MVC   CONHEAD(L'RESTMSG),RESTMSG                                       
         GOTO1 ERREX2                                                           
         DROP  R6                                                               
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
*                                                                               
NOCHANGE LA    R2,PROOGRH                                                       
         LA    R1,CONACTH          YES, CHANGE ACTION                           
         MVC   8(7,R1),=C'DISPLAY'                                              
         MVI   5(R1),7                                                          
         MVI   6(R1),X'80'                                                      
         MVI   ACTNUM,ACTDIS                                                    
         MVI   ERROR,RECNOTUP                                                   
         B     ERREND                                                           
*                                                                               
WRITEIT  ST    RE,SAVERE                                                        
         GOTO1 PERSIN                                                           
         GOTO1 WRITE                                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
EFFS     DC    8XL1'FF'                                                         
*                                                                               
TITLES   DS    0CL20                                                            
         DC    CL20'USUSER SUPPLIED'                                            
         DC    CL20'CYCURRENT YEAR'                                             
         DC    CL20'OYOPEN YEAR'                                                
         DC    CL20'SNSEQUENTIAL NUMBER'                                        
         DC    CL20'OCOFFICE CODE'                                              
         DC    CL20'O2OFFICE CODE 2'                                            
         DC    CL20'OGOFFICE GROUP'                                             
         DC    CL20'NANOT APPLICABLE'                                           
         DC    XL2'FFFF',CL18'CONSTANT'                                         
*                                                                               
DELMSG   DC    CL60'RECORD IS NOW INACTIVE'                                     
RESTMSG  DC    CL60'RECORD IS NOW ACTIVE'                                       
DACTMSG  DC    CL60'RECORD IS INACTIVE'                                         
STRTMSG  DC    CL60'STARTING # NOT CONSISTENT WITH NUMBER OF SEQUENTIALX        
                FIELDS'                                                         
MANMSG   DC    CL60'MANUAL # NOT CONSISTENT WITH NUMBER OF SEQUENTIAL FX        
               IELDS'                                                           
SEQMSG   DC    CL60'MINIMUM OF ONE (1) SEQUENTIAL NUMBER IS REQUIRED'           
VALMSG   DC    CL60'MINIMUM OF THREE (3) VALID FIELDS IS REQUIRED'              
ASCMSG   DC    CL60'MANUAL RANGE MUST BE IN ASCENDING ORDER'                    
SMANMSG  DC    CL60'STARTING NUMBER IS WITHIN MANUAL RANGE'                     
*                                                                               
*                                                                               
STRTERR  MVC   CONHEAD(L'STRTMSG),STRTMSG                                       
         B     MYEND                                                            
*                                                                               
SEQERR   MVC   CONHEAD(L'SEQMSG),SEQMSG                                         
         B     MYEND                                                            
*                                                                               
MANERR   MVC   CONHEAD(L'MANMSG),MANMSG                                         
         B     MYEND                                                            
*                                                                               
VALERR   MVC   CONHEAD(L'VALMSG),VALMSG                                         
         B     MYEND                                                            
*                                                                               
SMANERR  MVC   CONHEAD(L'SMANMSG),SMANMSG                                       
         B     MYEND                                                            
*                                                                               
ASCERR   MVC   CONHEAD(L'ASCMSG),ASCMSG                                         
*                                                                               
MYEND    OI    GENSTAT2,USMYOK                                                  
         OI    CONHEADH+1,X'08'                                                 
         GOTO1 ERREX2                                                           
*                                                                               
INVEND   MVI   ERROR,INVALID       INVALID EXIT                                 
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROFAD                                                       
         DS    0F                                                               
USERKEY  DS    CL64                                                             
TODAY    DS    PL3                                                              
EFFDATE  DS    PL3                                                              
SAVER6   DS    A                                                                
SAVERE   DS    A                                                                
NUMSN    DS    X                   NUMBER OF 'SN' FIELDS                        
NUMNA    DS    X                   NUMBER OF 'NA' FIELDS                        
NUMOTHR  DS    X                   NUMBER OF ALL FIELDS EXCEPT US               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACPRO0A   05/20/11'                                      
         END                                                                    
