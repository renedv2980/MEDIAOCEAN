*          DATA SET ACBAT12    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T61B12A                                                                  
         TITLE 'SPECIAL JOURNAL ENTRY'                                          
T61B12   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,**T61B12,CLEAR=YES                                  
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
         EJECT                                                                  
*                                                                               
* DCUR   L 006 DO NOT ALLOW DEBIT/CREDIT ACCOUNTS THAT ARE XJOBS                
*                                                                               
*---------------------------------------------------------------                
*        VALIDATE DEBIT & CREDIT ACCOUNTS                                       
*---------------------------------------------------------------                
*                                                                               
         MVC   KEY,SPACES                                                       
         MVI   SW,C'Y'                                                          
         SR    R6,R6               NO PROFILES                                  
         MVC   KEY(1),COMPANY                                                   
         LA    R2,TRADACH                                                       
         BAS   RE,ANY                                                           
         MVI   ERRNUM,17                                                        
         SR    R3,R3                                                            
         IC    R3,TRADACH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),TRADAC                                                  
*                                                                               
         BAS   RE,GETACC                                                        
         MVC   DRNAME,ACCTNAME                                                  
         MVC   DRNUM,ACCTNUM                                                    
*                                                                               
         BAS   RE,CHKXJOB          EXCLUDE X-JOBS                               
         CLI   ERRNUM,45           IS ACCOUNT AN XJOB                           
         BE    ERROR               YES                                          
*                                                                               
         MVI   ERRNUM,18                                                        
         TM    ACCTSTAT,X'80'      CAN WE POST TO THIS ACCOUNT                  
         BZ    ERROR                                                            
         TM    ACCTSTAT,X'10'      LOCKED ACCOUNT                               
         BO    ERROR                                                            
         TM    ACCTSTAT,X'20'                                                   
         BO    ERROR                                                            
         TM    TRADACH+4,X'20'                                                  
         BO    TRAN2                                                            
         MVC   TRADACN,DRNAME                                                   
         MVI   SW,C'Y'                                                          
         CLC   COUNTRY,=C'US'                                                   
         BE    TRAN1                                                            
         CLI   DRNUM,C'1'          JWT LONDON                                   
         BNE   TRAN1                                                            
         CLC   DRNUM+1(2),=C'SI'                                                
         BNE   TRAN1                                                            
         CLC   TRACAC(2),=C'SI'    SKIP TEST IF BOTH 'SI'                       
         BE    *+12                                                             
*                                                                               
TRAN1    MVI   WORK,C'D'                                                        
         BAS   RE,TESTIT                                                        
         CLI   SW,C'N'                                                          
         BE    ERROR                                                            
         OI    TRADACNH+6,X'80'                                                 
         OI    TRADACH+4,X'20'                                                  
*                                                                               
TRAN2    LA    R2,TRACACH                                                       
         BAS   RE,ANY                                                           
         MVC   KEY+1(14),SPACES                                                 
         MVI   ERRNUM,17                                                        
         IC    R3,TRACACH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),TRACAC                                                  
         BAS   RE,GETACC                                                        
         MVC   CRNAME,ACCTNAME                                                  
         MVC   CRNUM,ACCTNUM                                                    
         MVI   ERRNUM,18                                                        
*                                                                               
         TM    ACCTSTAT,X'80'      CAN WE POST TO THIS ACCOUNT                  
         BZ    ERROR                                                            
         TM    ACCTSTAT,X'10'      LOCKED ACCOUNT                               
         BO    ERROR                                                            
         TM    ACCTSTAT,X'20'                                                   
         BO    ERROR                                                            
         TM    TRACACH+4,X'20'                                                  
         BO    TRAN2A                                                           
         MVC   TRACACN,CRNAME                                                   
         OI    TRACACNH+6,X'80'                                                 
         OI    TRACACH+4,X'20'                                                  
*                                                                               
TRAN2A   CLC   COUNTRY,=C'US'                                                   
         BE    TRAN2AA                                                          
         CLI   CRNUM,C'1'                                                       
         BNE   TRAN2AA                                                          
         CLC   CRNUM+1(2),=C'SI'   JWT LONDON                                   
         BE    *+12                                                             
*                                                                               
TRAN2AA  MVI   WORK,C'C'                                                        
         BAS   RE,TESTIT                                                        
         CLI   SW,C'N'                                                          
         BE    ERROR                                                            
         EJECT                                                                  
*---------------------------------------------------------------                
*       TRY TO READ CONTRA-ACCOUNTS                                             
*---------------------------------------------------------------                
*                                                                               
         XC    DRCANAME,DRCANAME                                                
         XC    CRCANAME,CRCANAME                                                
         LA    R2,TRADCAH                                                       
         BAS   RE,ANY                                                           
         CLC   DRNUM+1(2),=C'SR'                                                
         BNE   TRAN2AB                                                          
         MVI   ERRNUM,2                                                         
         CLC   TRADCA(3),=C'***'                                                
         BNE   ERROR                                                            
         MVC   DRCANUM,TRADCA                                                   
         MVC   DRCANUM(3),SPACES                                                
         OC    DRCANUM,SPACES                                                   
         CLC   DRCANUM,SPACES                                                   
         BE    ERROR                                                            
         B     TRAN2B                                                           
*                                                                               
TRAN2AB  CLC   TRADCA(3),=C'***'   ONLY ALLOW *** FOR SR CONTRA                 
         BNE   TRAN2ABA                                                         
         MVI   ERRNUM,2                                                         
         B     ERROR                                                            
*                                                                               
TRAN2ABA MVC   KEY+1(14),SPACES                                                 
         IC    R3,TRADCAH+5        ASSUME COMPANY NOT INPUT                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),TRADCA                                                  
         BAS   RE,HIGH                                                          
         MVC   DRCANUM,KEYSAVE                                                  
         CLC   KEY(15),KEYSAVE                                                  
         BNE   TRAN2B                                                           
*                                                                               
TRAN2AC  DS    0H                                                               
         BAS   RE,GETACC                                                        
         MVC   DRCANAME,ACCTNAME                                                
         TM    TRADCAH+4,X'20'                                                  
         BO    TRAN2B                                                           
         MVC   TRADCAN,ACCTNAME                                                 
         OI    TRADCANH+6,X'80'                                                 
         OI    TRADCAH+4,X'20'                                                  
*                                                                               
TRAN2B   LA    R2,TRACCAH                                                       
         BAS   RE,ANY                                                           
         CLC   CRNUM+1(2),=C'SR'                                                
         BNE   TRAN2BA1                                                         
         MVI   ERRNUM,2                                                         
         CLC   TRACCA(3),=C'***'                                                
         BNE   ERROR                                                            
         MVC   CRCANUM,TRACCA                                                   
         MVC   CRCANUM(3),SPACES                                                
         OC    CRCANUM,SPACES                                                   
         CLC   CRCANUM,SPACES                                                   
         BE    ERROR                                                            
         B     TRAN2C                                                           
*                                                                               
TRAN2BA1 CLC   TRACCA(3),=C'***'   ONLY ALLOW *** FOR SR CONTRA                 
         BNE   TRAN2BAA                                                         
         MVI   ERRNUM,2                                                         
         B     ERROR                                                            
*                                                                               
TRAN2BAA MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),SPACES                                                 
         IC    R3,TRACCAH+5                                                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+1(0),TRACCA                                                  
         BAS   RE,HIGH                                                          
         MVC   CRCANUM,KEYSAVE                                                  
         CLC   KEY(15),KEYSAVE                                                  
         BNE   TRAN2C                                                           
*                                                                               
TRAN2BA  DS    0H                                                               
         BAS   RE,GETACC                                                        
         MVC   CRCANAME,ACCTNAME                                                
         TM    TRACCAH+4,X'20'                                                  
         BO    TRAN2C                                                           
         MVC   TRACCAN,ACCTNAME                                                 
         OI    TRACCANH+6,X'80'                                                 
         OI    TRACCAH+4,X'20'                                                  
*                                                                               
TRAN2C   DS    0H                                                               
         LA    R2,TRADACH          NOW TEST THE UNITS                           
         CLI   TRADAC,C'1'                                                      
         BL    TRAN3                                                            
         CLC   TRADAC(1),TRACAC    IF UNITS ARE 1,2 OR 3,THEY                   
         BNE   ERROR               MUST BE BOTH THE SAME UNITS                  
         B     TRAN4                                                            
*                                                                               
TRAN3    CLI   TRACAC,C'G'         DR UNIT IS G OR S                            
         BE    TRAN4                                                            
         CLI   TRACAC,C'S'         AND CR UNIT MUST BE G OR S ALSO              
         BE    TRAN4                                                            
         CLC   COUNTRY,=C'US'                                                   
         BE    TRAN3A                                                           
         CLI   TRACAC,C'A'         PLUS UNIT A FOR UK                           
         BE    TRAN4                                                            
*                                                                               
TRAN3A   DS    0H                                                               
         LA    R2,TRACACH                                                       
         B     ERROR                                                            
         EJECT                                                                  
TRAN4    DS    0H                  DEAL WITH WORK CODES                         
         LA    R2,TRAWRKDH                                                      
         MVI   ERRNUM,2                                                         
         CLC   TRAWRKD(2),=C'99'                                                
         BE    ERROR                                                            
         LA    R4,COMPEL                                                        
         USING ACCOMPD,R4                                                       
         CLC   ACMPJOB,TRADAC                                                   
         BNE   TRAN4G                                                           
         LA    R2,TRAWRKDH                                                      
         BAS   RE,ANY                                                           
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(3),DRNUM                                                   
         MVC   KEY+4(2),TRAWRKD                                                 
         BAS   RE,READ                                                          
*                                                                               
         LA    R5,IOAREA                                                        
TRAN4C   CLI   0(R5),0                                                          
         BE    TRAN4G                                                           
         CLI   0(R5),X'12'                                                      
         BE    TRAN4E                                                           
         ZIC   RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     TRAN4C                                                           
*                                                                               
         USING ACANALD,R5                                                       
TRAN4E   MVC   TRADWCN,ACANDESC                                                 
         OI    TRADWCNH+6,X'80'                                                 
*                                                                               
TRAN4G   DS    0H                                                               
         LA    R2,TRACACH          CAN'T POST CREDITS TO JOBS                   
         MVI   ERRNUM,2                                                         
         CLC   ACMPJOB,TRACAC                                                   
         BE    ERROR                                                            
*                                                                               
         TM    COMPSTAT,X'20'      OFFICE AGENCY MUST HAVE WORK-CODE            
         BZ    TRAN4H                                                           
         LA    R2,TRAWRKDH                                                      
         BAS   RE,ANY                                                           
         CLC   ACMPJOB,TRADAC      DON'T CHECK FOR SJ                           
         BE    TRAN4G2                                                          
         OC    8(2,R2),SPACES                                                   
         GOTO1 AVALOFFC,DMCB,(X'80',8(R2))                                      
         BNE   ERROR                                                            
TRAN4G2  LA    R2,TRAWRKCH                                                      
         BAS   RE,ANY                                                           
         OC    8(2,R2),SPACES                                                   
         GOTO1 AVALOFFC,DMCB,(X'80',8(R2))                                      
         BNE   ERROR                                                            
TRAN4H   DS    0H                                                               
*&&UK                                                                           
         CLI   TWAACCS,C'*'        FOR OFFICE LOGON-TEST CORRECT INPUT          
         BNE   TRAN5                                                            
         MVI   ERRNUM,SECLOCK                                                   
         CLC   ACMPJOB,TRADAC      BUT SKIP DEBIT IF ITS TO A JOB               
         BE    TRAN4J                                                           
         LA    R2,TRAWRKDH                                                      
         CLC   8(1,R2),TWAACCS+1                                                
         BNE   ERROR                                                            
         MVI   ERRNUM,INVALID                                                   
         CLI   5(R2),1                                                          
         BNE   ERROR                                                            
*                                                                               
TRAN4J   LA    R2,TRAWRKCH                                                      
         MVI   ERRNUM,SECLOCK                                                   
         CLC   8(1,R2),TWAACCS+1                                                
         BNE   ERROR                                                            
         MVI   ERRNUM,INVALID                                                   
         CLI   5(R2),1                                                          
         BNE   ERROR                                                            
*&&                                                                             
         EJECT                                                                  
*---------------------------------------------------------------                
*        BUILD '64' ELEMENT (DESCRIPTION)                                       
*---------------------------------------------------------------                
*                                                                               
TRAN5    LA    R8,IOAREA+2                                                      
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,X'64'                                                     
         LA    R2,TRADOCH                                                       
         BAS   RE,ANY                                                           
         MVC   DLDSREF,SPACES                                                   
         IC    R3,TRADOCH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DLDSREF(0),TRADOC                                                
         LA    R2,TRADATH                                                       
         MVI   ERRNUM,13                                                        
         CLI   TRADATH+5,0         ANY DATE INPUT                               
         BNE   TRAN6               IF THERE WAS BRANCH                          
         BAS   RE,GETODAY                                                       
         B     TRAN8                                                            
*                                                                               
TRAN6    DS    0H                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         CLC   WORK(2),=C'70'                                                   
         BL    ERROR                                                            
*                                                                               
TRAN8    DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,DLDSDATE)                                
         GOTO1 DATECHK,DMCB,DLDSDATE                                            
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
*&&UK                                                                           
         OI    DLDSSTAT,X'08'      AUTHORISE                                    
         LA    R2,TRAURGH                                                       
         MVI   ERRNUM,2                                                         
         CLI   5(R2),0                                                          
         BE    TRAN10                                                           
         CLI   TRAURG,C'U'                                                      
         BNE   ERROR                                                            
         OI    DLDSSTAT,X'40'                                                   
*                                                                               
TRAN10   DS    0H                                                               
*&&                                                                             
         LA    R2,TRANARH                                                       
         LA    R3,DLDSNARR                                                      
         XC    DLDSNARR,DLDSNARR                                                
         BAS   RE,NARRSCAN                                                      
         LA    R5,DLDSNARR                                                      
         SR    R5,R8               LENGTH OF ELEMENT - NARRATIVE                
         AR    R5,R6               R6 = LENGTH OF NARRATIVE                     
         STH   R5,FULL                                                          
         MVC   DLDSLEN,FULL+1                                                   
         AR    R8,R5               BUMP TO NEXT ELEMENT                         
         EJECT                                                                  
*---------------------------------------------------------------                
*        BUILD X'23' ELEMENT                                                    
*---------------------------------------------------------------                
*                                                                               
*&&US                                                                           
         USING ACOTHERD,R8                                                      
         MVC   ACOTNUM(13),SPACES                                               
         LA    R2,TRANUMH                                                       
         MVI   ERRNUM,2                                                         
         CLI   5(R2),0             ANYTHING IN SPECIAL NUMBER                   
         BNE   BLD23B                                                           
         CLI   TRASDTH+5,0         IF THERE'S A DATE                            
         BE    BLD69                                                            
         MVI   ERRNUM,1            THEN MUST HAVE SPECIAL NUMBER                
         B     ERROR                                                            
*                                                                               
BLD23B   CLC   DRNUM+1(2),=C'SR'   ONLY FOR SR POSTINGS                         
         BE    *+14                                                             
         CLC   CRNUM+1(2),=C'SR'                                                
         BNE   ERROR                                                            
         CLI   5(R2),3                                                          
         BL    ERROR                                                            
         CLI   TRANUM+1,C','                                                    
         BNE   ERROR                                                            
         LA    R1,SYSLIST          VALIDATE SYSTEM CODE                         
         LA    RE,SYSLEN                                                        
         CLC   TRANUM(1),0(R1)                                                  
         BE    *+16                                                             
         LA    R1,1(R1)                                                         
         BCT   RE,*-14                                                          
         B     ERROR                                                            
         MVC   ACOTPROF(1),TRANUM  MOVE IT IN                                   
         ZIC   RF,5(R2)                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACOTNUM(0),TRANUM+2 AS WELL AS FREE FORM NUMBER                  
         MVC   ACOTEL(2),=X'230F'                                               
         CLI   TRASDTH+5,0         CHECK FOR SPECIAL DATE                       
         BE    BLD23X                                                           
         LA    R2,TRASDTH          VALIDATE THE DATE                            
         MVI   ERRNUM,13                                                        
         GOTO1 DATVAL,DMCB,(2,TRASDT),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   ACOTDATE,WORK+6                                                  
*                                                                               
BLD23X   ZIC   R5,ACOTLEN                                                       
         AR    R8,R5                                                            
         B     BLD69                                                            
SYSLIST  DC    C'PSNJ'                                                          
SYSLEN   EQU   *-SYSLIST                                                        
*&&                                                                             
         EJECT                                                                  
*---------------------------------------------------------------                
*        BUILD '69 & 6A ELEMENTS                                                
*---------------------------------------------------------------                
*                                                                               
         USING DLPOSTD,R8                                                       
BLD69    MVI   DLPSEL,X'69'                                                     
         MVI   DLPSLEN,X'71'                                                    
         MVC   DLPSDBAC,DRNUM                                                   
         MVC   DLPSDBNM,DRNAME                                                  
         MVC   DLPSCRAC,DRCANUM                                                 
         XC    DLPSCRNM,DLPSCRNM                                                
         MVI   DLPSCRNM,C' '                                                    
         OC    DRCANAME,DRCANAME                                                
         BZ    *+10                                                             
         MVC   DLPSCRNM,DRCANAME                                                
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,SPACES                                                  
         CLI   TRAWRKDH+5,0                                                     
         BE    *+10                                                             
         MVC   DLPSANAL,TRAWRKD                                                 
         OC    DLPSANAL,SPACES                                                  
*                                                                               
         LA    R2,TRAAMTH                                                       
         SR    R3,R3                                                            
         IC    R3,TRAAMTH+5                                                     
         MVI   ERRNUM,25                                                        
         GOTO1 AMTVAL,DMCB,TRAAMT,(R3)                                          
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         L     R4,DMCB+4                                                        
         LA    R4,0(R4)                                                         
         ZAP   DLPSAMNT,0(8,R4)                                                 
         ZAP   TRANSAMT,DLPSAMNT   SAVE AMOUNT                                  
         LR    R7,R8                                                            
         IC    R3,DLPSLEN                                                       
         AR    R8,R3                                                            
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(R7)       2ND ELEMENT                                  
         MVI   DLPSEL,X'6A'                                                     
         MVC   DLPSCRAC,CRNUM                                                   
         MVC   DLPSCRNM,CRNAME                                                  
         MVC   DLPSDBAC,CRCANUM                                                 
         XC    DLPSDBNM,DLPSDBNM                                                
         MVI   DLPSDBNM,C' '                                                    
         OC    CRCANAME,CRCANAME                                                
         BZ    *+10                                                             
         MVC   DLPSDBNM,CRCANAME                                                
         MVC   DLPSANAL,SPACES                                                  
         CLI   TRAWRKCH+5,0                                                     
         BE    *+10                                                             
         MVC   DLPSANAL,TRAWRKC                                                 
         OC    DLPSANAL,SPACES                                                  
*                                                                               
         IC    R3,DLPSLEN          LENGTH OF SECOND ELEMENT                     
         AR    R8,R3                                                            
         MVI   0(R8),0             END OF RECORD                                
         LA    R4,IOAREA-1                                                      
         SR    R8,R4                                                            
         STH   R8,HALF                                                          
         MVC   IOAREA(2),HALF      TOTAL LENGTH                                 
         BAS   RE,PUTDAY                                                        
         EJECT                                                                  
*---------------------------------------------------------------                
*        BUILD TWA1 RECORD & PUT IT THERE                                       
*---------------------------------------------------------------                
*                                                                               
         XC    WORK,WORK                                                        
         IC    R3,TRADOCH+5                                                     
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),TRADOC      REF                                          
         L     R3,DMCB+8                                                        
         MVC   WORK+10(4),0(R3)    DISK ADDRESS                                 
         BAS   RE,ADTWA1                                                        
         LA    R2,TRADOCH                                                       
         MVI   ERRNUM,X'FF'        RETURN 'OK' TO ROOT                          
         B     EXIT                                                             
         EJECT                                                                  
*--------------------------------------------------------------                 
*        SEE IF GETACC HAS PUT AN XJOB IN ACCTNUM                               
*        RETURNS ERRNUM SET TO 45 IF AN XJOB                                    
*--------------------------------------------------------------                 
CHKXJOB  NTR1                                                                   
         MVI   ERRNUM,0                                                         
*                                                                               
         CLC   PRODUL,ACCTNUM+1    PRODUCTION LEDGER?                           
         BNE   CHKXX                                                            
*                                                                               
         LA    R3,ACCTNUM+3                                                     
         ZIC   R1,PRDLNGTH                                                      
         LR    R0,R1                                                            
         LA    R3,0(R1,R3)                                                      
         IC    R1,JOBLNGTH                                                      
         SR    R1,R0                                                            
         LTR   R1,R1                                                            
         BNP   CHKXX               NO JOBS !!!!                                 
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),SPACES      IS THIS A JOB?                               
         BNH   CHKXX               NO                                           
*                                                                               
         LA    R3,ACCTNUM                                                       
         GOTO1 ASETJOB,DMCB,(R3)                                                
         TM    ACOPSTAT,ACOXJOB                                                 
         BNO   CHKXX                                                            
         MVI   ERRNUM,45                                                        
CHKXX    XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------------                
*        LOOK FOR ACCOUNT FLAGS TO DISQUALIFY POSTINGS                          
*---------------------------------------------------------------                
*                                                                               
TESTIT   NTR1                                                                   
         CLI   ACCOST,C' '         ANALYSIS FLAG                                
         BNH   TEST2                                                            
         CLI   ACCOST,0                                                         
         BE    TEST2                                                            
         CLI   ACCOST,C'('         SPECIAL FLAG FOR COSTING CLIENTS             
         BE    TEST2                                                            
         CLI   ACCOST,C')'                                                      
         BNE   NOEXIT                                                           
*                                                                               
TEST2    DS    0H                                                               
         TM    ACCTSTAT,X'40'      PERSONAL EXPENSE ITEM                        
         BO    NOEXIT                                                           
         TM    ACCTSTAT,X'08'      DEPARTMENTAL FLAG                            
         BO    NOEXIT                                                           
*                                                                               
         LA    RF,DLIST            NOW TEST DEBIT/CREDIT EXCLUSIONS             
         CLI   WORK,C'D'                                                        
         BE    *+8                                                              
         LA    RF,CLIST                                                         
*                                                                               
TEST4    CLI   0(RF),X'FF'                                                      
         BE    TEST6                                                            
         CLC   0(2,RF),8(R2)                                                    
         BE    NOEXIT                                                           
         LA    RF,2(RF)                                                         
         B     TEST4                                                            
*                                                                               
TEST6    CLC   =C'SE',8(R2)        TEST FURTHER FOR SE                          
         BNE   YESEXIT                                                          
*                                                                               
         MVC   WORK,SPACES                                                      
         SR    R3,R3               GET THE ACCOUNT                              
         IC    R3,5(R2)                                                         
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
*                                                                               
         LA    R1,CATBLK                                                        
         USING CATD,R1                                                          
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,DATAMGR                                                  
         MVC   CATSEAC(1),COMPANY                                               
         MVC   CATSEAC+1(L'CATSEAC-1),WORK                                      
         GOTO1 VCATCALL                                                         
         CLI   CATPST,C'N'                                                      
         BE    YESEXIT                                                          
*                                                                               
NOEXIT   MVI   SW,C'N'                                                          
YESEXIT  XIT1                                                                   
*                                                                               
DLIST    DS    0H                                                               
*&&UK                                                                           
         DC    C'SVSXSFSIST'                                                    
*&&                                                                             
*&&US                                                                           
         DC    C'SKSISPSQSSSTSUSVSWSXSY'                                        
*&&                                                                             
         DC    X'FF'                                                            
*                                                                               
CLIST    DS    0H                                                               
*&&UK                                                                           
         DC    C'SE'                                                            
*&&                                                                             
*&&US                                                                           
         DC    C'SJ'                                                            
*&&                                                                             
         DC    X'FF'                                                            
*                                                                               
         DS    0D                                                               
CATBLK   DS    XL(CATLNQ)                                                       
         EJECT                                                                  
*---------------------------------------------------------------                
*        ACBATCODE                                                              
*---------------------------------------------------------------                
*                                                                               
       ++INCLUDE ACBATCODE                                                      
         EJECT                                                                  
*---------------------------------------------------------------                
*        ACBATDSECT                                                             
*---------------------------------------------------------------                
*                                                                               
       ++INCLUDE ACBATDSECT                                                     
         EJECT                                                                  
*---------------------------------------------------------------                
*        ACBATECD                                                               
*---------------------------------------------------------------                
*                                                                               
       ++INCLUDE ACBATECD                                                       
         EJECT                                                                  
*---------------------------------------------------------------                
*        LOCAL WORKING STORAGE                                                  
*---------------------------------------------------------------                
*                                                                               
PROGD    DSECT                                                                  
SW       DS    CL1                                                              
DRNUM    DS    CL15                                                             
DRNAME   DS    CL36                                                             
CRNUM    DS    CL15                                                             
CRNAME   DS    CL36                                                             
DRCANUM  DS    CL15                                                             
DRCANAME DS    CL36                                                             
CRCANUM  DS    CL15                                                             
CRCANAME DS    CL36                                                             
KEY      DS    CL49                                                             
IOAREA   DS    2000C                                                            
PROGDX   DS    0C                                                               
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* ACCATCALLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCATCALLD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACBAT12   05/01/02'                                      
         END                                                                    
