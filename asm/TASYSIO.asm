*          DATA SET TASYSIO    AT LEVEL 161 AS OF 01/21/16                      
*PHASE T00A86C,*                                                                
*INCLUDE TASYSIOB                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'T00A86 - SYSIO - TALENT SYSTEM I/O CONTROLLER'                  
TASYSIO  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (MYEND-MYD),**SYIO*,RA,R9,R8,R5,RR=R2,CLEAR=YES                  
         USING MYD,RC                                                           
         ST    R2,RELO                                                          
         MVC   USERRD,4(RD)        SAVE LINK BACK TO USER                       
         L     R7,0(R1)                                                         
         USING TASYSIOD,R7                                                      
         BRAS  RE,TALINIT                                                       
         CLI   TIREAD,TIFONLY      MAY BE USING SYSIO FOR FILTERING             
         BNE   TASYSIO1                                                         
         L     RC,TISSIORC         (USE RC FROM ORIGINAL ENTRY)                 
         BAS   RE,FILTREC          IN WHICH CASE, FILTER THIS RECORD            
         B     XIT                 AND RETURN CONDITION CODE                    
         SPACE 1                                                                
TASYSIO1 ST    RC,TISSIORC         (SAVE RC FOR COLFILT REENTRY)                
         BAS   RE,SETLISTS         READ ANY FILTER LISTS IF REQUESTED           
         BAS   RE,SETREAD          SET MAIN AND SUB READ FROM TIREAD            
         BAS   RE,SETKEY           SET START KEY                                
         L     R1,=A(READADDS)     GET ADDRESS OF READ ROUTINE                  
         A     R1,RELO                                                          
         SPACE 1                                                                
TASYSIO2 CLC   MAINREAD,0(R1)      MATCH ON MAIN REC NUMBER                     
         BNE   TASYSIO3                                                         
         CLC   MAINSBRC,1(R1)      MATCH ON MAIN SUB REC NUMBER                 
         BE    TASYSIO4                                                         
TASYSIO3 LA    R1,4(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   TASYSIO2                                                         
         MVI   TIERROR,TINOROUT    NO READ ROUTINE FOR THIS RECORD              
         B     XIT                                                              
         SPACE 1                                                                
TASYSIO4 NI    STATUS,X'FF'-FOUNDONE                                            
         SPACE 1                                                                
         LA    R4,KEY              PRESET A SIMPLE STARTING KEY                 
         XC    KEY,KEY                                                          
         MVC   KEY(1),MAINREAD                                                  
         MVC   KEY+1(1),MAINSBRC                                                
                                                                                
         SR    RF,RF                                                            
         ICM   RF,3,2(R1)          PICK UP ROUTINE DISPLACEMENT                 
         AR    RF,RB               ADD A(BEGIN OF SYSIO)                        
         BASR  RE,RF               AND EXECUTE                                  
         SPACE 1                                                                
         TM    STATUS,FOUNDONE     DID WE PASS A RECORD TO APPLIC.              
         BO    TASYSIOX                                                         
         OC    TIQSKEY,TIQSKEY     TEST CONTINUE KEY PRESENT                    
         BZ    TASYSIOX                                                         
         XC    TIQSKEY,TIQSKEY     CLEAR CONTINUE KEY                           
         B     TASYSIO4            AND START OVER                               
         SPACE 1                                                                
TASYSIOX XC    TIKEY,TIKEY         RELEASE SYSIO'S KEY                          
*                                  SHOW THAT SYSIO NO LONGER DOING IO           
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION CODE                                              
*              SET ACTUAL RECORDS TO BE READ                                    
         SPACE 3                                                                
SETREAD  NTR1                                                                   
         MVC   MAINREAD,TIREAD     SET DEFAULTS ESTABLISHED BY USER             
         MVC   SUBREAD,TISUBRD                                                  
         MVC   MAINSBRC,TIRDSUBT                                                
                                                                                
         CLI   SUBREAD,0           IF SUBREAD ALREADY DEFINED, GET OUT          
         BNE   SETRX                                                            
         SPACE 1                                                                
         CLI   TIREAD,TLCACDQ      CAST RECORDS                                 
         BNE   SETR2                                                            
         OC    TIFCOM,TIFCOM            IF INTERNAL CMCL NUMBER FILTER          
         BZ    SETR1                                                            
         MVC   INTCOMM,TIFCOM           THEN SAVE IN INTCOMM                    
* NO-OP  OC    TIAGY,TIAGY                                                      
* NO-OP  BNZ   *+10                                                             
* NO-OP  MVC   TIAGY,TIFAGY             SET TIAGY WHERE POSS.                   
         B     SETRX                                                            
         SPACE 1                                                                
SETR1    OC    TIFSSN,TIFSSN            IF SS NO. SPECIFIED                     
         BZ    SETR1B                                                           
         MVI   MAINREAD,TLCACCDQ        READ EMPLOYEE'S COMMLS                  
         B     SETRX                                                            
         SPACE 1                                                                
SETR1B   MVI   MAINREAD,TLCOCDQ         ELSE READ COMMERCIALS                   
         MVI   SUBREAD,TLCACDQ          SUBREAD CAST                            
         B     SETRX                                                            
         SPACE 1                                                                
SETR2    CLI   TIREAD,TLCOCDQ      COMMERCIAL RECORDS                           
         BNE   SETR10                                                           
         CLI   TIFSSN,X'41'             IF EMPLOYEE SPECIFIED                   
         BL    SETR4                                                            
         MVI   MAINREAD,TLCACCDQ        READ CAST PASSIVE                       
         MVI   SUBREAD,TLCOCDQ          SUBREAD COMMERCIALS                     
         B     SETRX                                                            
         SPACE 1                                                                
SETR4    CLI   TIFMUSIC,X'41'           IF MUSIC SPECIFIED                      
         BL    *+12                                                             
         MVI   MAINREAD,TLCOMCDQ        READ MUSIC PASSIVE                      
         B     SETRX                                                            
         CLI   TIFCLG,X'41'             IF CLIENT GROUP SPECIFIED               
         BL    SETRX                                                            
         MVI   MAINREAD,TLCOGCDQ        READ CLIENT GROUP PASSIVE               
         B     SETRX                                                            
         SPACE 1                                                                
SETR10   CLI   TIREAD,TLAYCDQ      AGENCY RECORDS                               
         BNE   SETR20                                                           
         CLI   TIFAGG,X'41'             IF AGENCY GROUP SPECIFIED               
         BL    SETR20                                                           
         MVI   MAINREAD,TLAYGCDQ        READ AGENCY GROUP PASSIVE               
         MVI   SUBREAD,TLAYCDQ          SUBREAD AGENCY                          
         B     SETRX                                                            
         SPACE 1                                                                
SETR20   CLI   TIREAD,TLCLCDQ      CLIENT RECORDS                               
         BNE   SETR30                                                           
         CLI   TIFCLG,X'41'             IF CLIENT GROUP SPECIFIED               
         BL    SETR30                                                           
         MVI   MAINREAD,TLCLGCDQ        READ CLIENT GROUP PASSIVE               
         MVI   SUBREAD,TLCLCDQ          SUBREAD CLIENT                          
         B     SETRX                                                            
         SPACE 1                                                                
SETR30   CLI   TIREAD,TLPRCDQ      PRODUCT RECORDS                              
         BNE   SETR40                                                           
         CLI   TIFPRG,X'41'             IF PRODUCT GROUP SPECIFIED              
         BL    SETR40                                                           
         MVI   MAINREAD,TLPRGCDQ        READ PRODUCT GROUP PASSIVE              
         MVI   SUBREAD,TLPRCDQ          SUBREAD PRODUCT                         
         B     SETRX                                                            
         SPACE 1                                                                
SETR40   CLI   TIREAD,TLW4CDQ      W4 RECORDS                                   
         BNE   SETR45                                                           
         CLI   TIFCORP,X'41'            IF CORPORATION SPECIFIED                
         BL    SETR45                                                           
         MVI   MAINREAD,TLW4CCDQ        READ CORPORATION PASSIVE                
         MVI   SUBREAD,TLW4CDQ          SUBREAD W4                              
         B     SETRX                                                            
         SPACE 1                                                                
SETR45   CLI   TIREAD,TLECCDQ      ECAST RECORDS                                
         BNE   SETR50                                                           
         OC    TIFCOM,TIFCOM            IF INTERNAL CMCL NUMBER FILTER          
         BZ    *+14                                                             
         MVC   INTCOMM,TIFCOM           THEN SAVE IN INTCOMM                    
         B     SETRX                                                            
         OC    TIFSSN,TIFSSN            IF SS NUMBER SPECIFIED                  
         BZ    *+12                                                             
         MVI   MAINREAD,TLECCCDQ        READ ECAST PASSIVE                      
         B     SETRX                                                            
         MVI   MAINREAD,TLCOCDQ         ELSE READ COMMERCIALS                   
         MVI   SUBREAD,TLECCDQ          SUBREAD ECAST                           
         B     SETRX                                                            
         EJECT                                                                  
*              SET MAINREAD/SUBREAD FOR INVOICES                                
         SPACE 3                                                                
SETR50   CLI   TIREAD,TLINCDQ      INVOICE RECORDS                              
         BNE   SETR60                                                           
         TM    TIFCID,X'C0'        IF CID SPECIFIED                             
         BNO   SETR52                                                           
         OC    TIFCOM,TIFCOM          AND NOT INT COMM                          
         BNZ   SETR52                                                           
         XC    KEY,KEY             GET PASSIVE FOR INT COMM NO.                 
         LA    R4,KEY                                                           
         USING TLCOPD,R4                                                        
         MVI   TLCOPCD,TLCOICDQ                                                 
         MVC   TLCOIAGY,TIFAGY                                                  
         MVC   TLCOICID,TIFCID                                                  
         BAS   RE,HIGH                                                          
         MVC   TIFCOM,TLCOICOM                                                  
         SPACE 1                                                                
SETR52   OC    TIFCOM,TIFCOM       IF INT COMMERCIAL SPECIFIED                  
         BZ    SETR54                                                           
         MVI   MAINREAD,TLINHCDQ      READ HISTORY PASSIVE                      
         OC    TISVCOM,TISVCOM                                                  
         BNZ   SETRX                                                            
         MVC   TISVCOM,TIFCOM      SAVE ORIGINAL INTERNAL COMML NUMBER          
         MVC   TISVAGY,TIFAGY      & AGENCY                                     
         B     SETRX                                                            
         SPACE 1                                                                
SETR54   OC    TIFINVD,TIFINVD     USE ACTIVE PTR IF HAVE TIFINVD               
         BNZ   SETRX                                                            
         CLI   TIQDTYPE,TIQDDUE    IF DUE DATE FILTERING                        
         BNE   *+12                                                             
         MVI   MAINREAD,TLINCCDQ      READ OPEN ITEM POINTER                    
         B     SETRX                                                            
         SPACE 1                                                                
         CLI   TIQDTYPE,TIQDCHK    IF CHECK DATE FILTERING                      
         BNE   *+12                                                             
         MVI   MAINREAD,TLINKCDQ      READ CHECK DATE POINTER                   
         B     SETRX                                                            
*                                                                               
         CLI   TIFCUST,10          IF FILTERING COD PRINTED                     
         BNE   *+12                                                             
         MVI   MAINREAD,TLINBCDQ      READ BILLING OPEN ITEM                    
         B     SETRX                                                            
*                                                                               
         CLI   TIQDTYPE,TIQDPAY    IF PAY DATE FILTERING                        
         BNE   *+12                                                             
         MVI   MAINREAD,TLINPCDQ      READ PAY DATE POINTER                     
         B     SETRX                                                            
*                                                                               
         OC    TIQPSTR(6),TIQPSTR  IF NO PERIOD REQUESTED                       
         BZ    SETR56                 USE ACTIVE POINTERS                       
         CLI   TIQDTYPE,0          IF NO DATE TYPE SPECIFIED                    
         BNE   *+8                                                              
         MVI   TIQDTYPE,TIQDBILL      DEFAULT TO BILL DATES                     
         CLI   TIQDTYPE,TIQDBILL   IF BILL DATE FILTERING                       
         BNE   *+12                                                             
         MVI   MAINREAD,TLINDCDQ      READ BILL DATE POINTER                    
         B     SETRX                                                            
         SPACE 1                                                                
SETR56   B     SETRX                                                            
         EJECT                                                                  
*              SET MAINREAD/SUBREAD FOR CHECKS                                  
         SPACE 3                                                                
SETR60   CLI   TIREAD,TLCKCDQ      CHECK RECORDS                                
         BNE   SETR70                                                           
         OC    TIFINV,TIFINV       IF INVOICE SPECIFIED                         
         BNZ   SETRX                                                            
         OC    TIFINVD,TIFINVD     OR IF DISPLAYABLE INVOICE SPECIFIED          
         BNZ   SETRX                                                            
         MVI   MAINREAD,TLCKHCDQ   CAST PAYMENT HISTORY                         
         OC    TIFCOM,TIFCOM          IF HAVE INTERNAL COMM NUMBER              
         BNZ   SETRX                                                            
         MVI   MAINREAD,TLCKYCDQ   YTD POINTERS                                 
**NO**   TM    TIFUNIT,X'C0'          IF UNIT SPECIFIED                         
**NO**   BO    SETRX                                                            
         TM    TIQFLAGS,TIQFSKIP      OR IF SKIP SPECIFIED                      
         BO    SETRX                                                            
         MVI   MAINREAD,TLCKECDQ   EMPLOYEE'S CHECKS                            
         OC    TIFSSN,TIFSSN          IF SS# SPECIFIED                          
         BNZ   SETRX                                                            
         MVI   MAINREAD,TLW4CDQ    READ W4 RECORDS                              
         MVI   SUBREAD,TLCKECDQ       SUBREAD EMPLOYEE'S CHECKS                 
         OC    TIFW4W4,TIFW4W4     IF W4 TYPE FROM W4 REC SPECIFIED             
         BNZ   SETRX                                                            
         SPACE 1                                                                
         MVI   MAINREAD,TLINKCDQ   DEFAULT TO READ CHECK DATE INVOICES          
         MVI   SUBREAD,TLCKCDQ        SUBREAD MAIN CHECKS                       
         CLI   TIQDTYPE,TIQDBILL   IF FILTERING ON BILL DATE                    
         BNE   *+8                                                              
         MVI   MAINREAD,TLINDCDQ   READ BILL DATE INVOICES                      
         CLI   TIQDTYPE,TIQDDUE    IF FILTERING ON DUE DATE                     
         BNE   *+8                                                              
         MVI   MAINREAD,TLINCCDQ   READ CHECK OPEN ITEMS                        
         CLI   TIQDTYPE,TIQDRNR    IF FILTERING ON CHECK RETURNED DATE          
         BE    SETR68                                                           
         CLI   TIQDTYPE,TIQDRND    OR ON CHECK RETURNED DISPOSITION DTE         
         BE    SETR68                                                           
         CLI   TIQDTYPE,TIQDPAY    OR ON PAY DATE                               
         BNE   *+8                                                              
SETR68   MVI   MAINREAD,TLINCDQ    READ ACTIVE INVOICES                         
         B     SETRX                                                            
         SPACE 1                                                                
SETR70   CLI   TIREAD,TLINHCDQ      INVOICE PASSIVE RECORDS                     
         BNE   SETR80                                                           
         OC    TISVCOM,TISVCOM                                                  
         BNZ   SETRX                                                            
         MVC   TISVCOM,TIFCOM     SAVE ORIGINAL INTERNAL COMML NUMBER           
         MVC   TISVAGY,TIFAGY     & AGENCY                                      
         SPACE 1                                                                
SETR80   CLI   TIREAD,TLUHCDQ       USAGE HISTORY RECORDS                       
         BNE   SETR90                                                           
         OC    TIFCOM,TIFCOM        IF COMMERCIAL FILTER NOT SPECIFIED          
         BNZ   SETRX                                                            
         MVI   MAINREAD,TLCOCDQ         READ COMMERCIALS                        
         MVI   SUBREAD,TLUHCDQ          SUBREAD USAGE                           
         B     SETRX                                                            
         SPACE 1                                                                
SETR90   DS    0H                                                               
         SPACE 1                                                                
SETRX    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*              I/O ROUTINES - STAFF                                             
         SPACE 3                                                                
STAFF    NTR1                                                                   
         USING TLSTD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLSTUSER,TIFID      OPTIONAL USER ID                             
         CLI   TLSTUSER,0                                                       
         BE    *+10                                                             
         MVC   LKEY,LSTUSER                                                     
         MVC   TLSTSTAF,TIQSTART   OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
STNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         OC    TLSTUSER,TLSTUSER                                                
         BZ    STNEXT                                                           
         CLI   TLSTSSEQ,0                                                       
         BNE   STNEXT                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
***NO-OP EDIT  (2,TLSTUSER),(4,TIID),FILL=0                                     
         MVC   TIID,TLSTUSER                                                    
         MVC   TISTAFF,TLSTSTAF                                                 
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   STNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   STNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     STNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - STAFF NAME                                        
         SPACE 3                                                                
NSTAFF   NTR1                                                                   
         USING TLSTPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLSTNLST,TIQSTART   OPTIONAL START KEY                           
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
NSTNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NSTNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLSTD,R4                                                         
         OC    TLSTUSER,TLSTUSER                                                
         BZ    NSTNEXT                                                          
***NO-OP EDIT  (2,TLSTUSER),(4,TIID),FILL=0                                     
         MVC   TIID,TLSTUSER                                                    
         MVC   TISTAFF,TLSTSTAF                                                 
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NSTNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     NSTNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - AGENCY                                            
         SPACE 3                                                                
AGENCY   NTR1                                                                   
         USING TLAYD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLAYAGY,TIQSTART    OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
AYNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLAYAGY                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   AYNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   AYNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     AYNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - AGENCY NAME                                       
         SPACE 3                                                                
NAGENCY  NTR1                                                                   
         USING TLAYPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLAYNAME,TIQSTART   OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
NAYNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NAYNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLAYD,R4                                                         
         MVC   TIAGY,TLAYAGY                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NAYNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     NAYNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - AGENCIES IN GROUP                                 
         SPACE 3                                                                
GAGENCY  NTR1                                                                   
         USING TLAYPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLAYGAGG,TIFAGG     POSSIBLE AGENCY GROUP                        
         TM    TLAYGAGG,X'C0'                                                   
         BNO   *+10                                                             
         MVC   LKEY,LAYGAGG                                                     
         MVC   TLAYGAGY,TIQSTART   OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
GAYNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGG,TLAYGAGG                                                   
         MVC   TIAGY,TLAYGAGY                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   GAYNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   GAYNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     GAYNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - AGENCY GROUP                                      
         SPACE 3                                                                
AGROUP   NTR1                                                                   
         USING TLAGD,R4                                                         
         MVC   TLAGAGG,TIQSTART    OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
AGNEXT   BAS   RE,SEQ                                                           
         CLC   KEY(1),KEYSAVE                                                   
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGG,TLAGAGG                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   AGNEXT                                                           
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLAGD,R4                                                         
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   AGNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     AGNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - AGENCY GROUP NAME                                 
         SPACE 3                                                                
NAGROUP  NTR1                                                                   
         USING TLAGPD,R4                                                        
         MVC   TLAGNAME,TIQSTART   OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
NAGNEXT  BAS   RE,SEQ                                                           
         CLC   KEY(1),KEYSAVE                                                   
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NAGNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLAGD,R4                                                         
         MVC   TIAGG,TLAGAGG                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NAGNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     NAGNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - ATTENTION                                         
         SPACE 3                                                                
ATTN     NTR1                                                                   
         USING TLATD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLATAGY,TIFAGY      OPTIONAL AGENCY                              
         CLI   TLATAGY,0                                                        
         BE    *+10                                                             
         MVC   LKEY,LATAGY                                                      
         MVC   TLATATT,TIQSTART    OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
ATNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLATAGY                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   ATNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   ATNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     ATNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - CLIENT                                            
         SPACE 3                                                                
CLIENT   NTR1                                                                   
         USING TLCLD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLCLAGY,TIFAGY      OPTIONAL AGENCY                              
         TM    TLCLAGY,X'C0'                                                    
         BNO   *+10                                                             
         MVC   LKEY,LCLAGY                                                      
         MVC   TLCLCLI,TIQSTART    OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
CLNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLCLAGY                                                    
         MVC   TICLI,TLCLCLI                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   CLNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   CLNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     CLNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - CLIENT NAME                                       
         SPACE 3                                                                
NCLIENT  NTR1                                                                   
         USING TLCLPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLCLNAGY,TIFAGY      OPTIONAL AGENCY                             
         TM    TLCLNAGY,X'C0'                                                   
         BNO   *+10                                                             
         MVC   LKEY,LCLNAGY                                                     
         MVC   TLCLNAME,TIQSTART   OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
NCLNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLCLNAGY                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NCLNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLCLD,R4                                                         
         MVC   TICLI,TLCLCLI                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NCLNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     NCLNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - CLIENTS IN GROUP                                  
         SPACE 3                                                                
GCLIENT  NTR1                                                                   
         USING TLCLPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLCLGCLG,TIFCLG     POSSIBLE CLIENT GROUP                        
         MVC   TLCLGCLI,TIQSTART   OPTIONAL START                               
         TM    TLCLGCLG,X'C0'                                                   
         BNO   *+10                                                             
         MVC   LKEY,LCLGCLG                                                     
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
GCLNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TICLG,TLCLGCLG                                                   
         MVC   TIAGY,TLCLGAGY                                                   
         MVC   TICLI,TLCLGCLI                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   GCLNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   GCLNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     GCLNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - PAYMENT TYPE                                      
         SPACE 3                                                                
PMTYPE   NTR1                                                                   
         USING TLPMD,R4                                                         
         MVI   LKEY,1                                                           
         MVC   TLPMPTYP,TIQSTART    OPTIONAL START                              
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
                                                                                
PMNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIPTYP,TLPMPTYP                                                  
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   PMNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   PMNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     PMNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - PAYMENT TYPE NAME                                 
         SPACE 3                                                                
NPMTYPE  NTR1                                                                   
         USING TLPMPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLPMNAME,TIQSTART   OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
                                                                                
NPMNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NPMNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLAYD,R4                                                         
         MVC   TIAGY,TLAYAGY                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NPMNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     NPMNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - TASK                                              
         SPACE 3                                                                
TASK     NTR1                                                                   
         USING TLTKD,R4                                                         
         MVI   LKEY,1                                                           
         MVC   TLTKTASK,TIQSTART    OPTIONAL START                              
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
                                                                                
TKNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TITASK,TLTKTASK                                                  
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   TKNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   TKNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     TKNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - TASK NAME                                         
         SPACE 3                                                                
NTASK    NTR1                                                                   
         USING TLTKPCD,R4                                                       
         MVI   LKEY,0                                                           
         MVC   TLTKNAME,TIQSTART   OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
                                                                                
NTKNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NTKNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLAYD,R4                                                         
         MVC   TIAGY,TLAYAGY                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NTKNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     NTKNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - TPROFILE                                          
         SPACE 3                                                                
TPROFILE NTR1                                                                   
         USING TLTRD,R4                                                         
         MVI   LKEY,1                                                           
         MVC   TLTRAGY,TIFAGY      POSSIBLE AGENCY                              
         TM    TLTRAGY,X'C0'                                                    
         BNO   TPRFHIGH                                                         
         MVC   LKEY,LTRAGY                                                      
         MVC   TLTRCLI,TIFCLI      POSSIBLE CLIENT                              
         TM    TLTRCLI,X'C0'                                                    
         BNO   TPRFHIGH                                                         
         MVC   LKEY,LTRCLI                                                      
                                                                                
TPRFHIGH BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
                                                                                
TPRFNEXT BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLTRAGY                                                    
         MVC   TICLI,TLTRCLI                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   TPRFNEXT                                                         
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   TPRFNEXT                                                         
         BAS   RE,GOHOOK                                                        
         B     TPRFNEXT                                                         
         EJECT                                                                  
*              I/O ROUTINES - CLIENT GROUP                                      
         SPACE 3                                                                
CGROUP   NTR1                                                                   
         USING TLCGD,R4                                                         
         MVC   TLCGCLG,TIQSTART    OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
CGNEXT   BAS   RE,SEQ                                                           
         CLC   KEY(1),KEYSAVE                                                   
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TICLG,TLCGCLG                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   CGNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   CGNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     CGNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - CLIENT GROUP NAME                                 
         SPACE 3                                                                
NCGROUP  NTR1                                                                   
         USING TLCGPD,R4                                                        
         MVC   TLCGNAME,TIQSTART   OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
NCGNEXT  BAS   RE,SEQ                                                           
         CLC   KEY(1),KEYSAVE                                                   
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NCGNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLCGD,R4                                                         
         MVC   TICLG,TLCGCLG                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NCGNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     NCGNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - PRODUCT                                           
         SPACE 3                                                                
PRODUCT  NTR1                                                                   
         USING TLPRD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLPRAGY,TIFAGY      POSSIBLE AGENCY                              
         TM    TLPRAGY,X'C0'                                                    
         BNO   PRHIGH                                                           
         MVC   LKEY,LPRAGY                                                      
         MVC   TLPRCLI,TIFCLI      POSSIBLE CLIENT                              
         TM    TLPRCLI,X'C0'                                                    
         BNO   PRHIGH                                                           
         MVC   LKEY,LPRCLI                                                      
         SPACE 1                                                                
PRHIGH   MVC   TLPRPRD,TIQSTART    OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
PRNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLPRAGY                                                    
         MVC   TICLI,TLPRCLI                                                    
         MVC   TIPRD,TLPRPRD                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   PRNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   PRNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     PRNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - PRODUCT NAME                                      
         SPACE 3                                                                
NPRODUCT NTR1                                                                   
         USING TLPRPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLPRNAGY,TIFAGY     POSSIBLE AGENCY                              
         TM    TLPRNAGY,X'C0'                                                   
         BNO   NPRHIGH                                                          
         MVC   LKEY,LPRNAGY                                                     
         MVC   TLPRNCLI,TIFCLI     POSSIBLE CLIENT                              
         TM    TLPRNCLI,X'C0'                                                   
         BNO   NPRHIGH                                                          
         MVC   LKEY,LPRNCLI                                                     
         SPACE 1                                                                
NPRHIGH  MVC   TLPRNAME,TIQSTART   OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
NPRNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLPRNAGY                                                   
         MVC   TICLI,TLPRNCLI                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NPRNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLPRD,R4                                                         
         MVC   TIPRD,TLPRPRD                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NPRNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     NPRNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - PRODUCTS IN GROUP                                 
         SPACE 3                                                                
GPRODUCT NTR1                                                                   
         USING TLPRPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLPRGAGY,TIFAGY     POSSIBLE AGENCY                              
         TM    TLPRGAGY,X'C0'                                                   
         BNO   GPRHIGH                                                          
         MVC   LKEY,LPRGAGY                                                     
         MVC   TLPRGCLI,TIFCLI     POSSIBLE CLIENT                              
         TM    TLPRGCLI,X'C0'                                                   
         BNO   GPRHIGH                                                          
         MVC   LKEY,LPRGCLI                                                     
         MVC   TLPRGPRG,TIFPRG     POSSIBLE PRODUCT GROUP                       
         TM    TLPRGPRG,X'C0'                                                   
         BNO   GPRHIGH                                                          
         MVC   LKEY,LPRGPRG                                                     
         MVC   TLPRGPRD,TIQSTART   OPTIONAL START                               
         SPACE 1                                                                
GPRHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
GPRNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLPRGAGY                                                   
         MVC   TICLI,TLPRGCLI                                                   
         MVC   TIPRG,TLPRGPRG                                                   
         MVC   TIPRD,TLPRGPRD                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   GPRNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   GPRNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     GPRNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - PRODUCT GROUP                                     
         SPACE 3                                                                
PGROUP   NTR1                                                                   
         USING TLPGD,R4                                                         
         MVC   TLPGAGY,TIFAGY      POSSIBLE AGENCY                              
         TM    TLPGAGY,X'C0'                                                    
         BNO   PGHIGH                                                           
         MVC   LKEY,LPGAGY                                                      
         MVC   TLPGCLI,TIFCLI      POSSIBLE CLIENT                              
         TM    TLPGCLI,X'C0'                                                    
         BNO   PGHIGH                                                           
         MVC   LKEY,LPGCLI                                                      
         MVC   TLPGPRG,TIQSTART    OPTIONAL START                               
         SPACE 1                                                                
PGHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
PGNEXT   BAS   RE,SEQ                                                           
         CLC   KEY(1),KEYSAVE                                                   
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLPGAGY                                                    
         MVC   TICLI,TLPGCLI                                                    
         MVC   TIPRG,TLPGPRG                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   PGNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   PGNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     PGNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - PRODUCT GROUP NAME                                
         SPACE 3                                                                
NPGROUP  NTR1                                                                   
         USING TLPGPD,R4                                                        
         MVC   TLPGNAGY,TIFAGY    POSSIBLE AGENCY                               
         TM    TLPGNAGY,X'C0'                                                   
         BNO   NPGHIGH                                                          
         MVC   LKEY,LPGNAGY                                                     
         MVC   TLPGNAME,TIQSTART   OPTIONAL START                               
         SPACE 1                                                                
NPGHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
NPGNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLPGNAGY                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NPGNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLPGD,R4                                                         
         MVC   TICLI,TLPGCLI                                                    
         MVC   TIPRG,TLPGPRG                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NPGNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     NPGNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - PRODUCT TYPE                                      
         SPACE 3                                                                
PTYPE    NTR1                                                                   
         USING TLPTD,R4                                                         
         MVC   TLPTAGY,TIFAGY      POSSIBLE AGENCY                              
         TM    TLPTAGY,X'C0'                                                    
         BNO   PTHIGH                                                           
         MVC   LKEY,LPTAGY                                                      
         MVC   TLPTPRT,TIQSTART    OPTIONAL START                               
         SPACE 1                                                                
PTHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
PTNEXT   BAS   RE,SEQ                                                           
         CLC   KEY(1),KEYSAVE                                                   
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLPTAGY                                                    
         MVC   TIPRT,TLPTPRT                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   PTNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   PTNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     PTNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - PRODUCT TYPE NAME                                 
         SPACE 3                                                                
NPTYPE   NTR1                                                                   
         USING TLPTPD,R4                                                        
         MVC   TLPTNAGY,TIFAGY    POSSIBLE AGENCY                               
         TM    TLPTNAGY,X'C0'                                                   
         BNO   NPTHIGH                                                          
         MVC   LKEY,LPTNAGY                                                     
         MVC   TLPTNAME,TIQSTART   OPTIONAL START                               
         SPACE 1                                                                
NPTHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
NPTNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLPTNAGY                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NPTNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLPTD,R4                                                         
         MVC   TIPRT,TLPTPRT                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NPTNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     NPTNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - COMMERCIAL                                        
         SPACE 3                                                                
COMM     NTR1                                                                   
         USING TLCOD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLCOAGY,TIFAGY      POSSIBLE AGENCY                              
         TM    TLCOAGY,X'C0'                                                    
         BNO   COHIGH                                                           
         MVC   LKEY,LCOAGY                                                      
         MVC   TLCOCLI,TIFCLI      POSSIBLE CLIENT                              
         TM    TLCOCLI,X'C0'                                                    
         BNO   COHIGH                                                           
         MVC   LKEY,LCOCLI                                                      
         MVC   TLCOCID,TIQSTART    OPTIONAL START                               
         MVC   TLCOPRD,TIFPRD      POSSIBLE PRODUCT                             
         TM    TLCOPRD,X'C0'                                                    
         BNO   COHIGH                                                           
         MVC   LKEY,LCOPRD                                                      
         SPACE 1                                                                
COHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
CONEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         SPACE 1                                                                
         CLI   TLCOVER,0           EXIT WHEN START HITTING COMMERCIALS          
         BNE   XIT                 WITH SEQUENCE NUMBERS                        
         SPACE 1                                                                
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLCOAGY                                                    
         MVC   TICLI,TLCOCLI                                                    
         MVC   TIPRD,TLCOPRD                                                    
         MVC   TICOM,TLCOCOM                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   CONEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,FILTCOMM         FILTER OUT COPIES                            
         BNE   CONEXT                                                           
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   CONEXT                                                           
         BAS   RE,SUBCOMM          (CHECK FOR SUBREAD)                          
         B     CONEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - COMMERCIAL VERSIONS                               
         SPACE 3                                                                
COMMV    NTR1                                                                   
         USING TLCOPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLCOVAGY,TIFAGY     POSSIBLE AGENCY                              
         TM    TLCOVAGY,X'C0'                                                   
         BNO   COVHIGH                                                          
         MVC   LKEY,LCOVAGY                                                     
         MVC   TLCOVCLI,TIFCLI     POSSIBLE CLIENT                              
         TM    TLCOVCLI,X'C0'                                                   
         BNO   COVHIGH                                                          
         MVC   LKEY,LCOVCLI                                                     
         MVC   TLCOVCID,TIQSTART   OPTIONAL START                               
         MVC   TLCOVPRD,TIFPRD     POSSIBLE PRODUCT                             
         TM    TLCOVPRD,X'C0'                                                   
         BNO   COVHIGH                                                          
         MVC   LKEY,LCOVPRD                                                     
         SPACE 1                                                                
COVHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
COVNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLCOVAGY                                                   
         MVC   TICLI,TLCOVCLI                                                   
         MVC   TIPRD,TLCOVPRD                                                   
         MVC   TICOM,TLCOVCOM                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   COVNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,FILTCOMM         FILTER OUT COPIES                            
         BNE   COVNEXT                                                          
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   COVNEXT                                                          
         BAS   RE,SUBCOMM          (CHECK FOR SUBREAD)                          
         B     COVNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - COMMERCIALS IN GROUP                              
         SPACE 3                                                                
COMCGC   NTR1                                                                   
         USING TLCOPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLCOGICG,TIFPRG     POSSIBLE COMMERCIAL GROUP                    
         TM    TLCOGICG,X'C0'                                                   
         BNO   CGCHIGH                                                          
         MVC   LKEY,LCGCCG                                                      
         MVC   TLCOGIID,TIQSTART   OPTIONAL START                               
         SPACE 1                                                                
CGCHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
CGCNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIPRG,TLCOGICG                                                   
         MVC   TICID,TLCOGIID                                                   
         MVC   TIAGY,TLCOGIAY                                                   
         MVC   TICOM,TLCOGIIN                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   CGCNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   CGCNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     CGCNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - COMMERCIAL GROUP COMMERCIAL NAME                  
         SPACE 3                                                                
COMCGN   NTR1                                                                   
         USING TLCOPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLCOGNCG,TIFPRG     POSSIBLE COMMERCIAL GROUP                    
         TM    TLCOGNCG,X'C0'                                                   
         BNO   CGNHIGH                                                          
         MVC   LKEY,LCGNCG                                                      
         MVC   TLCOGNNM,TIQSTART   OPTIONAL START                               
         SPACE 1                                                                
CGNHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
CGNNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIPRG,TLCOGNCG                                                   
         MVC   TICID,TLCOGNNM                                                   
         MVC   TICOM,TLCOLNIN                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   CGNNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,FILTCOMM         FILTER ON COPY                               
         BNE   CGNNEXT                                                          
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLCOD,R4                                                         
         MVC   TIAGY,TLCOAGY                                                    
         MVC   TICLI,TLCOCLI                                                    
         MVC   TIPRD,TLCOPRD                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   CGNNEXT                                                          
         BAS   RE,SUBCOMM          (CHECK FOR SUBREAD)                          
         B     CGNNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - COMMERCIAL NAME                                   
         SPACE 3                                                                
NCOMM    NTR1                                                                   
         USING TLCOPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLCONCLI,TIFCLI     POSSIBLE CLIENT                              
         TM    TLCONCLI,X'C0'                                                   
         BNO   NCOHIGH                                                          
         MVC   LKEY,LCONCLI                                                     
         MVC   TLCONAME,TIQSTART   OPTIONAL START                               
         SPACE 1                                                                
NCOHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
NCONEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TICLI,TLCONCLI                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NCONEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,FILTCOMM         FILTER ON COPY                               
         BNE   NCONEXT                                                          
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLCOD,R4                                                         
         MVC   TIAGY,TLCOAGY                                                    
         MVC   TIPRD,TLCOPRD                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NCONEXT                                                          
         BAS   RE,SUBCOMM          (CHECK FOR SUBREAD)                          
         B     NCONEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - COMMERCIAL ISCII                                  
         SPACE 3                                                                
ICOMM    NTR1                                                                   
         USING TLCOPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLCOIAGY,TIFAGY     POSSIBLE AGENCY                              
         TM    TLCOIAGY,X'C0'                                                   
         BNO   ICOHIGH                                                          
         MVC   LKEY,LCOIAGY                                                     
         MVC   TLCOICID,TIQSTART   OPTIONAL START                               
         TM    TIFCID,X'C0'                                                     
         BNO   ICOHIGH                                                          
         MVC   TLCOICID,TIFCID     POSSIBLE CID                                 
         MVC   LKEY,LCOICID                                                     
         SPACE 1                                                                
ICOHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
ICONEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLCOIAGY                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   ICONEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,FILTCOMM         FILTER ON COPY                               
         BNE   ICONEXT                                                          
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLCOD,R4                                                         
         CLI   TLCOVER,TLCOV026    NEVER RETURN "VERSION-ONLY" RECORDS          
         BNE   ICONEXT                                                          
         MVC   TICLI,TLCOCLI                                                    
         MVC   TIPRD,TLCOPRD                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   ICONEXT                                                          
         BAS   RE,SUBCOMM          (CHECK FOR SUBREAD)                          
         B     ICONEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - PRINT COMMERCIAL                                  
         SPACE 3                                                                
PCOMM    NTR1                                                                   
         USING TLCOPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLCOPAGY,TIFAGY     POSSIBLE AGENCY                              
         TM    TLCOPAGY,X'C0'                                                   
         BNO   PCOHIGH                                                          
         MVC   LKEY,LCOPAGY                                                     
         MVC   TLCOPCLI,TIFCLI     POSSIBLE CLIENT                              
         TM    TLCOPCLI,X'C0'                                                   
         BNO   PCOHIGH                                                          
         MVC   LKEY,LCOPCLI                                                     
         MVC   TLCOPCID,TIQSTART   OPTIONAL START                               
         MVC   TLCOPPRD,TIFPRD     POSSIBLE PRODUCT                             
         TM    TLCOPPRD,X'C0'                                                   
         BNO   PCOHIGH                                                          
         MVC   LKEY,LCOPPRD                                                     
         SPACE 1                                                                
PCOHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
PCONEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLCOPAGY                                                   
         MVC   TICLI,TLCOPCLI                                                   
         MVC   TIPRD,TLCOPPRD                                                   
         MVC   TICOM,TLCOPCOM                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   PCONEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,FILTCOMM         FILTER OUT COPIES                            
         BNE   PCONEXT                                                          
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   PCONEXT                                                          
         BAS   RE,SUBCOMM          (CHECK FOR SUBREAD)                          
         B     PCONEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - COMMERCIAL MUSIC                                  
         SPACE 3                                                                
MCOMM    NTR1                                                                   
         USING TLCOPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLCOMAGY,TIFAGY     POSSIBLE AGENCY                              
         TM    TLCOMAGY,X'C0'                                                   
         BNO   MCOHIGH                                                          
         MVC   LKEY,LCOMAGY                                                     
         MVC   TLCOMMUS,TIFMUSIC   POSSIBLE MUSIC                               
         TM    TLCOMMUS,X'C0'                                                   
         BNO   MCOHIGH                                                          
         MVC   LKEY,LCOMMUS                                                     
         MVC   TLCOMCID,TIQSTART   OPTIONAL START                               
         SPACE 1                                                                
MCOHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
MCONEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         CLI   TLCOMVER,0                                                       
         BNE   MCONEXT                                                          
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLCOMAGY                                                   
         MVC   TIMUSIC,TLCOMMUS                                                 
         MVC   TICID,TLCOMCID                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   MCONEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,FILTCOMM         FILTER ON COPY                               
         BNE   MCONEXT                                                          
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLCOD,R4                                                         
         MVC   TICLI,TLCOCLI                                                    
         MVC   TIPRD,TLCOPRD                                                    
         MVC   TICOM,TLCOCOM                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   MCONEXT                                                          
         BAS   RE,SUBCOMM          (CHECK FOR SUBREAD)                          
         B     MCONEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - COMMERCIAL MUSIC (NO AGENCY PASSIVE)              
         SPACE 3                                                                
PMCOMM   NTR1                                                                   
         USING TLCOPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLCOOMUS,TIFMUSIC   POSSIBLE MUSIC                               
         TM    TLCOMMUS,X'C0'                                                   
         BNO   PMCOHIGH                                                         
         MVC   LKEY,LCOMMUS                                                     
         MVC   TLCOOMUS,TIQSTART   OPTIONAL START                               
         SPACE 1                                                                
PMCOHIGH BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
PMCONEXT BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         CLI   TLCOMVER,0                                                       
         BNE   PMCONEXT                                                         
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIMUSIC,TLCOOMUS                                                 
         MVC   TICOM,TLCOOCOM                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   PMCONEXT                                                         
         BAS   RE,GETREC                                                        
         BAS   RE,FILTCOMM         FILTER ON COPY                               
         BNE   PMCONEXT                                                         
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLCOD,R4                                                         
         MVC   TICLI,TLCOCLI                                                    
         MVC   TIPRD,TLCOPRD                                                    
         MVC   TICOM,TLCOCOM                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   PMCONEXT                                                         
         BAS   RE,SUBCOMM          (CHECK FOR SUBREAD)                          
         B     PMCONEXT                                                         
         EJECT                                                                  
*              I/O ROUTINES - COMMERCIAL CLIENT GROUP                           
         SPACE 3                                                                
GCOMM    NTR1                                                                   
         USING TLCOPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLCOGCLG,TIFCLG     POSSIBLE CLIENT GROUP                        
         TM    TLCOGCLG,X'C0'                                                   
         BNO   GCOHIGH                                                          
         MVC   LKEY,LCOGCLG                                                     
         MVC   TLCOGCID,TIQSTART   OPTIONAL START                               
         SPACE 1                                                                
GCOHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
GCONEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TICLG,TLCOGCLG                                                   
         MVC   TICID,TLCOGCID                                                   
         MVC   TIAGY,TLCOGAGY                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   GCONEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,FILTCOMM         FILTER ON COPY                               
         BNE   GCONEXT                                                          
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLCOD,R4                                                         
         MVC   TICLI,TLCOCLI                                                    
         MVC   TIPRD,TLCOPRD                                                    
         MVC   TICOM,TLCOCOM                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   GCONEXT                                                          
         BAS   RE,SUBCOMM          (CHECK FOR SUBREAD)                          
         B     GCONEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - CLIENT GROUP COMMERCIAL NAME                      
         SPACE 3                                                                
LCOMM    NTR1                                                                   
         USING TLCOPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLCOLCLG,TIFCLG     POSSIBLE CLIENT GROUP                        
         TM    TLCOLCLG,X'C0'                                                   
         BNO   LCOHIGH                                                          
         MVC   LKEY,LCOLCLG                                                     
         MVC   TLCOLNAM,TIQSTART   OPTIONAL START                               
         SPACE 1                                                                
LCOHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
LCONEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TICLG,TLCOLCLG                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   LCONEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,FILTCOMM         FILTER ON COPY                               
         BNE   LCONEXT                                                          
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLCOD,R4                                                         
         MVC   TIAGY,TLCOAGY                                                    
         MVC   TICLI,TLCOCLI                                                    
         MVC   TIPRD,TLCOPRD                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   LCONEXT                                                          
         BAS   RE,SUBCOMM          (CHECK FOR SUBREAD)                          
         B     LCONEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - COMMERCIAL AFM CONTRACT                           
         SPACE 3                                                                
ACOMM    NTR1                                                                   
         USING TLCOPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLCOAAGY,TIFAGY     POSSIBLE AGENCY                              
         TM    TLCOAAGY,X'C0'                                                   
         BNO   ACOHIGH                                                          
         MVC   LKEY,LCOAAGY                                                     
         MVC   TLCOACON,TIQSTART   OPTIONAL START AFM CONTRACT                  
         SPACE 1                                                                
ACOHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
ACONEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLCOAAGY                                                   
         MVC   TICID,TLCOACID                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   ACONEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,FILTCOMM         FILTER ON COPY                               
         BNE   ACONEXT                                                          
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLCOD,R4                                                         
         MVC   TICLI,TLCOCLI                                                    
         MVC   TIPRD,TLCOPRD                                                    
         MVC   TICOM,TLCOCOM                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   ACONEXT                                                          
         BAS   RE,SUBCOMM          (CHECK FOR SUBREAD)                          
         B     ACONEXT                                                          
         EJECT                                                                  
*              CONTROL RECORDS RELATED TO COMMERCIAL                            
         SPACE 3                                                                
SUBCOMM  NTR1                                                                   
         CLI   SUBREAD,0                                                        
         BNE   SUBCOM05                                                         
         BAS   RE,GOHOOK                                                        
         B     XIT                                                              
*                                                                               
SUBCOM05 TM    TISTAT,TISTSPSC     IF SPECIAL HANDLING FOR COMM'L               
         BZ    SUBCOM30            SUBREAD SET                                  
         CLI   MAINREAD,TLCOPCDQ   DEAL, DOESN'T THE FIRST TIME                 
         BNE   SUBCOM10                                                         
         CLI   SUBREAD,TLCACDQ                                                  
         BNE   SUBCOM30                                                         
         B     SUBCOM40                                                         
                                                                                
SUBCOM10 CLI   MAINREAD,TLCOCDQ    CKFRCCOM, DOESN'T THE FIRST TIME             
         BE    SUBCOM20                                                         
         CLI   MAINREAD,TLCOGCDQ     OR COMML BY CLI GROUP                      
         BNE   SUBCOM30                                                         
SUBCOM20 CLI   SUBREAD,TLCKHCDQ                                                 
         BE    SUBCOM40                                                         
         CLI   SUBREAD,TLINHCDQ    INFRCCOM TOO                                 
         BE    SUBCOM40                                                         
*                                                                               
SUBCOM30 BAS   RE,GOHKCOMM         HOOK WITH PROCCOMM                           
*                                                                               
SUBCOM40 CLI   TIMODE,PROCNOCA     USER CAN ASK NOT TO PROCESS CASTS            
         BE    XIT                                                              
         CLI   TIMODE,PROCNOIN     (OR INVOICES)                                
         BE    XIT                                                              
         CLI   TIMODE,PROCNOUH     (OR USAGE HISTORY)                           
         BE    XIT                                                              
         LA    R4,IO                                                            
         USING TLCOD,R4                                                         
         MVC   INTCOMM,TLCOCOM                                                  
         LR    RE,R4                                                            
         AHI   RE,L'IO                                                          
         ST    RE,TIAMAIN                                                       
         LHI   RF,L'IO                                                          
         LR    R0,R4                                                            
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         MVC   PRIMEKEY,KEY                                                     
         MVC   PRIMESAV,KEYSAVE                                                 
         MVC   PRIMELKY,LKEY                                                    
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         CLI   SUBREAD,TLCACDQ                                                  
         BE    SUBCOMCA                                                         
         CLI   SUBREAD,TLECCDQ                                                  
         BE    SUBCOMCA                                                         
         CLI   SUBREAD,TLUHCDQ                                                  
         BE    SUBCOMUH                                                         
         CLI   SUBREAD,TLINHCDQ                                                 
         BE    SUBCOMIN                                                         
         CLI   SUBREAD,TLCKHCDQ    CAST PAYMENT HISTORY                         
         BE    SUBCOMCK                                                         
         B     SUBCOMMX                                                         
         SPACE 1                                                                
SUBCOMCA MVC   SAVEFCOM,TIFCOM                                                  
         MVC   TIFCOM,INTCOMM      FORCE COMM FILTER                            
         MVC   SAVEFAGY,TIFAGY     SAVE POSSIBLE AGENCY FILTER                  
         MVC   SAVEFCLI,TIFCLI     SAVE POSSIBLE CLIENT FILTER                  
         MVC   SAVEFLG2,TIQFLAG2   SAVE POSSIBLE EXACT MATCH FILTER             
         XC    TIFAGY,TIFAGY       AND CLEAR (SO DON'T DO NEEDDATA)             
         XC    TIFCLI,TIFCLI                                                    
         NI    TIQFLAG2,X'FF'-TIQFEXCT                                          
         CLI   SUBREAD,TLECCDQ     IF SUBREADING ECAST                          
         BNE   *+12                                                             
         BAS   RE,ECAST            GET ECAST RECORDS FOR COMMERCIAL             
         B     *+8                                                              
         BAS   RE,CAST             ELSE, GET CAST FOR COMMERCIAL                
         MVC   TIFCOM,SAVEFCOM     RESTORE POSSIBLE COMMERCIAL FILTER           
         MVC   TIFAGY,SAVEFAGY         AND POSSIBLE AGENCY FILTER               
         MVC   TIFCLI,SAVEFCLI         AND POSSIBLE CLIENT FILTER               
         BAS   RE,RESTFLG2             AND POSSIBLE EXACT MATCH FILTER          
         TM    TIQFLAG2,TIQFHINV   IF REQUESTED INVOICE HISTORY TOO             
         BZ    SUBCOMC1            MUST SET TIMODE,PROCNOCK                     
         XC    KEY,KEY             CLEAR KEY                                    
         SPACE 1                                                                
SUBCOMIN MVI   KEY,TLINHCDQ        INVOICE HISTORY FOR COMMERCIAL               
         MVC   SAVEFCOM,TIFCOM                                                  
         MVC   TIFCOM,INTCOMM      FORCE COMM FILTER                            
         MVC   SAVEFAGY,TIFAGY     SAVE POSSIBLE AGENCY FILTER                  
         MVC   SAVEFCLI,TIFCLI     SAVE POSSIBLE CLIENT FILTER                  
         MVC   SAVEFLG2,TIQFLAG2   SAVE POSSIBLE EXACT MATCH FILTER             
         XC    TIFAGY,TIFAGY       AND CLEAR (TO GET TRANSFERS)                 
         XC    TIFCLI,TIFCLI                                                    
         NI    TIQFLAG2,X'FF'-TIQFEXCT                                          
         XC    TIQSTART,TIQSTART   CLEAR START (NO NEED TO RESET)               
         BAS   RE,HINVOICE                                                      
         MVC   TIFCOM,SAVEFCOM     RESTORE POSSIBLE COMMERCIAL FILTER           
         MVC   TIFAGY,SAVEFAGY         AND POSSIBLE AGENCY FILTER               
         MVC   TIFCLI,SAVEFCLI         AND POSSIBLE CLIENT FILTER               
         BAS   RE,RESTFLG2             AND POSSIBLE EXACT MATCH FILTER          
         SPACE 1                                                                
SUBCOMC1 TM    TIQFLAG2,TIQFHCK    IF REQUESTED CHECK PAYMENT HISTORY           
         BZ    SUBCOMC2                                                         
         XC    KEY,KEY             CLEAR KEY                                    
         SPACE 1                                                                
SUBCOMCK MVI   KEY,TLCKHCDQ        CHECK PAYMENT HISTORY FOR COMMERCIAL         
         MVC   SAVEFCOM,TIFCOM                                                  
         MVC   TIFCOM,INTCOMM      FORCE COMM FILTER                            
         MVC   SAVEFAGY,TIFAGY     SAVE POSSIBLE AGENCY FILTER                  
         MVC   SAVEFCLI,TIFCLI     SAVE POSSIBLE CLIENT FILTER                  
         MVC   SAVEFLG2,TIQFLAG2   SAVE POSSIBLE EXACT MATCH FILTER             
         XC    TIFAGY,TIFAGY       AND CLEAR (TO GET TRANSFERS)                 
         XC    TIFCLI,TIFCLI                                                    
         NI    TIQFLAG2,X'FF'-TIQFEXCT                                          
         BAS   RE,HCHECK                                                        
         NI    TISTAT,X'FF'-TISTRDCK                                            
         MVC   TIFCOM,SAVEFCOM     RESTORE POSSIBLE COMMERCIAL FILTER           
         MVC   TIFAGY,SAVEFAGY         AND POSSIBLE AGENCY FILTER               
         MVC   TIFCLI,SAVEFCLI         AND POSSIBLE CLIENT FILTER               
         BAS   RE,RESTFLG2             AND POSSIBLE EXACT MATCH FILTER          
         SPACE 1                                                                
SUBCOMC2 TM    TIQFLAG2,TIQFHCA    IF REQUESTED CAST                            
         BZ    SUBCOMMX                                                         
         MVC   SAVEFCOM,TIFCOM                                                  
         MVC   TIFCOM,INTCOMM      FORCE COMM FILTER                            
         MVC   SAVEFAGY,TIFAGY     SAVE POSSIBLE AGENCY FILTER                  
         MVC   SAVEFCLI,TIFCLI     SAVE POSSIBLE CLIENT FILTER                  
         MVC   SAVEFLG2,TIQFLAG2   SAVE POSSIBLE EXACT MATCH FILTER             
         XC    TIFAGY,TIFAGY       AND CLEAR (SO DON'T DO NEEDDATA)             
         XC    TIFCLI,TIFCLI                                                    
         NI    TIQFLAG2,X'FF'-TIQFEXCT                                          
         BAS   RE,CAST             CAST RECORDS FOR COMMERCIAL                  
         MVC   TIFCOM,SAVEFCOM     RESTORE POSSIBLE COMMERCIAL FILTER           
         MVC   TIFAGY,SAVEFAGY         AND POSSIBLE AGENCY FILTER               
         MVC   TIFCLI,SAVEFCLI         AND POSSIBLE CLIENT FILTER               
         BAS   RE,RESTFLG2             AND POSSIBLE EXACT MATCH FILTER          
         B     SUBCOMMX                                                         
         SPACE 1                                                                
SUBCOMUH MVI   KEY,TLUHCDQ         USAGE HISTORY FOR COMMERCIAL                 
         MVC   SAVEFCOM,TIFCOM                                                  
         MVC   TIFCOM,INTCOMM      FORCE COMM FILTER                            
         MVC   SAVEFAGY,TIFAGY     SAVE POSSIBLE AGENCY FILTER                  
         MVC   SAVEFLG2,TIQFLAG2   SAVE POSSIBLE EXACT MATCH FILTER             
         XC    TIFAGY,TIFAGY       AND CLEAR (TO GET TRANSFERS)                 
         NI    TIQFLAG2,X'FF'-TIQFEXCT                                          
         BAS   RE,USAGE                                                         
         MVC   TIFCOM,SAVEFCOM     RESTORE POSSIBLE COMMERCIAL FILTER           
         MVC   TIFAGY,SAVEFAGY         AND POSSIBLE AGENCY FILTER               
         BAS   RE,RESTFLG2             AND POSSIBLE EXACT MATCH FILTER          
         B     SUBCOMMX                                                         
         SPACE 1                                                                
SUBCOMMX MVC   KEY,PRIMEKEY                                                     
         BAS   RE,HIGH                                                          
         MVC   KEYSAVE,PRIMESAV                                                 
         MVC   LKEY,PRIMELKY                                                    
         XC    TIAMAIN,TIAMAIN                                                  
         B     XIT                                                              
         SPACE 2                                                                
RESTFLG2 DS    0H                                                               
         TM    SAVEFLG2,TIQFEXCT   RESTORE EXACT FILTER NECESSARY               
         BZ    *+8                                                              
         OI    TIQFLAG2,TIQFEXCT                                                
         BR    RE                                                               
         EJECT                                                                  
*              I/O ROUTINES - BRATE BY CODE                                     
         SPACE 3                                                                
BRATEC   NTR1                                                                   
         USING TLBRPD,R4                                                        
         MVI   LKEY,0                                                           
         SPACE 1                                                                
BRCHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
BRCNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   BRCNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   BRCNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     BRCNEXT                                                          
*              I/O ROUTINES - BRATE BY NAME                                     
         SPACE 3                                                                
BRATEN   NTR1                                                                   
         USING TLBRPD,R4                                                        
         MVI   LKEY,0                                                           
         SPACE 1                                                                
BRNHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
BRNNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         OC    TIQSTART,TIQSTART                                                
         JZ    *+14                                                             
         CLC   TLBRNNAM,TIQSTART                                                
         JL    BRNNEXT                                                          
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   BRNNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   BRNNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     BRNNEXT                                                          
*              I/O ROUTINES - MUSIC                                             
         SPACE 3                                                                
MUSIC    NTR1                                                                   
         USING TLMUD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLMUMUS,TIQSTART    OPTIONAL START                               
         SPACE 1                                                                
MUHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
MUNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         OC    TLMUAGY,TLMUAGY     NEW STYLE?                                   
         BNZ   MUNEXT                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIMUSIC,TLMUMUS                                                  
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   MUNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   MUNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     MUNEXT                                                           
*              I/O ROUTINES - MUSIC COMPOSITION NAME                            
         SPACE 3                                                                
PMUSIC   NTR1                                                                   
         USING TLMUPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLMUPCMP,TIQSTART    OPTIONAL START                              
         SPACE 1                                                                
PMUHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
PMUNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIMUSIC,TLMUPMUS                                                 
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   PMUNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   PMUNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     PMUNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - GCON                                              
         SPACE 3                                                                
GCON     NTR1                                                                   
         USING TLGCD,R4                                                         
         MVI   LKEY,0                                                           
         SPACE 1                                                                
GCHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
GCNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   GCNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   GCNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     GCNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - GCONTRK                                           
         SPACE 3                                                                
GCONTRK  NTR1                                                                   
         USING TLOTD,R4                                                         
         MVI   LKEY,0                                                           
         SPACE 1                                                                
OTHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
OTNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   OTNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   OTNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     OTNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - AGENT                                             
         SPACE 3                                                                
AGENT    NTR1                                                                   
         USING TLAND,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLANAGT,TIQSTART    OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
ANNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGT,TLANAGT                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   ANNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   ANNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     ANNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - AGENT NAME                                        
         SPACE 3                                                                
NAGENT   NTR1                                                                   
         USING TLANPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLANNAME,TIQSTART   OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
NANNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NANNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLAND,R4                                                         
         MVC   TIAGT,TLANAGT                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NANNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     NANNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - AGENT NUMBER                                      
         SPACE 3                                                                
CAGENT   NTR1                                                                   
         USING TLANPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLANCAGT,TIQSTART   OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
CANNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGT,TLANCAGT                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   CANNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   CANNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     CANNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - INTERFACE                                         
         SPACE 3                                                                
INTER    NTR1                                                                   
         USING TLIFD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLIFAGY,TIFAGY      AGENCY                                       
         TM    TLIFAGY,X'C0'                                                    
         BNO   *+10                                                             
         MVC   LKEY,LIFAGY                                                      
         MVC   TLIFCLI,TIQSTART    OPTIONAL START CLIENT                        
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
IFNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLIFAGY                                                    
         MVC   TICLI,TLIFCLI                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   IFNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   IFNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     IFNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - EMPLOYER                                          
         SPACE 3                                                                
EMP      NTR1                                                                   
         USING TLEMD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLEMEMP,TIQSTART    OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
EMNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIEMP,TLEMEMP                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   EMNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   EMNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     EMNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - USAGE HISTORY                                     
         SPACE 3                                                                
USAGE    NTR1                                                                   
         USING TLUHD,R4                                                         
         MVC   TLUHCOM,TIFCOM      REQUIRED COMMERCIAL                          
         MVC   LKEY,LUHCOM                                                      
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
UHNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TICOM,TLUHCOM                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   UHNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   UHNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     UHNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - W4                                                
         SPACE 3                                                                
W4       NTR1                                                                   
         USING TLW4D,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLW4SSN,TIQSTART    OPTIONAL START                               
         TM    TIQFLAG3,TIQFLTW4   IF FILTERING W4 READ                         
         BZ    *+16                                                             
         MVC   TLW4SSN,TIFSSN      SSN REQUIRED                                 
         MVC   LKEY,LW4SSN                                                      
         SPACE 1                                                                
         BAS   RE,FIRSTHI                                                       
         B     W4NEXTK                                                          
         SPACE 1                                                                
W4NEXT   BAS   RE,SEQ                                                           
W4NEXTK  BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TISSN,TLW4SSN                                                    
         MVC   TIW4W4,TLDRSTAT+1-TLDRD(R4)                                      
         MVI   TINHA,0                                                          
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   W4NEXT                                                           
         BAS   RE,GETREC                                                        
         CLI   SUBREAD,TLCKECDQ    (MAY REALLY BE READING CHECKS)               
         BE    W4NEXT5                                                          
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   W4NEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     W4NEXT                                                           
         SPACE 1                                                                
W4NEXT5  MVC   INTSSN,TLW4SSN                                                   
         XR    R1,R1               CLEAR ELEMENT CODE INDICATOR                 
         CLI   TIFW4TY,C'C'        IF WANT CORP CHECKS                          
         BNE   W4NEXT8                                                          
         LA    R6,IO               LOOK FOR CORP ID'S                           
         MVI   ELCODE,TATIELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
W4NEXT6  BAS   RE,NEXTEL                                                        
         BNE   W4NEXT                                                           
         USING TATID,R6                                                         
         CLI   TATITYPE,TATITYCO                                                
         BNE   W4NEXT6                                                          
         MVC   INTSSN,TATIID       SET CORP ID IN INTERNAL SSN                  
         ZIC   R1,TATIEL           SET PROC. CORP ID BY SAVING ELCODE           
*                                                                               
W4NEXT8  MVC   PRIMEKEY,KEY                                                     
         MVC   PRIMESAV,KEYSAVE                                                 
         MVC   PRIMELKY,LKEY                                                    
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,TLCKECDQ                                                     
         BAS   RE,ECHECK           CHECK RECORDS FOR EMPLOYEE                   
         SPACE 1                                                                
         MVC   KEY,PRIMEKEY                                                     
         BAS   RE,HIGH                                                          
         MVC   KEYSAVE,PRIMESAV                                                 
         MVC   LKEY,PRIMELKY                                                    
         XC    INTSSN,INTSSN                                                    
         LTR   R1,R1               IF NOT PROCESSING CORP ID'S                  
         BZ    W4NEXT              ELSE, READ NEXT W4 RECORD                    
         STC   R1,ELCODE           RESET ELEMENT CODE                           
         B     W4NEXT6             AND LOOK FOR MORE                            
         EJECT                                                                  
*              I/O ROUTINES - W4 NAME                                           
         SPACE 3                                                                
NW4      NTR1                                                                   
         USING TLW4PD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLW4NLST(24),TIQSTART  OPTIONAL START                            
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
NW4NEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVI   TINHA,0                                                          
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NW4NEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLW4D,R4                                                         
         MVC   TISSN,TLW4SSN                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NW4NEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     NW4NEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - W4 CORPORATION                                    
         SPACE 3                                                                
CW4      NTR1                                                                   
         USING TLW4PD,R4                                                        
         MVC   TLW4CCRP,TIFCORP   POSSIBLE CORP                                 
         TM    TLW4CCRP,X'C0'                                                   
         BNO   CW4HIGH                                                          
         MVC   LKEY,LW4CCRP                                                     
         MVC   TLW4CSSN,TIQSTART   OPTIONAL START                               
         SPACE 1                                                                
CW4HIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
CW4NEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TICORP,TLW4CCRP                                                  
         MVC   TISSN,TLW4CSSN                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   CW4NEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLW4D,R4                                                         
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   CW4NEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     CW4NEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - GUARANTEE RECORDS                                 
         SPACE 3                                                                
GUAR     NTR1                                                                   
         USING TLGUD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLGUSSN,TIFSSN      OPTIONAL SSN                                 
         TM    TLGUSSN,X'C0'                                                    
         BNO   GUHIGH                                                           
         MVC   LKEY,LGUSSN                                                      
         MVC   TLGUGUA,TIQSTART    OPTIONAL START CODE                          
         CLI   TLGUGUA,0                                                        
         BE    GUHIGH                                                           
         XC    TLGUGUA,=4X'FF'                                                  
**NO-OP  MVC   LKEY,LGUGUA                                                      
         SPACE 1                                                                
GUHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
GUNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TISSN,TLGUSSN                                                    
         MVC   TIGUA,TLGUGUA                                                    
         XC    TIGUA,=4X'FF'                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   GUNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   GUNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     GUNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - SOAP GUARANTEE RECORDS                            
         SPACE 3                                                                
SGUAR    NTR1                                                                   
         USING TLSGD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLSGSSN,TIFSSN      OPTIONAL SSN                                 
         TM    TLSGSSN,X'C0'                                                    
         BNO   SGUHIGH                                                          
         MVC   LKEY,LSGSSN                                                      
         MVC   TLSGGUA,TIQSTART    OPTIONAL START CODE                          
         CLI   TLSGGUA,0                                                        
         BE    SGUHIGH                                                          
         XC    TLSGGUA,=4X'FF'                                                  
         SPACE 1                                                                
SGUHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
SGUNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TISSN,TLSGSSN                                                    
         MVC   TIGUA,TLSGGUA                                                    
         XC    TIGUA,=4X'FF'                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   SGUNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   SGUNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     SGUNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - GUARANTEE TRACKING RECORDS                        
         SPACE 3                                                                
GTRACK   NTR1                                                                   
         USING TLGTD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLGTSSN,TIFSSN      OPTIONAL SSN                                 
         TM    TLGTSSN,X'C0'                                                    
         BNO   GTHIGH                                                           
         MVC   LKEY,LGTSSN                                                      
         MVC   TLGTGUA,TIFGUA      OPTIONAL GUARANTEE CODE                      
         TM    TLGTGUA,X'C0'                                                    
         BNO   GTHIGH                                                           
         MVC   LKEY,LGTGUA                                                      
         MVC   TLGTSTRT,TIQSTART   OPTIONAL START DATE                          
         CLI   TLGTSTRT,0                                                       
         BE    GTHIGH                                                           
         MVC   LKEY,LGTSTRT                                                     
         SPACE 1                                                                
GTHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
GTNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TISSN,TLGTSSN                                                    
         MVC   TIGUA,TLGTGUA                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   GTNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   GTNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     GTNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - HOLDING FEE TRACKING RECORDS                      
         SPACE 3                                                                
FTRACK   NTR1                                                                   
         USING TLFTD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLFTSSN,TIFSSN      OPTIONAL SSN                                 
         TM    TLFTSSN,X'C0'                                                    
         BNO   FTHIGH                                                           
         MVC   LKEY,LFTSSN                                                      
         MVC   TLFTCOM,TIFCOM      OPTIONAL INTERNAL COMMERCIAL NO.             
         OC    TLFTCOM,TLFTCOM                                                  
         BZ    FTHIGH                                                           
         MVC   LKEY,LFTCOM                                                      
         MVC   TLFTCAST,TIFCSEQ    OPTIONAL CAST INPUT SEQUENCE NO.             
         OC    TLFTCAST,TLFTCAST                                                
         BZ    FTHIGH                                                           
         MVC   LKEY,LFTCAST                                                     
         MVC   TLFTSTRT,TIQSTART   OPTIONAL START DATE                          
         CLI   TLFTSTRT,0                                                       
         BE    FTHIGH                                                           
         MVC   LKEY,LFTSTRT                                                     
         SPACE 1                                                                
FTHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
FTNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TISSN,TLFTSSN                                                    
         MVC   TICOM,TLFTCOM                                                    
         MVC   TIINV,TLFTINV                                                    
         MVC   TICASEQ,TLFTCAST                                                 
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   FTNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   FTNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     FTNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - ECAST TRACKING RECORDS                            
         SPACE 3                                                                
ETRACK   NTR1                                                                   
         USING TLETD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLETSSN,TIFSSN      OPTIONAL SSN                                 
         TM    TLETSSN,X'C0'                                                    
         BNO   ETHIGH                                                           
         MVC   LKEY,LETSSN                                                      
         MVC   TLETCOM,TIFCOM      OPTIONAL INTERNAL COMMERCIAL NO.             
         OC    TLETCOM,TLETCOM                                                  
         BZ    ETHIGH                                                           
         MVC   LKEY,LETCOM                                                      
         MVC   TLETCAT,TIFCAT      OPTIONAL CATEGORY                            
         OC    TLETCAT,TLETCAT                                                  
         BZ    ETHIGH                                                           
         MVC   LKEY,LETCAT                                                      
         MVC   TLETEPI,TIFEPI      POSSIBLE EPISODE NUMBER                      
         OC    TLETEPI,TLETEPI                                                  
         BZ    ECSHIGH                                                          
         XC    TLETEPI,=6X'FF'                                                  
         MVC   LKEY,LETEPI                                                      
         SPACE 1                                                                
ETHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
ETNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TISSN,TLETSSN                                                    
         MVC   TICOM,TLETCOM                                                    
         MVC   TICAT,TLETCAT                                                    
         MVC   TIEPI,TLETEPI                                                    
         XC    TIEPI,=6X'FF'                                                    
         MVC   TIINV,TLETINV                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   ETNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   ETNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     ETNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - DUE COMPANY RECORDS                               
         SPACE 3                                                                
DUECOMP  NTR1                                                                   
         USING TLDUD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLDUSSN,TIFSSN      OPTIONAL SSN                                 
         TM    TLDUSSN,X'C0'                                                    
         BNO   DUHIGH                                                           
         MVC   LKEY,LDUSSN                                                      
         MVC   TLDUDUC,TIQSTART                                                 
         SPACE 1                                                                
DUHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
DUNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TISSN,TLDUSSN                                                    
         MVC   TIDUC,TLDUDUC                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   DUNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   DUNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     DUNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - LIENS AND LEVYS                                   
         SPACE 3                                                                
LIEN     NTR1                                                                   
         USING TLLND,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLLNSSN,TIFSSN      OPTIONAL SSN                                 
         TM    TLLNSSN,X'C0'                                                    
         BNO   LNHIGH                                                           
         MVC   LKEY,LLNSSN                                                      
         MVC   TLLNLIN,TIQSTART                                                 
         SPACE 1                                                                
LNHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
LNNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TISSN,TLLNSSN                                                    
         MVC   TILNC,TLLNLIN                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   LNNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   LNNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     LNNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - CAST RECORDS                                      
         SPACE 3                                                                
CAST     NTR1                                                                   
         USING TLCAD,R4                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,INTCOMM     INTERNAL COMMERCIAL NUMBER                   
         MVC   LKEY,LCACOM                                                      
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
CANEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TIONOF,TIONOF       SET ANY CODES FROM KEY                       
         XC    TIUN(6),TIUN                                                     
         XC    TIYEAR,TIYEAR                                                    
         XC    TIAGT,TIAGT                                                      
         MVC   TISSN,TLCASSN                                                    
         MVC   TICAT,TLCACAT                                                    
         MVC   TICOM,TLCACOM                                                    
         MVC   TICASEQ,TLCASEQ                                                  
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   CANEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   CANEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     CANEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - CAST FOR EMPLOYEE                                 
         SPACE 3                                                                
CCAST    NTR1                                                                   
         USING TLCAPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLCACSSN,TIFSSN     OPTIONAL SSN                                 
         TM    TLCACSSN,X'C0'                                                   
         BNO   CCAHIGH                                                          
         MVC   LKEY,LCACSSN                                                     
         SPACE 1                                                                
CCAHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
CCANEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TISSN,TLCACSSN                                                   
         MVC   TICAT,TLCACCAT                                                   
         MVC   TICOM,TLCACCOM                                                   
         MVC   TICASEQ,TLCACSEQ                                                 
         MVC   INTCOMM,TLCACCOM                                                 
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   CCANEXT                                                          
         CLI   SUBREAD,TLCOCDQ     (MAY REALLY BE READING COMMERCIALS)          
         BE    CCACOM                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   CCANEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     CCANEXT                                                          
         SPACE 1                                                                
CCACOM   MVC   PRIMEKEY,KEY                                                     
         MVC   PRIMESAV,KEYSAVE                                                 
         MVC   INTCOMM,TLCACCOM                                                 
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLCOPD,R4                                                        
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,INTCOMM                                                 
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,FILTCOMM         FILTER ON COPY                               
         BNE   CCACOM2                                                          
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLCOD,R4                                                         
         MVC   TIAGY,TLCOAGY                                                    
         MVC   TIPRD,TLCOPRD                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   CCACOM2                                                          
         BAS   RE,GOHOOK                                                        
         SPACE 1                                                                
CCACOM2  MVC   KEY,PRIMEKEY        RESTORE SEQUENCE AGAIN                       
         BAS   RE,HIGH                                                          
         MVC   KEYSAVE,PRIMESAV                                                 
         B     CCANEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - CAST AGENT                                        
         SPACE 3                                                                
ACAST    NTR1                                                                   
         USING TLCAPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLCAAAGT,TIFAGT     POSSIBLE AGENT                               
         TM    TLCAAAGT,X'C0'                                                   
         BNO   ACAHIGH                                                          
         MVC   LKEY,LCAAAGT                                                     
         MVC   TLCAASSN,TIFSSN     POSSIBLE SS NUMBER                           
         TM    TLCAASSN,X'C0'                                                   
         BNO   ACAHIGH                                                          
         MVC   LKEY,LCAASSN                                                     
         SPACE 1                                                                
ACAHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
ACANEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGT,TLCAAAGT                                                   
         MVC   TISSN,TLCAASSN                                                   
         MVC   TICOM,TLCAACOM                                                   
         MVC   TICAT,TLCAACAT                                                   
         MVC   TICASEQ,TLCAASEQ                                                 
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   ACANEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   ACANEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     ACANEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - ECAST RECORDS FOR EPISODES                        
         SPACE 3                                                                
ECAST    NTR1                                                                   
         USING TLECD,R4                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVI   TLECCD,TLECCDQ                                                   
         MVC   TLECCOM,INTCOMM     INTERNAL COMM NUMBER                         
         MVC   LKEY,LECCOM                                                      
         TM    TIFEPI,X'C0'                                                     
         BNO   ECSHIGH                                                          
         MVC   TLECEPI,TIFEPI      POSSIBLE EPISODE NUMBER                      
         XC    TLECEPI,=6X'FF'                                                  
         MVC   LKEY,LECEPI                                                      
         MVC   TLECSSN,TIQSTART    OPTIONAL START                               
         SPACE 1                                                                
ECSHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
ECSNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIEPI,TLECEPI                                                    
         XC    TIEPI,=6X'FF'                                                    
         MVC   TICOM,TLECCOM                                                    
         MVC   TISSN,TLECSSN                                                    
         MVC   TICAT,TLECCAT                                                    
         MVC   TIINV,TLECINV                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   ECSNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   ECSNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     ECSNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - ECAST RECORDS BY SSN FOR EPISODES                 
         SPACE 3                                                                
ECC      NTR1                                                                   
         USING TLECPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLECCSSN,TIFSSN     POSSIBLE SS NUMBER                           
         TM    TLECCSSN,X'C0'                                                   
         BNO   ECCHIGH                                                          
         MVC   LKEY,LECCSSN                                                     
         MVC   TLECCEPI,TIQSTART   OPTIONAL START EPISODE NUMBER                
         SPACE 1                                                                
ECCHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
ECCNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIEPI,TLECCEPI                                                   
         XC    TIEPI,=6X'FF'                                                    
         MVC   TISSN,TLECCSSN                                                   
         MVC   TICOM,TLECCCOM                                                   
         MVC   TICAT,TLECCCAT                                                   
         MVC   TIINV,TLECCINV                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   ECCNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   ECCNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     ECCNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - CAST FOR GUARANTEE                                
         SPACE 3                                                                
GCAST    NTR1                                                                   
         USING TLCAPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLCAGSSN,TIFSSN     POSSIBLE SS NUMBER                           
         TM    TLCAGSSN,X'C0'                                                   
         BNO   GCAHIGH                                                          
         MVC   LKEY,LCAGSSN                                                     
         MVC   TLCAGGUA,TIFGUA     POSSIBLE GUARANTEE CODE                      
         TM    TLCAGGUA,X'C0'                                                   
         BNO   GCAHIGH                                                          
         MVC   LKEY,LCAGGUA                                                     
         MVC   TLCAGCOM,TIFCOM     POSSIBLE COMMERCIAL NUMBER                   
         OC    TLCAGCOM,TLCAGCOM                                                
         BZ    GCAHIGH                                                          
         MVC   LKEY,LCAGCOM                                                     
         SPACE 1                                                                
GCAHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
GCANEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TISSN,TLCAGSSN                                                   
         MVC   TIGUA,TLCAGGUA                                                   
         MVC   TICOM,TLCAGCOM                                                   
         MVC   TICAT,TLCAGCAT                                                   
         MVC   TICASEQ,TLCAGSEQ                                                 
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   GCANEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   GCANEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     GCANEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - CAST BY NAME RECORDS                              
         SPACE 3                                                                
NCAST    NTR1                                                                   
         USING TLCAPD,R4                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVI   TLCAPCD,TLCANCDQ                                                 
         MVC   TLCANCOM,TIFCOM      INTERNAL COMMERCIAL NUMBER                  
         MVC   LKEY,LCANCOM                                                     
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
NCANEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         MVC   TICOM,TLCANCOM                                                   
         MVC   TICASEQ,TLCANSEQ                                                 
         DROP  R4                                                               
         XC    TIAGY,TIAGY                                                      
         XC    TICLI,TICLI                                                      
         XC    TISSN,TISSN                                                      
         XC    TICAT,TICAT                                                      
**NO-OP  XC    TICASEQ,TICASEQ                                                  
         XC    TICKDATE,TICKDATE                                                
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NCANEXT                                                          
         BAS   RE,NCASUB                                                        
         BE    NCANEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NCANEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     NCANEXT                                                          
         SPACE 2                                                                
NCASUB   NTR1                                                                   
         CLI   SUBREAD,TLCKCDQ                                                  
         BNE   NOTOK                                                            
         MVC   PRIMEKEY,KEY                                                     
         MVC   PRIMESAV,KEYSAVE                                                 
         MVC   PRIMELKY,LKEY                                                    
                                                                                
         USING TLCAPD,RE                                                        
         LA    RE,KEY                                                           
         MVC   TIFCSEQ,TLCANSEQ                                                 
         DROP  RE                                                               
                                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,TLCKCDQ                                                      
                                                                                
         BAS   RE,CHECK                                                         
                                                                                
         XC    TIFCSEQ,TIFCSEQ                                                  
         MVC   KEY,PRIMEKEY                                                     
         BAS   RE,HIGH                                                          
         MVC   KEYSAVE,PRIMESAV                                                 
         MVC   LKEY,PRIMELKY                                                    
         B     OK                                                               
         EJECT                                                                  
*              I/O ROUTINES - EPISODE RECORDS                                   
         SPACE 3                                                                
EPISODE  NTR1                                                                   
         USING TLEPD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLEPAGY,TIFAGY      POSSIBLE AGENCY                              
         TM    TLEPAGY,X'C0'                                                    
         BNO   EPIHIGH                                                          
         MVC   LKEY,LEPIAGY                                                     
         MVC   TLEPEPI,TIQSTART    OPTIONAL START EPISODE NUMBER                
         SPACE 1                                                                
EPIHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
EPINEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLEPAGY       AGENCY                                       
         MVC   TIEPI,TLEPEPI       EPISODE NUMBER                               
         XC    TIEPI,=6X'FF'                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   EPINEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   EPINEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     EPINEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - AREA                                              
         SPACE 3                                                                
AREA     NTR1                                                                   
         USING TLARD,R4                                                         
         MVC   TLARAREA,TIQSTART   OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
ARNEXT   BAS   RE,SEQ                                                           
         CLC   KEY(1),KEYSAVE                                                   
         BNE   XIT                                                              
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         LA    R4,KEY                                                           
         MVC   TIAREA,TLARAREA                                                  
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   ARNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   ARNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     ARNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - USE                                               
         SPACE 3                                                                
USE      NTR1                                                                   
         USING TLUSD,R4                                                         
         MVC   TLUSUSE,TIQSTART    OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
USNEXT   BAS   RE,SEQ                                                           
         CLC   KEY(1),KEYSAVE                                                   
         BNE   XIT                                                              
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         LA    R4,KEY                                                           
         MVC   TIPUSE,TLUSUSE                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   USNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   USNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     USNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - DEAL                                              
         SPACE 3                                                                
DEAL     NTR1                                                                   
         USING TLDLD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLDLAGY,TIFAGY      AGENCY                                       
         TM    TLDLAGY,X'C0'                                                    
         BNO   *+10                                                             
         MVC   LKEY,LDLAGY                                                      
         MVC   TLDLCLI,TIQSTART    OPTIONAL START CLIENT                        
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
DLNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLDLAGY                                                    
         MVC   TICLI,TLDLCLI                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   DLNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   DLNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     DLNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - HOLD (NETWORK TRANSFER RECORDS)                   
         SPACE 3                                                                
ALIAS    NTR1                                                                   
         USING TLAKD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLAKAGY,TIFAGY      AGENCY                                       
         TM    TLAKAGY,X'C0'                                                    
         BNO   AKHIGH                                                           
         MVC   LKEY,LAKAGY                                                      
         MVC   TLAKADID,TIQSTART   OPTIONAL START                               
*                                                                               
AKHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
AKNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLAKAGY                                                    
         MVC   TICID,SPACES                                                     
         MVC   TICID,TLAKADID                                                   
         MVC   TICLI,SPACES                                                     
         MVC   TICLI(L'TLAKNCLI),TLAKNCLI                                       
         MVC   TIPRD,SPACES                                                     
         MVC   TIPRD(L'TLAKNPRD),TLAKNPRD                                       
         MVC   TIMED,TLAKMED                                                    
         MVC   TICOM,TLAKCOM                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   AKNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   AKNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     AKNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - MARKET                                            
         SPACE 3                                                                
MARKET   NTR1                                                                   
         USING TLMTD,R4                                                         
         MVI   LKEY,0                                                           
         SPACE 1                                                                
MTHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
MTNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   MTNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   MTNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     MTNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - MARKET NAME                                       
         SPACE 3                                                                
NMARKET  NTR1                                                                   
         USING TLMTPD,R4                                                        
         MVI   LKEY,0                                                           
         SPACE 1                                                                
NMTHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
NMTNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NMTNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NMTNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     NMTNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - HOLD (NETWORK/SPOT TRANSFER RECORDS)              
         SPACE 3                                                                
HOLD     NTR1                                                                   
         USING TLNXD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLNXAGY,TIFAGY      AGENCY                                       
         TM    TLNXAGY,X'C0'                                                    
         BNO   NXHIGH                                                           
         MVC   LKEY,LNXAGY                                                      
         MVC   TLNXNID,TIQSTART    OPTIONAL START                               
*                                                                               
NXHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
NXNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BE    NXNXT10                                                          
         TM    TIQFLAG3,TIQHLPCK   IF PACKED STATUS ON,                         
         BZ    XIT                                                              
         NI    TIQFLAG3,X'FF'-TIQHLPCK   TURN IT OFF                            
         XC    WORK,WORK                                                        
         GOTO1 TIATRPAK,DMCB,(C'U',TIQSTART),WORK   AND UNPACK COMM             
         XC    KEY,KEY                                                          
         MVI   LKEY,0                                                           
         MVC   TLNXAGY,TIFAGY      AGENCY                                       
         TM    TLNXAGY,X'C0'                                                    
         BNO   NXHIGH                                                           
         MVC   LKEY,LNXAGY                                                      
         MVC   TLNXNID,WORK        OPTIONAL START  (UNPACKED COMM)              
         B     NXHIGH                                                           
NXNXT10  LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLNXAGY                                                    
         MVC   TICID,SPACES                                                     
         MVC   TICID(L'TLNXNID),TLNXNID                                         
         MVC   TICLI,SPACES                                                     
         MVC   TICLI(L'TLNXNCLI),TLNXNCLI                                       
         MVC   TIPRD,SPACES                                                     
         MVC   TIPRD(L'TLNXNPRD),TLNXNPRD                                       
         MVC   TIMED,TLNXMED                                                    
         MVC   TIUSE,TLNXUSE                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NXNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NXNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     NXNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - HOLD BY NAME (NET/SPOT TRANSFER RECORDS)          
         SPACE 3                                                                
NHOLD    NTR1                                                                   
         USING TLNXPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLNXNAGY,TIFAGY     AGENCY                                       
         TM    TLNXNAGY,X'C0'                                                   
         BNO   NNXHIGH                                                          
         MVC   LKEY,LNXNAGY                                                     
         MVC   TLNXNAME,TIQSTART   OPTIONAL START                               
*                                                                               
NNXHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
NNXNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLNXNAGY                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NNXNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLNXD,R4                                                         
         MVC   TICID,SPACES                                                     
         MVC   TICID(L'TLNXNID),TLNXNID                                         
         MVC   TICLI,SPACES                                                     
         MVC   TICLI(L'TLNXNCLI),TLNXNCLI                                       
         MVC   TIPRD,SPACES                                                     
         MVC   TIPRD(L'TLNXNPRD),TLNXNPRD                                       
         MVC   TIMED,TLNXMED                                                    
         MVC   TIUSE,TLNXUSE                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NNXNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     NNXNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - HOLD BY ID (NET/SPOT TRANSFER RECORDS)            
         SPACE 3                                                                
CHOLD    NTR1                                                                   
         USING TLNXPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLNXCAGY,TIFAGY      AGENCY                                      
         TM    TLNXCAGY,X'C0'                                                   
         BNO   CNXHIGH                                                          
         MVC   LKEY,LNXCAGY                                                     
         MVC   TLNXCCID,TIQSTART    OPTIONAL START                              
*                                                                               
CNXHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
CNXNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLNXCAGY                                                   
         MVC   TICID,TLNXCCID                                                   
         MVC   TICLI,SPACES                                                     
         MVC   TICLI(L'TLNXCCLI),TLNXCCLI                                       
         MVC   TIPRD,SPACES                                                     
         MVC   TIPRD(L'TLNXCPRD),TLNXCPRD                                       
         MVC   TIMED,TLNXCMED                                                   
         MVC   TIUSE,TLNXCUSE                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   CNXNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   CNXNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     CNXNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - INVOICES                                          
         SPACE 3                                                                
INVOICE  NTR1                                                                   
         USING TLIND,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLINAGY,TIFAGY      POSSIBLE AGENCY                              
         TM    TIFINS2Y,TAINSADJ   IF FILTERING ON ADJUSTMENTS                  
         BNO   *+10                   USE AGENCY 999999                         
         MVC   TLINAGY,=C'999999'                                               
         MVC   TLININV,TIQSTART    OPTIONAL START                               
         TM    TLINAGY,X'C0'                                                    
         BNO   INHIGH                                                           
         MVC   LKEY,LINAGY                                                      
         CLI   TIFINVD,0           IF HAVE TIFINVD SET (COULD HAVE *'S)         
         BE    INHIGH                                                           
         MVC   LKEY,LININV         SET L'KEY FOR COMPARE                        
         BAS   RE,INVLKEY          AND ADJUST USING TIFINVD                     
         SPACE 1                                                                
INHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
INNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLINAGY                                                    
         MVC   TIINV,TLININV                                                    
         XC    TIINV,=6X'FF'                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   INNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   INNEXT                                                           
         BAS   RE,SUBINV                                                        
         B     INNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - INVOICE HISTORY                                   
         SPACE 3                                                                
HINVOICE NTR1                                                                   
         USING TLINPD,R4                                                        
         SPACE 1                                                                
HINV10   MVI   LKEY,0                                                           
         MVC   TLINHCOM,TIFCOM     POSSIBLE INTERNAL COMMERCIAL #               
         OC    TLINHCOM,TLINHCOM                                                
         BZ    HINHIGH                                                          
         MVC   LKEY,LINHCOM                                                     
         MVC   TLINHINV,TIQSTART   OPTIONAL START                               
         CLI   TIFINVD,0           IF HAVE TIFINVD SET (COULD HAVE *'S)         
         BE    HINHIGH                                                          
         MVC   LKEY,LINHINV        SET L'KEY FOR COMPARE                        
         BAS   RE,INVLKEY          AND ADJUST USING TIFINVD                     
         SPACE 1                                                                
HINHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
HINNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   HINN10              FIND INVOICES FROM PREVIOUS COMML            
         SPACE 1                                                                
         LA    R4,KEY              SET ANY CODES FROM KEY                       
         XC    TICODES,TICODES                                                  
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   HINNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               CODES FROM ACTIVE KEY                        
         USING TLIND,R4                                                         
         TM    TIQFLAG3,TIQFFCOM   IF FORCING VALUES FROM COMML                 
         BZ    *+12                                                             
         BAS   RE,FIXINV           REPLACE COMML VALUES ON INVOICE              
         B     *+10                                                             
         MVC   TIAGY,TLINAGY                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   HINNEXT                                                          
         BAS   RE,SUBINV                                                        
         TM    TIQFLAG3,TIQFLAST   IF ONLY WANT LATEST HISTORY                  
         BO    HINN30              EXIT                                         
         B     HINNEXT             ELSE, PROCESS NEXT                           
         SPACE 1                                                                
HINN10   DS    0H                                                               
         BAS   RE,GETCOMM          GET 'FROM' OLD AGY/CID                       
         BNE   HINN30                                                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLINPD,R4                                                        
         MVI   TLINPCD,TLINHCDQ    RE-SET RECORD CODE                           
         B     HINV10                                                           
         SPACE 1                                                                
HINN30   DS    0H                                                               
         XC    TIQSTART,TIQSTART   OPTIONAL START                               
         XC    TIQSKEY,TIQSKEY                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              REPLACE ALL COMMERCIAL RELATED VALUES ON INVOICE WITH            
*              CURRENT COMMERCIAL VALUES                                        
         SPACE 1                                                                
FIXINV   NTR1                                                                   
         L     R6,TIAMAIN          R4=A(COMMERCIAL RECORD)                      
         CLI   0(R6),TLCOCDQ                                                    
         BNE   XIT                                                              
         USING TLCOD,R6                                                         
         MVC   TIAGY,TLCOAGY                                                    
         MVC   TICLI,TLCOCLI                                                    
         MVC   TIPRD,TLCOPRD                                                    
*                                                                               
         LA    R4,COELS            R4=A(COMML RELATED ELS ON INVOICE)           
FIXINV5  CLI   0(R4),X'FF'                                                      
         BE    XIT                                                              
*                                                                               
         MVC   ELCODE,0(R4)                                                     
         LA    R6,IO               DELETE OUTDATED ELEMENT FROM INV             
         BAS   RE,GETEL                                                         
         B     *+8                                                              
FIXINV10 BAS   RE,NEXTEL                                                        
         BNE   FIXINV15                                                         
         CLI   1(R4),0                                                          
         BE    *+14                                                             
         CLC   2(1,R6),1(R4)                                                    
         BNE   FIXINV10                                                         
         MVI   0(R6),X'FF'                                                      
         B     FIXINV10                                                         
*                                                                               
FIXINV15 GOTO1 =V(HELLO),DMCB,(C'D',=C'TALFIL'),(X'FF',TIAREC),0,0,0            
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R6,TIAMAIN          FIND CURRENT ELEMENT IN COMMERCIAL           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
FIXINV18 BAS   RE,NEXTEL                                                        
         BNE   FIXINV20                                                         
         CLI   1(R4),0                                                          
         BE    *+14                                                             
         CLC   2(1,R6),1(R4)                                                    
         BNE   FIXINV18                                                         
*                                  AND ADD CURRENT ELEMENT BACK TO INV          
         GOTO1 =V(HELLO),DMCB,(C'P',=C'TALFIL'),TIAREC,0(R6)                    
         CLI   12(R1),0                                                         
         BE    FIXINV18                                                         
         DC    H'0'                                                             
*                                                                               
FIXINV20 LA    R4,L'COELS(R4)      LOOP FOR NEXT ELEMENT IN TABLE               
         B     FIXINV5                                                          
         EJECT                                                                  
*               GET 'FROM' INT COMMERCIAL # FROM TAOC EL.                       
         SPACE 3                                                                
GETCOMM  NTR1                                                                   
         LA    R4,KEY              GET COMMERCIAL RECORD                        
         XC    KEY,KEY                                                          
         USING TLCOPD,R4                                                        
         MVI   TLCOPCD,TLCOCCDQ                                                 
         OC    TIFCOM,TIFCOM       IF THERE IS A FILTER FOR INVOICE             
         BZ    GETC02                                                           
         MVC   TLCOCCOM,TIFCOM     INTERNAL COMMERCIAL NUMBER - USE IT          
         B     GETC04                                                           
*                                                                               
GETC02   MVC   TLCOCCOM,TICOM      ELSE USE INT COMML NUMBER FROM INV           
*                                                                               
GETC04   MVC   LKEY,LCOCCOM                                                     
         BAS   RE,HIGH                                                          
         BAS   RE,KEYCOMP                                                       
         BNE   NOTOK                                                            
         BAS   RE,GETREC                                                        
         BNE   NOTOK                                                            
         LA    R6,IO                                                            
         USING TAOCD,R6            GET 'FROM' COMMERCIAL ELEMENT                
         MVI   ELCODE,TAOCELQ                                                   
         BAS   RE,GETEL                                                         
         B     GETC20                                                           
*                                                                               
GETC10   BAS   RE,NEXTEL                                                        
*                                                                               
GETC20   BNE   NOTOK                                                            
         TM    TAOCSTAT,TAOCSFRO   TEST IF THIS IS 'FROM' COMML                 
         BNO   GETC10                                                           
**NOOP** MVC   TIFAGY,TAOCAGY      SET PREVIOUS AGENCY                          
         MVC   TIFCOM,TAOCCOM                   INT COMML #                     
         XC    TIQSKEY,TIQSKEY     CLEAR CONTINUE KEY &                         
         XC    TIQSTART,TIQSTART         OPTIONAL START                         
         B     OK                                                               
         EJECT                                                                  
*              I/O ROUTINES - INVOICE BILLING OPEN ITEMS                        
         SPACE 3                                                                
BINVOICE NTR1                                                                   
         USING TLINPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLINBAGY,TIFAGY     POSSIBLE AGENCY                              
         TM    TIFINS2Y,TAINSADJ   IF FILTERING ON ADJUSTMENTS                  
         BNO   *+10                   USE AGENCY 999999                         
         MVC   TLINBAGY,=C'999999'                                              
         TM    TLINBAGY,X'C0'                                                   
         BNO   BINHIGH                                                          
         MVC   LKEY,LINBAGY                                                     
         SPACE 1                                                                
BINHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
BINNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLINBAGY                                                   
         MVC   TIINV,TLINBINV                                                   
         XC    TIINV,=6X'FF'                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   BINNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   BINNEXT                                                          
         BAS   RE,SUBINV                                                        
         B     BINNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - INVOICE DUE DATE OPEN ITEMS                       
         SPACE 3                                                                
CINVOICE NTR1                                                                   
         USING TLINPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLINCDUE,TIQPSTR                                                 
         CLC   TLINCDUE,=X'000004' IF THIS IS ADJUSTMENTS                       
         BNE   CINHIGH                                                          
         MVI   TLINCSRT,X'08'      SORT WILL ALWAYS BE X'80'                    
         MVC   LKEY,LINCAGY                                                     
         MVC   TLINCAGY,=C'999999' & AGENCY WILL BE 999999                      
         MVC   TLINCINV,TIQSTART   OPTIONAL START                               
         SPACE 1                                                                
CINHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
CINNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         OC    TIQPEND,TIQPEND                                                  
         BZ    CINNEXT2                                                         
         CLC   TLINCDUE,TIQPEND                                                 
         BH    XIT                                                              
         SPACE 1                                                                
CINNEXT2 LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIDUDATE,TLINCDUE                                                
         MVC   TIAGY,TLINCAGY                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   CINNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   CINNEXT                                                          
         BAS   RE,SUBINV                                                        
         B     CINNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - INVOICE CHECK DATE SEQUENCE                       
         SPACE 3                                                                
KINVOICE NTR1                                                                   
         USING TLINPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLINKDTE,TIQPSTR                                                 
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
KINNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         OC    TIQPEND,TIQPEND                                                  
         BZ    KINNEXT2                                                         
         CLC   TLINKDTE,TIQPEND                                                 
         BH    XIT                                                              
         SPACE 1                                                                
KINNEXT2 LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TICKDATE,TLINKDTE                                                
         MVC   TIAGY,TLINKAGY                                                   
         MVC   TIEMP,TLINKEMP                                                   
         MVC   TIOFF,TLINKOFF                                                   
         MVC   TICLI,TLINKCLI                                                   
         MVC   TICUR,TLINKCUR                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   KINNEXT                                                          
         TM    TIQFLAGS,TIQFNOGR   USING POINTERS TO GET TO CHECKS?             
         BO    KINNEXT6            SPECIAL LITTLE ROUTINE FOR THIS              
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         SPACE 1                                                                
         TM    PAYPSTAT,TAPDPBNP   IGNORE BNP                                   
         BNO   KINNEXT4                                                         
         TM    TIQFLAGS,TIQFPBNP   UNLESS REQUESTED TO PASS BNP                 
         BNO   KINNEXT                                                          
         MVC   TICKDATE,TIBIDATE   MAKE 'CHECK' DATE = BILLED DATE              
         OI    INVSTAT,TAINSCHK    AND 'CHECKS WRITTEN' STATUS                  
         SPACE 1                                                                
KINNEXT4 BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   KINNEXT                                                          
         BAS   RE,SUBINV           (MAY BE READING CHECKS)                      
         B     KINNEXT                                                          
         SPACE 1                                                                
KINNEXT6 MVC   INTAGY,TLINKAGY                                                  
         MVC   INTINVNO,TLINKINV                                                
         TM    TIQFLAG3,TIQFADJ    IF NEED ADJUSTMENTS                          
         BZ    KINNEXT7                                                         
         OC    TLINKADJ,TLINKADJ   AND THERE IS AN ADJUSTMENT INVOICE           
         BZ    KINNEXT7                                                         
         MVC   INTAGY,=C'999999'   USE IT                                       
         MVC   INTINVNO+2(3),TLINKADJ                                           
KINNEXT7 MVC   PRIMEKEY,KEY                                                     
         MVC   PRIMESAV,KEYSAVE                                                 
         MVC   PRIMELKY,LKEY                                                    
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,TLCKCDQ                                                      
         BAS   RE,CHECK            CHECK RECORDS FOR INVOICE                    
         SPACE 1                                                                
         MVC   KEY,PRIMEKEY                                                     
         BAS   RE,HIGH                                                          
         MVC   KEYSAVE,PRIMESAV                                                 
         MVC   LKEY,PRIMELKY                                                    
         XC    INTAGY,INTAGY                                                    
         XC    INTINVNO,INTINVNO                                                
         B     KINNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - INVOICE BILL DATE                                 
         SPACE 3                                                                
DINVOICE NTR1                                                                   
         USING TLINPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLINDDTE,TIQPSTR                                                 
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
DINNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         OC    TIQPEND,TIQPEND                                                  
         BZ    DINNEXT2                                                         
         CLC   TLINDDTE,TIQPEND                                                 
         BH    XIT                                                              
         SPACE 1                                                                
DINNEXT2 LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIBIDATE,TLINDDTE                                                
         MVC   TIAGY,TLINDAGY                                                   
         MVC   TIEMP,TLINDEMP                                                   
         MVC   TIOFF,TLINDOFF                                                   
         MVC   TICLI,TLINDCLI                                                   
         MVC   TICUR,TLINDCUR                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   DINNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,SFCFINV          SPECIAL FILTERING FOR CHKRCINV               
         BNE   DINNEXT                                                          
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   DINNEXT                                                          
         BAS   RE,SUBINV           (CHECK FOR SUB READ)                         
         B     DINNEXT                                                          
         SPACE 2                                                                
*              ROUTINE TO PERFORM SOME SPECIAL FILTERING FOR                    
*              WRITER REPORTS READING WITH THE CKFRCINV KEYWORD                 
         SPACE 1                                                                
SFCFINV  NTR1                                                                   
         TM    TISTAT,TISTSPCI     IF SPECIAL HANDLING FOR CKFRCINV             
         BZ    SCIYES              STATUS IS ON                                 
         SPACE 1                                                                
         USING TAPDD,R6                                                         
         LA    R6,IO               R6=A(INVOICE RECORD)                         
         MVI   ELCODE,TAPDELQ      GET PAYMENT DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BNE   SCINO                                                            
         CLC   TAPDCOM,TIFCOM      INTERNAL COMMERCIAL ID MUST                  
         BNE   SCINO               MATCH FILTER                                 
         DROP  R6                                                               
         SPACE 1                                                                
         USING TACOD,R6                                                         
         LA    R6,IO               PUT CURRENT COMMERCIAL ID                    
         MVI   ELCODE,TACOELQ      INTO COMMERCIAL DETAILS ELEMENT              
         BAS   RE,GETEL                                                         
         BNE   SCINO                                                            
         MVC   TACOCID,TIFISCI                                                  
         DROP  R6                                                               
         SPACE 1                                                                
SCIYES   XR    RC,RC                                                            
SCINO    LTR   RC,RC                                                            
         B     XIT                                                              
         EJECT                                                                  
*              I/O ROUTINES - INVOICE BOVER                                     
         SPACE 3                                                                
OINVOICE NTR1                                                                   
         USING TLINPD,R4                                                        
         MVI   LKEY,0                                                           
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
OINNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   OINNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   OINNEXT                                                          
         BAS   RE,SUBINV           (CHECK FOR SUB READ)                         
         B     OINNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - INVOICE PAY DATE                                  
         SPACE 3                                                                
PINVOICE NTR1                                                                   
         USING TLINPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLINPDTE,TIQPSTR                                                 
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
PINNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         OC    TIQPEND,TIQPEND                                                  
         BZ    PINNEXT2                                                         
         CLC   TLINPDTE,TIQPEND                                                 
         BH    XIT                                                              
         SPACE 1                                                                
PINNEXT2 LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIBIDATE,TLINPDTE                                                
         MVC   TIAGY,TLINPAGY                                                   
         MVC   TIEMP,TLINPEMP                                                   
         MVC   TIOFF,TLINPOFF                                                   
         MVC   TICLI,TLINPCLI                                                   
         MVC   TICUR,TLINPCUR                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   PINNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   PINNEXT                                                          
         BAS   RE,SUBINV           (CHECK FOR SUB READ)                         
         B     PINNEXT                                                          
         EJECT                                                                  
*              CONTROL RECORDS RELATED TO INVOICES                              
         SPACE 3                                                                
SUBINV   NTR1                                                                   
         CLI   SUBREAD,0           IF DON'T HAVE SUBREAD                        
         BE    SUBINV2                                                          
         CLI   SUBREAD,TLINHCDQ    OR WE ARE ALREADY IN SUBREAD                 
         BNE   SUBINV4                                                          
         SPACE 1                                                                
SUBINV2  BAS   RE,GOHOOK           HOOK FOR INVOICE RECORD                      
         B     XIT                 AND GET OUT                                  
         SPACE 1                                                                
SUBINV4  BAS   RE,GOHKINV          ELSE HOOK WITH PROCINV                       
         CLI   TIMODE,PROCNOCK     USER CAN ASK NOT TO PROCESS CHECKS           
         BE    XIT                                                              
         LA    R4,IO                                                            
         USING TLIND,R4                                                         
         MVC   INTAGY,TLINAGY                                                   
         MVC   INTINVNO,TLININV                                                 
         XC    INTINVNO,=6X'FF'                                                 
         LR    RE,R4                                                            
         AHI   RE,L'IO                                                          
         ST    RE,TIAMAIN                                                       
         LHI   RF,L'IO                                                          
         LR    R0,R4                                                            
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         MVC   PRIMEKEY,KEY                                                     
         MVC   PRIMESAV,KEYSAVE                                                 
         MVC   PRIMELKY,LKEY                                                    
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,TLCKCDQ                                                      
         SPACE 1                                                                
         CLI   SUBREAD,TLCKCDQ                                                  
         BNE   *+8                                                              
         BAS   RE,CHECK            CHECK RECORDS FOR INVOICE                    
         SPACE 1                                                                
         MVC   KEY,PRIMEKEY                                                     
         BAS   RE,HIGH                                                          
         MVC   KEYSAVE,PRIMESAV                                                 
         MVC   LKEY,PRIMELKY                                                    
         XC    INTAGY,INTAGY                                                    
         XC    INTINVNO,INTINVNO                                                
         XC    TIAMAIN,TIAMAIN                                                  
         B     XIT                                                              
         EJECT                                                                  
*              I/O ROUTINES - CHECKS                                            
         SPACE 3                                                                
CHECK    NTR1                                                                   
         USING TLCKD,R4                                                         
         OI    TISTAT,TISTRDCK     SET READ FROM CHKDIR/CHKFIL                  
         MVI   LKEY,0                                                           
         MVC   TLCKAGY,TIFAGY      POSSIBLE AGENCY                              
         TM    TIFINS2Y,TAINSADJ   IF FILTERING ON ADJUSTMENTS                  
         BNO   *+10                   USE AGENCY 999999                         
         MVC   TLCKAGY,=C'999999'                                               
         OC    INTAGY,INTAGY       (AGENCY MAYBE PASSED INTERNALLY)             
         BZ    *+10                                                             
         MVC   TLCKAGY,INTAGY                                                   
         TM    TLCKAGY,X'C0'                                                    
         BNO   CKHIGH                                                           
         MVC   LKEY,LCKAGY                                                      
         MVC   TLCKINV,TIFINV      POSSIBLE INVOICE NUMBER                      
         OC    INTINVNO,INTINVNO   (INV# MAYBE PASSED INTERNALLY)               
         BZ    *+10                                                             
         MVC   TLCKINV,INTINVNO                                                 
         CLI   TLCKINV,0           IF ALREADY HAVE INV NUM                      
         BE    *+14                                                             
         MVC   LKEY,LCKINV         SET L'KEY AND GO PROCESS                     
         B     CKHIGH                                                           
         CLI   TIFINVD,0           ELSE IF TIFINVD SET                          
         BE    CKHIGH                                                           
         MVC   TLCKINV,TIQSTART    SET INVOICE FROM TIQSTART                    
         MVC   LKEY,LCKINV         SET L'KEY FOR COMPARE                        
         BAS   RE,INVLKEY          AND ADJUST LKEY USING TIFINVD                
         SPACE 1                                                                
CKHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
CKNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   CKXIT                                                            
         LA    R4,KEY                                                           
         XC    TIUN(6),TIUN        CLEAR PREVIOUS RECORD POSTINGS               
         XC    TIONOF,TIONOF                                                    
         XC    TIYEAR,TIYEAR                                                    
         XC    TIAGT,TIAGT                                                      
         XC    TIUNIT,TIUNIT                                                    
         XC    TISEX(3),TISEX                                                   
         MVI   TICUR,0             RESET DUE TO MIXED CAST BUG                  
         MVI   TIW4TY,0                                                         
         MVI   TIW4W4,0                                                         
         MVI   TINHA,0                                                          
         MVC   TIAGY,TLCKAGY                                                    
         MVC   TISSN,TLCKSSN                                                    
         MVC   TICAT,TLCKCAT                                                    
         MVC   TICASEQ,TLCKSORT+4  CAST SEQUENCE                                
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   CKNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         SPACE 1                                                                
         TM    PAYPSTAT,TAPDPBNP   IF THIS IS A BNP 'CHECK'                     
         BZ    *+14                                                             
         MVC   TICKDATE,TIBIDATE   MAKE 'CHECK' DATE = BILLED DATE              
         OI    INVSTAT,TAINSCHK    AND 'CHECKS WRITTEN' STATUS                  
         SPACE 1                                                                
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   CKNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     CKNEXT                                                           
         SPACE 2                                                                
CKXIT    NI    TISTAT,X'FF'-TISTRDCK  SET NO LONGER READING CHECK RECS.         
         B     XIT                                                              
         EJECT                                                                  
*              I/O ROUTINES - CAST PAYMENT HISTORY CHECKS                       
         SPACE 3                                                                
HCHECK   NTR1                                                                   
         USING TLCKPD,R4                                                        
         OI    TISTAT,TISTRDCK     SET READ FROM CHKDIR/CHKFIL                  
         MVI   LKEY,0                                                           
         MVC   TLCKHCOM,TIFCOM     POSSIBLE INT. COMM NUM                       
         OC    TLCKHCOM,TLCKHCOM                                                
         BZ    HCKHIGH                                                          
         MVC   LKEY,LCKHCOM                                                     
         MVC   TLCKHSSN,TIFSSN     POSSIBLE SS# FILTER                          
         TM    TLCKHSSN,X'C0'                                                   
         BNO   HCKHIGH                                                          
         MVC   LKEY,LCKHSSN                                                     
         MVC   TLCKHCAT,TIFCAT     POSSIBLE CAT FILTER                          
         TM    TLCKHCAT,X'C0'                                                   
         BNO   HCKHIGH                                                          
         MVC   LKEY,LCKHCAT                                                     
         SPACE 1                                                                
HCKHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
HCKNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   CKXIT                                                            
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TISSN,TLCKHSSN                                                   
         MVC   TICAT,TLCKHCAT                                                   
         MVC   TICASEQ,TLCKHSEQ                                                 
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   HCKNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               CODES FROM ACTIVE KEY                        
         USING TLCKD,R4                                                         
         MVC   TIAGY,TLCKAGY                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   HCKNEXT                                                          
         CLI   MAINREAD,TLCOCDQ    CKFRCCOM, DOESN'T THE FIRST TIME             
         BNE   HCKNXT5                                                          
         CLI   SUBREAD,TLCKHCDQ                                                 
         BE    HCKNXT5                                                          
         OC    TIDATE,TIDATE       THERE HAS TO BE SOMETHING                    
         BZ    HCKNEXT                                                          
HCKNXT5  BAS   RE,GOHOOK                                                        
         B     HCKNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - EMPLOYEE'S CHECKS                                 
         SPACE 3                                                                
ECHECK   NTR1                                                                   
         USING TLCKPD,R4                                                        
         OI    TISTAT,TISTRDCK     SET READ FROM CHKDIR/CHKFIL                  
         MVI   LKEY,0                                                           
         MVC   TLCKESSN,TIFSSN     POSSIBLE SS# FILTER                          
         OC    INTSSN,INTSSN       (SSN MAY BE PASSED INTERNALLY)               
         BZ    *+10                                                             
         MVC   TLCKESSN,INTSSN                                                  
         TM    TLCKESSN,X'C0'                                                   
         BNO   ECKHIGH                                                          
         MVC   LKEY,LCKESSN                                                     
         MVC   TLCKECUR,TIFCUR     POSSIBLE CURRENCY                            
         TM    TLCKECUR,X'C0'                                                   
         BNO   ECKHIGH                                                          
         MVC   LKEY,LCKECUR                                                     
         MVC   TLCKEEMP,TIFEMP     POSSIBLE EMPLOYER                            
         TM    TLCKEEMP,X'C0'                                                   
         BNO   ECKHIGH                                                          
         MVC   LKEY,LCKEEMP                                                     
         OC    TIQPEND,TIQPEND     OPTIONAL END DATE                            
         BZ    ECKHIGH                                                          
         MVC   TLCKEDTE,TIQPEND                                                 
         XC    TLCKEDTE,=X'FFFFFF'                                              
         SPACE 1                                                                
ECKHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
ECKNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   CKXIT                                                            
         SPACE 1                                                                
         LA    R4,KEY                                                           
         CLC   LKEY,LCKEEMP        IF ALL THE FIELDS WERE FILLED IN             
         BNE   ECKNEXT2                                                         
         OC    TIQPSTR,TIQPSTR     POSSIBLE START DATE                          
         BZ    ECKNEXT2                                                         
         MVC   WORK(3),TIQPSTR                                                  
         XC    WORK(3),=X'FFFFFF'  COMPLEMENT START DATE                        
         CLC   TLCKEDTE,WORK       IF BEFORE START DATE - EXIT                  
         BH    CKXIT                                                            
         SPACE 1                                                                
ECKNEXT2 ZIC   R1,TIW4W4                                                        
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         CLI   MAINREAD,TLW4CDQ    IF MAIN READ WAS W4                          
         BNE   *+8                                                              
         STC   R1,TIW4W4           RESTORE CURRENT W4TYPE FROM W4 REC           
         MVC   TISSN,TLCKESSN                                                   
         MVC   TICUR,TLCKECUR                                                   
         MVC   TIEMP,TLCKEEMP                                                   
         MVC   TIAGY,TLCKEAGY                                                   
         MVC   TIAGT,TLCKEAGT                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   ECKNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               CODES FROM ACTIVE KEY                        
         USING TLCKD,R4                                                         
         MVC   TIAGY,TLCKAGY                                                    
         MVC   TICAT,TLCKCAT                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   ECKNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     ECKNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - EMPLOYEE'S YTD CHECKS                             
*                             RANGE OF RECORDS FOR 1 TAX UNIT                   
*                             REQUIRED - UNIT/PERIOD                            
         SPACE 3                                                                
YCHKS    NTR1                                                                   
         BRAS  RE,YCHECK                                                        
         J     XIT                                                              
         EJECT                                                                  
*              I/O ROUTINES - DUE COMPANY TRACKING                              
         SPACE 3                                                                
DCHECK   NTR1                                                                   
         USING TLCKPD,R4                                                        
         OI    TISTAT,TISTRDCK     SET READ FROM CHKDIR/CHKFIL                  
         MVI   LKEY,0                                                           
         MVC   TLCKDSSN,TIFSSN     POSSIBLE SS# FILTER                          
         TM    TLCKDSSN,X'C0'                                                   
         BNO   DCKHIGH                                                          
         MVC   LKEY,LCKDSSN                                                     
         MVC   TLCKDDUC,TIQSTART   CAN BE FOR 1 DUE COMPANY CODE                
         TM    TLCKDDUC,X'C0'                                                   
         BNO   DCKHIGH                                                          
         MVC   LKEY,LCKDDUC                                                     
         SPACE 1                                                                
DCKHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
DCKNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   CKXIT                                                            
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TISSN,TLCKDSSN                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   DCKNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               CODES FROM ACTIVE KEY                        
         USING TLCKD,R4                                                         
         MVC   TIAGY,TLCKAGY                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   DCKNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     DCKNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - LIEN TRACKING                                     
         SPACE 3                                                                
LCHECK   NTR1                                                                   
         USING TLCKPD,R4                                                        
         OI    TISTAT,TISTRDCK     SET READ FROM CHKDIR/CHKFIL                  
         MVI   LKEY,0                                                           
         MVC   TLCKLSSN,TIFSSN     POSSIBLE SS# FILTER                          
         TM    TLCKLSSN,X'C0'                                                   
         BNO   LCKHIGH                                                          
         MVC   LKEY,LCKLSSN                                                     
         MVC   TLCKLLIN,TIQSTART   CAN BE FOR 1 DUE COMPANY CODE                
         TM    TLCKLLIN,X'C0'                                                   
         BNO   LCKHIGH                                                          
         MVC   LKEY,LCKLLIN                                                     
         SPACE 1                                                                
LCKHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
LCKNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   CKXIT                                                            
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TISSN,TLCKDSSN                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   LCKNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               CODES FROM ACTIVE KEY                        
         USING TLCKD,R4                                                         
         MVC   TIAGY,TLCKAGY                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   LCKNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     LCKNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - CHECKS PASSIVE                                    
         SPACE 3                                                                
CCHECK   NTR1                                                                   
         USING TLCKPD,R4                                                        
         OI    TISTAT,TISTRDCK     SET READ FROM CHKDIR/CHKFIL                  
         MVI   LKEY,0                                                           
*******  MVC   TLCKCBNK,?????      BANK?                                        
         SPACE 1                                                                
CCKHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
CCKNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   CKXIT                                                            
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   CCKNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               CODES FROM ACTIVE KEY                        
         USING TLCKD,R4                                                         
         MVC   TIAGY,TLCKAGY                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   CCKNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     CCKNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - LOCAL                                             
         SPACE 3                                                                
LOCAL    NTR1                                                                   
         USING TLLOD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLLOUN,TIFUN        POSSIBLE UNION                               
         TM    TLLOUN,X'C0'                                                     
         BNO   *+10                                                             
         MVC   LKEY,LLOUN                                                       
         MVC   TLLOLCL,TIQSTART    OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
LONEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIUN,TLLOUN                                                      
         MVC   TILOCL,TLLOLCL                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   LONEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   LONEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     LONEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - CONTROL                                           
         SPACE 3                                                                
CONTROL  NTR1                                                                   
         USING TLCTD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLCTAGY,TIFAGY      AGENCY                                       
         TM    TLCTAGY,X'C0'                                                    
         BNO   *+10                                                             
         MVC   LKEY,LCTAGY                                                      
         MVC   TLCTCLI,TIQSTART    OPTIONAL START CLIENT                        
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
CTNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLCTAGY                                                    
         MVC   TICLI,TLCTCLI                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   CTNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   CTNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     CTNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - ESTIMATE                                          
         SPACE 3                                                                
ESTIMATE NTR1                                                                   
         USING TLESD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLESAGY,TIFAGY      POSSIBLE AGENCY                              
         TM    TLESAGY,X'C0'                                                    
         BNO   ESHIGH                                                           
         MVC   LKEY,LESAGY                                                      
         MVC   TLESEST,TIQSTART    OPTIONAL START                               
         SPACE 1                                                                
ESHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
ESNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLESAGY                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   ESNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   ESNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     ESNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - SESSION ESTIMATES                                 
         SPACE 3                                                                
SESSEST  NTR1                                                                   
         USING TLSSD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLSSTYPE,TIFTYPE    POSSIBLE TYPE CODE                           
         TM    TLSSTYPE,X'C0'                                                   
         BNO   SSHIGH                                                           
         MVC   LKEY,LSSTYPE                                                     
         SPACE 1                                                                
         MVC   TLSSAGY,TIFAGY      POSSIBLE AGENCY                              
         TM    TLSSAGY,X'C0'                                                    
         BNO   SSHIGH                                                           
         MVC   LKEY,LSSAGY                                                      
         MVC   TLSSEST,TIQSTART    OPTIONAL START                               
         SPACE 1                                                                
SSHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
SSNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TITYPE,TLSSTYPE                                                  
         MVC   TIAGY,TLSSAGY                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   SSNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   SSNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     SSNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - OFFICE                                            
         SPACE 3                                                                
OFF      NTR1                                                                   
         USING TLOFD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLOFOFF,TIQSTART    OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
OFNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIOFF,TLOFOFF                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   OFNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   OFNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     OFNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - BALANCE                                           
         SPACE 3                                                                
BAL      NTR1                                                                   
         USING TLBAD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLBADATE,TIQSTART   OPTIONAL START                               
         BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
BANEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   BANEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   BANEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     BANEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - GENERAL LISTS                                     
         SPACE 3                                                                
GLIST    NTR1                                                                   
         USING TLGLD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLGLTYPE,TIFGLTY    POSSIBLE TYPE CODE                           
         TM    TLGLTYPE,X'C0'                                                   
         BNO   GLHIGH                                                           
         MVC   LKEY,LGLTYPE                                                     
         MVC   TLGLLST,TIQSTART    OPTIONAL START                               
         SPACE 1                                                                
GLHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
GLNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIGLTY,TLGLTYPE                                                  
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   GLNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   GLNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     GLNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - GENERAL LISTS                                     
         SPACE 3                                                                
NGLIST   NTR1                                                                   
         USING TLGLPD,R4                                                        
         MVI   LKEY,1                                                           
         MVC   TLGLNTYP,TIFGLTY    POSSIBLE TYPE CODE                           
         TM    TLGLNTYP,X'C0'                                                   
         BNO   NGLHIGH                                                          
         MVI   LKEY,1                                                           
         MVC   TLGLNNAM,TIQSTART    OPTIONAL START                              
         SPACE 1                                                                
NGLHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
NGLNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIGLTY,TLGLNTYP                                                  
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   NGLNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NGLNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     NGLNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - JOB RECORDS                                       
         SPACE 3                                                                
JOB      NTR1                                                                   
         USING TLJBD,R4                                                         
         MVI   LKEY,0                                                           
*                                                                               
         TM    TIQFLAG3,TIQFJWTJ   READING JWT JOBS?                            
         BO    JB100                                                            
         TM    TIQFLAG4,TIQFBBDJ   READING BBDO JOBS?                           
         BO    JB110                                                            
*                                                                               
         MVC   TLJBAGY,TIFAGY      POSSIBLE AGENCY                              
         TM    TLJBAGY,X'C0'                                                    
         BNO   JBHIGH                                                           
         MVC   LKEY,LJBAGY                                                      
         MVC   TLJBDTE,TIQPSTR     POSSIBLE DATE VALID                          
         CLI   TLJBDTE,0                                                        
         BE    JBHIGH                                                           
         MVC   LKEY,LJBDTE                                                      
         MVC   TLJBCLI,TIFCLI      POSSIBLE CLIENT                              
         TM    TLJBCLI,X'C0'                                                    
         BNO   JBHIGH                                                           
         MVC   LKEY,LJBCLI                                                      
         MVC   TLJBPRD,TIFPRD      POSSIBLE PRODUCT                             
         TM    TLJBPRD,X'C0'                                                    
         BNO   JBHIGH                                                           
         MVC   LKEY,LJBPRD                                                      
         B     JBHIGH                                                           
*                                                                               
JB100    MVI   LKEY,1                                                           
         MVI   TLJBSPCL,TLJBSPJW   JWT JOBS                                     
         MVC   TLJBPRJI,TIFEST     POSSIBLE JOB                                 
         B     JBHIGH                                                           
*                                                                               
JB110    MVI   LKEY,1                                                           
         MVI   TLJBSPCL,TLJBSPBD   BBDO JOBS                                    
         MVC   TLJBPROJ,TIFEST     POSSIBLE JOB                                 
*                                                                               
JBHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
JBNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
*                                                                               
         TM    TIQFLAG3,TIQFJWTJ   READING JWT JOBS?                            
         BO    JB200                                                            
         TM    TIQFLAG4,TIQFBBDJ   READING BBDO JOBS?                           
         BO    JB210                                                            
*                                                                               
         MVC   TIAGY,TLJBAGY                                                    
         MVC   TIDATE,TLJBDTE                                                   
         MVC   TICLI,TLJBCLI                                                    
         MVC   TIPRD,TLJBPRD                                                    
         B     JB300                                                            
*                                                                               
JB200    XC    TIEST,TIEST                                                      
         MVC   TIEST(L'TLJBPRJI),TLJBPRJI                                       
         B     JB300                                                            
*                                                                               
JB210    XC    TIEST,TIEST                                                      
         MVC   TIEST(L'TLJBPROJ),TLJBPROJ                                       
*                                                                               
JB300    BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   JBNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   JBNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     JBNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - TIME                                              
         SPACE 3                                                                
TIME     NTR1                                                                   
         USING TLTMD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLTMCOM,TIFCOM      POSSIBLE COMMERCIAL                          
         OC    TLTMCOM,TLTMCOM                                                  
         BZ    TMHIGH                                                           
         MVC   LKEY,LTMCOM                                                      
         MVC   TLTMINV,TIFINV      POSSIBLE INVOICE                             
         OC    TLTMINV,TLTMINV                                                  
         BZ    TMHIGH                                                           
         MVC   LKEY,LTMINV                                                      
TMHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
TMNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TICOM,TLTMCOM                                                    
         MVC   TIINV,TLTMINV                                                    
         XC    TIINV,=XL6'FFFFFFFFFFFF'                                         
         MVC   TISSN,TLTMSSN                                                    
         MVC   TICAT,TLTMCAT                                                    
         MVC   TICASEQ,TLTMSORT+4                                               
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   TMNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   TMNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     TMNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - ADVICE RECORDS                                    
         SPACE 3                                                                
ADVICE   NTR1                                                                   
         USING TLDVD,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLDVAGY,TIFAGY      POSSIBLE AGENCY                              
         TM    TLDVAGY,X'C0'                                                    
         BNO   ADHIGH                                                           
         MVC   LKEY,LDVAGY                                                      
         MVC   TLDVCID,TIQSTART    OPTIONAL START                               
         SPACE 1                                                                
ADHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
ADNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLDVAGY                                                    
         MVC   TICID,TLDVCID                                                    
         MVC   TIADV,TLDVADV                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   ADNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   ADNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     ADNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - ADVICE BY OFFICE                                  
         SPACE 3                                                                
OADVICE  NTR1                                                                   
         USING TLDVPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLDVOOFF,TIFOFF     POSSIBLE OFFICE                              
         TM    TLDVOOFF,X'C0'                                                   
         BNO   OADHIGH                                                          
         MVC   LKEY,LDVOOFF                                                     
         MVC   TLDVOAGY,TIFAGY     POSSIBLE AGENCY                              
         TM    TLDVOAGY,X'C0'                                                   
         BNO   OADHIGH                                                          
         MVC   LKEY,LDVOAGY                                                     
         MVC   TLDVODTE,TIQPSTR    POSSIBLE SEND DATE                           
         CLI   TLDVODTE,0                                                       
         BE    OADHIGH                                                          
         MVC   LKEY,LDVODTE                                                     
         MVC   TLDVOCID,TIQSTART   OPTIONAL START                               
         SPACE 1                                                                
OADHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
OADNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIOFF,TLDVOOFF                                                   
         MVC   TIAGY,TLDVOAGY                                                   
         MVC   TICID,TLDVOCID                                                   
         MVC   TIADV,TLDVOADV                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   OADNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   OADNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     OADNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - ADVICE BY DUE DATE                                
         SPACE 3                                                                
DADVICE  NTR1                                                                   
         USING TLDVPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLDVEOFF,TIFOFF     POSSIBLE OFFICE                              
         TM    TLDVEOFF,X'C0'                                                   
         BNO   DADHIGH                                                          
         MVC   LKEY,LDVEOFF                                                     
         MVC   TLDVEAGY,TIFAGY     POSSIBLE AGENCY                              
         TM    TLDVEAGY,X'C0'                                                   
         BNO   DADHIGH                                                          
         MVC   LKEY,LDVEAGY                                                     
         SPACE 1                                                                
DADHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
DADNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIOFF,TLDVEOFF                                                   
         MVC   TIAGY,TLDVEAGY                                                   
         MVC   TICID,TLDVECID                                                   
         MVC   TIADV,TLDVEADV                                                   
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   DADNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   DADNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     DADNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - W2 RECORDS                                        
         SPACE 3                                                                
W2S      NTR1                                                                   
         BRAS  RE,W2SUB                                                         
         J     XIT                                                              
*              I/O ROUTINES - CONTRACT RECORDS                                  
         SPACE 3                                                                
CONTR    NTR1                                                                   
         USING TLCND,R4                                                         
         MVI   LKEY,0                                                           
         MVC   TLCNAGY,TIFAGY      POSSIBLE AGENCY                              
         TM    TLCNAGY,X'C0'                                                    
         BNO   CONHIGH                                                          
         MVC   LKEY,LCNAGY                                                      
         MVC   TLCNCNID,TIQSTART                                                
         SPACE 1                                                                
CONHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
CONNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLCNAGY                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   CONNEXT                                                          
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   CONNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     CONNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - CONTRACT RECORDS BY COMMERCIAL                    
         SPACE 3                                                                
CONTRC   NTR1                                                                   
         USING TLCNPD,R4                                                        
         MVI   LKEY,0                                                           
         MVC   TLCNPCOM,TIFCOM     POSSIBLE COMM ID                             
         TM    TLCNPCOM,X'C0'                                                   
         BNO   CONHIGH                                                          
         MVC   LKEY,LCNTCOM                                                     
         MVC   TLCNPAGY,TIFAGY                                                  
         MVC   TLCNPCND,TIQSTART                                                
         SPACE 1                                                                
CONCHIGH BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
CONCNEXT BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   CONCNEXT                                                         
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   CONCNEXT                                                         
         BAS   RE,GOHOOK                                                        
         B     CONCNEXT                                                         
         EJECT                                                                  
*              I/O ROUTINES - COMMERCIAL GROUP                                  
         SPACE 3                                                                
OGROUP   NTR1                                                                   
         USING TLOGD,R4                                                         
         MVC   TLOGAGY,TIFAGY      POSSIBLE AGENCY                              
         TM    TLOGAGY,X'C0'                                                    
         BNO   OGHIGH                                                           
         MVC   LKEY,LOGAGY                                                      
         MVC   TLOGCLI,TIFCLI      POSSIBLE CLIENT                              
         OC    TIFCLI,TIFCLI                                                    
         BNZ   *+10                                                             
         MVC   TLOGCLI,TIQSTART    OPTIONAL START                               
         TM    TLOGCLI,X'C0'                                                    
         BNO   OGHIGH                                                           
         MVC   LKEY,LOGCLI                                                      
         MVC   TLOGPRD,TIFPRD      POSSIBLE PRODUCT                             
         TM    TLOGPRD,X'C0'                                                    
         BNO   OGHIGH                                                           
         MVC   LKEY,LOGPRD                                                      
         SPACE 1                                                                
OGHIGH   BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
OGNEXT   BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         CLC   KEY(1),KEYSAVE                                                   
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLOGAGY                                                    
         MVC   TICLI,TLOGCLI                                                    
         MVC   TIPRD,TLOGPRD                                                    
         MVC   TIPRG,TLOGCOG                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   OGNEXT                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   OGNEXT                                                           
         BAS   RE,GOHOOK                                                        
         B     OGNEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - COMMERCIAL GROUP NAME                             
         SPACE 3                                                                
NOGROUP  NTR1                                                                   
         USING TLOGPD,R4                                                        
         MVC   TLOGNAGY,TIFAGY    POSSIBLE AGENCY                               
         TM    TLOGNAGY,X'C0'                                                   
         BNO   NOGHIGH                                                          
         MVC   LKEY,LOGNAGY                                                     
         MVC   TLOGNAME,TIQSTART  OPTIONAL START                                
         SPACE 1                                                                
NOGHIGH  BAS   RE,FIRSTHI                                                       
         B     *+8                                                              
         SPACE 1                                                                
NOGNEXT  BAS   RE,SEQ                                                           
         BAS   RE,KEYCOMP                                                       
         BNE   XIT                                                              
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLOGNAGY                                                   
         BAS   RE,FILTKEY                                                       
         BNE   NOGNEXT                                                          
         BAS   RE,GETREC                                                        
         LA    R4,IO               SET CODES FROM ACTIVE KEY                    
         USING TLOGD,R4                                                         
         MVC   TICLI,TLOGCLI                                                    
         MVC   TIPRD,TLOGPRD                                                    
         MVC   TIPRG,TLOGCOG                                                    
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   NOGNEXT                                                          
         BAS   RE,GOHOOK                                                        
         B     NOGNEXT                                                          
         EJECT                                                                  
*              I/O ROUTINES - T4 / RL1 RECORDS                                  
         SPACE 3                                                                
T4RL1S   NTR1                                                                   
         BRAS  RE,T4RL1                                                         
         J     XIT                                                              
         EJECT                                                                  
*              I/O ROUTINES - ARCHIVED ADVICES                                  
         SPACE 3                                                                
PADVICES NTR1                                                                   
         BRAS  RE,PADVICE                                                       
         J     XIT                                                              
         EJECT                                                                  
*              CONTROL ACCESS TO UTILITIES MODULE                               
         SPACE 3                                                                
FILTKEY  NTR1                                                                   
         SR    R2,R2                                                            
         B     IOUTIL                                                           
         SPACE 1                                                                
FILTREC  NTR1                                                                   
         LA    R2,1                                                             
         B     IOUTIL                                                           
         SPACE 1                                                                
FILTCOMM NTR1                                                                   
         LA    R2,2                                                             
         B     IOUTIL                                                           
         SPACE 1                                                                
POSTREC  NTR1                                                                   
         LA    R2,3                                                             
         B     IOUTIL                                                           
         SPACE 1                                                                
SETLISTS NTR1                      READ ANY FILTER LISTS IF REQUESTED           
         LA    R2,4                                                             
         B     IOUTIL                                                           
         SPACE 1                                                                
IOUTIL   L     RF,=V(TASYSIOB)     NOW GO TO EXECUTE UTILS                      
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,((R2),(RC)),(R7)                                       
         B     XIT                                                              
         EJECT                                                                  
*              DATA MANAGER INTERFACE                                           
         SPACE 1                                                                
READ     MVC   COMMAND,=CL6'DMREAD'                                             
         MVC   KEYSAVE,KEY                                                      
         B     READDIR                                                          
         SPACE 1                                                                
SEQ      MVC   COMMAND,=CL6'DMRSEQ'                                             
         B     READDIR                                                          
         SPACE 1                                                                
FIRSTHI  OC    TIQSKEY,TIQSKEY     POSSIBLE OVERRIDING KEY                      
         BZ    HIGH                                                             
         MVC   KEY(32),TIQSKEY                                                  
         SPACE 1                                                                
HIGH     MVC   COMMAND,=CL6'DMRDHI'                                             
         MVC   KEYSAVE,KEY                                                      
         SPACE 1                                                                
READDIR  NTR1                                                                   
         BAS   RE,CATCHIO          ENSURE DON'T DO TOO MANY IO'S                
         TM    TISTAT,TISTRDCK                                                  
         BNO   *+10                                                             
         MVC   DIRNAME,=CL8'CHKDIR'                                             
         GOTO1 ,DMCB,(DMINBITS,COMMAND),DIRNAME,KEY,KEY                         
         TM    TIQFLAGS,TIQFUPDR   TEST READ FOR UPDATE REQUESTED               
         BZ    *+8                                                              
         OI    0(R1),X'80'         SET APPROPRIATE BIT IN HOB OF P1             
         GOTO1 DATAMGR                                                          
         MVC   DIRNAME,=CL8'TALDIR'                                             
         MVC   TIKEY,KEY                   PASS BACK KEY                        
         MVC   TIKEYST,KEY+TLDRSTAT-TLDRD  STATUS BYTES                         
         MVC   TIDSKADD,KEY+TLDRDA-TLDRD   AND DISK ADDRESS                     
         TM    DMCB+8,X'12'                                                     
         BNZ   NOTOK               RECORD NOT FOUND/DELETED                     
         B     DMCHECK                                                          
         SPACE 1                                                                
GETREC   MVC   COMMAND,=CL6'GETREC'                                             
         B     READFILE                                                         
         SPACE 1                                                                
PUTREC   MVC   COMMAND,=CL6'PUTREC'                                             
         SPACE 1                                                                
READFILE NTR1                                                                   
         TM    TISTAT,TISTRDCK                                                  
         BNO   *+10                                                             
         MVC   FILENAME,=CL8'CHKFIL'                                            
         GOTO1 ,DMCB,(DMINBITS,COMMAND),FILENAME,KEY+34,IO,DMWORK               
         TM    TIQFLAGS,TIQFUPRC   TEST READ FOR UPDATE REQUESTED               
         BZ    *+8                                                              
         OI    0(R1),X'80'         SET APPROPRIATE BIT IN HOB OF P1             
         GOTO1 DATAMGR                                                          
         MVC   FILENAME,=CL8'TALFIL'                                            
         LA    R1,IO               PASS USER A(CURRENT RECORD)                  
         ST    R1,TIAREC                                                        
         SPACE 1                                                                
DMCHECK  CLI   DMCB+8,0                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*        ENSURE DON'T DO TOO MANY I/O'S ONLINE                                  
         SPACE 1                                                                
CATCHIO  NTR1                                                                   
         GOTO1 GETFACT,DMCB,0      GET A(SYSTEM INFO BLOCK)                     
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         OC    FATMAXIO,FATMAXIO   IF MAXIMUM I/O = 0                           
         BZ    XIT                 DON'T BOTHER                                 
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         ICM   R3,3,FATMAXIO       MAXIMUM ALLOWABLE IOS                        
         MH    R3,=H'9'                                                         
         D     R2,=F'10'           90 PERCENT OF MAX IOS IN R3                  
         CLM   R3,3,FATIOCNT       TEST RUNNING OUT OF IOS                      
         BH    XIT                 NO - STILL WITHIN 90 PERCENT                 
         MVI   TIERROR,TINOTOLN    JOB CANNOT BE RUN ONLINE                     
         B     ERRXIT                                                           
         EJECT                                                                  
*              ASSORTED ROUTINES                                                
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
ERRXIT   L     R1,USERRD           RETURN TO USER WITH ERROR                    
         L     RD,8(R1)                                                         
         B     XIT                                                              
         SPACE 1                                                                
KEYCOMP  ZIC   R1,LKEY             VARAIABLE LENGTH FOR COMPARISON              
         EX    R1,*+6              SET CC                                       
         BR    RE                  AND EXIT                                     
         CLC   KEY(0),KEYSAVE                                                   
         SPACE 1                                                                
INVLKEY  DS    0H                                                               
         ZIC   R1,LKEY             ADJUST LKEY BASED ON TIFINVD                 
         CLI   TIFINVD+1,C'*'      IF YEAR IS WILDCARD                          
         BNE   *+12                                                             
         AHI   R1,-6               SUBTRACT 6 FROM LKEY                         
         B     INVLX                                                            
         CLI   TIFINVD,C'*'        IF MONTH IS WILDCARD                         
         BNE   *+12                                                             
         AHI   R1,-4               SUBTRACT 4 FROM LKEY                         
         B     INVLX                                                            
         CLI   TIFINVD+2,C'*'      IF 1ST INV NUM IS WILDCARD                   
         BNE   *+12                                                             
         AHI   R1,-3               MINUS 3 FROM LKEY (2ND-4TH IS ALSO)          
         B     INVLX                                                            
         CLI   TIFINVD+4,C'*'      IF 3RD INV NUM IS WILDCARD                   
         BNE   *+8                                                              
         AHI   R1,-2               SUBTRACT 2 FROM LKEY (4TH IS ALSO)           
INVLX    STC   R1,LKEY                                                          
         BR    RE                                                               
         SPACE 1                                                                
OK       SR    R1,R1                                                            
         B     *+8                                                              
         SPACE 1                                                                
NOTOK    LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINES CONTROL KEY SETTING AND I/O HOOKS TO CALLER             
         SPACE 1                                                                
GOHOOK   NTR1                                                                   
         MVI   TIMODE,PROCREC      HAVE A FILE RECORD                           
         BAS   RE,IOHOOK                                                        
         B     HOOKEND                                                          
         SPACE 1                                                                
GOHKCOMM NTR1                                                                   
         MVI   TIMODE,PROCCOMM     HAVE AN COMMERCIAL RECORD                    
         BAS   RE,IOHOOK                                                        
         B     HOOKEND                                                          
         SPACE 1                                                                
GOHKINV  NTR1                                                                   
         MVI   TIMODE,PROCINV      HAVE AN INVOICE RECORD                       
         BAS   RE,IOHOOK                                                        
         SPACE 1                                                                
HOOKEND  CLI   TIMODE,PROCPTRC     USER MAY REQUEST A PUTREC                    
         BNE   *+8                                                              
         BAS   RE,PUTREC                                                        
         TM    TIQFLAG3,TIQFUNLK   IF ALSO REQUEST AN UNLOCK                    
         BZ    XIT                                                              
         GOTO1 DATAMGR,DMCB,=C'DMUNLK',DIRNAME                                  
         GOTO1 DATAMGR,DMCB,=C'DMUNLK',FILENAME                                 
         NI    TIQFLAG3,X'FF'-TIQFUNLK  MAKE SURE TO TURN IT OFF                
         B     XIT                                                              
         SPACE 3                                                                
GOHKDIR  DS    0H                                                               
         MVI   TIMODE,PROCDIR      HAVE A DIRECTORY RECORD                      
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         OI    STATUS,FOUNDONE     SET WE PASSED A RECORD TO APPLIC.            
         L     RF,TIHOOK                                                        
         L     RE,USERRD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF               MAY RETURN CC                                
         XIT1                                                                   
         SPACE 3                                                                
SETKEY   NTR1                                                                   
         ICM   RF,15,TIKHOOK       TEST USER HOOK TO SET KEY                    
         BZ    XIT                                                              
         GOTO1 ,DMCB,(MAINREAD,TIQSKEY) BUILD PARAMETER LIST                    
         L     RE,USERRD           RE=USER'S RD                                 
         L     RC,68(RE)           RESTORE USER'S W/S REGISTER                  
         BASR  RE,RF               OFF TO USER                                  
         B     XIT                                                              
         EJECT                                                                  
*              CONSTANTS TABLES ETC                                             
         SPACE 1                                                                
DATADISP DC    H'40'                                                            
DMINBITS DC    X'08'                                                            
         SPACE 1                                                                
LSTUSER  DC    AL1(TLSTUSER+L'TLSTUSER-TLSTKEY-1)                               
LAYGAGG  DC    AL1(TLAYGAGG+L'TLAYGAGG-TLAYPKEY-1)                              
LATAGY   DC    AL1(TLATAGY+L'TLATAGY-TLATKEY-1)                                 
LCLAGY   DC    AL1(TLCLAGY+L'TLCLAGY-TLCLKEY-1)                                 
LCLNAGY  DC    AL1(TLCLNAGY+L'TLCLNAGY-TLCLPKEY-1)                              
LCLGCLG  DC    AL1(TLCLGCLG+L'TLCLGCLG-TLCLPKEY-1)                              
LPRAGY   DC    AL1(TLPRAGY+L'TLPRAGY-TLPRKEY-1)                                 
LPRCLI   DC    AL1(TLPRCLI+L'TLPRCLI-TLPRKEY-1)                                 
LTRAGY   DC    AL1(TLTRAGY+L'TLTRAGY-TLTRKEY-1)                                 
LTRCLI   DC    AL1(TLTRCLI+L'TLTRCLI-TLTRKEY-1)                                 
LPRNAGY  DC    AL1(TLPRNAGY+L'TLPRNAGY-TLPRPKEY-1)                              
LPRNCLI  DC    AL1(TLPRNCLI+L'TLPRNCLI-TLPRPKEY-1)                              
LPRGAGY  DC    AL1(TLPRGAGY+L'TLPRGAGY-TLPRPKEY-1)                              
LPRGCLI  DC    AL1(TLPRGCLI+L'TLPRGCLI-TLPRPKEY-1)                              
LPRGPRG  DC    AL1(TLPRGPRG+L'TLPRGPRG-TLPRPKEY-1)                              
LPGAGY   DC    AL1(TLPGAGY+L'TLPGAGY-TLPGKEY-1)                                 
LPGCLI   DC    AL1(TLPGCLI+L'TLPGCLI-TLPGKEY-1)                                 
LPGNAGY  DC    AL1(TLPGNAGY+L'TLPGNAGY-TLPGPKEY-1)                              
LCOAGY   DC    AL1(TLCOAGY+L'TLCOAGY-TLCOKEY-1)                                 
LCOCLI   DC    AL1(TLCOCLI+L'TLCOCLI-TLCOKEY-1)                                 
LCOPRD   DC    AL1(TLCOPRD+L'TLCOPRD-TLCOKEY-1)                                 
LCOVAGY  DC    AL1(TLCOVAGY+L'TLCOVAGY-TLCOPKEY-1)                              
LCOVCLI  DC    AL1(TLCOVCLI+L'TLCOVCLI-TLCOPKEY-1)                              
LCOVPRD  DC    AL1(TLCOVPRD+L'TLCOVPRD-TLCOPKEY-1)                              
LCOIAGY  DC    AL1(TLCOIAGY+L'TLCOIAGY-TLCOPKEY-1)                              
LCOICID  DC    AL1(TLCOICID+L'TLCOICID-TLCOPKEY-1)                              
LCOCCOM  DC    AL1(TLCOCCOM+L'TLCOCCOM-TLCOPKEY-1)                              
LCOPAGY  DC    AL1(TLCOPAGY+L'TLCOPAGY-TLCOPKEY-1)                              
LCOPCLI  DC    AL1(TLCOPCLI+L'TLCOPCLI-TLCOPKEY-1)                              
LCOPPRD  DC    AL1(TLCOPPRD+L'TLCOPPRD-TLCOPKEY-1)                              
LCONCLI  DC    AL1(TLCONCLI+L'TLCONCLI-TLCOPKEY-1)                              
LCOMAGY  DC    AL1(TLCOMAGY+L'TLCOMAGY-TLCOPKEY-1)                              
LCOMMUS  DC    AL1(TLCOMMUS+L'TLCOMMUS-TLCOPKEY-1)                              
LCOGCLG  DC    AL1(TLCOGCLG+L'TLCOGCLG-TLCOPKEY-1)                              
LCOAAGY  DC    AL1(TLCOAAGY+L'TLCOAAGY-TLCOPKEY-1)                              
LCOLCLG  DC    AL1(TLCOLCLG+L'TLCOLCLG-TLCOPKEY-1)                              
LIFAGY   DC    AL1(TLIFAGY+L'TLIFAGY-TLIFKEY-1)                                 
LW4CCRP  DC    AL1(TLW4CCRP+L'TLW4CCRP-TLW4PKEY-1)                              
LGUSSN   DC    AL1(TLGUSSN+L'TLGUSSN-TLGUKEY-1)                                 
LGUGUA   DC    AL1(TLGUGUA+L'TLGUGUA-TLGUKEY-1)                                 
LSGSSN   DC    AL1(TLSGSSN+L'TLSGSSN-TLSGKEY-1)                                 
LGTSSN   DC    AL1(TLGTSSN+L'TLGTSSN-TLGTKEY-1)                                 
LGTGUA   DC    AL1(TLGTGUA+L'TLGTGUA-TLGTKEY-1)                                 
LGTSTRT  DC    AL1(TLGTSTRT+L'TLGTSTRT-TLGTKEY-1)                               
LFTSSN   DC    AL1(TLFTSSN+L'TLFTSSN-TLFTKEY-1)                                 
LFTCOM   DC    AL1(TLFTCOM+L'TLFTCOM-TLFTKEY-1)                                 
LFTCAST  DC    AL1(TLFTCAST+L'TLFTCAST-TLFTKEY-1)                               
LFTSTRT  DC    AL1(TLFTSTRT+L'TLFTSTRT-TLFTKEY-1)                               
LETSSN   DC    AL1(TLETSSN+L'TLETSSN-TLETKEY-1)                                 
LETCOM   DC    AL1(TLETCOM+L'TLETCOM-TLETKEY-1)                                 
LETCAT   DC    AL1(TLETCAT+L'TLETCAT-TLETKEY-1)                                 
LETEPI   DC    AL1(TLETEPI+L'TLETEPI-TLETKEY-1)                                 
LDUSSN   DC    AL1(TLDUSSN+L'TLDUSSN-TLDUKEY-1)                                 
LLNSSN   DC    AL1(TLLNSSN+L'TLLNSSN-TLLNKEY-1)                                 
LCAAAGT  DC    AL1(TLCAAAGT+L'TLCAAAGT-TLCAPKEY-1)                              
LCAASSN  DC    AL1(TLCAASSN+L'TLCAASSN-TLCAPKEY-1)                              
LCACSSN  DC    AL1(TLCACSSN+L'TLCACSSN-TLCAPKEY-1)                              
LCAGSSN  DC    AL1(TLCAGSSN+L'TLCAGSSN-TLCAPKEY-1)                              
LCAGGUA  DC    AL1(TLCAGGUA+L'TLCAGGUA-TLCAPKEY-1)                              
LCAGCOM  DC    AL1(TLCAGCOM+L'TLCAGCOM-TLCAPKEY-1)                              
LCACOM   DC    AL1(TLCACOM+L'TLCACOM-TLCAKEY-1)                                 
LCANCOM  DC    AL1(TLCANCOM+L'TLCANCOM-TLCAPKEY-1)                              
LDLAGY   DC    AL1(TLDLAGY+L'TLDLAGY-TLDLKEY-1)                                 
LNXAGY   DC    AL1(TLNXAGY+L'TLNXAGY-TLNXKEY-1)                                 
LNXNAGY  DC    AL1(TLNXNAGY+L'TLNXNAGY-TLNXPKEY-1)                              
LNXCAGY  DC    AL1(TLNXCAGY+L'TLNXCAGY-TLNXPKEY-1)                              
LAKAGY   DC    AL1(TLAKAGY+L'TLAKAGY-TLAKKEY-1)                                 
LINAGY   DC    AL1(TLINAGY+L'TLINAGY-TLINKEY-1)                                 
LININV   DC    AL1(TLININV+L'TLININV-TLINKEY-1)                                 
LINHCOM  DC    AL1(TLINHCOM+L'TLINHCOM-TLINPKEY-1)                              
LINHINV  DC    AL1(TLINHINV+L'TLINHINV-TLINPKEY-1)                              
LINBAGY  DC    AL1(TLINBAGY+L'TLINBAGY-TLINPKEY-1)                              
LINCAGY  DC    AL1(TLINCAGY+L'TLINCAGY-TLINPKEY-1)                              
LCKAGY   DC    AL1(TLCKAGY+L'TLCKAGY-TLCKKEY-1)                                 
LCKINV   DC    AL1(TLCKINV+L'TLCKINV-TLCKKEY-1)                                 
LCKHCOM  DC    AL1(TLCKHCOM+L'TLCKHCOM-TLCKPKEY-1)                              
LCKHSSN  DC    AL1(TLCKHSSN+L'TLCKHSSN-TLCKPKEY-1)                              
LCKHCAT  DC    AL1(TLCKHCAT+L'TLCKHCAT-TLCKPKEY-1)                              
LCKESSN  DC    AL1(TLCKESSN+L'TLCKESSN-TLCKPKEY-1)                              
LCKECUR  DC    AL1(TLCKECUR+L'TLCKECUR-TLCKPKEY-1)                              
LCKEEMP  DC    AL1(TLCKEEMP+L'TLCKEEMP-TLCKPKEY-1)                              
LCKYEAR  DC    AL1(TLCKYEAR+L'TLCKYEAR-TLCKPKEY-1)                              
LCKYCUR  DC    AL1(TLCKYCUR+L'TLCKYCUR-TLCKPKEY-1)                              
LCKYEMP  DC    AL1(TLCKYEMP+L'TLCKYEMP-TLCKPKEY-1)                              
LCKYSSN  DC    AL1(TLCKYSSN+L'TLCKYSSN-TLCKPKEY-1)                              
LCKYTXU  DC    AL1(TLCKYTXU+L'TLCKYTXU-TLCKPKEY-1)                              
LCKDSSN  DC    AL1(TLCKDSSN+L'TLCKDSSN-TLCKPKEY-1)                              
LCKDDUC  DC    AL1(TLCKDDUC+L'TLCKDDUC-TLCKPKEY-1)                              
LCKLSSN  DC    AL1(TLCKLSSN+L'TLCKLSSN-TLCKPKEY-1)                              
LCKLLIN  DC    AL1(TLCKLLIN+L'TLCKLLIN-TLCKPKEY-1)                              
LLOUN    DC    AL1(TLLOUN+L'TLLOUN-TLLOKEY-1)                                   
LCTAGY   DC    AL1(TLCTAGY+L'TLCTAGY-TLCTKEY-1)                                 
LECCOM   DC    AL1(TLECCOM+L'TLECCOM-TLECKEY-1)                                 
LECEPI   DC    AL1(TLECEPI+L'TLECEPI-TLECKEY-1)                                 
LECCSSN  DC    AL1(TLECCSSN+L'TLECCSSN-TLECPKEY-1)                              
LEPIAGY  DC    AL1(TLEPAGY+L'TLEPAGY-TLEPKEY-1)                                 
LESAGY   DC    AL1(TLESAGY+L'TLESAGY-TLESKEY-1)                                 
LSSTYPE  DC    AL1(TLSSTYPE+L'TLSSTYPE-TLSSKEY-1)                               
LSSAGY   DC    AL1(TLSSAGY+L'TLSSAGY-TLSSKEY-1)                                 
LGLTYPE  DC    AL1(TLGLTYPE+L'TLGLTYPE-TLGLKEY-1)                               
LJBAGY   DC    AL1(TLJBAGY+L'TLJBAGY-TLJBKEY-1)                                 
LJBDTE   DC    AL1(TLJBDTE+L'TLJBDTE-TLJBKEY-1)                                 
LJBCLI   DC    AL1(TLJBCLI+L'TLJBCLI-TLJBKEY-1)                                 
LJBPRD   DC    AL1(TLJBPRD+L'TLJBPRD-TLJBKEY-1)                                 
LDVAGY   DC    AL1(TLDVAGY+L'TLDVAGY-TLDVKEY-1)                                 
LDVOOFF  DC    AL1(TLDVOOFF+L'TLDVOOFF-TLDVPKEY-1)                              
LDVOAGY  DC    AL1(TLDVOAGY+L'TLDVOAGY-TLDVPKEY-1)                              
LDVODTE  DC    AL1(TLDVODTE+L'TLDVODTE-TLDVPKEY-1)                              
LDVEOFF  DC    AL1(TLDVEOFF+L'TLDVEOFF-TLDVPKEY-1)                              
LDVEAGY  DC    AL1(TLDVEAGY+L'TLDVEAGY-TLDVPKEY-1)                              
LW2YEAR  DC    AL1(TLW2YEAR+L'TLW2YEAR-TLW2KEY-1)                               
LUHCOM   DC    AL1(TLUHCOM+L'TLUHCOM-TLUHKEY-1)                                 
LW4SSN   DC    AL1(TLW4SSN+L'TLW4SSN-TLW4KEY-1)                                 
LW2CUR   DC    AL1(TLW2CUR+L'TLW2CUR-TLW2KEY-1)                                 
LW2EMP   DC    AL1(TLW2EMP+L'TLW2EMP-TLW2KEY-1)                                 
LW2SSN   DC    AL1(TLW2SSN+L'TLW2SSN-TLW2KEY-1)                                 
LTMCOM   DC    AL1(TLTMCOM+L'TLTMCOM-TLTMKEY-1)                                 
LTMINV   DC    AL1(TLTMINV+L'TLTMINV-TLTMKEY-1)                                 
LCNAGY   DC    AL1(TLCNAGY+L'TLCNAGY-TLCNKEY-1)                                 
LCNCNID  DC    AL1(TLCNCNID+L'TLCNCNID-TLCNKEY-1)                               
LCNTRMS  DC    AL1(TLCNTRMS+L'TLCNTRMS-TLCNKEY-1)                               
LCNTRME  DC    AL1(TLCNTRME+L'TLCNTRME-TLCNKEY-1)                               
LCNTCOM  DC    AL1(TLCNPCOM+L'TLCNPCOM-TLCNPKEY-1)                              
LOGAGY   DC    AL1(TLOGAGY+L'TLOGAGY-TLOGKEY-1)                                 
LOGCLI   DC    AL1(TLOGCLI+L'TLOGCLI-TLOGKEY-1)                                 
LOGPRD   DC    AL1(TLOGPRD+L'TLOGPRD-TLOGKEY-1)                                 
LOGNAGY  DC    AL1(TLOGNAGY+L'TLOGNAGY-TLOGPKEY-1)                              
LCGCCG   DC    AL1(TLCOGICG+L'TLCOGICG-TLCOPKEY-1)                              
LCGNCG   DC    AL1(TLCOGNCG+L'TLCOGNCG-TLCOPKEY-1)                              
LPTAGY   DC    AL1(TLPTAGY+L'TLPTAGY-TLPTKEY-1)                                 
LPTNAGY  DC    AL1(TLPTNAGY+L'TLPGNAGY-TLPTPKEY-1)                              
LEDAGY   DC    AL1(TLEDAGY+L'TLEDAGY-TLEDKEY-1)                                 
LT4YEAR  DC    AL1(TLT4YEAR+L'TLT4YEAR-TLT4KEY-1)                               
LT4CUR   DC    AL1(TLT4CUR+L'TLT4CUR-TLT4KEY-1)                                 
LT4EMP   DC    AL1(TLT4EMP+L'TLT4EMP-TLT4KEY-1)                                 
LT4SSN   DC    AL1(TLT4SSN+L'TLT4SSN-TLT4KEY-1)                                 
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
COELS    DS    0XL2                                                             
         DC    AL1(TAFLELQ,0)                                                   
         DC    AL1(TACOELQ,0)                                                   
         DC    AL1(TACSELQ,0)                                                   
         DC    AL1(TACCELQ,0)                                                   
         DC    AL1(TALFELQ,0)                                                   
         DC    AL1(TAVRELQ,0)                                                   
**NO-OP**DC    AL1(TAOCELQ,0)                                                   
         DC    AL1(TAFNELQ,TAFNTTTL)                                            
         DC    AL1(TAFNELQ,TAFNTPRD)                                            
         DC    AL1(TAFNELQ,TAFNTSVC)                                            
         DC    AL1(TAFNELQ,TAFNTPHO)                                            
         DC    AL1(TANUELQ,TANUTPHO)                                            
         DC    AL1(TANUELQ,TANUTSVC)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* GHO - 2012 JUL 25:                                                  *         
*       CHANGED LAYOUT FROM REC BYTE, AL3(ROUTINE DISPLACEMENT)       *         
*       TO REC BYTE, REC SUB BYTE, AL2(ROUTINE DISPLACEMENT)          *         
*---------------------------------------------------------------------*         
READADDS DS    0A                                                               
         DC    AL1(TLSTCDQ),AL1(0),AL2(STAFF-TASYSIO)                           
         DC    AL1(TLSTNCDQ),AL1(0),AL2(NSTAFF-TASYSIO)                         
         DC    AL1(TLAYCDQ),AL1(0),AL2(AGENCY-TASYSIO)                          
         DC    AL1(TLAYNCDQ),AL1(0),AL2(NAGENCY-TASYSIO)                        
         DC    AL1(TLAYGCDQ),AL1(0),AL2(GAGENCY-TASYSIO)                        
         DC    AL1(TLATCDQ),AL1(0),AL2(ATTN-TASYSIO)                            
         DC    AL1(TLAGCDQ),AL1(0),AL2(AGROUP-TASYSIO)                          
         DC    AL1(TLAGNCDQ),AL1(0),AL2(NAGROUP-TASYSIO)                        
         DC    AL1(TLBRCCDQ),AL1(0),AL2(BRATEC-TASYSIO)                         
         DC    AL1(TLBRNCDQ),AL1(0),AL2(BRATEN-TASYSIO)                         
         DC    AL1(TLCLCDQ),AL1(0),AL2(CLIENT-TASYSIO)                          
         DC    AL1(TLCLNCDQ),AL1(0),AL2(NCLIENT-TASYSIO)                        
         DC    AL1(TLCLGCDQ),AL1(0),AL2(GCLIENT-TASYSIO)                        
         DC    AL1(TLPMCDQ),AL1(TLPMSCDQ),AL2(PMTYPE-TASYSIO)                   
         DC    AL1(TLPMNCDQ),AL1(0),AL2(NPMTYPE-TASYSIO)                        
         DC    AL1(TLTKCDQ),AL1(TLTKSCDQ),AL2(TASK-TASYSIO)                     
         DC    AL1(TLTKNCDQ),AL1(0),AL2(NTASK-TASYSIO)                          
         DC    AL1(TLTRCDQ),AL1(TLTRSCDQ),AL2(TPROFILE-TASYSIO)                 
         DC    AL1(TLCGCDQ),AL1(0),AL2(CGROUP-TASYSIO)                          
         DC    AL1(TLCGNCDQ),AL1(0),AL2(NCGROUP-TASYSIO)                        
         DC    AL1(TLPRCDQ),AL1(0),AL2(PRODUCT-TASYSIO)                         
         DC    AL1(TLPRNCDQ),AL1(0),AL2(NPRODUCT-TASYSIO)                       
         DC    AL1(TLPRGCDQ),AL1(0),AL2(GPRODUCT-TASYSIO)                       
         DC    AL1(TLPGCDQ),AL1(0),AL2(PGROUP-TASYSIO)                          
         DC    AL1(TLPGNCDQ),AL1(0),AL2(NPGROUP-TASYSIO)                        
         DC    AL1(TLPTCDQ),AL1(0),AL2(PTYPE-TASYSIO)                           
         DC    AL1(TLPTNCDQ),AL1(0),AL2(NPTYPE-TASYSIO)                         
         DC    AL1(TLCOCDQ),AL1(0),AL2(COMM-TASYSIO)                            
         DC    AL1(TLCOVRDQ),AL1(0),AL2(COMMV-TASYSIO)                          
         DC    AL1(TLCOGIDQ),AL1(0),AL2(COMCGC-TASYSIO)                         
         DC    AL1(TLCOGNDQ),AL1(0),AL2(COMCGN-TASYSIO)                         
         DC    AL1(TLCONCDQ),AL1(0),AL2(NCOMM-TASYSIO)                          
         DC    AL1(TLCOICDQ),AL1(0),AL2(ICOMM-TASYSIO)                          
         DC    AL1(TLCOOCDQ),AL1(0),AL2(PMCOMM-TASYSIO)                         
         DC    AL1(TLCOPCDQ),AL1(0),AL2(PCOMM-TASYSIO)                          
         DC    AL1(TLCOMCDQ),AL1(0),AL2(MCOMM-TASYSIO)                          
         DC    AL1(TLCOGCDQ),AL1(0),AL2(GCOMM-TASYSIO)                          
         DC    AL1(TLCOLCDQ),AL1(0),AL2(LCOMM-TASYSIO)                          
         DC    AL1(TLCOACDQ),AL1(0),AL2(ACOMM-TASYSIO)                          
         DC    AL1(TLGCCDQ),AL1(0),AL2(GCON-TASYSIO)                            
         DC    AL1(TLOTCDQ),AL1(0),AL2(GCONTRK-TASYSIO)                         
         DC    AL1(TLMUCDQ),AL1(0),AL2(MUSIC-TASYSIO)                           
         DC    AL1(TLMUPCDQ),AL1(0),AL2(PMUSIC-TASYSIO)                         
         DC    AL1(TLANCDQ),AL1(0),AL2(AGENT-TASYSIO)                           
         DC    AL1(TLANNCDQ),AL1(0),AL2(NAGENT-TASYSIO)                         
         DC    AL1(TLANCCDQ),AL1(0),AL2(CAGENT-TASYSIO)                         
         DC    AL1(TLIFCDQ),AL1(0),AL2(INTER-TASYSIO)                           
         DC    AL1(TLEMCDQ),AL1(0),AL2(EMP-TASYSIO)                             
         DC    AL1(TLW4CDQ),AL1(0),AL2(W4-TASYSIO)                              
         DC    AL1(TLW4NCDQ),AL1(0),AL2(NW4-TASYSIO)                            
         DC    AL1(TLW4CCDQ),AL1(0),AL2(CW4-TASYSIO)                            
         DC    AL1(TLGUCDQ),AL1(0),AL2(GUAR-TASYSIO)                            
         DC    AL1(TLSGCDQ),AL1(0),AL2(SGUAR-TASYSIO)                           
         DC    AL1(TLGTCDQ),AL1(0),AL2(GTRACK-TASYSIO)                          
         DC    AL1(TLFTCDQ),AL1(0),AL2(FTRACK-TASYSIO)                          
         DC    AL1(TLETCDQ),AL1(0),AL2(ETRACK-TASYSIO)                          
         DC    AL1(TLDUCDQ),AL1(0),AL2(DUECOMP-TASYSIO)                         
         DC    AL1(TLLNCDQ),AL1(0),AL2(LIEN-TASYSIO)                            
         DC    AL1(TLCACDQ),AL1(0),AL2(CAST-TASYSIO)                            
         DC    AL1(TLCAACDQ),AL1(0),AL2(ACAST-TASYSIO)                          
         DC    AL1(TLCACCDQ),AL1(0),AL2(CCAST-TASYSIO)                          
         DC    AL1(TLCAGCDQ),AL1(0),AL2(GCAST-TASYSIO)                          
         DC    AL1(TLCANCDQ),AL1(0),AL2(NCAST-TASYSIO)                          
         DC    AL1(TLECCDQ),AL1(0),AL2(ECAST-TASYSIO)                           
         DC    AL1(TLECCCDQ),AL1(0),AL2(ECC-TASYSIO)                            
         DC    AL1(TLEPCDQ),AL1(0),AL2(EPISODE-TASYSIO)                         
         DC    AL1(TLARCDQ),AL1(0),AL2(AREA-TASYSIO)                            
         DC    AL1(TLUSCDQ),AL1(0),AL2(USE-TASYSIO)                             
         DC    AL1(TLDLCDQ),AL1(0),AL2(DEAL-TASYSIO)                            
         DC    AL1(TLNXCDQ),AL1(0),AL2(HOLD-TASYSIO)                            
         DC    AL1(TLNXNCDQ),AL1(0),AL2(NHOLD-TASYSIO)                          
         DC    AL1(TLNXCCDQ),AL1(0),AL2(CHOLD-TASYSIO)                          
         DC    AL1(TLAKCDQ),AL1(0),AL2(ALIAS-TASYSIO)                           
         DC    AL1(TLMTCDQ),AL1(0),AL2(MARKET-TASYSIO)                          
         DC    AL1(TLMTNMDQ),AL1(0),AL2(NMARKET-TASYSIO)                        
         DC    AL1(TLINCDQ),AL1(0),AL2(INVOICE-TASYSIO)                         
         DC    AL1(TLINHCDQ),AL1(0),AL2(HINVOICE-TASYSIO)                       
         DC    AL1(TLINBCDQ),AL1(0),AL2(BINVOICE-TASYSIO)                       
         DC    AL1(TLINCCDQ),AL1(0),AL2(CINVOICE-TASYSIO)                       
         DC    AL1(TLINDCDQ),AL1(0),AL2(DINVOICE-TASYSIO)                       
         DC    AL1(TLINKCDQ),AL1(0),AL2(KINVOICE-TASYSIO)                       
         DC    AL1(TLINOCDQ),AL1(0),AL2(OINVOICE-TASYSIO)                       
         DC    AL1(TLINPCDQ),AL1(0),AL2(PINVOICE-TASYSIO)                       
         DC    AL1(TLCKCDQ),AL1(0),AL2(CHECK-TASYSIO)                           
         DC    AL1(TLCKECDQ),AL1(0),AL2(ECHECK-TASYSIO)                         
         DC    AL1(TLCKYCDQ),AL1(0),AL2(YCHKS-TASYSIO)                          
         DC    AL1(TLCKHCDQ),AL1(0),AL2(HCHECK-TASYSIO)                         
         DC    AL1(TLCKDCDQ),AL1(0),AL2(DCHECK-TASYSIO)                         
         DC    AL1(TLCKLCDQ),AL1(0),AL2(LCHECK-TASYSIO)                         
         DC    AL1(TLCKCCDQ),AL1(0),AL2(CCHECK-TASYSIO)                         
         DC    AL1(TLLOCDQ),AL1(0),AL2(LOCAL-TASYSIO)                           
         DC    AL1(TLCTCDQ),AL1(0),AL2(CONTROL-TASYSIO)                         
         DC    AL1(TLESCDQ),AL1(0),AL2(ESTIMATE-TASYSIO)                        
         DC    AL1(TLSSCDQ),AL1(0),AL2(SESSEST-TASYSIO)                         
         DC    AL1(TLOFCDQ),AL1(0),AL2(OFF-TASYSIO)                             
         DC    AL1(TLBACDQ),AL1(0),AL2(BAL-TASYSIO)                             
         DC    AL1(TLDVCDQ),AL1(0),AL2(ADVICE-TASYSIO)                          
         DC    AL1(TLDVOCDQ),AL1(0),AL2(OADVICE-TASYSIO)                        
         DC    AL1(TLDVEDDQ),AL1(0),AL2(DADVICE-TASYSIO)                        
         DC    AL1(TLDVRDDQ),AL1(0),AL2(DADVICE-TASYSIO)                        
         DC    AL1(TLDVMDDQ),AL1(0),AL2(DADVICE-TASYSIO)                        
         DC    AL1(TLW2CDQ),AL1(0),AL2(W2S-TASYSIO)                             
         DC    AL1(TLGLCDQ),AL1(0),AL2(GLIST-TASYSIO)                           
         DC    AL1(TLGLNCDQ),AL1(0),AL2(NGLIST-TASYSIO)                         
         DC    AL1(TLUHCDQ),AL1(0),AL2(USAGE-TASYSIO)                           
         DC    AL1(TLJBCDQ),AL1(0),AL2(JOB-TASYSIO)                             
         DC    AL1(TLTMCDQ),AL1(0),AL2(TIME-TASYSIO)                            
         DC    AL1(TLCNCDQ),AL1(0),AL2(CONTR-TASYSIO)                           
         DC    AL1(TLCNPCDQ),AL1(0),AL2(CONTRC-TASYSIO)                         
         DC    AL1(TLOGCDQ),AL1(0),AL2(OGROUP-TASYSIO)                          
         DC    AL1(TLOGNCDQ),AL1(0),AL2(NOGROUP-TASYSIO)                        
         DC    AL1(TLT4CDQ),AL1(TLT4SCDQ),AL2(T4RL1S-TASYSIO)                   
         DC    AL1(TLR1CDQ),AL1(TLR1SCDQ),AL2(T4RL1S-TASYSIO)                   
         DC    AL1(TLPVCDQ),AL1(TLPVSCDQ),AL2(PADVICES-TASYSIO)                 
         DC    X'FF'                                                            
         EJECT                                                                  
*              I/O ROUTINES - EMPLOYEE'S YTD CHECKS                             
*                             RANGE OF RECORDS FOR 1 TAX UNIT                   
*                             REQUIRED - UNIT/PERIOD                            
         SPACE 3                                                                
YCHECK   NTR1  BASE=*,LABEL=*                                                   
         USING TLCKPD,R4                                                        
         TM    TIQFLAGS,TIQFSKIP                                                
         JO    YSKIP                                                            
         OI    TISTAT,TISTRDCK     SET READ FROM CHKDIR/CHKFIL                  
         GOTO1 DATCON,DMCB,(1,TIQPEND),(20,DUB)                                 
         MVC   TLCKYEAR,DUB                                                     
         MVC   LKEY,LCKYEAR                                                     
         MVC   TLCKYCUR,TIFCUR     OPTIONAL CURRENCY                            
         TM    TLCKYCUR,X'C0'                                                   
         JNO   YCKNXSSN                                                         
         MVC   LKEY,LCKYCUR                                                     
         MVC   TLCKYEMP,TIFEMP          AND EMPLOYER                            
         TM    TLCKYEMP,X'C0'                                                   
         JNO   YCKNXSSN                                                         
         MVC   LKEY,LCKYEMP                                                     
         MVC   TLCKYSSN,TIFSSN     SS#                                          
         TM    TLCKYSSN,X'C0'                                                   
         JNO   YCKHIGH                                                          
         MVC   LKEY,LCKYSSN                                                     
         J     YCKHIGH                                                          
                                                                                
YCKNXSSN LA    R4,KEY                                                           
         AI    TLCKYSSN+8,1        SKIP TO NEXT SS#                             
                                                                                
YCKHIGH  LA    R4,KEY                                                           
         MVC   TLCKYTXU,TIFUNIT    REQUIRED TAX UNIT                            
         MVC   TLCKYDTE,TIQPEND    END DATE IS REQUIRED                         
         XC    TLCKYDTE,=X'FFFFFF'                                              
         XC    TLCKYSEQ,TLCKYSEQ                                                
         MVI   TLCKYW4T,0                                                       
         MVI   TLCKYSTS,0                                                       
         MVI   TLCKYCSQ,0                                                       
         BRAS  RE,FIRSTHI                                                       
         J     *+8                                                              
                                                                                
YCKNEXT  BRAS  RE,SEQ                                                           
         BRAS  RE,KEYCOMP                                                       
         JNE   YCKXIT                                                           
         LA    R4,KEY                                                           
         CLI   TIFSSN,0            IF WE REQUESTED ALL SS#                      
         JNE   YCK20                                                            
         CLC   TLCKYTXU,TIFUNIT    DO WE HAVE UNIT YET?                         
         JL    YCKHIGH                NO - SKIP TO IT                           
         JE    YCK20                                                            
         JH    YCKNXSSN               PAST  - GO TO NEXT SS#                    
                                                                                
*                                  DATE CONTROL                                 
YCK20    MVC   THREE,TLCKYDTE      COMPLEMENT KEY DATE                          
         XC    THREE,=X'FFFFFF'                                                 
         CLC   THREE,TIQPSTR       DID WE GET PAST START                        
         JNL   YCKTHIS                                                          
         CLI   TIFSSN,0            YES - IS PARTICULAR SS# SELECTED?            
         JNE   YCKXIT                       YES - WE'RE THROUGH                 
         J     YCKNXSSN                     NO - SKIP TO NEXT SS#               
                                                                                
YCKTHIS  LA    R4,KEY                                                           
         CLI   TIFTUNIT,0          IF HAVE UNIT TYPE FILTER DO IT NOW           
         JE    YCK40                                                            
         MVC   BYTE,TIFTUNIT                                                    
         NC    BYTE,TLCKYSTS       'AND' WITH WITHHOLDING EL. STATUS            
         JZ    YCKNEXT             FILTER STATUS NOT SET                        
                                                                                
YCK40    XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIYEAR(2),TLCKYEAR+2                                             
         MVI   TIYEAR+2,X'40'                                                   
         MVC   TICUR,TLCKYCUR                                                   
         MVC   TIEMP,TLCKYEMP                                                   
         MVC   TISSN,TLCKYSSN                                                   
         MVC   TIUNIT,TLCKYTXU                                                  
         MVC   TIW4TY,TLCKYW4T                                                  
         BRAS  RE,FILTKEY          POSSIBLE KEY FILTER                          
         JNE   YCKNEXT                                                          
         BRAS  RE,GETREC                                                        
         LA    R4,IO               CODES FROM ACTIVE KEY                        
         USING TLCKD,R4                                                         
         MVC   TIAGY,TLCKAGY                                                    
         MVC   TICAT,TLCKCAT                                                    
         BRAS  RE,POSTREC          SET CODES FROM RECORD                        
         BRAS  RE,FILTREC          FILTER ON RECORD                             
         JNE   YCKNEXT                                                          
         BRAS  RE,GOHOOK                                                        
         J     YCKNEXT                                                          
                                                                                
YCKXIT   NI    TISTAT,X'FF'-TISTRDCK  SET NO LONGER READING CHECK RECS.         
         J     XIT                                                              
         EJECT                                                                  
*              I/O ROUTINES - EMPLOYEE'S YTD POINTERS - SKIPPING                
*                             1 RECORD FOR EACH TAX UNIT                        
*                             REQUIRED - CUR/EMP/NO UNIT/END DATE               
         SPACE 3                                                                
         USING TLCKPD,R4                                                        
YSKIP    OI    TISTAT,TISTRDCK     SET READ FROM CHKDIR/CHKFIL                  
         GOTO1 DATCON,DMCB,(1,TIQPEND),(20,DUB)                                 
         MVC   TLCKYEAR,DUB                                                     
         MVC   TLCKYCUR,TIFCUR     REQUIRED CURRENCY                            
         MVC   TLCKYEMP,TIFEMP              EMPLOYER                            
         MVC   LKEY,LCKYEMP                                                     
         MVC   TLCKYSSN,TIFSSN     SS#                                          
         TM    TLCKYSSN,X'C0'                                                   
         JNO   YSKNXSSN                                                         
         MVC   LKEY,LCKYSSN                                                     
         J     YSKHIGH                                                          
                                                                                
YSKNXSSN LA    R4,KEY                                                           
         AI    TLCKYSSN+8,1        SKIP TO NEXT SS#                             
         XC    TLCKYTXU,TLCKYTXU                                                
                                                                                
YSKNXTXU LA    R4,KEY                                                           
         AI    TLCKYTXU+2,1        SKIP TO NEXT TAX UNIT                        
                                                                                
YSKHIGH  LA    R4,KEY                                                           
         MVC   TLCKYDTE,TIQPEND    END DATE IS REQUIRED                         
         XC    TLCKYDTE,=X'FFFFFF'                                              
         XC    TLCKYSEQ,TLCKYSEQ                                                
         MVI   TLCKYW4T,0                                                       
         MVI   TLCKYSTS,0                                                       
         MVI   TLCKYCSQ,0                                                       
         BRAS  RE,FIRSTHI                                                       
         J     *+8                                                              
                                                                                
YSKNEXT  BRAS  RE,SEQ                                                           
         BRAS  RE,KEYCOMP                                                       
         JNE   CKXIT                                                            
         LA    R4,KEY                                                           
*                                  DATE CONTROL                                 
         MVC   THREE,TLCKYDTE      COMPLEMENT KEY DATE                          
         XC    THREE,=X'FFFFFF'                                                 
         CLC   THREE,TIQPEND       DID WE REACH END YET?                        
         JH    YSKHIGH             NO - SKIP TO IT                              
         CLC   THREE,TIQPSTR       DID WE GET PAST START                        
         JL    YSKNXTXU            YES - SKIP TO NEXT TAX UNIT                  
*                                                                               
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIYEAR(2),TLCKYEAR+2                                             
         MVI   TIYEAR+2,X'40'                                                   
         MVC   TIEMP,TLCKYEMP                                                   
         MVC   TICUR,TLCKYCUR                                                   
         MVC   TISSN,TLCKYSSN                                                   
         MVC   TIUNIT,TLCKYTXU                                                  
         MVC   TISKUNIT,TLCKYTXU   FIX SKIP MULTIPLIER BUG - JAN 2015           
         MVC   TIW4TY,TLCKYW4T                                                  
         BRAS  RE,FILTKEY          POSSIBLE KEY FILTER                          
         JNE   YSKNEXT                                                          
         BRAS  RE,GETREC                                                        
         LA    R4,IO               CODES FROM ACTIVE KEY                        
         USING TLCKD,R4                                                         
         MVC   TIAGY,TLCKAGY                                                    
         MVC   TICAT,TLCKCAT                                                    
         BRAS  RE,POSTREC          SET CODES FROM RECORD                        
         BRAS  RE,FILTREC          FILTER ON RECORD                             
         JNE   YSKNEXT                                                          
         BRAS  RE,GOHOOK                                                        
         J     YSKNXTXU            SKIPPING TO NEXT TAX UNIT                    
         EJECT                                                                  
*              I/O ROUTINES - W2 RECORDS                                        
W2SUB    NTR1  BASE=*,LABEL=*                                                   
         USING TLW2D,R4                                                         
         OI    TISTAT,TISTRDCK                                                  
         MVI   LKEY,1                                                           
         OC    TIFYEAR,TIFYEAR     MAKE SURE VALID                              
         JZ    W2S0100                                                          
         MVC   LKEY,LW2YEAR                                                     
         MVC   DUB(2),TIFYEAR                 YEAR IS REQUIRED                  
         MVC   DUB+2(4),=C'0101'              MAKE DUMMY DATE FIELD             
         GOTO1 DATCON,DMCB,(0,DUB),(20,WORK)  FOR DATCON TO GET CENTURY         
         J     W2S0300                                                          
*                                                                               
W2S0100  OC    TIQPEND,TIQPEND     USE END PERIOD YEAR                          
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   LKEY,LW2YEAR                                                     
         GOTO1 DATCON,DMCB,(1,TIQPEND),(20,WORK)                                
*                                                                               
W2S0300  MVC   TLW2YEAR,WORK                                                    
         MVC   TLW2CUR,TIFCUR      POSSIBLE CURRENCY                            
         TM    TLW2CUR,X'C0'                                                    
         JNO   W2HIGH                                                           
         MVC   LKEY,LW2CUR                                                      
         MVC   TLW2EMP,TIFEMP      POSSIBLE EMPLOYER                            
         TM    TLW2EMP,X'C0'                                                    
         JNO   W2HIGH                                                           
         MVC   LKEY,LW2EMP                                                      
         MVC   TLW2SSN,TIQSTART    OPTIONAL START                               
         SPACE 1                                                                
W2HIGH   BRAS  RE,FIRSTHI                                                       
         J     *+8                                                              
         SPACE 1                                                                
W2NEXT   BRAS  RE,SEQ                                                           
         BRAS  RE,KEYCOMP                                                       
         JNE   CKXIT                                                            
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIYEAR,TIFYEAR      (FUDGED -RETURNING REQUIRED FIELD)           
         MVC   TICUR,TLW2CUR                                                    
         MVC   TIEMP,TLW2EMP                                                    
         MVC   TISSN,TLW2SSN                                                    
         BRAS  RE,FILTKEY          POSSIBLE KEY FILTER                          
         JNE   W2NEXT                                                           
         BRAS  RE,GETREC                                                        
         BRAS  RE,POSTREC          SET CODES FROM RECORD                        
         BRAS  RE,FILTREC          FILTER ON RECORD                             
         JNE   W2NEXT                                                           
         BRAS  RE,GOHOOK                                                        
         J     W2NEXT                                                           
         EJECT                                                                  
*              I/O ROUTINES - T4 / RL-1 RECORDS                                 
         SPACE 3                                                                
T4RL1    NTR1  BASE=*,LABEL=*                                                   
         USING TLT4D,R4                                                         
         OI    TISTAT,TISTRDCK                                                  
         MVI   LKEY,1                                                           
         CLC   TIQPSTR(1),TIQPEND  YEAR SAME?                                   
         BNE   T4RL1HI                                                          
         MVC   LKEY,LT4YEAR                                                     
         GOTO1 DATCON,DMCB,(1,TIQPSTR),(20,WORK)                                
         MVC   TLT4YEAR,WORK                                                    
         MVC   TLT4CUR,TIFCUR      POSSIBLE CURRENCY                            
         TM    TLT4CUR,X'C0'                                                    
         JNO   T4RL1HI                                                          
         MVC   LKEY,LT4CUR                                                      
         MVC   TLT4EMP,TIFEMP      POSSIBLE EMPLOYER                            
         TM    TLT4EMP,X'C0'                                                    
         JNO   T4RL1HI                                                          
         MVC   LKEY,LT4EMP                                                      
         MVC   TLT4SSN,TIQSTART    OPTIONAL START                               
                                                                                
T4RL1HI  BRAS  RE,FIRSTHI                                                       
         B     *+8                                                              
T4RL1NXT BRAS  RE,SEQ                                                           
         BRAS  RE,KEYCOMP                                                       
         JNE   CKXIT                                                            
         LA    R4,KEY                                                           
         TM    TLDRSTAT-TLDRD(R4),X'80'      SKIP DELETED                       
         BO    T4RL1NXT                                                         
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   DUB(4),TLT4YEAR                                                  
         MVC   DUB+4(4),=C'0101'                                                
         GOTO1 DATCON,DMCB,(9,DUB),(1,WORK)                                     
         CLC   WORK(1),TIQPSTR                                                  
         BL    T4RL1NXT                                                         
         CLC   WORK(1),TIQPEND                                                  
         BH    T4RL1NXT                                                         
         GOTO1 DATCON,DMCB,(9,DUB),(0,WORK)                                     
         MVC   TIYEAR(2),WORK                                                   
         MVC   TICUR,TLT4CUR                                                    
         MVC   TIEMP,TLT4EMP                                                    
         MVC   TISSN,TLT4SSN                                                    
         BRAS  RE,FILTKEY          POSSIBLE KEY FILTER                          
         JNE   T4RL1NXT                                                         
         BRAS  RE,GETREC                                                        
         BRAS  RE,POSTREC          SET CODES FROM RECORD                        
         BRAS  RE,FILTREC          FILTER ON RECORD                             
         JNE   T4RL1NXT                                                         
         MVC   DUB(4),TLT4YEAR                                                  
         MVC   DUB+4(4),=C'0101'                                                
         GOTO1 DATCON,DMCB,(9,DUB),(1,WORK)                                     
         MVC   TIDATE,WORK         POSTREC CLEARS TIDATE                        
         BRAS  RE,GOHOOK                                                        
         J     T4RL1NXT                                                         
         EJECT                                                                  
*              I/O ROUTINES - ARCHIVED ADVICE RECORDS                           
         SPACE 3                                                                
PADVICE  NTR1  BASE=*,LABEL=*                                                   
         USING TLDVD,R4                                                         
         MVI   LKEY,1                                                           
         MVC   TLDVAGY,TIFAGY      POSSIBLE AGENCY                              
         TM    TLDVAGY,X'C0'                                                    
         BNO   PADVHIGH                                                         
         MVC   LKEY,LDVAGY                                                      
         MVC   TLDVCID,TIQSTART    OPTIONAL START                               
                                                                                
PADVHIGH BRAS  RE,FIRSTHI                                                       
         B     *+8                                                              
PADVNEXT BRAS  RE,SEQ                                                           
         BRAS  RE,KEYCOMP                                                       
         JNE   CKXIT                                                            
         LA    R4,KEY                                                           
         XC    TICODES,TICODES     SET ANY CODES FROM KEY                       
         MVC   TIAGY,TLDVAGY                                                    
         MVC   TICID,TLDVCID                                                    
         MVC   TIADV,TLDVADV                                                    
         BAS   RE,FILTKEY          POSSIBLE KEY FILTER                          
         BNE   PADVNEXT                                                         
         BAS   RE,GETREC                                                        
         BAS   RE,POSTREC          SET CODES FROM RECORD                        
         BAS   RE,FILTREC          FILTER ON RECORD                             
         BNE   PADVNEXT                                                         
         BAS   RE,GOHOOK                                                        
         B     PADVNEXT                                                         
         EJECT                                                                  
         SPACE 3                                                                
TALINIT  NTR1  BASE=*,LABEL=*                                                   
         L     R1,TIACOMFC         COMFACS ADDRESSES                            
         USING COMFACSD,R1                                                      
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   DATCON,CDATCON                                                   
         MVC   HEXOUT,CHEXOUT                                                   
         MVC   GETFACT,CGETFACT                                                 
         L     RF,CCALLOV          GET ADDRESS OF SQUASHER                      
         GOTO1 (RF),DMCB,0,X'D9000A0D'                                          
         MVC   SQUASHER,0(R1)                                                   
         SPACE 1                                                                
*                                                                               
         L     R1,TIACOMFC         COMFACS ADDRESSES                            
         USING COMFACSD,R1                                                      
         L     RF,CCALLOV          GET ADDRESS OF TRPACK                        
         GOTO1 (RF),DMCB,0,X'D9000AFE'                                          
         MVC   TIATRPAK,0(R1)                                                   
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         SPACE 1                                                                
         MVC   DIRNAME,=CL8'TALDIR'                                             
         MVC   FILENAME,=CL8'TALFIL'                                            
         NI    TISTAT,X'FF'-TISTRDCK                                            
         SPACE 1                                                                
*                                  SEED DUMP FOR LEGIBILITY                     
         MVC   TASYSIOD,=CL8'*SYSIOD*'                                          
         MVC   TIMODEH,=CL8'**MODE**'                                           
         MVC   TIADDSH,=CL8'**ADDS**'                                           
         MVC   TIKEYH,=CL6'**KEY*'                                              
         LA    R1,IO                                                            
         ST    R1,TIAREC                                                        
         AHI   R1,ACBLK-IO                                                      
         ST    R1,TIAACBLK                                                      
         XC    INTAGY,INTAGY                                                    
         XC    INTINVNO,INTINVNO                                                
         CLI   TIREAD,TIFONLY      DON'T INITIALIZE IF ONLY FILTERING           
         JE    XIT                                                              
         OC    TIABUFF,TIABUFF     INITIALIZE BUFFER                            
         JZ    XIT                 IF USER PROVIDED ONE                         
                                                                                
         USING COMFACSD,R1                                                      
         L     R1,TIACOMFC                                                      
         CLC   CWSSVR,=F'0'        IF ONLINE                                    
         JE    TINIT10                                                          
         CLC   TILBUFF,=F'6000'    CATCH BUG WHERE L'BUFFER EXCEEDS             
         JL    TINIT10             6K                                           
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
TINIT10  L     RE,TIABUFF          FIRST CLEAR IT                               
         L     RF,TILBUFF                                                       
         XCEF                                                                   
         L     R2,TIABUFF                                                       
         L     R1,TILBUFF          FIGURE OUT N'26 BYTE ENTRIES                 
         SH    R1,=H'8'                                                         
         SR    R0,R0                                                            
         D     R0,=F'26'                                                        
         ST    R1,0(R2)            SAVE THIS IN FIRST 4 BYTES                   
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
* TASYSWORKD                                                                    
* TASYSUSESD                                                                    
         PRINT OFF                                                              
TGLOBD   DSECT                                                                  
       ++INCLUDE TASYSWORKD                                                     
       ++INCLUDE TASYSUSESD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
       ++INCLUDE TASYSIOMYD                                                     
         DSECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         SPACE 1                                                                
*TAGENFILE                                                                      
*DDCOMFACS                                                                      
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'161TASYSIO   01/21/16'                                      
         END                                                                    
