*          DATA SET ACINT10    AT LEVEL 010 AS OF 10/02/00                      
*PHASE T61910A,*                                                                
         TITLE 'T61910 - PROFILE RECORD MAINTENENCE'                            
T61910   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (MYDEND-MYD),T61910**                                            
         LR    R4,RC                                                            
         USING MYD,R4                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE KEY MODE?                           
         BNE   MODE1                                                            
         BAS   RE,VKEY                                                          
         BAS   RE,PROCPF           PROCESS PF KEYS                              
         B     OKEXIT                                                           
         SPACE 1                                                                
MODE1    CLI   MODE,RECDEL         DELETE THE RECORD?                           
         BNE   MODE2                                                            
         MVI   ERROR,SECLOCK                                                    
         TM    TWAAUTH,X'10'       MUST HAVE THIS ON TO DELETE                  
         BZ    ERREXIT                                                          
         BAS   RE,OKDEL                                                         
         B     RESETKEY                                                         
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         BAS   RE,VREC             VALIDATE RECORD                              
         BAS   RE,DREC             DISPLAY NEW RECORD                           
         BAS   RE,PROCPF           PROCESS PF KEYS                              
         B     RESETKEY                                                         
         SPACE 1                                                                
MODE4    CLI   MODE,DISPKEY                                                     
         BNE   MODE6                                                            
         BAS   RE,DKEY             DISPLAY KEY                                  
         BAS   RE,VKEY                                                          
         B     RESETKEY                                                         
         SPACE 1                                                                
MODE6    CLI   MODE,DISPREC                                                     
         BNE   OKEXIT                                                           
         BAS   RE,DREC             DISPLAY RECORD                               
         BAS   RE,PROCPF           PROCESS PF KEYS                              
         SPACE 1                                                                
RESETKEY MVC   KEY,SAVEKEY                                                      
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                          DISPLAY KEY                                *         
***********************************************************************         
         SPACE 1                                                                
DKEY     NTR1                                                                   
         L     R6,AIO                                                           
         USING ACINKEY,R6                                                       
         CLI   ACINCOD,ACINEQU                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    PRORCVH+6,X'80'                                                  
         MVC   PRORCV,ACINACC                                                   
         OI    PROCLTH+6,X'80'                                                  
         MVC   PROCLT,ACINCLT                                                   
         OI    PROPRDH+6,X'80'                                                  
         MVC   PROPRD,ACINPRD                                                   
         OI    PROMEDH+6,X'80'                                                  
         MVC   PROMED(L'ACINMED),ACINMED                                        
         OI    PROESTH+6,X'80'                                                  
         MVC   PROEST,ACINEST                                                   
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                           VALIDATE KEY                              *         
***********************************************************************         
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LA    R2,PRORCVH          VALIDATE ACCOUNT                             
         MVC   CUL+1(2),RECVLEDG                                                
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         MVI   OPTION,C'Y'                                                      
         GOTO1 VALACCT                                                          
         SPACE 1                                                                
         LA    R2,PROCLTH          CLIENT                                       
         MVI   ERROR,INVALID                                                    
         SR    R1,R1                                                            
         ICM   R1,1,PROCLTH+5                                                   
         CH    R1,=H'1'                                                         
         BNH   ERREXIT                                                          
         CLI   8(R2),C' '          DOES CLIENT START WITH A BLANK ?             
         BE    ERREXIT             YES, THIS IS AN ERROR                        
         OI    6(R2),X'80'         SET TRANSMIT                                 
         SPACE 1                                                                
         MVC   CUL+1(2),PRODLEDG                                                
         GOTO1 VALCLI              VALIDATE CLIENT                              
         CLC   5(1,R2),LCLI        MAKE SURE INPUT IS NOT TOO LONG              
         MVI   ERROR,TOOLONG                                                    
         BH    ERREXIT                                                          
         SPACE 1                                                                
         LA    R2,PROPRDH          PRODUCT                                      
         MVI   ERROR,INVALID                                                    
         SR    R1,R1                                                            
         ICM   R1,1,PROPRDH+5                                                   
         CH    R1,=H'1'                                                         
         BNH   ERREXIT                                                          
         CLI   8(R2),C' '          DOES PRODUCT START WITH A BLANK ?            
         BE    ERREXIT             YES, THIS IS AN ERROR                        
         OI    6(R2),X'80'         SET TRANSMIT                                 
         SPACE 1                                                                
         GOTO1 VALPROD              VALIDATE PRODUCT                            
         CLC   5(1,R2),LCLI        MAKE SURE INPUT IS NOT TOO LONG              
         MVI   ERROR,TOOLONG                                                    
         BH    ERREXIT                                                          
         SPACE 1                                                                
         LA    R2,PROMEDH          MEDIA                                        
         OI    6(R2),X'80'         SET TRANSMIT                                 
         GOTO1 VALMED              VALIDATE MEDIA                               
         LA    R2,PROMEDNH         MEDIA DESCRIPTION                            
         OI    6(R2),X'80'         SET TRANSMIT                                 
         MVC   PROMEDN,MEDNAME                                                  
         SPACE 1                                                                
         LA    R2,PROESTH          ESTIMATE NUMBER                              
         OI    6(R2),X'80'         SET TRANSMIT                                 
         GOTO1 VALEST                                                           
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(42),SPACES          CLEAR KEY                                
         MVC   CUL+1(L'RECVLEDG),RECVLEDG                                       
         LA    R6,KEY                                                           
         USING ACINKEY,R6                                                       
         MVI   ACINCOD,ACINEQU                                                  
         MVI   ACINSREC,ACINSEQU                                                
         MVC   ACINCUL,CUL                                                      
         MVC   ACINACC,RECCODE                                                  
         MVC   ACINCLT,CLICODE                                                  
         MVC   ACINPRD,PRODCODE                                                 
         MVC   ACINMED,MEDIA                                                    
         MVC   ACINEST,ESTIMATE                                                 
         CLC   PROMED(3),=C'MI='                                                
         BNE   *+8                                                              
         OI    (ACSTATUS-ACKEYD)(R6),X'02'  SET STATUS AS MI=                   
         MVC   SAVEKEY,KEY                                                      
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                        DISPLAY RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
DREC     NTR1                                                                   
         L     R6,AIO                                                           
         LA    R2,PROMEDH                                                       
         MVI   ERROR,NOEL         NO PROFILE ELEMENT                            
         OI    PROMEDH+6,X'80'    TRANSMIT                                      
         MVC   PROMED,SPACES                                                    
         MVC   PROMED(L'ACINMED),(ACINMED-ACINKEY)(R6)                          
         TM    (ACSTATUS-ACKEYD)(R6),X'02'   IS MEDIA AN MI RECORD?             
         BZ    DREC02                                                           
         MVC   PROMED(3),=C'MI='                                                
         MVC   PROMED+3(L'ACINMED),(ACINMED-ACINKEY)(R6)                        
         MVC   SAVEKEY,0(R6)                 SAVE KEY INCLUDING STATUS          
         SPACE 1                                                                
DREC02   MVI   ELCODE,ACIPFEQU    GET PROFILE ELEMENT                           
         BAS   RE,GETELIO                                                       
         BNE   ERREXIT                                                          
         SPACE 1                                                                
         USING ACINPRFD,R6                                                      
         OI    PROESTNH+6,X'80'    TRANSMIT                                     
         MVC   PROESTN,SPACES      ESTIMATE DESCRIPTION                         
         MVC   PROESTN,ACIPFDES                                                 
         SPACE 1                                                                
         LA    R2,PROPERH                                                       
         OI    PROPERH+6,X'80'                       TRANSMIT                   
         OI    PROPERH+4,X'20'                       MARK AS VALIDATED          
         MVC   WORK(2),ACIPFPRS                                                 
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,8(R2))    EST PERIOD                     
         MVI   PROPER+6,C'-'                                                    
         MVC   WORK(2),ACIPFPRE                                                 
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,15(R2))                                  
         SPACE 1                                                                
         ZAP   TOTPCT,=P'0'                                                     
         AP    TOTPCT,ACIPFNET                                                  
         OI    PRONETH+6,X'80'                                                  
         EDIT  ACIPFNET,(7,PRONET),2                 NET PCT                    
         AP    TOTPCT,ACIPFFEE                                                  
         OI    PROFEEH+6,X'80'                                                  
         OI    PROFEEH+4,X'20'                       MARK VALIDATED             
         EDIT  ACIPFFEE,(7,PROFEE),2                 FEE PCT                    
         AP    TOTPCT,ACIPFREC                                                  
         OI    PRORECH+6,X'80'                                                  
         OI    PRORECH+4,X'20'                       MARK VALIDATED             
         EDIT  ACIPFREC,(7,PROREC),2                 REC PCT                    
         SPACE 1                                                                
         OI    PROTOTH+6,X'80'                                                  
         EDIT  TOTPCT,(7,PROTOT),2                   TOTAL                      
         SPACE 1                                                                
         MVC   AIO,AIO2                              USE 2ND IO AREA            
         XC    KEY,KEY                                                          
         MVC   FILENAME,=CL8'CTFILE'                                            
         LA    R3,KEY                                                           
         USING CTIKEY,R3                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,ACIPFTWA                      READ ID RECORD             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 HELLO,DMCB,(C'G',FILENAME),('CTDSCELQ',AIO),0                    
         CLI   12(R1),0                              TEST FOUND                 
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,12(R1)                             DESCRIPTION EL             
         USING CTDSCD,R3                                                        
         MVC   PROPID,CTDSC                          USER ID                    
         OI    PROPIDH+6,X'80'                                                  
         XC    FILENAME,FILENAME                                                
         MVC   AIO,AIO1                              RESTORE AIO                
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
VREC     NTR1                                                                   
         CLI   ACTNUM,ACTADD                                                    
         BNE   VREC01                                                           
         L     R6,AIO              AIO HAS RECORD KEY ALREADY, JUST             
         MVC   ACLENGTH-ACKEYD(L'ACLENGTH,R6),=H'50'    THE LENGTH              
         CLC   PROMED(3),=C'MI='                                                
         BNE   VREC01                                                           
         OI    (ACSTATUS-ACKEYD)(R6),X'02'   MARK THE RECORD AS MI=             
         SPACE 1                                                                
VREC01   MVI   ELCODE,ACIPFEQU     ESTIMATE PROFILE ELEMENT                     
         BAS   RE,GETELIO                                                       
         BE    VREC03                                                           
         BAS   RE,ADDPROF                                                       
         B     VREC01                                                           
         SPACE 1                                                                
         USING ACINPRFD,R6                                                      
VREC03   LA    R2,PROESTNH         ESTIMATE NAME                                
         GOTO1 ANY                                                              
         MVC   ACIPFDES,SPACES                                                  
         ZIC   R1,5(R2)            LENGTH OF SCREEN EST DESCRIPTION             
         BCTR  R1,0                REDUCED FOR EXECUTE                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACIPFDES(0),8(R2)   ESTIMATE DESCRIPTION                         
         SPACE 1                                                                
         CLI   ACTNUM,ACTADD                                                    
         BNE   *+8                                                              
         NI    PROPERH+4,X'DF'     UNMARK AS VALIDATED FOR ACTION ADD           
         LA    R2,PROPERH                                                       
         GOTO1 VALPERI             VALIDATE ESTIMATE PERIOD                     
         OI    PROPERH+4,X'20'     MARK AS VALIDATED                            
         BAS   RE,MTHBLD           BUILD LIST OF MONTHS IN EST PERIOD           
         MVC   ACIPFPRS,PERIODS                                                 
         MVC   ACIPFPRE,PERIODE                                                 
         MVC   ACIPFTWA,USERID                                                  
         SPACE 1                                                                
         ZAP   TOTPCT,=P'0'                                                     
         LA    R2,PRONETH          A.O.R. NET PERCENT                           
         BAS   RE,VALPCTS                                                       
         ZAP   ACIPFNET,DUB                                                     
         LA    R2,PROFEEH          A.O.R. FEE PERCENT                           
         BAS   RE,VALPCTS                                                       
         ZAP   ACIPFFEE,DUB                                                     
         ZAP   AORPCT,DUB                                                       
         LA    R2,PRORECH          CREATIVE AGENCY RECEIVABLE PERCENT           
         BAS   RE,VALPCTS                                                       
         ZAP   ACIPFREC,DUB                                                     
         ZAP   RECPCT,DUB                                                       
         LA    R2,PROTOTH          TOTAL PCT LINE                               
         OI    6(R2),X'80'         SET TRANSMIT                                 
         EDIT  TOTPCT,(7,8(R2)),2                                               
         CP    TOTPCT,=P'10000'    TOTAL MUST BE 100 %                          
         BE    VREC04                                                           
         MVI   ERROR,BADFORM                                                    
         B     ERREXIT                                                          
         SPACE 1                                                                
VREC04   MVI   ELCODE,ACIESEQU     MONTHLY ESTIMATE ELEMENTS                    
         BAS   RE,GETELIO                                                       
         BE    VREC06              ALREADY EXIST SO DO SOME CHECKING            
         SPACE 1                                                                
         LA    R3,MTHLIST          ADD BASIC MTHLY EST ELEMENTS                 
         CLI   0(R3),X'FF'         FOR EACH MTH IN EST PERIOD                   
         BE    ERREXIT                                                          
         BAS   RE,ADESTEL                                                       
         LA    R3,2(R3)                                                         
         CLI   0(R3),X'FF'         END OF LIST                                  
         BNE   *-12                                                             
         B     OKEXIT              AND YOUR DONE                                
         SPACE 1                                                                
*                                  ************************************         
*                                  ** EVERY EST MTH MUST HAVE EST EL **         
*                                  ************************************         
*                                  ** MAKE SURE THAT THERE IS A MTHLY**         
*                                  ** ESTIMATE ELEMENT FOR EVERY MTH **         
*                                  ** OF THE EST PERIOD              **         
*                                  ************************************         
         SPACE 1                                                                
VREC06   LA    R3,MTHLIST          LIST OF MTHS IN EST PERIOD                   
         USING ACINESTD,R6                                                      
VREC06A  CLI   0(R3),X'FF'         END OF LIST?                                 
         BE    VREC08                                                           
         CLC   ACIESMTH,0(R3)      IS ELEMENT MTH IN LIST?                      
         BE    VREC06B                                                          
         ZIC   RF,1(R6)                                                         
         AR    R6,RF               NEXT ELEMENT                                 
         CLI   0(R6),ACIESEQU                                                   
         BE    VREC06A                                                          
         BAS   RE,ADESTEL          ADD A MONTHLY ELEMENT                        
         SPACE 1                                                                
VREC06B  LA    R3,2(R3)                                                         
         MVI   ELCODE,ACIESEQU     MONTHLY ESTIMATE ELEMENTS                    
         BAS   RE,GETELIO                                                       
         BE    VREC06A                                                          
         DC    H'0'                                                             
         SPACE 1                                                                
*                                  ***** CHANGE IN ESTIMATE PERIOD*****         
*                                  ************************************         
*                                  ** MAKE SURE THAT ALL ESTIMATE    **         
*                                  ** ELEMENTS ON THIS RECORD, ARE   **         
*                                  ** WITHIN THE EST PERIOD.         **         
*                                  ** ELEMENTS NOT WITHIN THE PERIOD **         
*                                  ** ARE DELETED IF THERE IS NO INFO**         
*                                  ** ON THEM, ELSE IT IS NOT VALID  **         
*                                  ** TO CHANGE THE PERIOD           **         
*                                  ************************************         
         SPACE 1                                                                
VREC08   LA    R3,MTHLIST          LIST OF MTHS IN EST PERIOD                   
         MVI   ELCODE,ACIESEQU     MONTHLY ESTIMATE ELEMENTS                    
         BAS   RE,GETELIO                                                       
         BE    VREC08A                                                          
         DC    H'0'                                                             
VREC08A  CLC   ACIESMTH,0(R3)      IS ELEMENT MTH IN LIST OF EST MTHS?          
         BE    VREC08B             YES - GO CHECK IF PCT HAVE CHANGED           
         LA    R3,2(R3)            NO  - TRY NEXT MTH IN LIST                   
         CLI   0(R3),X'FF'         END OF LIST?                                 
         BNE   VREC08A             NO  - CONTINUE CHECKING                      
         SPACE 1                                                                
*                                  YES - THEY'VE CHANGED THE EST PERIOD         
*                                  CHECK IF IT'S OK TO CHANGE                   
         MVI   ERROR,NOCHANGE                                                   
         LA    R2,PROPERH                                                       
         CP    ACIESREC,=P'0'      HAS ESTIMATE BEEN POSTED?                    
         BNZ   ERREXIT             YES- PERIOD CAN'T BE CHANGED                 
         MVI   ACIESEL,X'FF'       NO - MARK IT FOR DELETION                    
         B     VREC09              AND CHECK THE NEXT X'C7' EL                  
         SPACE 1                                                                
VREC08B  MVI   ERROR,NOCHANGE                                                   
         TM    PROFEEH+4,X'20'     FEE PCT BEEN CHANGED                         
         BNZ   VREC08C                                                          
         LA    R2,PROFEEH                                                       
         OC    ACIESDAT,ACIESDAT   THE X'C7' EL HAVE POSTING DATE?              
         BNZ   ERREXIT             YES - THE PCT'S CAN'T BE CHANGED             
         ZAP   ACIESFEE,AORPCT                                                  
         ZAP   ACIESFEO,AORPCT                                                  
VREC08C  TM    PRORECH+4,X'20'     REC PCT BEEN CHANGED                         
         BNZ   VREC09                                                           
         LA    R2,PRORECH                                                       
         OC    ACIESDAT,ACIESDAT   THE X'C7' EL HAVE POSTING DATE?              
         BNZ   ERREXIT             YES - THE PCT'S CAN'T BE CHANGED             
         ZAP   ACIESRCV,RECPCT                                                  
         ZAP   ACIESRCO,RECPCT                                                  
         SPACE 1                                                                
VREC09   LA    R3,MTHLIST          LIST OF MTHS IN EST PERIOD                   
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         CLI   0(R6),ACIESEQU      NEXT X'C7' EL                                
         BE    VREC08A                                                          
         MVI   ELCODE,X'FF'        GET RID OF ANY DELETED X'C7' EL'S            
         GOTO1 REMELEM                                                          
         B     OKEXIT                                                           
         EJECT                                                                  
***********************************************************************         
*                        CHECK IF OK TO DELETE A RECORD               *         
***********************************************************************         
         SPACE 1                                                                
OKDEL    NTR1                                                                   
         LA    R2,PRORCVH                                                       
         MVC   AIO,AIO2           USE 2ND                                       
         MVC   KEY,SPACES                                                       
         MVC   KEY(42),SAVEKEY                                                  
         MVI   ERROR,NOTFOUND                                                   
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(42),KEY                                                  
         BNE   ERREXIT                                                          
         L     R6,AIO                                                           
         SPACE 1                                                                
         MVI   ERROR,NOEL                                                       
         MVI   ELCODE,ACIPFEQU    GET PROFILE ELEMENT                           
         BAS   RE,GETELIO                                                       
         BNE   ERREXIT                                                          
         MVI   ELCODE,ACIESEQU    GET ESTIMATE ELEMENT                          
         BAS   RE,GETELIO                                                       
         BNE   ERREXIT                                                          
         MVI   ERROR,CANTDEL                                                    
         SPACE 1                                                                
         USING ACINESTD,R6                                                      
OKDEL02  TM    ACIESTAT,X'80'      HAS ESTIMATE BEEN POSTED                     
         BNZ   OKDEL03             YES- MAKE SURE IT'S SATISFIED                
         OC    ACIESDAT,ACIESDAT   NO - MUST DELETE DATE PASSIVE PTER           
         BNZ   OKERR                                                            
OKDEL03  CP    ACIESREC,ACIESPD    RCV AMT = PAID AMOUNT?                       
         BNE   OKERR               NO- CANT DELETE                              
         SPACE 1                                                                
OKDEL04  ZIC   RF,1(R6)            NEXT ELEMENT                                 
         AR    R6,RF                                                            
         CLI   0(R6),ACIESEQU                                                   
         BE    OKDEL02                                                          
*                                                                               
OKDEL06  MVC   AIO,AIO1            RESTORE AIO                                  
         B     OKEXIT              IF YOU LEAVE HERE,THIS EST OK TO             
*                                  DELETE                                       
*                                                                               
OKERR    CLC   PROPRO(5),=C'ECROF' IF ECROF ENTERED, OK TO DELETE IT            
         BE    OKDEL06             (THIS FIELD CAN ONLY BE ENTERED              
         B     ERREXIT              FROM A DDS TERMINAL)                        
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                      PROCESS PF KEYS                                *         
***********************************************************************         
*                                                                               
PROCPF   NTR1                                                                   
         LA    R2,PRORCVH                                                       
         LA    R6,SAVEKEY                                                       
         USING ACINKEY,R6                                                       
         XC    DUB,DUB                                                          
         MVI   DUB,L'ACINMED                                                    
         MVC   DUB+1(L'ACINMED),ACINMED                                         
         TM    (ACSTATUS-ACKEYD)(R6),X'02'   MI= RECORD?                        
         BZ    PROCPFA                                                          
         MVI   DUB,X'05'                                                        
         MVC   DUB+1,=C'MI='                                                    
         MVC   DUB+4(L'ACINMED),ACINMED                                         
         SPACE 1                                                                
PROCPFA  CLI   PFKEY,PF3           PF3 FOR ESTIMATE ACTION CHANGE               
         BE    PROCPF3                                                          
         CLI   PFKEY,PF2           PF2 FOR ESTIMATE ACTION DISPLAY              
         BNE   PROCPFX                                                          
         SPACE 1                                                                
         LA    R1,=C'DIS'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         SPACE 1                                                                
PROCPF2  MVI   PFKEY,0                                                          
         GOTO1 VTRANSF,WORK,=C'ESTIMATE',,(12,ACINACC),(3,ACINCLT),(3,AX        
               CINPRD),(DUB,DUB+1),(6,ACINEST),0                                
         B     PROCPFX                                                          
         SPACE 1                                                                
PROCPF3  LA    R1,=C'CHA'          ACTION                                       
         ST    R1,WORK+4                                                        
         MVI   WORK+4,X'03'        LENGTH OF ACTION                             
         B     PROCPF2                                                          
         SPACE 1                                                                
PROCPFX  B     OKEXIT                                                           
         EJECT                                                                  
*****************************************************************               
*        VALIDATE THE INPUT PROFILE PERCENTS                    *               
*****************************************************************               
VALPCTS  NTR1                                                                   
         MVI   MAX,2               2 DECIMAL PLACES                             
         GOTO1 VALIDEC             VALIDATE THE PCT                             
         OI    GENSTAT2,X'20'      USE LOWER THAN 60                            
         MVI   ERROR,BADFORM                                                    
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         CP    DUB,=P'0'           EDIT RETURNS INPUT PACKED IN DUB             
         BL    ERREXIT             NO MINUS AMOUNTS                             
         CP    DUB,=P'10000'       PCT GREATER THAN 100                         
         BH    ERREXIT                                                          
         OI    6(R2),X'80'         SET TRANSMIT                                 
         EDIT  (P8,DUB),(7,8(R2)),2                                             
         AP    TOTPCT,DUB                                                       
         CP    TOTPCT,=P'10000'    TOTAL GREATER THAN 100 PERCENT               
         BNH   OKEXIT                                                           
         SPACE 1                                                                
         OI    PROTOTH+6,X'80'     SET TRANSMIT                                 
         EDIT  TOTPCT,(7,PROTOT),2                                              
         B     ERREXIT                                                          
         EJECT                                                                  
***********************************************************************         
*              BUILD A BLANK PROFILE ELEMENT                                    
***********************************************************************         
         SPACE 1                                                                
         USING ACINPRFD,R6                                                      
ADDPROF  NTR1  ,                                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         MVI   ACIPFEL,ACIPFEQU                                                 
         MVI   ACIPFLEN,ACIPLENQ   LENGTH                                       
         MVC   ACIPFDES,SPACES                                                  
         ZAP   ACIPFNET,=P'0'                                                   
         ZAP   ACIPFFEE,=P'0'                                                   
         ZAP   ACIPFREC,=P'0'                                                   
         GOTO1 ADDELEM                                                          
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
         SPACE 1                                                                
***********************************************************************         
*              BUILD BASIC ESTIMATE ELEMENTS                                    
*              FOR PROFILE ESTIMATE PERIOD DATES                                
***********************************************************************         
         SPACE 1                                                                
         USING ACINESTD,R6                                                      
ADESTEL  NTR1  ,                                                                
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     CLEAR ELEMENT                                
         LA    R6,ELEMENT                                                       
         MVI   ACIESEL,ACIESEQU                                                 
         MVI   ACIESLEN,ACIELENQ   LENGTH                                       
         MVC   ACIESMTH,0(R3)      MONTH                                        
         ZAP   ACIESFEE,AORPCT     CLEAR ALL PACKED FIELDS                      
         ZAP   ACIESRCV,RECPCT                                                  
         ZAP   ACIESGRS,=P'0'                                                   
         ZAP   ACIESREC,=P'0'                                                   
         ZAP   ACIESPD,=P'0'                                                    
         ZAP   ACIESFEO,AORPCT                                                  
         ZAP   ACIESRCO,RECPCT                                                  
         ZAP   ACIESGRO,=P'0'                                                   
         ZAP   ACIESRAO,=P'0'                                                   
         XC    DMCB(16),DMCB       MAKE SURE P1 - P4 IS CLEAR                   
         GOTO1 ADDELEM                                                          
         B     OKEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
**************************************************************                  
******** BUILD A TABLE OF PACKED ESTIMATE PERIOD MONTHS*******                  
**************************************************************                  
         SPACE 1                                                                
MTHBLD   NTR1                                                                   
         LA    R2,MTHLIST                 MONTHLY TABLE                         
         LA    R5,11                      12 MONTH LIMIT                        
         MVC   0(2,R2),PERIODS            MOVE IN START DATE YM PACKED          
         MVC   WORK+6(2),PERIODS                                                
         MVI   WORK+8,1                   SET DAY TO THE 1ST                    
         GOTO1 DATCON,DMCB,(1,WORK+6),(0,WORK)    EBCDIC                        
         LA    R2,2(R2)                   NEXT TABLE ENTRY                      
         MVI   0(R2),X'FF'                MARK END OF TABLE                     
         CLC   PERIODS,PERIODE            IF START N END ARE THE SAME           
         BE    OKEXIT                     THE TABLE IS COMPLETE                 
         SPACE 1                                                                
MTHBLD2  MVC   WORK+4(2),=C'01'                                                 
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,1                                  
         MVC   WORK(6),WORK+6                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)    YMD PACKED                    
         MVC   0(2,R2),WORK+6                     INTO TABLE                    
         SPACE 1                                                                
         LA    R2,2(R2)                                                         
         MVI   0(R2),X'FF'                 MARK END OF TABLE                    
         CLC   WORK+6(2),PERIODE           EQUAL TO END MONTH                   
         BE    OKEXIT                      YES - TAB COMPLETE                   
         BCT   R5,MTHBLD2                                                       
         DC    H'0'                                                             
         SPACE 1                                                                
         EJECT                                                                  
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 3                                                                
OKEXIT   CR    R8,R8                                                            
EXIT     XIT1                                                                   
         SPACE 3                                                                
ERREXIT  GOTO1 VERRCUR                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MYD      DSECT                                                                  
MTHLIST  DS    CL25                LIST OF YYMM'S INESTIMATE PERIOD             
         DC    X'FF'                                                            
SAVERE   DS    A                   SAVE AREA FOR RE                             
MYDEND   EQU   *                                                                
         EJECT                                                                  
*  DDSPOOLD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*  DDSPLWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*  ACGENBOTH                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*  CTGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*  ACINTWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACINTWORKD                                                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACINTF0D                                                       
         DS    0F                                                               
TOTPCT   DS    PL3                 TOTAL EST PROFILE PCT (ALWAYS 100)           
AORPCT   DS    PL3                 AOR EST PROFILE PCT                          
RECPCT   DS    PL3                 REC EST PROFILE PCT                          
SAVEKEY  DS    CL48                HOLD SJ KEY FOR EXIT                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACINT10   10/02/00'                                      
         END                                                                    
