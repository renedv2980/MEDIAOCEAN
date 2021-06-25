*          DATA SET ACCAP02    AT LEVEL 118 AS OF 05/22/12                      
*PHASE T61D02A                                                                  
*INCLUDE QSORT                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE COVAIL                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCAP02 -- SALARY HISTORY MAINTENANCE/LIST           *         
*                                                                     *         
*  COMMENTS:     MAINTAINS SALARY HISTORY RECORDS                     *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS ACCAPF3 (MAINTENANCE)                        *         
*                        ACCAPF2 (LIST)                               *         
*                                                                     *         
*  OUTPUTS:      UPDATED SALARY HISTORY RECS                          *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- SYSSPARE                                       *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
**  DCUR - L 95 PUT IN REFERENCES TO LIST SCREEN AND ADDED XD=Y       *         
**  DCUR - L 97 CHECK FOR SPECIAL ADJ PCODE WHICH ALLOWS YOU TO ADD/  *         
**              CHANGE SALARY OUTSIDE OF LOCATION DATE                *         
**  DCUR - L 98 EXPND DISPLAY ENTRIES TO 80(HELD IN SYSSPARE) AND MOVE*         
**              PAYCODE TABLE TO DISPBLK (HELD IN WORKING STORAGE)    *         
**  DCUR - L101 FIX BUG WHEN SEL/CHA FROM LIST                        *         
**              PUT CHECK TO TRY TO CATCH BUG W/ TABLE BUILD (VKX)    *         
**              COMMENT OUT SVDATE-DOES NOT REFLECT ALL DATES IN REC  *         
**              WAS ONLY USING FIRST DATE ON REC                      *         
**  DCUR - L102 FIX BUG WHEN GOING FROM LIST TO ANOTHER ACTION WHEN   *         
**              FILTERING ON A MOA.                                   *         
**  DCUR - L103 FIX BUG WITH LIST                                     *         
**  DCUR - L104 NO LONGER ALLOW YTDADJ PCODE FOR MORE THAN ONE MONTH  *         
**              USING MONTH/ANNUAL OPTION.  SUPPRESS D/A PRINTING FOR *         
**              XD=Y FOR NON DDS TERMINALS                            *         
**  DCUR - L105 VALIDATE PAYDATE ENTERED BY READING ALL LOCATIONS ON  *         
**              PERSON REC(TO HANDLE DUP LOCATIONS WITHIN THE SAME MO)*         
**  DCUR - L106 CHECK FOR ATTRIBUTES FOR POSTING PCODE BEFORE COPYING *         
**              CALLING PCODES ATTRIBUTES.                            *         
**  DCUR - L108 MAKE BMONVAL CALL COUNTRY SPECIFC                     *         
**  DCUR - L109 FIX BUG WHICH ALLOWED USER TO ENTER SALARY PAST END   *         
**              DATE WHEN THEY HAD A YTDADJ PCODE ON THEIR PAYROLL REC*         
**  SMAN - L113 030907 BR13283L FIX TO OFFICE/DEPT/SUBDPT VALIDATION  *         
**              ROUTINE                                               *         
**  SMAN - L114 260907 BR13882L FIX TO VALIDATE (PAY)DATE ROUTINE(S)  *         
**  SMAN - L115 270907 BR11402D VERY SMALL BUG FIX TO L114            *         
**  SMAN - L116 170708 LO01-7701 RELAX HISTORY/ADD VALIDATION         *         
**  MPEN - L117 270709 LO01-8967 RELINK FOR BIGGER COBLOCK            *         
***********************************************************************         
         TITLE 'T61D02 - SALARY HISTORY RECORD MAINT/LIST'                      
T61D02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1D02**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T61DFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         APPLICATION STORAGE AREA                     
         USING BLOCKSD,R5                                                       
         ST    R3,RELO                                                          
         ST    RC,SAVERC                                                        
*                                                                               
         GOTO1 =A(SETUP),DMCB,RR=RELO  ANY INITIALIZING                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   *+8                                                              
         BAS   RE,VK                                                            
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   MAIN10                                                           
         GOTO1 =A(LR),DMCB,RR=RELO                                              
         B     XIT                                                              
MAIN10   CLI   MODE,XRECADD        AFTER AN ADD                                 
         BE    GOXD                                                             
         CLI   MODE,XRECPUT        AFTER A PUT                                  
         BE    GOXD                                                             
         B     XIT                                                              
GOXD     GOTO1 =A(XD),DMCB,RR=RELO                                              
         B     DR                  REDISPLAY                                    
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY                                                 *         
***********************************************************************         
*                                                                               
VK       NTR1                                                                   
         TM    TRANSTAT,RCHANG     REC CHANGED                                  
         BZ    *+8                                                              
         MVI   FIRSTIME,C'Y'      SET FIRST TIME FLAG                           
         XC    REVELEMS,REVELEMS                                                
         XC    LSTLKEY,LSTLKEY     CLEAR VALUES                                 
         XC    PREVDATE,PREVDATE                                                
         XC    PREVCODE,PREVCODE                                                
         MVC   OFFICE,SPACES                                                    
         MVC   DEPT,SPACES                                                      
         MVC   SUBDPT,SPACES                                                    
         MVC   PERSON,SPACES                                                    
         MVC   ACCNT,SPACES                                                     
         MVI   SEQNUM,0                                                         
         MVI   BITS,0                                                           
         NI    BIT2,X'FF'-(REDISP+ONEMONTH+KEYCHNG+FIRST)                       
         MVI   SPECBIT,0                                                        
         GOTO1 GETLDG,DMCB,C'1R'   GET LENGTHS                                  
         MVC   LEVELLN,ABCDLEN                                                  
         GOTO1 =A(VALOPTS),DMCB,RR=RELO                                         
         EJECT                                                                  
***********************************************************************         
*        VALIDATE MOA                                                           
***********************************************************************         
*                                                                               
VK10     NI    SPECBIT,X'FF'-CHAMOA                                             
*        XC    DPTSTART,DPTSTART                                                
*        XC    DPTEND,DPTEND                                                    
*        XC    DPTSALDT,DPTSALDT                                                
         XC    STDATE,STDATE                                                    
         XC    STDATECM,STDATECM                                                
         XC    ENDATE,ENDATE                                                    
         XC    ENDATECM,ENDATECM                                                
         XC    LISTMOA,LISTMOA     LIST FILTER                                  
*                                                                               
         CLI   ACTEQU,ACTLIST      NO DATE REQIRED ON LIST                      
         BE    VK20                                                             
         MVC   SHMWORD,SPACES      NOT ON LIST                                  
         TM    OPTSTAT,OPTSMON     MONTH OPT                                    
         BNO   *+10                                                             
         MVC   SHMWORD,AC@UPDU     NOT ON LIST                                  
         OI    SHMWORDH+6,X'80'                                                 
         OI    SHMUPDTH+6,X'80'                                                 
         OI    SHMUPDTH+1,X'20'    PROTECT                                      
*                                                                               
         LA    R2,SHMMOAH          ANY DATES GIVEN?                             
         CLI   5(R2),0                                                          
         BE    ERRPLS              PLEASE ENTER                                 
*                                                                               
         LA    R2,SHMMOAH                                                       
         TM    SHMMOAH+4,X'80'     INPUT THIS TIME                              
         BZ    *+8                                                              
         OI    SPECBIT,CHAMOA                                                   
         MVC   BYTE,SHMMOAH+5      LEN FOR VALMOA                               
         LA    R3,SHMMOA           A(PERIOD)                                    
         BAS   RE,VALMOA                                                        
         BNE   EINVDATE                                                         
         CLC   STMOA,ENDMOA        PERIOD MORE THAN ONE MONTH?                  
         BE    VK12                NO, OK                                       
         TM    OPTSTAT,OPTSMON     CAN ONLY HAVE RANGE WITH MONTH OPT           
         BO    VK12                                                             
         LA    R2,CONOPTH                                                       
         OI    6(R2),X'40'                                                      
         OI    BIT2,NOMONTH        NO MONTH ENTERED WHEN REQUIRED               
         B     EINVDTMO                                                         
*                                                                               
VK12     OI    SHMMOAH+6,X'80'     DISPLAY                                      
         MVC   SHMMOA,SPACES                                                    
         MVC   YYMMDD,STDATE                                                    
         GOTO1 DATCON,DMCB,(1,YYMMDD),(9,8(R2))                                 
         CLC   STDATE(2),ENDATE                                                 
         BE    VK14                                                             
         MVI   14(R2),C'-'                                                      
         LA    R3,15(R2)                                                        
         CLI   LANGCODE,3          GERMANY                                      
         BNE   *+12                                                             
         MVI   13(R2),C'-'                                                      
         LA    R3,14(R2)                                                        
         MVC   YYMMDD,ENDATE                                                    
         GOTO1 DATCON,DMCB,(1,YYMMDD),(9,(R3))                                  
*                                                                               
VK14     BAS   RE,MONOPEN          SEE IF MOA IS OPEN                           
         BNE   ACCERRX                                                          
         B     VK30                                                             
         EJECT                                                                  
***********************************************************************         
*        VALIDATE MOA RANGE ON LIST                                             
***********************************************************************         
*                                                                               
VK20     LA    R2,SHLMOAH          ANY DATES GIVEN?                             
         CLI   5(R2),0                                                          
         BE    VK30                                                             
*                                                                               
         GOTO1 SCANNER,DMCB,SHLMOAH,(2,BLOCK+L'PVALOUTB),C',=-='                
         LA    R4,BLOCK+L'PVALOUTB                                              
         CLI   DMCB+4,X'01'        DATE RANGE?                                  
         BNE   *+8                                                              
         OI    BIT2,ONEMONTH                                                    
         CLI   0(R4),0             ANY FIRST DATE                               
         BE    VK22                                                             
         ZIC   R6,0(R4)                                                         
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'20'                                                       
         GOTO1 PERVAL,DMCB,((R6),12(R4)),(BYTE,BLOCK)                           
         CLI   DMCB+4,PVRCINV1     DATE#1 INVALID                               
         BE    EINVDATE                                                         
         CLI   DMCB+4,PVRCMISS     MISSING                                      
         BE    EINVDATE                                                         
         MVC   STDATE,PVALPSTA                                                  
*                                                                               
VK22     LA    R4,32(R4)           NEXT LINE                                    
         CLI   0(R4),0             ANY END DATE                                 
         BE    VK24                                                             
         ZIC   R6,0(R4)                                                         
         LA    R3,BLOCK                                                         
         USING PERVALD,R3                                                       
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'20'                                                       
         GOTO1 PERVAL,DMCB,((R6),12(R4)),(BYTE,BLOCK)                           
         CLI   DMCB+4,PVRCINV1     DATE#1 INVALID                               
         BE    EINVDATE                                                         
         CLI   DMCB+4,PVRCMISS     MISSING                                      
         BE    EINVDATE                                                         
         MVC   ENDATE,PVALPSTA                                                  
*                                                                               
VK24     TM    BIT2,ONEMONTH                                                    
         BNO   *+10                                                             
         MVC   ENDATE,STDATE                                                    
         ZICM  R1,STDATE,2           USE 'FFFF'-YYMM                            
         LNR   R1,R1                                                            
         STH   R1,ENDATECM                                                      
         ZICM  R1,ENDATE,2                                                      
         LNR   R1,R1                                                            
         STH   R1,STDATECM                                                      
*                                                                               
         OC    STDATE,STDATE                                                    
         BNZ   VK26                                                             
         OC    ENDATE,ENDATE                                                    
         BZ    VK30                                                             
*                                                                               
VK26     LA    R4,SHLMOA                                                        
         MVC   SHLMOA,SPACES                                                    
         OC    STDATE,STDATE                                                    
         BZ    VK28                                                             
         XC    YYMMDD,YYMMDD                                                    
         MVC   YYMMDD(2),STDATE                                                 
         GOTO1 DATCON,DMCB,(1,YYMMDD),(9,(R4))                                  
         LA    R4,6(R4)                                                         
VK28     DS    0H                                                               
         TM    BIT2,ONEMONTH                                                    
         BO    VK29                                                             
         MVI   0(R4),C'-'                                                       
         LA    R4,1(R4)                                                         
         OC    ENDATE,ENDATE                                                    
         BZ    VK29                                                             
         XC    YYMMDD,YYMMDD                                                    
         MVC   YYMMDD(2),ENDATE                                                 
         GOTO1 DATCON,DMCB,(1,YYMMDD),(9,(R4))                                  
VK29     OI    SHLMOAH+6,X'80'                                                  
         MVC   LISTMOA(L'SHLMOA),SHLMOA    SAVE LIST FILTER                     
         EJECT                                                                  
***********************************************************************         
*        VALIDATE METHOD - DEFAULT TO METHOD 1                                  
*        SAVE METHOD REC IN AIO3 FOR LATER                                      
***********************************************************************         
*                                                                               
VK30     DS    0H                                                               
         XC    METHCODE,METHCODE                                                
         MVI   METHNUM,C'1'        DEFAULT TO METHOD 1                          
         CLI   ACTEQU,ACTLIST                                                   
         BE    VK31                                                             
         LA    R2,SHMMETHH                                                      
         CLI   SHMMETHH+5,0        ANY DATA?                                    
         BE    VK32                DEFAULT TO METHOD 1                          
         TM    4(R2),X'08'         VALID NUMERIC                                
         BNO   VK34                                                             
         B     VK31A                                                            
*                                                                               
VK31     LA    R2,SHLMETHH                                                      
         CLI   SHLMETHH+5,0                                                     
         BE    VK32                                                             
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BNO   VK34                                                             
         EJECT                                                                  
***********************************************************************         
*        READ RECORD BY METHOD NUMBER                                           
***********************************************************************         
*                                                                               
VK31A    CLI   5(R2),1             ONLY 1-9                                     
         BNE   EINVMETH                                                         
         MVC   METHNUM,8(R2)                                                    
VK32     LA    R6,BIGKEY           VALIDATE METHOD                              
         XC    BIGKEY,BIGKEY                                                    
         USING CAHRECD,R6                                                       
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    ALLOCATION METHOD RECORD TYPE BY NUM         
         MVI   CAHKSUB,CAHKSUBQ    METHOD BY NUM SUB TYPE                       
         MVC   CAHKCPY,CMPY        COMPANY                                      
         MVC   CAHKMTHD,METHNUM    METHOD NUMBER                                
         XC    CAHKOFC,CAHKOFC                                                  
         OI    BITS,BYNUM                                                       
         B     VK40                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        READ RECORD BY METHOD CODE                                             
***********************************************************************         
*                                                                               
VK34     CLI   ACTEQU,ACTLIST                                                   
         BE    *+14                                                             
         MVC   METHCODE,SHMMETH    SAVE METHOD CODE                             
         B     *+10                                                             
         MVC   METHCODE,SHLMETH    SAVE METHOD CODE FOR LIST                    
         OC    METHCODE,SPACES                                                  
VK36     LA    R6,BIGKEY           VALIDATE METHOD                              
         XC    BIGKEY,BIGKEY                                                    
         USING CMTRECD,R6                                                       
         MVC   CMTKEY,SPACES                                                    
         MVI   CMTKTYP,CMTKTYPQ    ALLOCATION METHOD RECORD TYPE                
         MVI   CMTKSUB,CMTKSUBQ    METHOD BY CODE SUB TYPE                      
         MVC   CMTKCPY,CMPY        COMPANY                                      
         MVC   CMTKMTHD,METHCODE   METHOD CODE                                  
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        GET BOTH METHNUM AND METHCODE                                          
***********************************************************************         
*                                                                               
VK40     GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   ENOMETH                                                          
         MVC   METHKEY,BIGKEY      SAVE METHOD KEY                              
         MVC   AIO,AIO3            METHOD RECORD IN AIO3 - NEED LATER           
         GOTO1 GETREC                                                           
         CLI   ACTEQU,ACTLIST                                                   
         BE    VK41                                                             
         GOTO1 GETNME,DMCB,AIO,SHMMNMEH     PUT OUT NAME                        
         MVC   AIO,AIO1                                                         
         MVC   METHNAME,SHMMNME    SAVE NAME FOR DISPKEY                        
         B     VK42                                                             
VK41     GOTO1 GETNME,DMCB,AIO,SHLMNMEH                                         
         MVC   AIO,AIO1                                                         
         MVC   METHNAME,SHLMNME                                                 
         USING METELD,R6                                                        
VK42     L     R6,AIO3                                                          
         MVI   ELCODE,METELQ       METHOD OF ALLOCATION ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         MVC   METHNUM,METNUM      SAVE BOTH NUMBER AND CODE                    
         MVC   METHCODE,METCODE                                                 
         CLI   ACTEQU,ACTLIST                                                   
         BE    VK45                                                             
         MVC   SHMMETH,METCODE     PUT OUT CODE                                 
         OI    SHMMETHH+6,X'80'                                                 
         B     *+14                                                             
VK45     MVC   SHLMETH,METCODE                                                  
         OI    SHLMETHH+6,X'80'                                                 
         TM    BITS,BYNUM          MUST HAVE METHOD REC BY NUM IN AIO3          
         BNO   VK32                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PERSON CODE                                                   
***********************************************************************         
*                                                                               
         CLI   ACTEQU,ACTLIST                                                   
         BE    VK50                                                             
         LA    R2,SHMPERH                                                       
         CLI   SHMPERH+5,0        ANY DATA?                                     
         BE    ERRPLS             PLEASE ENTER                                  
         CLC   SHMPERH+5(1),LEVELLN+3                                           
         BH    ETOOLONG                                                         
         MVC   PERSON,SPACES                                                    
         ZIC   R1,SHMPERH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERSON(0),SHMPER                                                 
         B     VK51                                                             
*                                                                               
VK50     LA    R2,SHLPERH                                                       
         CLI   SHLPERH+5,0         ANY DATA?                                    
         BE    ERRPLS                                                           
         CLC   SHLPERH+5(1),LEVELLN+3                                           
         BH    ETOOLONG                                                         
         MVC   PERSON,SPACES                                                    
         ZIC   R1,SHLPERH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERSON(0),SHLPER                                                 
*                                                                               
VK51     LA    R6,BIGKEY           READ PERSON RECORD                           
         XC    BIGKEY,BIGKEY                                                    
         USING PERRECD,R6                                                       
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    X'0F'                                        
         MVC   PERKCPY,CMPY                                                     
         MVC   PERKCODE,PERSON                                                  
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   EINVPER                                                          
         GOTO1 GETREC                                                           
         EJECT                                                                  
***********************************************************************         
*        DISPLAY PERSON'S NAME                                                  
***********************************************************************         
*                                                                               
         MVC   WORK,SPACES                                                      
         BAS   RE,FLDSEC           ALLOWED TO SEE NAME?                         
         BNE   VK52                                                             
         GOTO1 =A(DISPNAME),DMCB,RR=RELO  FORMATS NAME IN WORK                  
VK52     CLI   ACTEQU,ACTLIST                                                   
         BE    VK53                                                             
         MVC   SHMNAME,WORK                                                     
         OI    SHMNAMEH+6,X'80'                                                 
         MVC   PERNAME,WORK                                                     
         B     VK60                                                             
*                                                                               
VK53     MVC   SHLNAME,WORK                                                     
         OI    SHLNAMEH+6,X'80'                                                 
         MVC   PERNAME,WORK                                                     
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OFFICE/DEPT/SUBDPT IF ENTERED                                 
***********************************************************************         
*                                                                               
VK60     GOTO1 =A(PAYCDS),DMCB,RR=RELO    MAKE TABLE OF PAYCODES                
         CLI   ACTEQU,ACTLIST                                                   
         BNE   VK61                                                             
         GOTO1 =A(VAL1RLST),DMCB,RR=RELO                                        
         BE    VK80                                                             
         B     VK75                                                             
*                                                                               
*&&UK                                                                           
VK61     TM    SHMPERH+4,X'80'     PERSON ENTERED THIS TIME                     
         BO    VK80                THEN GET CURRENT LOCATION                    
         TM    SHMOFFH+4,X'80'     OFFICE ENTERED THIS TIME                     
*&&                                                                             
*&&US                                                                           
VK61     TM    SHMOFFH+4,X'80'     OFFICE ENTERED THIS TIME                     
*&&                                                                             
         BO    VK62                THEN USE ENTERED LOCATION                    
         TM    SHMOFFH+4,X'40'     OFFICE ENTERED PREVIOUSLY                    
         BO    VK62                                                             
         TM    SHMMOAH+4,X'80'     MONTH ENTERED THIS TIME                      
         BO    VK80                THEN GET CURRENT LOCATION                    
         B     VK62                                                             
*                                                                               
VK62     LA    R2,SHMOFFH          OFFICE                                       
         CLI   5(R2),0             ANY DATA?                                    
         BE    VK65                                                             
*                                                                               
         CLC   SHMOFFH+5(1),LEVELLN         SHOULD = LEN LEVEL A ACC            
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFICE(0),SHMOFF    SAVE OFFICE                                  
         OC    OFFICE,SPACES                                                    
         OI    BITS,YESOFF                                                      
*                                                                               
VK65     LA    R2,SHMDEPTH         ANY DEPARTMENT                               
         CLI   5(R2),0                                                          
         BE    VK70                                                             
         TM    BITS,YESOFF         MUST HAVE ENTERED AN OFFICE                  
         BZ    EMISHIGH                                                         
         CLC   5(1,R2),LEVELLN+1                                                
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DEPT(0),8(R2)                                                    
         OC    DEPT,SPACES                                                      
         OI    BITS,YESDPT                                                      
*                                                                               
VK70     LA    R2,SHMSDPTH         ANY SUB DEPT FILTER                          
         CLI   5(R2),0                                                          
         BE    VK75                                                             
         TM    BITS,YESOFF+YESDPT  MUST HAVE HIGHER LEVELS                      
         BNO   EMISHIGH                                                         
         CLC   5(1,R2),LEVELLN+2                                                
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SUBDPT(0),8(R2)                                                  
         OC    SUBDPT,SPACES                                                    
         OI    BITS,YESSDPT                                                     
*                                                                               
VK75     TM    BITS,YESOFF+YESDPT+YESSDPT                                       
         BZ    VK80                USE PERSON REC                               
         BM    EMISHIGH                                                         
         LA    R2,SHMOFFH          CURSOR AT OFFICE ON ERROR                    
         CLI   ACTEQU,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,SHLOFFH                                                       
         NI    SPECBIT,X'FF'-(VALPAYDT+LOWMOA)                                  
         GOTO1 =A(VALLOC),DMCB,RR=RELO  VALIDATE LOCATION GIVEN                 
         BE    VK90                DISPLAY                                      
         TM    SPECBIT,LOWMOA                                                   
         BO    VK90                                                             
         B     EINVLOCD                                                         
         EJECT                                                                  
***********************************************************************         
*        GET DEFAULT LOCATION FROM PERSON RECORD                                
***********************************************************************         
*                                                                               
         USING LOCELD,R6                                                        
VK80     L     R6,AIO                                                           
*                                                                               
         XC    WORK2,WORK2                                                      
         MVI   ELCODE,LOCELQ                                                    
         BAS   RE,GETEL                                                         
         B     VK85                                                             
*                                                                               
VK80LP   BAS   RE,NEXTEL                                                        
VK85     BNE   VK87                                                             
         OC    STDATE,STDATE                                                    
         BZ    VK86                                                             
         CLC   STDATE(2),LOCSTART                                               
         BL    VK80LP                                                           
         OC    LOCSALKD,LOCSALKD      CHECK SAL LOCKED DATE OR END DATE         
         BZ    VK85A                                                            
         CLC   STDATE(2),LOCSALKD                                               
         BH    VK80LP                                                           
         CLC   ENDATE(2),LOCSALKD                                               
         BH    VK80LP                                                           
         B     VK86                                                             
VK85A    OC    LOCEND,LOCEND                                                    
         BZ    VK86                                                             
         CLC   STDATE(2),LOCEND                                                 
         BH    VK80LP                                                           
         CLC   ENDATE(2),LOCEND                                                 
         BH    VK80LP                                                           
VK86     ST    R6,WORK2            SAVE ADDRESS OF ELEMENT                      
         B     VK80LP                                                           
*                                                                               
VK87     OC    WORK2,WORK2                                                      
         BNZ   *+12                                                             
         LA    R2,SHMOFFH          CURSOR AT OFFICE ON ERROR                    
         B     EINVLOCE                                                         
         L     R6,WORK2                                                         
         CLI   0(R6),LOCELQ                                                     
         BE    *+12                                                             
         LA    R2,SHMOFFH                                                       
         B     EINVLOCE                                                         
*                                                                               
         MVC   OFFICE,LOCOFF                                                    
         MVC   DEPT,LOCDEPT                                                     
         MVC   SUBDPT,LOCSUB                                                    
*        MVC   DPTSTART,LOCSTART   SAVE DEPT START AND END DATES                
*        MVC   DPTEND,LOCEND                                                    
*        MVC   DPTSALDT,LOCSALKD   AND SAL LOCK DATE                            
         B     VK90                                                             
*                                                                               
VK89     LA    R2,SHMOFFH                                                       
         MVC   OFFICE(0),SHMOFF    SAVE OFFICE                                  
         EJECT                                                                  
***********************************************************************         
*        DISPLAY LOCATION                                                       
***********************************************************************         
*                                                                               
VK90     CLI   ACTEQU,ACTLIST                                                   
         BE    VK91                                                             
         MVC   SHMOFF(L'OFFICE),OFFICE                                          
         OI    SHMOFFH+6,X'80'                                                  
         MVC   SHMDEPT(L'DEPT),DEPT                                             
         OI    SHMDEPTH+6,X'80'                                                 
         MVC   SHMSDPT(L'SUBDPT),SUBDPT                                         
         OI    SHMSDPTH+6,X'80'                                                 
         GOTO1 =A(PAYCDS),DMCB,RR=RELO   MAKE TABLE OF PAYCDS                   
         B     VK91A                                                            
*                                                                               
VK91     MVC   SHLOFF(L'OFFICE),OFFICE                                          
         OI    SHLOFFH+6,X'80'                                                  
         MVC   SHLDEPT(L'DEPT),DEPT                                             
         OI    SHLDEPTH+6,X'80'                                                 
         MVC   SHLSDPT(L'SUBDPT),SUBDPT                                         
         OI    SHLSDPTH+6,X'80'                                                 
         GOTO1 =A(PAYCDS),DMCB,RR=RELO MAKE TABLE OF PAYCDS                     
         EJECT                                                                  
***********************************************************************         
*        BUILD KEY                                                              
***********************************************************************         
*                                                                               
VK91A    XC    BIGKEY,BIGKEY                                                    
         LA    R6,BIGKEY                                                        
         USING ACTRECD,R6                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVC   ACTKUNT(2),=C'1R'   1R                                           
         LA    R3,ACTKACT          BUILD ACCOUNT                                
         L     RE,=A(BILDACCT)                                                  
         A     RE,RELO                                                          
         USING BILDD,RE                                                         
VK92     CLI   BILDLEV,X'FF'                                                    
         BE    VK94                                                             
         SR    RF,RF                                                            
         ICM   RF,3,BILDLEV        LEVEL LENGTH OF R1                           
         LA    RF,STARTWRK(RF)                                                  
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         SR    R2,R2                                                            
         ICM   R2,3,BILDCODE       1R LEVEL CODE                                
         LA    R2,STARTWRK(R2)                                                  
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)                                                    
         LA    R3,1(R1,R3)         SET 1R FOR NEXT LEVEL                        
         LA    RE,BILDLEN(RE)                                                   
         B     VK92                                                             
         DROP  RE                                                               
VK94     LA    R2,SHMOFFH          CURSOR AT OFFICE ON ERROR                    
         CLI   ACTEQU,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,SHLOFFH                                                       
         GOTO1 TSTSEC,0            SECURITY CHECK OF 1R IN BIGKEY               
*                                                                               
         USING PHIRECD,R6                                                       
VK96     LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ    SALARY HISTORY RECORD                        
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,CMPY        COMPANY                                      
         MVC   PHIKOFC,OFFICE                                                   
         MVC   PHIKDPT,DEPT                                                     
         MVC   PHIKSBD,SUBDPT                                                   
         MVC   PHIKPER,PERSON                                                   
         MVC   PHIKMOA,STDATECM                                                 
         MVC   PHIKSEQ,SEQNUM                                                   
*                                                                               
         CLI   ACTEQU,ACTLIST                                                   
         BE    VKX                                                              
         CLC   BIGKEY(L'ACTKEY),SAVEKEY     ANY CHANGE OF KEY                   
         BNE   VK100                                                            
         TM    SPECBIT,CHAMOA      CHANGE IN MOA                                
         BO    VK100                                                            
         CLC   OPTSTAT,SVOPTSTA             IF OPTIONS CHANGE                   
         BNE   VK100                                                            
         TM    TRANSTAT,RCHANG              RECORD CHANGE                       
         BZ    VKX                                                              
*                                                                               
VK100    BAS   RE,CLRSCRN                   CLEAR SCREEN                        
         OI    BIT2,KEYCHNG                                                     
         MVC   SAVEKEY,BIGKEY                                                   
         GOTO1 =A(MAKETAB),DMCB,RR=RELO                                         
         XC    STDISP,STDISP                                                    
         XC    PRVSTDSP,PRVSTDSP                                                
         MVI   SHMUPDT,C' '                                                     
         TM    OPTSTAT,OPTSMON                                                  
         BNO   *+10                                                             
         MVC   SHMUPDT,AC@NOU                                                   
         BAS   RE,UNPROT           UNPROTECT DATE AND TYPE FIELDS               
*                                                                               
VKX      MVC   SAVEKEY,BIGKEY                                                   
         MVC   SVOPTSTA,OPTSTAT                                                 
         MVC   BIGKEY(L'ACTKEY),SAVEKEY                                         
         CLI   ACTEQU,ACTLIST                                                   
         BE    VKXX                                                             
*                                                                               
*** TRYING TO FIND BUG WHERE MAKETAB DOES NOT GET BUILT                         
*                                                                               
         CLC   ELNAME,=C'*HISTAB*'       HAS A TABLE BEEN BUILT                 
         BE    VKXX                                                             
         GOTO1 =A(MAKETAB),DMCB,RR=RELO                                         
         CLI   FIRSTIME,C'Y'       IS THIS THE 1ST TIME                         
         BNE   VKXX                THEN DONE                                    
         BAS   RE,CLRSCRN                   CLEAR SCREEN                        
         OI    BIT2,KEYCHNG                                                     
         XC    STDISP,STDISP                                                    
         XC    PRVSTDSP,PRVSTDSP                                                
         MVI   SHMUPDT,C' '                                                     
         TM    OPTSTAT,OPTSMON                                                  
         BNO   *+10                                                             
         MVC   SHMUPDT,AC@NOU                                                   
         BAS   RE,UNPROT           UNPROTECT DATE AND TYPE FIELDS               
         MVI   FIRSTIME,C'N'       NOT FIRST TIME ANYMORE                       
VKXX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE RECORD                                              *         
***********************************************************************         
*                                                                               
VR       DS    0H                                                               
*  RESETTING MOA DATES WHICH ARE OVERWRITTEN BY PREV VALOPTS CALL  *            
         GOTO1 =A(VALOPTS),DMCB,RR=RELO                                         
         MVC   BYTE,SHMMOAH+5      LEN FOR VALMOA                               
         LA    R3,SHMMOA           A(PERIOD)                                    
         BAS   RE,VALMOA                                                        
         BNE   EINVDATE                                                         
         CLI   ACTEQU,ACTDIS                                                    
         BE    DR                                                               
*                                                                               
*IF USER WAS PROMPTED TO ENTER MONTH OPTION, MUST REDISPLAY FIRST               
         TM    BIT2,NOMONTH                                                     
         BO    VRA                                                              
         CLC   THISLSEL,AC@CHAU    OR IF CAME FROM LIST W/CHANGE                
         BE    VRA                                                              
         L     R1,=A(DDSEL)        TRANSLATE 'SELECT'                           
         A     R1,RELO                                                          
         MVC   DDSELW,0(R1)                                                     
         GOTO1 DICTATE,DMCB,C'SU  ',DDSELW,0                                    
         CLC   THISLSEL,DDSELW     OR CAME FROM LIST W/SELECT                   
         BNE   VR01                                                             
VRA      CLI   ACTEQU,ACTADD       ACTION ADD?                                  
         BE    VR01                NOTHING TO DISPLAY                           
         NI    BIT2,X'FF'-NOMONTH                                               
         MVI   IOOPT,C'Y'          OWN IO                                       
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         B     DR                                                               
*                                                                               
VR01     OI    GENSTAT2,RETEQSEL                                                
         MVI   SEQNUM,0                                                         
         NI    BITS,X'FF'-NEWREC                                                
         NI    BIT2,X'FF'-REDISP                                                
         BAS   RE,MONOPEN          RECHECK THAT MONTHS ARE OPEN                 
         BNE   ACCERRX                                                          
         TM    OPTSTAT,OPTSMON     MONTH OPTION                                 
         BNO   *+8                                                              
         MVI   IOOPT,C'Y'          DISABLE CHANGES TO REC                       
         EJECT                                                                  
***********************************************************************         
*        CHECK IF ADDING OR CHANGING ELEMENT                                    
***********************************************************************         
*                                                                               
VR02     MVC   AIO,AIO1                                                         
         TM    OPTSTAT,OPTSMON                                                  
         BNO   VR03                                                             
         TM    BIT2,KEYCHNG                                                     
         BNO   VR04                                                             
         NI    BIT2,X'FF'-KEYCHNG                                               
VR03     GOTO1 =A(MAKETAB),DMCB,RR=RELO                                         
VR04     TM    OPTSTAT,OPTSMON                                                  
         BNO   *+8                                                              
         BAS   RE,BLDREC           BUILD REC FROM TABLE                         
*                                                                               
         USING ELEMTABD,R4                                                      
         LA    R4,ELEMBLK          FIRST DISPLAY LINE IN TABLE                  
         AH    R4,STDISP           DISP IN TABLE TO FIRST DISPLAYED             
         USING DSPLINED,R2                                                      
         LA    R2,SHMLIN1H         FIRST DISPLAY LINE                           
*                                                                               
VR05NX   TM    TRANSTAT,RACHANG    REC OR ACTION CHANGE?                        
         BO    *+16                                                             
         MVC   ELPRDATE,ELDATE     IF SO-MOVE IN PREV DATE AND NUM SO           
         MVC   ELPRNUM,ELNUM       WILL NOT THINK IT'S A NEW ELEM               
         ZAP   SVAMT,=P'0'                                                      
         ZAP   SVADJ,=P'0'                                                      
         MVC   SVSTAT,ELSTAT                                                    
         MVC   SVSTAT2,ELSTAT2                                                  
         OC    ELPRDATE,ELPRDATE   NO PREVIOUS DATE = NEW ELEM                  
         BZ    VR100                                                            
         TM    ELSTAT,PDERVRSL     SKIP REVERSALS                               
         BO    VR140                                                            
         TM    OPTSTAT,OPTSMON     MONTH OPTION ON FOR PERCENTAGES              
         BNO   VR06                                                             
         TM    ELSTAT,PDEPCS                                                    
         BNZ   VR140                                                            
         EJECT                                                                  
***********************************************************************         
*        MAKING A CHANGE - DELETE OLD ELEM                                      
***********************************************************************         
*                                                                               
VR06     CLC   SEQNUM,ELSEQ        DO WE HAVE THE RIGHT SEQ NUM REC             
         BE    VR07                                                             
         MVC   SEQNUM,ELSEQ        THEN ALWAYS START LOOKING IN FIRST           
         BAS   RE,CHKDEL           CHECK IF SHOULD BE DELETED                   
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
         BAS   RE,NEXTSEQ          WILL READ FOR UPDATE 00 SEQ REC              
*                                                                               
         USING PDEELD,R6                                                        
VR07     L     R6,AIO              LOOK FOR MATCH ON ELEMENT                    
         MVI   ELCODE,PDEELQ                                                    
         BAS   RE,GETEL                                                         
         B     VR08                                                             
VR08NX   BAS   RE,NEXTEL                                                        
VR08     BNE   VR100               GO ADD                                       
         CLC   ELPRDATE,PDEDTE     SAME DATE                                    
         BNE   VR08NX                                                           
         CLC   ELPRNUM,PDENUM      SAME NUMBER                                  
         BNE   VR08NX                                                           
         ZAP   SVAMT,PDEAMT                                                     
         ZAP   SVADJ,PDEADJ                                                     
         MVC   SVSTAT,PDESTAT                                                   
         MVC   SVSTAT2,PDESTAT2                                                 
*                                                                               
         MVI   0(R6),X'FF'         DELETE OLD ELEM                              
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         XC    0(ELLEN,R4),0(R4)   CLEAR ENTRY IN TABLE                         
         EJECT                                                                  
***********************************************************************         
*        ADDING NEW ELEMENT                                                     
***********************************************************************         
*                                                                               
VR100    DS    0H                  CHECK FOR ELEMENTS TO ADD                    
         CLI   DSPDATEH+5,0        ANYTHING ON LINE                             
         BNE   VR104                                                            
         CLI   DSPCODEH+5,0                                                     
         BNE   VR104                                                            
         CLI   DSPADJH+5,0                                                      
         BNE   VR104                                                            
         XC    0(ELLEN,R4),0(R4)   CLEAR ENTRY IN TABLE                         
         B     VR140               NO, CHECK NEXT LINE                          
         EJECT                                                                  
***********************************************************************         
*        VALIDATE DATE                                                          
***********************************************************************         
*                                                                               
VR104    ZAP   ELAMT,SVAMT         MOVE IN SAVED INFO                           
         ZAP   ELADJ,SVADJ                                                      
         MVC   ELSTAT,SVSTAT                                                    
         MVC   ELSTAT2,SVSTAT2                                                  
*                                                                               
         TM    4(R2),X'80'         DATE INPUT THIS TIME?                        
         BZ    *+8                 NO - DO NOT TURN OFF 'LAST' BIT              
         NI    ELSTAT2,X'FF'-PDESLAST                                           
*                                                                               
         CLI   DSPDATEH+5,0        ANY DATE?                                    
         BNE   *+12                YES                                          
         LA    R2,DSPDATEH                                                      
         B     ERRMISS                                                          
         CLI   SHMLIN1,C'"'        NOT AVAILABLE ON FIRST LINE                  
         BE    EINVDATE                                                         
         CLI   DSPDATE,C'"'        SAME DATE AS PREVIOUS?                       
         BNE   *+16                                                             
         MVC   DSPDATE,PREVDATE                                                 
         MVC   DSPDATEH+5(1),PREVDTLN                                           
         MVC   PREVDATE,DSPDATE                                                 
         MVC   PREVDTLN,DSPDATEH+5                                              
         XC    PVALBLK,PVALBLK                                                  
         LA    R3,PVALBLK                                                       
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'40'                                                       
         GOTO1 PERVAL,DMCB,(DSPDATEH+5,DSPDATE),(BYTE,PVALBLK)                  
*                                                                               
         TM    DSPDATEH+4,X'80'    ENTERED THIS TIME                            
         BNO   *+8                                                              
         OI    BIT2,REDISP                                                      
         CLC   AC@LASTU,DSPDATE    LAST DAY OF MONTH                            
         BE    VR107                                                            
         CLC   AC@FRSTU,DSPDATE    FIRST DAY OF MONTH                           
         BNE   VR108                                                            
*                                                                               
VR107    MVC   YYMMDD(2),STDATE                                                 
         MVI   YYMMDD+2,X'01'                                                   
         GOTO1 DATCON,DMCB,(1,YYMMDD),(11,WORK+10)                              
         MVC   WORK(3),WORK+10                                                  
         MVC   WORK+3(3),WORK+15                                                
         XC    DMCB,DMCB                                                        
         LA    R1,WORK                                                          
         ST    R1,DMCB             MONTH                                        
         MVI   DMCB,6                                                           
         B     VR109                                                            
*                                                                               
VR108    XC    DMCB,DMCB                                                        
         LR    R1,R2                                                            
         LA    R1,8(R1)                                                         
         ST    R1,DMCB             DATE FIELD                                   
         ZIC   R1,5(R2)                                                         
         STC   R1,DMCB                                                          
*                                                                               
         USING PERVALD,R3                                                       
VR109    LA    R3,PVALBLK                                                       
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'40'                                                       
         GOTO1 PERVAL,DMCB,,(BYTE,PVALBLK)                                      
         CLI   DMCB+4,X'01'                                                     
         BE    EINVDATE                                                         
         MVC   INDATE,PVALPSTA                                                  
         CLC   AC@LASTU,DSPDATE    USE LAST DAY OF MONTH (MONTH OPT)            
         BNE   VR111                                                            
         MVC   INDATE,PVALPEND                                                  
         OI    ELSTAT2,PDESLAST    USE LAST DAY ON MONTH                        
         DROP  R3                                                               
*                                                                               
VR111    CLC   STMOA,INDATE        SAME MONTH?                                  
*&&US*&& BNE   EINVDATE                                                         
*&&UK                                                                           
         BE    VR111A                                                           
         MVC   TEMP,STMOA                                                       
         GOTO1 =A(MOAEND),DMCB,RR=RELO                                          
         BNE   EINVDATE                                                         
         CLC   INDATE,EPDATE                                                    
         BH    EINVDATE                                                         
*&&                                                                             
VR111A   TM    SPECBIT,SALOLOC     ARE WE TRYING TO ADD SALARY OUTSIDE          
         BZ    *+12                OF A PERSONS LOCATION                        
         TM    SPECBIT,ADJYES      IS THERE AN ADJUSTMENT                       
         BO    VR113               IS SO SAME MONTH IS ENOUGH                   
*                                                                               
         OI    SPECBIT,VALPAYDT    SET BIT TO VALIDATE PAYDATE                  
         GOTO1 =A(VALLOC),DMCB,RR=RELO                                          
         BE    VR112                                                            
*                                                                               
         TM    SPECBIT,ADJYES      IS THERE AN ADJUSTMENT                       
         BZ    EPERDATE            IS SO SAME MONTH IS ENOUGH                   
         OI    SPECBIT,SALOLOC     TRYING TO ADD SALARY OUTSIDE LOC             
*                                                                               
VR112    NI    SPECBIT,X'FF'-VALPAYDT                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PAYROLL CODE                                                  
***********************************************************************         
*                                                                               
VR113    GOTO1 =A(PAYCDS),DMCB,RR=RELO                                          
         CLI   DSPCODE,C'"'        SAME CODE AS PREVIOUS?                       
         BNE   *+10                NO                                           
         MVC   DSPCODE,PREVCODE                                                 
*                                                                               
         MVC   PREVCODE,DSPCODE                                                 
         USING PAYCDTAB,R3                                                      
         L     R3,ADISPBLK         CHECK TABLE FOR CODE                         
         B     VR115                                                            
*                                                                               
VR110NX  LA    R3,PAYCDLEN(R3)     NEXT ENTRY                                   
         L     R1,PAYEND                                                        
         CR    R3,R1                                                            
         BL    *+12                                                             
         LA    R2,DSPCODEH                                                      
         B     EINVTYPE                                                         
         OC    PAYCDNUM(PAYCDLEN),PAYCDNUM                                      
         BNZ   VR115                                                            
         LA    R2,DSPCODEH                                                      
         B     EINVTYPE            DIDN'T FIND MATCH                            
VR115    OC    DSPCODE,SPACES                                                   
         CLC   DSPCODE,PAYCDNME    MATCH ON PAYROLL CODE                        
         BNE   VR110NX                                                          
*                                                                               
         TM    SPECBIT,SALOLOC     ARE WE OUTSIDE THE LOC DATE?                 
         BZ    VR115A                                                           
         TM    PAYCDST,PAYADJRT    THEN ONLY ALLOW ADJ PAYCODE                  
         BO    VR115A                                                           
         LA    R2,DSPCODEH                                                      
         B     EINVADJT                                                         
*                                                                               
VR115A   TM    DSPCODEH+4,X'20'    FIELD VALIDATED?                             
         BO    VR115Z                                                           
VR115B   TM    PAYCDST,PAYADJRT    USING ADJUSTED PAYCODE?                      
         BZ    VR115Z                                                           
         TM    OPTSTAT,OPTSMON+OPTSANN ALSO USING MONTH OR ANNUAL OPT?          
         BZ    VR115Z                                                           
         CLC   STMOA,ENDMOA       ENTERING FOR ONE MONTH ONLY?                  
         BE    VR115Z              THEN IT'S ALLOWED                            
         LA    R2,DSPCODEH         ELSE CAN'T ENTER FOR MORE THAN               
         B     EINVYTD             ONE MONTH                                    
*                                                                               
VR115Z   OI    DSPCODEH+4,X'20'    VALIDATED                                    
         MVC   PCDNUM,PAYCDNUM     SAVE MATCHING NUMBER                         
         MVC   INNUM,PAYCDNUM                                                   
*                                                                               
VR116    BAS   RE,DUPCHK           CHECK FOR DUPLICATE ELEMENT                  
         BE    EDUPEN                                                           
         MVC   ELDATE,INDATE       MOVE NEW DATA INTO TABLE ENTRY               
         MVC   ELNUM,INNUM                                                      
*                                                                               
         NI    ELSTAT2,X'FF'-PDESHRTE      IS PAYCODE AN HRATE                  
         TM    PAYCDST,PAYSHRTE                                                 
         BNO   *+8                                                              
         OI    ELSTAT2,PDESHRTE                                                 
*                                                                               
         NI    ELSTAT2,X'FF'-PDESADJ       IS PAYCODE AN ADJ                    
         TM    PAYCDST,PAYADJRT                                                 
         BNO   *+8                                                              
         OI    ELSTAT2,PDESADJ                                                  
*                                                                               
         TM    DSPCODEH+4,X'80'    ENTERED THIS TIME                            
         BNO   *+8                                                              
         OI    BIT2,REDISP                                                      
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE AMOUNT (ONLY FOR MONTH OPTION)                                
***********************************************************************         
*                                                                               
VR120    TM    OPTSTAT,OPTSMON     MONTH OPTION                                 
         BNO   VR125                                                            
*                                                                               
         TM    DSPAMNTH+1,X'20'    PROTECTED                                    
         BO    VR125                                                            
*                                                                               
         LA    R3,DSPAMNTH                                                      
         GOTO1 =A(VALAMT),DMCB,RR=RELO                                          
*                                                                               
         ZAP   ELAMT(6),WORK(8)                                                 
         TM    DSPAMNTH+4,X'80'    ENTERED THIS TIME                            
         BNO   *+8                                                              
         OI    BIT2,REDISP                                                      
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ADJUSTMENT                                                    
***********************************************************************         
*                                                                               
VR125    TM    OPTSTAT,OPTSMON     MONTH OPTION                                 
         BNO   VR127                                                            
         CLI   DSPADJH+5,0         ANY ADJUSTMENT                               
         BNE   VR127                                                            
         ZAP   ELADJ(6),=P'0'      USE ADJ=0 IF NO ENTRY W/MONTH OPT            
         B     VR129                                                            
*                                                                               
VR127    LA    R3,DSPADJH                                                       
         GOTO1 =A(VALAMT),DMCB,RR=RELO                                          
         ZAP   ELADJ(6),WORK(8)                                                 
*                                                                               
VR129    TM    DSPADJH+4,X'80'    ENTERED THIS TIME                             
         BNO   *+8                                                              
         OI    BIT2,REDISP                                                      
         EJECT                                                                  
***********************************************************************         
*        CREATE ELEMENT                                                         
***********************************************************************         
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PDEELD,R6                                                        
         MVI   PDEEL,PDEELQ                                                     
         MVC   PDEDTE,ELDATE                                                    
         MVC   PDENUM,ELNUM                                                     
         MVC   PDESTAT,ELSTAT                                                   
         MVC   PDESTAT2,ELSTAT2                                                 
         ZAP   PDEAMT(6),ELAMT(6)                                               
         ZAP   PDEADJ(6),ELADJ(6)                                               
*                                                                               
         LA    R0,PDELNQ            DESCRIPTION AND LENGTH                      
         ZIC   R1,DSPDESCH+5                                                    
         STC   R1,ELDESCLN                                                      
         AR    R0,R1                                                            
         STC   R0,PDELN                                                         
         SH    R1,=H'1'                                                         
         BM    VR130                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PDEDESC(0),DSPDESC                                               
         OC    PDEDESC,SPACES                                                   
         MVC   ELDESC,PDEDESC                                                   
*                                                                               
VR129B   TM    DSPDESCH+4,X'80'    ENTERED THIS TIME                            
         BNO   *+8                                                              
         OI    BIT2,REDISP                                                      
         EJECT                                                                  
***********************************************************************         
*        ADD ELEMENT AND BUMP TO NEXT LINE                                      
***********************************************************************         
*                                                                               
VR130    BAS   RE,ADDS             ADDS ELEM IF CAN                             
         BE    VR135               EVERYTHING OK                                
         BAS   RE,CHKDEL           CHECK IF SHOULD BE DELETED                   
         GOTO1 PUTREC                                                           
         GOTO1 WRITE               WRITE OUT PREVIOUS REC FIRST                 
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         BAS   RE,NEXTSEQ          CHECK FOR NEXT SEQ REC                       
         BE    VR130               YES FOUND, ADD ELEM TO THAT REC              
         BAS   RE,NEWSEQ           MAKE NEW KEY IN AIO TO ADD                   
         B     VR130               ADD ELEM TO NEW REC                          
*                                                                               
VR135    MVC   ELPRDATE,ELDATE     ELEM HAS BEEN ADDED                          
         MVC   ELPRNUM,ELNUM                                                    
VR140    LA    R4,ELLEN(R4)        NEXT ENTRY IN TABLE                          
         LA    R2,DSPLLEN(R2)      NEXT SCREEN LINE                             
         LA    R1,SHMWORDH         END OF SCREEN                                
         CR    R2,R1                                                            
         BL    VR05NX              ADD NEXT LINE                                
         EJECT                                                                  
***********************************************************************         
*        SET UP REVERSALS, RECALCULATE PERCENTAGES AND UPDATE RECORD            
***********************************************************************         
*                                                                               
VR150    GOTO1 =A(REVELEM),DMCB,RR=RELO                                         
         GOTO1 =A(PERCENT),DMCB,RR=RELO                                         
         TM    OPTSTAT,OPTSMON     MONTH OPTION                                 
         BNO   VR152               RECALC PERCENTS                              
         CLC   SHMUPDT,AC@YESU      IS Y IN UPDATE                              
         BE    VR152                                                            
         TM    SHMUPDTH+1,X'20'    PROTECTED                                    
         BE    *+14                                                             
         CLC   SHMUPDT,AC@NOU       IS N IN UPDATE                              
         BNE   VR151                                                            
         OI    GENSTAT2,USMYOK                                                  
         LA    R1,SHMUPDTH                                                      
         ST    R1,ACURFORC                                                      
         B     VRX                                                              
VR151    LA    R2,SHMUPDTH                                                      
         B     ERRINV              NEED MESSAGE ?????                           
*                                                                               
VR152    MVI   IOOPT,C'Y'          DOING MY OWN IO                              
         L     R6,AIO                                                           
         MVC   BIGKEY(L'ACTKEY),0(R6)                                           
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),SAVEKEY                                         
         BNE   VR155               GO ADD THIS NEW RECORD                       
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         BAS   RE,DELOFF           TURN OFF DELETE BITS                         
         L     R6,AIO3                                                          
         CLC   BIGKEY(L'ACTKEY),0(R6)                                           
         BNE   VRX                                                              
         MVC   AIO,AIO1                                                         
         BAS   RE,CHKDEL           CHECK IF SHOULD BE DELETED                   
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
         B     VRX                                                              
*                                                                               
VR155    GOTO1 ADDREC                                                           
*                                                                               
VRX      B     XIT                 DISPLAY REC CHANGES                          
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY KEY                                                            
***********************************************************************         
*                                                                               
         USING PHIRECD,R6                                                       
DK       LA    R6,BIGKEY                                                        
*                                                                               
         MVC   SAVEKEY,BIGKEY                                                   
         XC    YYMMDD,YYMMDD                                                    
         SR    R1,R1                                                            
         ICM   R1,3,PHIKMOA                                                     
         LNR   R1,R1                                                            
         STCM  R1,3,YYMMDD                                                      
         MVC   STDATE,YYMMDD                                                    
         MVC   STMOA,YYMMDD                                                     
         MVC   ENDMOA,YYMMDD                                                    
         XC    SHMMOA,SHMMOA                                                    
         GOTO1 DATCON,DMCB,(1,YYMMDD),(9,SHMMOA)                                
         OI    SHMMOAH+6,X'80'                                                  
*                                                                               
         MVC   SHMMETH,METHCODE     PUT OUT CODE                                
         OI    SHMMETHH+6,X'80'                                                 
         MVC   SHMMNME,METHNAME    DISPLAY METHOD NAME                          
         OI    SHMMNMEH+6,X'80'                                                 
         MVC   SHMNAME,PERNAME     DISPLAY PERSON NAME                          
         OI    SHMNAMEH+6,X'80'                                                 
*                                                                               
         NI    SPECBIT,X'FF'-VALPAYDT                                           
         GOTO1 =A(VALLOC),DMCB,RR=RELO  VALIDATE LOCATION GIVEN                 
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY RECORD                                               *         
***********************************************************************         
*                                                                               
DR       NI    BITS,X'FF'-MULTRECS MORE THAT 1 SEQ NUM                          
         BAS   RE,CLRSCRN          CLEAR LOWER SCREEN                           
         BAS   RE,UNPROT           UNPROTECT ALL DATE AND TYPE FIELDS           
         CLI   ACTEQU,ACTDIS                                                    
         BNE   DR02                                                             
         TM    OPTSTAT,OPTSMON     MONTH OPTION                                 
         BZ    DR02                                                             
         LA    R6,BIGKEY                                                        
         USING PHIRECD,R6                                                       
         MVC   SVDSKADR,PHIKDA    SAVE D/A FOR EXTRA DETAILS DISPLAY            
         B     DR05                                                             
         DROP  R6                                                               
DR02     TM    OPTSTAT,OPTSMON                                                  
         BNO   DR05                                                             
         TM    BIT2,KEYCHNG                                                     
         BNO   DR10                                                             
         NI    BIT2,X'FF'-KEYCHNG                                               
DR05     GOTO1 =A(MAKETAB),DMCB,RR=RELO                                         
         EJECT                                                                  
***********************************************************************         
*        DISPLAY FROM SORTED TABLE                                              
***********************************************************************         
*                                                                               
DR10     TM    OPTSTAT,OPTXDET     WANT EXTRA DETAILS                           
         BZ    *+8                                                              
         GOTO1 =A(SHOWXDET),DMCB,RR=RELO SHOW EXTRA DETAILS                     
         LA    R4,ELEMBLK                                                       
         USING ELEMTABD,R4                                                      
         LA    R2,SHMLIN1H         FIRST DISPLAY LINE                           
         USING DSPLINED,R2                                                      
*                                  WHERE TO START DISPLAYING                    
         TM    BIT2,REDISP                                                      
         BO    DR37                                                             
         CLC   SVMTHNUM,METHNUM    SAME METHOD                                  
         BNE   DR37                                                             
*                                                                               
         CLI   PFKEY,0                                                          
         BNE   DR32                                                             
         CLI   ACTEQU,ACTSEL                                                    
         BE    DR37                                                             
         B     DR38                                                             
*                                                                               
DR32     CLI   PFKEY,7             UP                                           
         BNE   DR34                                                             
         MVC   STDISP,PRVSTDSP                                                  
         LA    R1,ELLEN                                                         
         MH    R1,=H'8'            8 LINES                                      
         LH    R0,PRVSTDSP                                                      
         SR    R0,R1                                                            
         CH    R0,=H'0'                                                         
         BNL   *+6                 DISP FROM TOP                                
         SR    R0,R0                                                            
         STH   R0,PRVSTDSP                                                      
         B     DR38                                                             
*                                                                               
DR34     CLI   PFKEY,8             DOWN                                         
         BNE   DR38                                                             
         MVC   PRVSTDSP,STDISP                                                  
         MVC   STDISP,DLINE1                                                    
         B     DR38                                                             
*                                                                               
DR37     MVC   STDISP,=H'0'        DEFAULT TO BEGINNING                         
         MVC   PRVSTDSP,=H'0'                                                   
         NI    BIT2,X'FF'-REDISP                                                
*                                                                               
DR38     MVC   SVMTHNUM,METHNUM                                                 
         LA    R0,ELLEN            LENGTH OF ONE ENTRY                          
         MH    R0,TABCOUNT         NUMBER OF ENTRIES                            
         LH    R1,STDISP                                                        
         CR    R0,R1                                                            
         BH    DR39                                                             
         LA    R1,0                                                             
         STH   R1,STDISP                                                        
DR39     AR    R4,R1                                                            
*                                                                               
DR40NX   LA    R1,ELEMBLK                                                       
         LR    R0,R4               R4 POINTS TO ELEMENT BEING DISPLAYED         
         SR    R0,R1                                                            
         STH   R0,DLINE1           SAVE DISPLACEMENT INTO TABLE                 
*                                                                               
         OC    0(ELCHKENT,R4),0(R4)        ANY MORE ENTRIES?                    
         BNZ   DR42                                                             
         NI    GENSTAT2,X'FF'-RETEQSEL     GET NEXT SELECTION                   
         LA    R0,0                START FROM TOP NEXT ENTER                    
         STH   R0,DLINE1                                                        
         B     DR100                                                            
*                                                                               
DR42     GOTO1 =A(PAYCDS),DMCB,RR=RELO     MAKE TABLE OF PAYCDS                 
         GOTO1 DATCON,DMCB,(1,ELDATE),(17,DSPDATE)                              
         OI    DSPDATEH+6,X'80'                                                 
         TM    ELSTAT,PDERVRSL     ALWAYS PROTECT FOR REVERSALS                 
         BO    DR45                                                             
         TM    OPTSTAT,OPTSMON     UNPROTECT FOR MONTH OPTION                   
         BNO   DR44                                                             
         TM    ELSTAT,PDEPCS       PROTECT MONTH OPTION PERCENTS                
         BNZ   DR45                                                             
         B     DR45A                                                            
DR44     CP    ELAMT(6),=P'0'      PROTECT NON MONTH OPT WITH AMT               
         BE    *+8                                                              
DR45     OI    DSPDATEH+6,X'20'     PROTECT                                     
*                                                                               
DR45A    MVC   PCDNUM,ELNUM                                                     
         BAS   RE,DISPCD          DISPLAY CODE FROM PAYROLL CODE REC            
         TM    ELSTAT,PDERVRSL     ALWAYS PROTECT FOR REVERSALS                 
         BO    DR47                                                             
         TM    OPTSTAT,OPTSMON     UNPROTECT FOR MONTH OPTION                   
         BNO   DR46                                                             
         TM    ELSTAT,PDEPCS       PROTECT MONTH OPTION PERCENTS                
         BNZ   DR47                                                             
         B     DR47A                                                            
DR46     CP    ELAMT(6),=P'0'      PROTECT NON MONTH OPT WITH AMT               
         BE    *+8                                                              
DR47     OI    DSPCODEH+6,X'20'     PROTECT                                     
*                                                                               
DR47A    GOTO1 =A(GETTYPE),DMCB,RR=RELO  DISPLAY TYPE FROM METHOD REC           
         USING TOTD,R3                                                          
         MVC   DSPTYPE,TOTCHAR                                                  
         OI    DSPTYPEH+6,X'80'                                                 
         DROP  R3                                                               
*                                                                               
         LA    R3,ELAMT            DISPLAY AMOUNT                               
         LA    R6,DSPAMNTH                                                      
         OC    DSPAMNT,SPACES                                                   
         GOTO1 =A(DISAMT),DMCB,RR=RELO                                          
         OI    DSPAMNTH+6,X'80'    XMIT                                         
         OI    DSPAMNTH+4,X'20'    VALIDATED                                    
         TM    ELSTAT,PDERVRSL     IS THIS ELEM A REVERSAL                      
         BO    DR49                                                             
         TM    OPTSTAT,OPTSMON     PROTECT NON MONTH OPTION                     
         BNO   DR49                                                             
         TM    ELSTAT,PDEPCS       PROTECT MONTH OPTION PERCENTS                
         BZ    DR50                                                             
DR49     OI    DSPAMNTH+6,X'20'    PROTECT AMOUNT                               
*                                                                               
DR50     LA    R3,ELADJ            DISPLAY ADJUSTMENT                           
         LA    R6,DSPADJH                                                       
         OC    DSPADJ,SPACES                                                    
         GOTO1 =A(DISAMT),DMCB,RR=RELO                                          
         OI    DSPADJH+6,X'80'                                                  
         OI    DSPADJH+4,X'20'     VALIDATED                                    
         TM    ELSTAT,PDERVRSL     ALWAYS PROTECT FOR REVERSALS                 
         BO    DR52                                                             
         TM    OPTSTAT,OPTSMON                                                  
         BNO   DR54                                                             
         TM    ELSTAT,PDEPCS       PROTECT MONTH OPTION PERCENTS                
         BZ    DR54                                                             
DR52     OI    DSPADJH+6,X'20'     PROTECT                                      
*                                                                               
DR54     ZAP   WORK2(8),ELAMT(6)                                                
         AP    WORK2(8),ELADJ(6)                                                
         LA    R3,WORK2+2                                                       
         LA    R6,DSPTOTH                                                       
         GOTO1 =A(DISAMT),DMCB,RR=RELO                                          
         OI    DSPTOTH+6,X'80'                                                  
*                                                                               
         MVC   DSPDESC,ELDESC                                                   
         MVC   DSPDESCH+5(1),ELDESCLN                                           
         OI    DSPDESCH+6,X'80'                                                 
         TM    ELSTAT,PDERVRSL     ALWAYS PROTECT FOR REVERSALS                 
         BO    DR56                                                             
         TM    OPTSTAT,OPTSMON                                                  
         BNO   DR58                                                             
         TM    ELSTAT,PDEPCS       PROTECT MONTH OPTION PERCENTS                
         BZ    DR58                                                             
DR56     OI    DSPDESCH+6,X'20'     PROTECT                                     
*                                                                               
DR58     LA    R2,DSPLLEN(R2)      NEXT SCREEN LINE                             
         LA    R1,SHMWORDH         END OF LIST                                  
         CR    R2,R1                                                            
         BNL   DR100               DISP TOTALS                                  
         LA    R4,ELLEN(R4)        NEXT TABLE ENTRY                             
         B     DR40NX                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY TOTALS                                                         
***********************************************************************         
*                                                                               
DR100    BAS   RE,TOTALS                                                        
         USING SUMLINED,R2                                                      
         LA    R2,SHMSALH                                                       
         USING TOTD,R3                                                          
         L     R3,ATOTTAB                                                       
*                                                                               
DR110    LA    R4,STARTWRK                                                      
         AH    R4,TOTDISP          DISP TO ACCUMULATOR                          
         AP    AMTTOT(8),0(8,R4)                                                
         CURED (P8,0(R4)),(11,SUMAMNT),2,FLOAT=-,ZERO=NOBLANK                   
         ZAP   WORK2(8),0(8,R4)                                                 
         LA    R4,STARTWRK                                                      
         AH    R4,TOTADJ           DISP TO ADJUST ACCUMULATOR                   
         AP    ADJTOT(8),0(8,R4)                                                
         CURED (P8,0(R4)),(11,SUMADJ),2,FLOAT=-,ZERO=NOBLANK                    
         AP    WORK2(8),0(8,R4)                                                 
         CURED (P8,WORK2),(11,SUMTOT),2,FLOAT=-,ZERO=NOBLANK                    
         OI    SUMHEAD+6,X'80'                                                  
*                                                                               
         LA    R2,SUMLLEN(R2)      NEXT SCREEN LINE                             
         LA    R3,TOTLEN(R3)       NEXT TABLE ENTRY                             
         CLI   0(R3),X'00'                                                      
         BNE   DR110                                                            
*                                                                               
         CURED (P8,AMTTOT),(11,SUMAMNT),2,FLOAT=-,ZERO=NOBLANK                  
         CURED (P8,ADJTOT),(11,SUMADJ),2,FLOAT=-,ZERO=NOBLANK                   
         ZAP   WORK2(8),AMTTOT                                                  
         AP    WORK2(8),ADJTOT                                                  
         CURED (P8,WORK2),(11,SUMTOT),2,FLOAT=-,ZERO=NOBLANK                    
         B     DRX                                                              
         DROP  R2,R3                                                            
*                                                                               
DRX      DS    0H                                                               
         MVI   SHMUPDT,C' '                                                     
         TM    OPTSTAT,OPTSMON                                                  
         BNO   *+10                                                             
         MVC   SHMUPDT,AC@NOU                                                   
         OI    SHMUPDTH+6,X'80'                                                 
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'SAVEKEY),SAVEKEY                                        
         TM    GENSTAT2,USMYOK                                                  
         BZ    *+8                                                              
         B     EYUPD                                                            
         LA    R1,SHMLIN1H                                                      
         ST    R1,ACURFORC                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        BUILD RECORD FROM DATA IN TABLE                                        
***********************************************************************         
*                                                                               
BLDREC   NTR1                      MAKES REC FROM TABLE                         
         MVI   ELCODE,PDEELQ       X'86'                                        
         GOTO1 REMELEM                                                          
*                                                                               
         USING PDEELD,R6                                                        
         LA    R6,ELEM             BUILD ELEMS IN ELEM                          
         USING ELEMTABD,R4                                                      
         LA    R4,ELEMBLK          BLOCK OF ELEMENTS                            
BR10NX   OC    0(ELCHKENT,R4),0(R4)   ANY ENTRY                                 
         BZ    BR50NX                                                           
*                                                                               
         XC    ELEM,ELEM           FILL IN ELEM                                 
         MVI   PDEEL,PDEELQ                                                     
         MVC   PDEDTE,ELDATE                                                    
         MVC   PDENUM,ELNUM                                                     
         MVC   PDESTAT,ELSTAT                                                   
         MVC   PDESTAT2,ELSTAT2                                                 
         ZAP   PDEAMT(6),ELAMT(6)                                               
         ZAP   PDEADJ(6),ELADJ(6)                                               
*                                                                               
         LA    R0,PDELNQ            DESCRIPTION AND LENGTH                      
         ZIC   R1,ELDESCLN                                                      
         AR    R0,R1                                                            
         STC   R0,PDELN                                                         
         SH    R1,=H'1'                                                         
         BM    BR20                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PDEDESC(0),ELDESC                                                
BR20     GOTO1 ADDELEM             ADD ELEMENT                                  
*                                                                               
BR50NX   LA    R4,ELLEN(R4)        NEXT ENTRY                                   
         LA    R1,ELEMEND          END OF BLOCK                                 
         CR    R4,R1                                                            
         BL    BR10NX                                                           
         B     XIT                                                              
         DROP  R6,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATES MOA RANGE AT R3 WITH LENGTH IN BYTE                          
*        OUTPUT IN STMOA AND ENDMOA                                             
***********************************************************************         
*                                                                               
VALMOA   NTR1                                                                   
         XC    STMOA,STMOA                                                      
         XC    ENDMOA,ENDMOA                                                    
         ZIC   R2,BYTE                                                          
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,((R2),(R3)),(BYTE,BLOCK)                             
         LA    R1,BLOCK                                                         
         USING PERVALD,R1                                                       
         CLI   DMCB+4,PVRCINV1     DATE 1 INVALID                               
         BE    VMXNO                                                            
         CLI   DMCB+4,PVRCONE      ONLY ENTERED IN ONE DATE                     
         BE    VM10                                                             
         CLI   DMCB+4,PVRCINV2     DATE 2 INVALID                               
         BE    VMXNO                                                            
*                                                                               
VM10     MVC   STMOA,PVALPSTA                                                   
         MVC   ENDMOA,PVALPEND                                                  
         MVC   STDATE,PVALPSTA                                                  
         MVC   ENDATE,PVALPEND                                                  
         ZICM  R1,STDATE,2         USE 'FFFF'-YYMM                              
         LNR   R1,R1                                                            
         STH   R1,STDATECM         COMPLEMENT                                   
         ZICM  R1,ENDATE,2         USE 'FFFF'-YYMM                              
         LNR   R1,R1                                                            
         STH   R1,ENDATECM         COMPLEMENT                                   
VMXYES   B     XYES                                                             
VMXNO    B     XNO                                                              
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        IS MOA OPEN                                                            
***********************************************************************         
*                                                                               
MONOPEN  NTR1                                                                   
         CLI   ACTEQU,ACTDIS       SKIP FOR ACTION DISPLAY                      
         BE    XYES                                                             
         MVC   YYMMDD(2),STDATE                                                 
         MVI   YYMMDD+2,X'01'                                                   
         GOTO1 DATCON,DMCB,(1,YYMMDD),(10,WORK)                                 
*&&US*&& GOTO1 VBMONVAL,DMCB,(8,WORK),(97,ACOMFACS),(0,BLOCK),(CMPY,0)          
*&&UK                                                                           
         GOTO1 VBMONVAL,DMCB,(8,WORK),(97,ACOMFACS),(0,BLOCK),         X        
               (CMPY,TWAACCS)                                                   
*&&                                                                             
         LA    R1,BLOCK                                                         
         USING BMONVALD,R1                                                      
         CLI   BMOERR,BMOEOKQ      EVERYTHING OK?                               
         BE    XYES                                                             
         MVC   GERROR,BMOMSG       SET ERROR MESSAGE                            
         B     XNO                                                              
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK FIELD SECURITY TO SEE WHETHER TO DISPLAY PERSON NAME             
***********************************************************************         
*                                                                               
FLDSEC   NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         MVI   SBYTE,NAMEFLDQ                                                   
         LA    R2,SBYTE                                                         
         GOTO1 SECRET,DMCB,('SECPFLDP',ASECBLK),(R2)                            
         CLI   DMCB,SECPYES                                                     
         BE    XYES                                                             
         CLI   DMCB,SECPREAD                                                    
         BE    XYES                                                             
         B     XNO                                                              
         EJECT                                                                  
***********************************************************************         
*        CLEAR TOTALS                                                           
***********************************************************************         
*                                                                               
CLRTOTS  NTR1                                                                   
         ZAP   PENMTOT,=P'0'       INITIALIZE TOTALS                            
         ZAP   BENMTOT,=P'0'                                                    
         ZAP   SALMTOT,=P'0'                                                    
         ZAP   INDMTOT,=P'0'                                                    
         ZAP   OTHMTOT,=P'0'                                                    
         ZAP   PENTOT,=P'0'                                                     
         ZAP   BENTOT,=P'0'                                                     
         ZAP   SALTOT,=P'0'                                                     
         ZAP   INDTOT,=P'0'                                                     
         ZAP   PENADJ,=P'0'                                                     
         ZAP   BENADJ,=P'0'                                                     
         ZAP   SALADJ,=P'0'                                                     
         ZAP   INDADJ,=P'0'                                                     
         ZAP   OTHADJ,=P'0'                                                     
         ZAP   MONTOT,=P'0'                                                     
         ZAP   ALLTOT,=P'0'                                                     
         ZAP   ADJTOT,=P'0'                                                     
         ZAP   AMTTOT,=P'0'                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CALCULATE  TOTALS                                                      
***********************************************************************         
*                                                                               
TOTALS   NTR1                                                                   
         BAS   RE,CLRTOTS                                                       
*                                                                               
         USING ELEMTABD,R4                                                      
         LA    R4,ELEMBLK                                                       
         B     TOT10                                                            
TOT10NX  LA    R4,ELLEN(R4)                                                     
TOT10    LA    R1,ELEMEND                                                       
         CR    R4,R1                                                            
         BNL   TOTX                                                             
         OC    0(ELCHKENT,R4),0(R4)                                             
         BZ    TOT10NX                                                          
TOT10A   TM    ELSTAT2,PDESHRTE    DON'T TOTAL HOURLY RATES                     
         BO    TOT10NX                                                          
*                                                                               
         MVC   PCDNUM,ELNUM        RUN TYPE TOTALS FOR WHOLE MONTH              
         GOTO1 =A(GETTYPE),DMCB,RR=RELO                                         
         USING TOTD,R3                                                          
         LA    R1,STARTWRK                                                      
         AH    R1,TOTDISP          DISP TO ACCUMULATOR                          
         AP    0(8,R1),ELAMT(6)                                                 
         LA    R1,STARTWRK                                                      
         AH    R1,TOTADJ           DISP TO ADJUST ACCUMULATOR                   
         AP    0(8,R1),ELADJ(6)                                                 
         B     TOT10NX                                                          
*                                                                               
TOTX     B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        GETS MATCHING CODE NAME FROM PAYCDBLK TABLE                            
*        USES PCDNUM FOR INPUT CODE : OUTPUTS TO SCREEN                         
***********************************************************************         
*                                                                               
DISPCD   NTR1                                                                   
         USING DSPLINED,R2                                                      
         USING PAYCDTAB,R3                                                      
         L     R3,ADISPBLK                                                      
         B     DS20                                                             
DS10NX   LA    R3,PAYCDLEN(R3)                                                  
         L     R1,PAYEND                                                        
         CR    R3,R1               CHECK FOR END OF TABLE                       
         BNL   DSXNO                                                            
*                                                                               
DS20     OC    PAYCDNUM(PAYCDLEN),PAYCDNUM                                      
         BZ    DSXNO                                                            
         CLC   PCDNUM,PAYCDNUM     MATCH ON PAYROLL CODE NUMBER                 
         BNE   DS10NX                                                           
         MVC   DSPCODE,PAYCDNME     DISP CODE                                   
         MVC   PREVCODE,DSPCODE                                                 
         OI    DSPCODEH+6,X'80'                                                 
         MVI   DSPCODEH+5,5        GIVE A LENGTH                                
         OI    DSPCODEH+4,X'20'    HAS BEEN VALIDATED                           
DSXYES   B     XYES                                                             
DSXNO    B     XNO                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        CHECK DELETE                                                           
***********************************************************************         
*                                                                               
CHKDEL   NTR1                                                                   
         USING PHIRECD,R6                                                       
         L     R6,AIO                                                           
         NI    PHIRSTA,X'FF'-X'80'    MAKE SURE NOT DELETED                     
         MVI   ELCODE,PDEELQ                                                    
         BAS   RE,GETEL                                                         
         BE    XIT                                                              
*                                                                               
         L     R6,AIO                                                           
         OI    PHIRSTA,X'80'       DELETE IF NO ELEMS                           
         LA    R6,BIGKEY                                                        
         OI    PHIKSTA,X'80'       DELETE IF NO ELEMS                           
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        TURN OFF DELETE BITS                                                   
***********************************************************************         
*                                                                               
DELOFF   NTR1                                                                   
         USING PHIRECD,R6                                                       
         L     R6,AIO                                                           
         NI    PHIRSTA,X'FF'-X'80'                                              
         LA    R6,BIGKEY                                                        
         NI    PHIKSTA,X'FF'-X'80'                                              
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK FOR DUPLICATE ELEMENT                                            
***********************************************************************         
*                                                                               
         USING ELEMTABD,R4                                                      
DUPCHK   NTR1                                                                   
         LR    R3,R4               R3 = ENTRY BEING CHECKED                     
         LA    R4,ELEMBLK          USE TABLE OF ELEMENTS                        
         B     DC20                                                             
DC10NX   LA    R4,ELLEN(R4)                                                     
DC20     OC    0(ELCHKENT,R4),0(R4)                                             
         BZ    DCXNO                                                            
DC20A    CR    R4,R3               DON'T CHECK THE ONE BEING CHECKED            
         BE    DC10NX                                                           
*                                                                               
         CLC   ELDATE,INDATE       CHECK FOR MATCH ON DATE AND NUM              
         BNE   DC10NX                                                           
         CLC   ELNUM,INNUM                                                      
         BNE   DC10NX                                                           
         B     DCXYES              YES FOUND MATCH                              
*                                                                               
DCXNO    B     XNO                                                              
DCXYES   B     XYES                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        ADDS ELEMENT IF POSSIBLE, IF REC TOO BIG                               
*        LOOKS FOR NEXT SEQ OR MAKES NEW REC                                    
***********************************************************************         
*                                                                               
ADDS     NTR1                                                                   
         L     R6,AIO                                                           
         USING PHIRECD,R6                                                       
         LH    R1,PHIRLEN                                                       
         ZIC   R0,ELEM+1                                                        
         AR    R1,R0                                                            
         CH    R1,=H'1990'                                                      
         BH    XNO                                                              
         GOTO1 ADDELEM                                                          
         B     XYES                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK FOR NEXT SEQ NUMBER                                              
***********************************************************************         
*                                                                               
NEXTSEQ  NTR1                                                                   
         L     R6,AIO                                                           
         USING PHIRECD,R6                                                       
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'ACTKEY),0(R6)                                           
         LA    R6,BIGKEY                                                        
         MVC   PHIKSEQ,SEQNUM                                                   
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   XNO                 NEED TO ADD NEW RECORD?                      
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         BAS   RE,DELOFF                                                        
         B     XYES                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        NEED TO ADD NEW RECORD WITH NEXT SEQ NUMBER                            
***********************************************************************         
*                                                                               
NEWSEQ   NTR1                                                                   
         L     RE,AIO              CLEAR I/O                                    
         LA    RF,LIOS                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     R6,AIO                                                           
         MVC   0(L'ACTKEY,R6),KEYSAVE                MOVE IN KEY                
         OI    BITS,NEWREC         FLAG TO ADD REC                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CLEAR SOME FIELDS                                                      
***********************************************************************         
*                                                                               
CLRSCRN  NTR1                                                                   
         LA    R2,SHMLIN1H         CLEAR ALL FIELDS                             
         LA    R3,SHMWORDH                                                      
*                                                                               
CSCLR    DS    0H                                                               
         ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         MVI   5(R2),X'00'                                                      
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    CSCLR               NO                                           
*                                                                               
CS10     DS    0H                  CLEAR TOTALS                                 
         LA    R2,SHMSALH                                                       
         LA    R3,5                FIVE TOTAL LINES                             
CS10LP   ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)            SKIP HEADER FIELD                            
         AR    R2,R1                                                            
         BCT   R3,CS10LP                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        UNPROTECT DATE AND TYPE FIELDS                                         
***********************************************************************         
*                                                                               
UNPROT   NTR1                                                                   
         LA    R2,SHMLIN1H              FIRST LINE                              
         USING DSPLINED,R2                                                      
UNPROT10 NI    DSPDATEH+1,X'FF'-X'20'                                           
         NI    DSPDATEH+1,X'FF'-X'01'   UNMODIFY OR GENCON FREAKS               
         NI    DSPCODEH+1,X'FF'-X'20'                                           
         NI    DSPCODEH+1,X'FF'-X'01'   UNMODIFY OR GENCON FREAKS               
         OI    DSPAMNTH+1,X'20'         PROTECT AMOUNT                          
         TM    OPTSTAT,OPTSMON          UNLESS USING MONTH OPTION               
         BNO   UNPROT20                                                         
         NI    DSPAMNTH+1,X'FF'-X'20'                                           
         NI    DSPAMNTH+1,X'FF'-X'01'   UNMODIFY OR GENCON FREAKS               
UNPROT20 LA    R2,DSPLLEN(R2)           NEXT LINE                               
         LA    R3,SHMWORDH                                                      
         CR    R2,R3                                                            
         BL    UNPROT10                                                         
UNPROTX  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        RANDOM STUFF                                                 *         
***********************************************************************         
*                                  GENERAL MESSAGES                             
ERRMISS  MVI   GERROR1,MISSING                                                  
         B     ERRX                                                             
ERRPLS   MVI   GERROR1,2                                                        
         MVI   GMSGTYPE,C'I'                                                    
         B     ERRX                                                             
ERRINV   MVI   GERROR1,INVALID                                                  
         B     ERRX                                                             
ERRX     MVI   GMSYS,X'FF'         GENERAL MESSAGE SYSTEM                       
         GOTO1 MYERR                                                            
*                                                                               
EYUPD    MVC   GERROR,=AL2(ACEYUPD)    ENTER Y TO UPDATE                        
         B     ACCERRX                                                          
EINVACC  MVC   GERROR,=AL2(ACEACCT)                                             
         B     ACCERRX                                                          
ETOOSHRT MVC   GERROR,=AL2(ACELSHO)                                             
         B     ACCERRX                                                          
ETOOLONG MVC   GERROR,=AL2(ACELLONG)                                            
         B     ACCERRX                                                          
EMISHIGH MVC   GERROR,=AL2(ACEHIGH)    MISSING HIGHER LEVELS                    
         B     ACCERRX                                                          
EINVYR   MVC   GERROR,=AL2(ACEINVYR)                                            
         B     ACCERRX                                                          
EINVDATE MVC   GERROR,=AL2(ACEIVDTE)                                            
         B     ACCERRX                                                          
EINVDTMO MVC   GERROR,=AL2(ACEOPTDT)                                            
         B     ACCERRX                                                          
EPERDATE MVC   GERROR,=AL2(ACEPERDT)  INVALID PERSON FO THIS DATE               
         B     ACCERRX                                                          
EINVHRS  MVC   GERROR,=AL2(ACEIVHRS)                                            
         B     ACCERRX                                                          
EACTHRS  MVC   GERROR,=AL2(ACEACTHR)                                            
         B     ACCERRX                                                          
EQRTHRS  MVC   GERROR,=AL2(ACEQTRHR)                                            
         B     ACCERRX                                                          
ENOMETH  MVC   GERROR,=AL2(ACENOMET)                                            
         B     ACCERRX                                                          
EINVMETH MVC   GERROR,=AL2(ACEIVMET)                                            
         B     ACCERRX                                                          
EINVPER  MVC   GERROR,=AL2(ACEIVPER)                                            
         B     ACCERRX                                                          
EINVLOC  MVC   GERROR,=AL2(ACEIVLOC)                                            
         B     ACCERRX                                                          
ETABDN   MVC   GERROR,=AL2(ACETABDN)                                            
         B     ACCERRX                                                          
EDUPEN   MVC   GERROR,=AL2(ACEDUPEN)                                            
         B     ACCERRX                                                          
EINVAMT  MVC   GERROR,=AL2(ACEAMNT)                                             
         B     ACCERRX                                                          
EINVTYPE MVC   GERROR,=AL2(ACEIVTYP)                                            
         B     ACCERRX                                                          
EINVOPT  MVC   GERROR,=AL2(ACEINVOP)                                            
         B     ACCERRX                                                          
EINVLOCD MVC   GERROR,=AL2(ACEILOCD)                                            
         B     ACCERRX                                                          
EINVLOCE MVC   GERROR,=AL2(ACEILOCE)                                            
         B     ACCERRX                                                          
EINVADJT MVC   GERROR,=AL2(ACEIADJO)                                            
         B     ACCERRX                                                          
EINVYTD  MVC   GERROR,=AL2(ACEYTDMA)                                            
         B     ACCERRX                                                          
EINVLOPT MVC   GERROR,=AL2(ACELSTOP)                                            
         B     ACCERRX                                                          
*                                                                               
ACCERRX  MVI   GMSYS,6             ACC MESSAGE SYSTEM                           
         GOTO1 MYERR                                                            
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         GETEL2 R2,DATADISP,ELCODE                                              
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PERCENTAGE INFO TABLE                                                  
***********************************************************************         
*                                                                               
PCTSTAB  DC    C'PC1'                  CHARACTER REPRESENTATION                 
         DC    AL2(PAYPC1-PAYELD)      DISP TO CODE IN '84' ELEM                
         DC    AL1(PDEPC1)             BIT TO TURN ON IN '86' ELEM              
         DC    AL1(PCTNOPC1)           DON'T CALC PERCENT(BIT IN PCTNO)         
         DC    AL1(PCTDATE1)           ONLY CALC PERCENT ON RANGE               
         DC    AL2(PC1STMOA-STARTWRK)  DISP TO START OF RANGE                   
         DC    AL2(PC1NDMOA-STARTWRK)  DISP TO END OF RANGE                     
         DC    AL2(PAYCDPC1-PAYCDTAB)  DISP TO PAYNUM FOR PCT IN TABLE          
         DC    AL2(PC1PCT-STARTWRK)    DISP TO PERCENTAGE                       
         DC    AL2(COPC1-COBLOCK)      DISP TO PERCENT IN PROFILE TAB           
*                                                                               
         DC    C'PC2'                                                           
         DC    AL2(PAYPC2-PAYELD)                                               
         DC    AL1(PDEPC2)                                                      
         DC    AL1(PCTNOPC2)                                                    
         DC    AL1(PCTDATE2)                                                    
         DC    AL2(PC2STMOA-STARTWRK)                                           
         DC    AL2(PC2NDMOA-STARTWRK)                                           
         DC    AL2(PAYCDPC2-PAYCDTAB)                                           
         DC    AL2(PC2PCT-STARTWRK)                                             
         DC    AL2(COPC2-COBLOCK)                                               
*                                                                               
         DC    X'FFFF'                                                          
         EJECT                                                                  
***********************************************************************         
*        TOTALS TABLE                                                           
***********************************************************************         
*                                                                               
TOTALTAB DC    AL1(PATTSAL),C'S',AL2(SALMTOT-STARTWRK)                          
         DC    AL2(SALADJ-STARTWRK)                                             
         DC    AL1(PATTPEN),C'P',AL2(PENMTOT-STARTWRK)                          
         DC    AL2(PENADJ-STARTWRK)                                             
         DC    AL1(PATTBEN),C'B',AL2(BENMTOT-STARTWRK)                          
         DC    AL2(BENADJ-STARTWRK)                                             
         DC    AL1(PATTIND),C'I',AL2(INDMTOT-STARTWRK)                          
         DC    AL2(INDADJ-STARTWRK)                                             
TOTDEFLT DC    AL1(0),C'*',AL2(OTHMTOT-STARTWRK)                                
         DC    AL2(OTHADJ-STARTWRK)                                             
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        DATA DICTIONARY                                                        
***********************************************************************         
*                                                                               
DDNODET  DCDD  AC#NODET,20         NO DETAILS AVAILABLE                         
DDSEL    DCDDL AC#SEL,5            SELECT                                       
         EJECT                                                                  
***********************************************************************         
*        MAINTENANCE SCREEN PFKEY TABLE DEFINITIONS                             
***********************************************************************         
PFTABLE  DS    0C                                                               
*                                                                               
*        PERSON  LIST                                                           
*                                                                               
         DC    AL1(PPF01X-*,01,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#LIST,8                                                        
*        DC    AL1(KEYTYTWA,L'SHMPER-1),AL2(SHMPER-T61DFFD)                     
PPF01X   EQU   *                                                                
*                                                                               
*        HISTORY LIST                                                           
*                                                                               
         DC    AL1(PPF03X-*,03,PFTCPROG,(PPF03X-PPF03)/KEYLNQ,0)                
         DC    CL3' '                                                           
         DCDD  AC#HIST,8                                                        
         DCDD  AC#LIST,8                                                        
PPF03    DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYCOM,3-1),AL2(0)                                         
         DC    AL1(KEYTYTWA,L'SHMPER-1),AL2(SHMPER-T61DFFD)                     
PPF03X   EQU   *                                                                
*                                                                               
*        MAD INPUT                                                              
*                                                                               
         DC    AL1(PPF06X-*,06,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#MAD,8                                                         
         DCDD  AC#INP,8                                                         
PPF06X   EQU   *                                                                
*                                                                               
*        RETURN TO CALLER                                                       
*                                                                               
         DC    AL1(PPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
PPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        LIST SCREEN PFKEY TABLE DEFINITIONS                                    
***********************************************************************         
LPFTABLE DS    0C                                                               
*                                                                               
*        HISTORY DISPLAY                                                        
*                                                                               
         DC    AL1(LPF03X-*,03,PFTCPROG,(LPF03X-LPF03)/KEYLNQ,0)                
         DCDD  AC#DSP,3                                                         
         DCDD  AC#HIST,8                                                        
         DCDD  AC#DSP,8                                                         
LPF03    DC    AL1(KEYTYCUR,L'LSTMONTH-1),AL2(LSTMONTH-LSTSTRT)                 
LPF03X   EQU   *                                                                
*                                                                               
*        PERSON  DISPLAY                                                        
*                                                                               
         DC    AL1(LPF02X-*,02,PFTCPROG,(LPF02X-LPF02)/KEYLNQ,0)                
         DCDD  AC#CPRSN,3                                                       
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#DSP,8                                                         
LPF02    DC    AL1(KEYTYTWA,L'SHMPER-1),AL2(SHMPER-T61DFFD)                     
LPF02X   EQU   *                                                                
*                                                                               
*        PERSON  LIST                                                           
*                                                                               
         DC    AL1(LPF01X-*,01,PFTCPROG,(LPF01X-LPF01)/KEYLNQ,0)                
         DC    CL3'   '                                                         
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#LIST,8                                                        
LPF01    DC    AL1(KEYTYTWA,L'SHMPER-1),AL2(SHMPER-T61DFFD)                     
LPF01X   EQU   *                                                                
*                                                                               
*        MAD INPUT                                                              
*                                                                               
         DC    AL1(LPF06X-*,06,PFTCPROG,0,0)                                    
         DCDD  AC#MAD,3                                                         
         DCDD  AC#MAD,8                                                         
         DCDD  AC#INP,8                                                         
LPF06X   EQU   *                                                                
*                                                                               
*        METHOD LIST                                                            
*                                                                               
         DC    AL1(LPF10X-*,10,PFTCPROG,0,0)                                    
         DCDD  AC#METH,3                                                        
         DCDD  AC#METH,8                                                        
         DCDD  AC#LIST,8                                                        
LPF10X   EQU   *                                                                
*                                                                               
*        HISTORY LAST (BACK PAGE)                                               
*                                                                               
         DC    AL1(LPF07X-*,07,PFTLIST,0,0)                                     
         DC    CL3'   ',CL8'        '                                           
         DCDD  AC#LAST,8                                                        
LPF07X   EQU   *                                                                
*                                                                               
*        HISTORY NEXT (NEXT PAGE)                                               
*                                                                               
         DC    AL1(LPF08X-*,08,PFTLIST,0,0)                                     
         DC    CL3'   ',CL8'        '                                           
         DCDD  AC#NXT,8                                                         
LPF08X   EQU   *                                                                
*                                                                               
*        RETURN TO CALLER                                                       
*                                                                               
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        BUILD 1R ACCOUNT TABLE                                                 
***********************************************************************         
*                                                                               
BILDACCT DC    AL2(LEVELLNA-STARTWRK),AL2(OFFICE-STARTWRK)                      
         DC    AL2(LEVELLNB-STARTWRK),AL2(DEPT-STARTWRK)                        
         DC    AL2(LEVELLNC-STARTWRK),AL2(SUBDPT-STARTWRK)                      
         DC    AL2(LEVELLND-STARTWRK),AL2(PERSON-STARTWRK)                      
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        DISPLAY AMOUNT (PL6 AT R3) IN FIELD AT R6                              
***********************************************************************         
*                                                                               
         USING ELEMTABD,R4                                                      
DISAMT   NMOD1 0,*DISAMT*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         CURED (P6,(R3)),(11,WORK),2,FLOAT=-,ZERO=NOBLANK,ALIGN=LEFT            
         TM    ELSTAT2,PDESHRTE    HOURLY RATE                                  
         BNO   DA100                                                            
         LA    R1,WORK                                                          
         AR    R1,R0                                                            
         MVC   0(L'AC@PERHR,R1),AC@PERHR        /HR                             
         LR    R1,R0                                                            
         LA    R1,3(R1)                                                         
         STC   R1,5(R6)            LENGTH                                       
         ZIC   R0,0(R6)                                                         
         SH    R0,=H'8'                                                         
         TM    1(R6),X'02'         XHEADER                                      
         BNO   *+8                                                              
         SH    R0,=H'8'                                                         
         SR    R0,R1                                                            
         LA    R3,8(R6)                                                         
         AR    R3,R0                                                            
         BCTR  R1,0                                                             
         MVC   0(0,R3),WORK                                                     
         EX    R1,*-6                                                           
         B     DAX                                                              
DA100    STC   R0,5(R6)            LENGTH                                       
         CURED (P6,(R3)),(11,8(R6)),2,FLOAT=-,ZERO=NOBLANK                      
DAX      XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        ON-SCREEN LIST                                               *         
***********************************************************************         
*                                                                               
LR       NMOD1 0,**LIST**                                                       
         L     RC,SAVERC                                                        
*                                                                               
         NI    BITS,X'FF'-LASTLINE                                              
         MVI   NLISTS,12                                                        
         OI    GLSTSTAT,RETEXTRA                                                
         BAS   RE,CLRTOTS                                                       
*                                                                               
         LA    R6,BIGKEY                                                        
         USING PHIRECD,R6                                                       
         L     R4,ATHISLST         LINE DSECT                                   
         USING LSTLINED,R4                                                      
*                                                                               
         OC    LSTLKEY,LSTLKEY     FIRST TIME THROUGH?                          
         BNZ   LRHI                                                             
         MVC   PHIKEY,SAVEKEY                                                   
LRHI     GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BNE   LR120                                                            
         GOTO1 GETREC                                                           
*                                                                               
LR10     CLC   BIGKEY(19),SAVEKEY   SAME PERSON/LOCATION                        
         BNE   LR120               NO MORE TO LIST                              
         MVC   LSTLKEY,BIGKEY                                                   
         MVC   KEY2,BIGKEY         SAVE BEFORE METHOD READ                      
*                                                                               
         LA    R6,BIGKEY                                                        
         OC    ENDATECM,ENDATECM                                                
         BZ    LR11                                                             
         CLC   PHIKMOA,ENDATECM                                                 
         BH    LR120                                                            
*                                                                               
LR11     MVC   STADDR,ATHISLST     SAVE ATHISLST                                
         GOTO1 LISTMON             GOTO LISTMON NOW WITH SEQ 00 KEY             
         BE    LR12                                                             
         OI    BITS,LASTLINE                                                    
         OI    LSTHEAD+6,X'80'                                                  
*                                                                               
LR12     L     R6,AIO                                                           
         L     R4,STADDR                                                        
         XC    YYMMDD,YYMMDD                                                    
         SR    R1,R1                                                            
         ICM   R1,3,PHIKMOA                                                     
         LNR   R1,R1               USE 'FFFF'-YYMM                              
         STCM  R1,3,YYMMDD           ONLY USING YEAR AND MONTH                  
*                                                                               
         NI    BIT2,X'FF'-(HOURLY+ADJUSTED+YTDADJ)                              
         MVC   LSTMONTH(LSTLEN),SPACES    CLEAR LINE                            
*                                                                               
LR10SQ   L     R6,AIO                                                           
         MVI   ELCODE,PDEELQ                                                    
         BAS   RE,GETEL                                                         
         B     LR20                                                             
LR10NX   MVI   ELCODE,PDEELQ                                                    
         BAS   RE,NEXTEL                                                        
*                                                                               
LR20     BNE   LR50                                                             
         USING PDEELD,R6                                                        
*                                                                               
         TM    PDESTAT2,PDESADJ    ADJUSTEMENT RATE                             
         BNO   *+8                                                              
         OI    BIT2,YTDADJ                                                      
         TM    PDESTAT2,PDESHRTE   HOURLY RATES                                 
         BNO   LR22                                                             
         OI    BIT2,HOURLY                                                      
         CP    PDEADJ,=P'0'                                                     
         BE    *+8                                                              
         OI    BIT2,ADJUSTED       FLAG ADJUSTED TOTALS                         
         B     LR10NX              DON'T ADD TO TOTALS - CHECK ADJUSTD          
*                                                                               
LR22     MVC   PCDNUM,PDENUM                                                    
         MVC   KEY2,BIGKEY         SAVE HISTORY KEY                             
         GOTO1 =A(GETTYPE),DMCB,RR=RELO GET TYPE FROM METHOD RECORD             
         USING TOTD,R3                                                          
         MVC   BIGKEY,KEY2         RESTORE HISTORY KEY                          
         LA    R1,STARTWRK                                                      
         AH    R1,TOTDISP          DISP TO ACCUMULATOR                          
         AP    0(8,R1),PDEAMT(6)                                                
         CP    PDEADJ,=P'0'                                                     
         BE    LR25                                                             
         AP    0(8,R1),PDEADJ(6)                                                
         OI    BIT2,ADJUSTED       FLAG ADJUSTED TOTALS                         
         DROP  R3                                                               
LR25     B     LR10NX                                                           
*                                                                               
*R50     MVC   BIGKEY,KEY2         RESTORE KEY                                  
LR50     GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
         GOTO1 GETREC                                                           
         CLC   BIGKEY(21),LSTLKEY   SAME PERSON/LOCATION/MONTH                  
         BE    LR10SQ              CONTINUE TOTALLING                           
         MVC   KEY2,BIGKEY                                                      
*                                                                               
LR60     GOTO1 DATCON,DMCB,(1,YYMMDD),(9,LSTMONTH)                              
         AP    SALTOT(8),SALMTOT(8)                                             
         AP    PENTOT(8),PENMTOT(8)                                             
         AP    BENTOT(8),BENMTOT(8)                                             
         AP    INDTOT(8),INDMTOT(8)                                             
*                                                                               
         AP    MONTOT(8),SALMTOT(8)                                             
         AP    MONTOT(8),PENMTOT(8)                                             
         AP    MONTOT(8),BENMTOT(8)                                             
         AP    MONTOT(8),INDMTOT(8)                                             
         AP    ALLTOT(8),MONTOT(8)                                              
*                                                                               
         CURED (P8,SALMTOT),(11,LSTSAL),2,FLOAT=-,ZERO=NOBLANK                  
         CURED (P8,BENMTOT),(11,LSTBEN),2,FLOAT=-,ZERO=NOBLANK                  
         CURED (P8,PENMTOT),(11,LSTPEN),2,FLOAT=-,ZERO=NOBLANK                  
         CURED (P8,INDMTOT),(11,LSTIND),2,FLOAT=-,ZERO=NOBLANK                  
         CURED (P8,MONTOT),(12,LSTTOT),2,FLOAT=-,ZERO=NOBLANK                   
*                                                                               
         TM    BIT2,ADJUSTED                                                    
         BZ    *+10                                                             
         MVC   LSTADJ(2),=C'*M'                                                 
         TM    BIT2,HOURLY                                                      
         BZ    *+10                                                             
         MVC   LSTADJ(2),=C'*H'                                                 
         TM    BIT2,YTDADJ                                                      
         BZ    *+10                                                             
         MVC   LSTADJ(2),=C'*Y'                                                 
         TM    BIT2,ADJUSTED+HOURLY                                             
         BNO   *+10                                                             
         MVC   LSTADJ,=C'*MH'                                                   
         TM    BIT2,YTDADJ+ADJUSTED                                             
         BNO   *+10                                                             
         MVC   LSTADJ,=C'*YM'                                                   
         OI    6(R4),X'80'                                                      
*                                                                               
         ZAP   SALMTOT(8),=P'0'                                                 
         ZAP   PENMTOT(8),=P'0'                                                 
         ZAP   BENMTOT(8),=P'0'                                                 
         ZAP   INDMTOT(8),=P'0'                                                 
         ZAP   OTHMTOT(8),=P'0'                                                 
         ZAP   MONTOT(8),=P'0'                                                  
*                                                                               
         TM    BITS,LASTLINE       ON LAST LIST LINE                            
         BNO   LR10                NO THEN TOTAL UP NEXT MONTH                  
*                                                                               
LR120    LA    R4,SHLTOTLH                                                      
         MVC   LSTMONTH,AC@TOTSU   TOTALS                                       
         CURED (P8,SALTOT),(11,LSTSAL),2,FLOAT=-,ZERO=NOBLANK                   
         CURED (P8,BENTOT),(11,LSTBEN),2,FLOAT=-,ZERO=NOBLANK                   
         CURED (P8,PENTOT),(11,LSTPEN),2,FLOAT=-,ZERO=NOBLANK                   
         CURED (P8,INDTOT),(11,LSTIND),2,FLOAT=-,ZERO=NOBLANK                   
         CURED (P8,ALLTOT),(12,LSTTOT),2,FLOAT=-,ZERO=NOBLANK                   
         OI    6(R4),X'80'                                                      
LRX      XIT1                                                                   
         DROP  R6,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        UPDATE ANY FUTURE RECORDS                                              
***********************************************************************         
*                                                                               
XD       NMOD1 0,**REVD**                                                       
         L     RC,SAVERC                                                        
*                                                                               
         OI    BIT2,FIRST          FIRST TIME THROUGH                           
         MVC   CURRMOA,STMOA       ALREADY UPDATED FIRST MONTH                  
         MVC   AIO,AIO2            USE AIO2                                     
*                                                                               
XD10NX   NI    BITS,X'FF'-NEWREC                                                
         MVC   INDATE,CURRMOA                                                   
         MVI   INDATE+2,1                                                       
         BAS   RE,NXTMTH           GET NEXT MONTH                               
         MVC   CURRMOA,NXTMOA                                                   
         ZICM  R1,NXTMOA,2                                                      
         LNR   R1,R1                                                            
         STCM  R1,3,NXTMOA                                                      
*                                                                               
         USING PHIRECD,R6                                                       
         LA    R6,BIGKEY           READ FOR RECORD                              
         XC    BIGKEY,BIGKEY                                                    
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ    SALARY HISTORY RECORD                        
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,CMPY        COMPANY                                      
         MVC   PHIKOFC,OFFICE                                                   
         MVC   PHIKDPT,DEPT                                                     
         MVC   PHIKSBD,SUBDPT                                                   
         MVC   PHIKPER,PERSON                                                   
         MVC   PHIKMOA,NXTMOA                                                   
         MVC   PHIKSEQ,SEQNUM                                                   
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE   DOES THIS MONTH EXIST                 
         BE    XD20                                                             
         BAS   RE,NEWSEQ                  NO, THEN ADD                          
         B     XD30                                                             
XD20     MVI   RDUPDATE,C'Y'              ELSE, READ FOR UPDATE                 
         GOTO1 GETREC                                                           
         BAS   RE,DELOFF           TURN OFF DELETE BITS                         
*                                                                               
XD30     CLC   CURRMOA,ENDMOA      WITHIN PERIOD                                
         BNH   XD50                YES, UPDATE ENTIRE RECORD                    
         DROP  R6                  ELSE JUST UPDATE REVERSALS                   
         EJECT                                                                  
***********************************************************************         
*        REMOVE ANY PREVIOUS REVERSALS                                          
***********************************************************************         
*                                                                               
         USING PDEELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,PDEELQ                                                    
         BAS   RE,GETEL                                                         
         B     XD42                                                             
XD42NX   MVI   ELCODE,PDEELQ                                                    
         BAS   RE,NEXTEL                                                        
XD42     BNE   XD44                                                             
         TM    PDESTAT,PDERVRSL    REVERSAL                                     
         BNO   XD42NX                                                           
         MVI   0(R6),X'FF'                                                      
         B     XD42NX                                                           
*                                                                               
XD44     MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         B     XD100               NOW GO ADD NEW REVERSALS                     
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        REMOVE ALL ELEMS AND ADD NEW ELEMS FROM TABLE (NOT REVERSALS)          
***********************************************************************         
*                                                                               
XD50     MVI   ELCODE,PDEELQ       X'86'                                        
         GOTO1 REMELEM                                                          
*                                                                               
         USING PDEELD,R6                                                        
         LA    R6,ELEM             BUILD ELEMS IN ELEM                          
         USING ELEMTABD,R4                                                      
         LA    R4,ELEMBLK          BLOCK OF ELEMENTS                            
*                                                                               
XD55     OC    0(ELCHKENT,R4),0(R4)                                             
         BZ    XD90NX                                                           
XD55A    TM    ELSTAT,PDERVRSL     DON'T ADD REVERSALS                          
         BO    XD90NX                                                           
*                                                                               
         TM    ELSTAT,PDEPCS       PERCENTAGE                                   
         BZ    XD65                                                             
*                                                                               
         USING PCTSTABD,R3                                                      
         L     R3,APCTSTAB                                                      
         B     XD60                                                             
XD60NX   LA    R3,PCTSLEN(R3)                                                   
XD60     CLI   0(R3),X'FF'                                                      
         BE    XD90NX                                                           
         MVC   BYTE,ELSTAT                                                      
         NC    BYTE,PCTSBIT        SAME PC NUMBER                               
         BZ    XD60NX                                                           
*                                                                               
         MVC   BYTE,PCTDATE        ANY DATES                                    
         NC    BYTE,PCTSDATE                                                    
         BZ    XD65                NO DATES ADD PCT ELEM                        
*                                                                               
         LA    R0,STARTWRK                                                      
         ZICM  R1,PCTSSTRT,2       DISP TO PCT START DATE                       
         AR    R1,R0                                                            
         CLC   CURRMOA,0(R1)       IS MOA IN RANGE                              
         BL    XD60NX                                                           
         ZICM  R1,PCTSEND,2        DISP TO PCT END DATE                         
         AR    R1,R0                                                            
         CLC   CURRMOA,0(R1)      IS MOA IN RANGE                               
         BH    XD60NX                                                           
         DROP  R3                                                               
*                                                                               
XD65     TM    ELSTAT2,PDESLAST    USE LAST OPTION?                             
         BNO   XD66                NO                                           
         MVC   INDATE,ELDATE                                                    
         BAS   RE,NXTMTH                                                        
         MVC   INDATE,NXTDATE                                                   
         GOTO1 =A(MONDATES),DMCB,RR=RELO                                        
         MVC   ELDATE,MONLAST                                                   
         B     XD70                                                             
*                                                                               
XD66     XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,ELDATE),(0,WORK)                                  
*        TM    BIT2,FIRST          ONLY SAVE THE FIRST TIME IN'                 
*        BZ    *+14                                                             
         MVC   SVDATE,WORK+4     SAVE DAY OF MONTH                              
         NI    BIT2,X'FF'-FIRST                                                 
         MVC   EBDATE(4),WORK                                                   
         MVC   EBDATE+4(2),=C'01'                                               
         BAS   RE,GETNXTDT                                                      
         MVC   ELDATE,NXTDATE                                                   
*                                                                               
XD70     XC    ELEM,ELEM           FILL IN ELEM                                 
         MVI   PDEEL,PDEELQ                                                     
         MVC   PDEDTE,ELDATE                                                    
         MVC   PDENUM,ELNUM                                                     
         MVC   PDESTAT,ELSTAT                                                   
         MVC   PDESTAT2,ELSTAT2                                                 
         ZAP   PDEAMT(6),ELAMT(6)                                               
         ZAP   PDEADJ(6),ELADJ(6)                                               
*                                                                               
         LA    R0,PDELNQ            DESCRIPTION AND LENGTH                      
         ZIC   R1,ELDESCLN                                                      
         AR    R0,R1                                                            
         STC   R0,PDELN                                                         
         SH    R1,=H'1'                                                         
         BM    XD80                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PDEDESC(0),ELDESC                                                
XD80     GOTO1 ADDELEM             ADD ELEMENT                                  
*                                                                               
XD90NX   LA    R4,ELLEN(R4)        NEXT ENTRY                                   
         LA    R1,ELEMEND          END OF BLOCK                                 
         CR    R4,R1                                                            
         BL    XD55                                                             
*                                                                               
         DROP  R6,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        ADD ALL NEW REVERSALS                                                  
***********************************************************************         
*                                                                               
XD100    OC    REVELEMS,REVELEMS                                                
         BZ    XD110                                                            
         L     R6,AIO                                                           
         OC    REVELEM1,REVELEM1                                                
         BZ    XD102                                                            
         XC    ELEM,ELEM                                                        
         MVC   ELEM(50),REVELEM1                                                
         BAS   RE,XDADD                                                         
         MVC   REVELEM1,ELEM       COPY BACK FOR BUMPING DATE                   
*                                                                               
XD102    OC    REVELEM2,REVELEM2                                                
         BZ    XD104                                                            
         XC    ELEM,ELEM                                                        
         MVC   ELEM(50),REVELEM2                                                
         BAS   RE,XDADD                                                         
         MVC   REVELEM2,ELEM                                                    
*                                                                               
XD104    OC    REVELEM3,REVELEM3                                                
         BZ    XD110                                                            
         XC    ELEM,ELEM                                                        
         MVC   ELEM(50),REVELEM3                                                
         BAS   RE,XDADD                                                         
         MVC   REVELEM3,ELEM                                                    
         EJECT                                                                  
***********************************************************************         
*        WRITE OUT RECORD                                                       
***********************************************************************         
*                                                                               
XD110    TM    OPTSTAT,OPTSMON     MONTH OPTION                                 
         BNO   XD112                                                            
         CLC   SHMUPDT,AC@YESU     NEED Y TO MAKE UPDATES                       
         BNE   XD120                                                            
XD112    TM    BITS,NEWREC                                                      
         BNO   XDWRITE                                                          
         BAS   RE,ANYELEMS         ONLY ADD REC IF THERE ARE ELEMS              
         BNE   XD120                                                            
         GOTO1 ADDREC                                                           
         B     XD120                                                            
XDWRITE  BAS   RE,CHKDEL           CHECK IF SHOULD BE DELETED                   
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
*                                                                               
XD120    CLC   CURRMOA,ENDMOA      WITHIN PERIOD                                
         BNH   XD10NX              TO TOP FOR NEXT MONTH                        
XDX      MVC   AIO,AIO1                                                         
         TM    OPTSTAT,OPTSMON                                                  
         BNO   XDXX                                                             
         GOTO1 =A(MAKETAB),DMCB,RR=RELO                                         
XDXX     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        ANY '86' ELEMENTS ON RECORD                                            
***********************************************************************         
*                                                                               
ANYELEMS NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,PDEELQ       X'86'                                        
         BAS   RE,GETEL                                                         
         BNE   XNO                                                              
         B     XYES                                                             
         EJECT                                                                  
***********************************************************************         
*        ADD REVERSAL ELEMENT                                                   
***********************************************************************         
*                                                                               
XDADD    NTR1                                                                   
         LA    R3,ELEM             ELEMENT TO UPDATE                            
ELEMUSE  USING PDEELD,R3                                                        
         L     R6,AIO              LOOK FOR MATCH IN RECORD                     
AIOUSE   USING PDEELD,R6                                                        
*                                                                               
         MVC   INDATE,ELEMUSE.PDEDTE                                            
         BAS   RE,NXTMTH           ADD A MONTH TO DATE                          
         MVC   ELEMUSE.PDEDTE,NXTDATE                                           
*                                                                               
         MVI   ELCODE,PDEELQ                                                    
         BAS   RE,GETEL                                                         
         B     XDA05                                                            
XDA05NX  BAS   RE,NEXTEL                                                        
XDA05    BNE   XDA10                                                            
*                                                                               
         CLC   ELEMUSE.PDEDTE,AIOUSE.PDEDTE     SAME DATE                       
         BNE   XDA05NX                                                          
         CLC   ELEMUSE.PDENUM,AIOUSE.PDENUM     SAME NUMBER                     
         BNE   XDA05NX                                                          
         ZAP   ELEMUSE.PDEAMT,AIOUSE.PDEAMT     COPY ANY AMT FROM TAPE          
*                                                                               
         MVI   AIOUSE.PDEEL,X'FF'         DELETE OLD ELEM                       
         MVI   ELCODE,X'FF'                                                     
         MVC   WORK(50),ELEM       REMELEM CORRUPTS ELEM                        
         GOTO1 REMELEM                                                          
         MVC   ELEM(50),WORK                                                    
         MVI   ELCODE,PDEELQ                                                    
*                                                                               
XDA10    BAS   RE,ADDS             ADDS ELEM IF CAN                             
         BE    XDAYES              EVERYTHING OK                                
*                                                                               
         BAS   RE,CHKDEL           CHECK IF SHOULD BE DELETED                   
         GOTO1 PUTREC                                                           
         GOTO1 WRITE               WRITE OUT PREVIOUS REC FIRST                 
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         BAS   RE,NEXTSEQ          CHECK FOR NEXT SEQ REC                       
         BE    XDA10               YES FOUND, ADD ELEM TO THAT REC              
         BAS   RE,NEWSEQ           MAKE NEW KEY IN AIO TO ADD                   
         B     XDA10               ADD ELEM TO NEW REC                          
*                                                                               
XDAYES   DS    0H                                                               
         CLI   SEQNUM,0                                                         
         BE    XYES                                                             
         MVI   SEQNUM,0                                                         
         BAS   RE,NEXTSEQ                                                       
         B     XYES                                                             
*                                                                               
         DROP  ELEMUSE,AIOUSE                                                   
         EJECT                                                                  
***********************************************************************         
*        GETS THE NEXT MONTHS DATE USING INDATE                                 
***********************************************************************         
*                                                                               
NXTMTH   NTR1                                                                   
         GOTO1 DATCON,DMCB,(1,INDATE),(11,WORK)                                 
         MVC   WORK+8(5),=C'(1M)'  ADD ONE MONTH                                
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(12,WORK),(BYTE,BLOCK)                               
         CLI   DMCB+4,X'01'                                                     
         BE    XNO                                                              
         LA    R1,BLOCK                                                         
         USING PERVALD,R1                                                       
         MVC   NXTDATE,PVALPSTA                                                 
         MVC   NXTMOA,PVALPSTA                                                  
         B     XYES                                                             
         EJECT                                                                  
***********************************************************************         
*        GET NEXT MONTH AND SUBTRACT ONE IF INVALID                             
***********************************************************************         
         SPACE 1                                                                
GETNXTDT NTR1                                                                   
*                                                                               
*****ADD ONE MONTH                                                              
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 ADDAY,DMCB,(C'M',EBDATE),(X'40',WORK),1                          
         GOTO1 DATCON,DMCB,(0,WORK),(11,AMDATE)                                 
         MVC   AMDATE+3(2),SVDATE    INPUT REAL DAY                             
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         MVC   BYTE,LANGCODE                                                    
         OI    BYTE,X'40'                                                       
         GOTO1 PERVAL,DMCB,(8,AMDATE),(BYTE,BLOCK)                              
         CLI   DMCB+4,X'01'        VALID DATE?                                  
         BNE   GETNXT20            YES                                          
*                                                                               
****GET LAST DAY OF MONTH                                                       
*                                                                               
         MVC   WORK+4(2),SVDATE                                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),(X'80',WORK+10),0                         
         GOTO1 DATCON,DMCB,(0,WORK+10),(1,NXTDATE)                              
         B     GETNXX                                                           
*                                                                               
GETNXT20 LA    R1,BLOCK                                                         
         USING PERVALD,R1                                                       
         MVC   NXTDATE,PVALPSTA                                                 
*                                                                               
GETNXX   DS    0H                                                               
         DROP  R1                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        GET TYPE FROM METHOD REC IN AIO3                                       
*        INPUT : PAYROLL CODE NUMBER IN PCDNUM                                  
*        OUTPUT: ADDRESS OF MATCHING LINE IN R3                                 
***********************************************************************         
*                                                                               
GETTYPE  NMOD1 0,*GETTYP*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         L     R6,AIO3                                                          
         CLC   METHKEY,0(R6)       HAVE THE RIGHT METHOD REC?                   
         BE    GT02                                                             
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'METHKEY),METHKEY                                        
         MVC   KEYSAVE,BIGKEY                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI ',=C'ACCDIR  ',BIGKEY,BIGKEY,0            
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         LA    R2,BIGKEY                                                        
         AH    R2,LKEY                                                          
         AH    R2,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC ',=C'ACCMST  ',(R2),AIO,WORK              
         MVC   AIO,AIO1                                                         
*                                                                               
GT02     L     R3,ATOTDFLT         NO ASSIGNMENT TOTAL                          
         USING PATELD,R6                                                        
         MVI   ELCODE,PATELQ                                                    
         BAS   RE,GETEL                                                         
         B     GT20                                                             
GT10NX   BAS   RE,NEXTEL                                                        
GT20     BNE   GTXNO                                                            
         CLC   PCDNUM,PATNUM       MATCH ON PAYROLL CODE NUMBER                 
         BNE   GT10NX                                                           
*                                                                               
         USING TOTD,R3                                                          
         L     R3,ATOTTAB            TOTALS TABLE                               
GT25     CLC   TOTTYPE,PATTYPE     MATCH ON TYPE                                
         BE    GT30                                                             
         LA    R3,TOTLEN(R3)                                                    
         CLI   0(R3),X'00'                                                      
         BNE   GT25                                                             
         B     GTXNO                                                            
*                                                                               
GT30     DS    0H                                                               
GTXYES   SR    RC,RC                                                            
GTXNO    LTR   RC,RC                                                            
GTX      XIT1  REGS=(R3)           PASS BACK ADDRESS IN R3                      
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        GET MOA END DATE                                                       
***********************************************************************         
MOAEND   NMOD1 0,**MOAEN*                                                       
         L     RC,SAVERC                                                        
                                                                                
         XC    EPDATE,EPDATE                                                    
         USING CASRECD,R6                                                       
         LA    R6,KEY2                                                          
         XC    KEY2,KEY2                                                        
         MVI   CASKTYP,CASKTYPQ    X'3E'                                        
         MVI   CASKSUB,CASKSUBQ    X'0B'                                        
         MVC   CASKCPY,CMPY                                                     
         MVC   CASKEMOA,TEMP                                                    
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI ',=C'ACCDIR  ',KEY2,KEY2,0                
         CLC   KEY2(CASKEMOA-CASKEY),KEYSAVE  SAME COMPANY?                     
         BNE   XNO                                                              
         LA    R2,KEY2                                                          
         AH    R2,LKEY                                                          
         AH    R2,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC ',=C'ACCMST  ',(R2),AIO2,WORK             
         L     R6,AIO2                                                          
         USING TMPELD,R6                                                        
         MVI   ELCODE,TMPELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   XNO                                                              
MOA00    CLC   TMPMTH,TEMP         CHECK MOA                                    
         BE    MOA01                                                            
         BH    XYES                                                             
         BL    *+10                                                             
MOA01    MVC   EPDATE,TMPEND       EPDATE=FINAL DATE FOR MOA PERIOD             
         BAS   RE,NEXTEL                                                        
         BE    MOA00                                                            
         OC    EPDATE,EPDATE                                                    
         BZ    XNO                                                              
         B     XYES                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE LOCATION IN PERSON REC IN AIO                       *         
*     OR VALIDATE PAYDATE ENTERED WITH LOCATION IF VALPAYDT BIT IS ON *         
*        INPUT                     OFFICE = OFFICE CODE               *         
*        INPUT                     DEPT   = DEPATMENT CODE            *         
*        INPUT                     SUBDPT = SUB-DEPARTMENT CODE       *         
*        OUTPUT                    CC = EQUAL IF VALID, ELSE NOT      *         
***********************************************************************         
*                                                                               
VALLOC   NMOD1 0,*VALOC**          VALIDATE LOCATION ENTEREDD                   
         L     RC,SAVERC                                                        
*                                                                               
*&&UK                                                                           
         OC    STDATE,STDATE                                                    
         BZ    VL02                                                             
         MVC   TEMP,STDATE                                                      
         GOTO1 =A(MOAEND),DMCB,RR=RELO                                          
*&&                                                                             
                                                                                
         USING PERRECD,R6                                                       
VL02     XC    KEY2,KEY2                                                        
         LA    R6,KEY2             READ PERSON RECORD                           
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CMPY                                                     
         MVC   PERKCODE,PERSON                                                  
         L     R6,AIO                                                           
         CLC   KEY2,0(R6)          ALREADY READ FOR PERSON                      
         BE    VL10                                                             
*                                                                               
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI ',=C'ACCDIR  ',KEY2,KEY2,0                
         CLC   KEY2(L'ACTKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,KEY2                                                          
         AH    R2,LKEY                                                          
         AH    R2,LSTATUS                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC ',=C'ACCMST  ',(R2),AIO2,WORK             
         L     R6,AIO2                                                          
         B     VL11                                                             
*                                                                               
         USING LOCELD,R6                                                        
VL10     L     R6,AIO                                                           
VL11     MVI   ELCODE,LOCELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   VLNO                                                             
         B     VL20                                                             
VL10LP   BAS   RE,NEXTEL                                                        
VL20     BNE   VL60                                                             
         CLC   OFFICE,LOCOFF                                                    
         BNE   VL10LP                                                           
         CLC   DEPT,LOCDEPT                                                     
         BNE   VL10LP                                                           
         CLC   SUBDPT,LOCSUB                                                    
         BNE   VL10LP                                                           
         TM    SPECBIT,VALPAYDT    VALIDATING PAYDATE W/LOCATION?               
         BO    VL50                                                             
*                                                                               
         OC    STDATE,STDATE                                                    
         BZ    VLYES                                                            
         CLC   STDATE(2),LOCSTART     MUST FIT MONTH ENTERED                    
         BL    *+12                                                             
         OI    SPECBIT,LOWMOA                                                   
         B     VL21                                                             
         OC    EPDATE,EPDATE                                                    
         BZ    VL21                                                             
         CLC   EPDATE,LOCSTART                                                  
         BL    VL10LP                                                           
VL21     OC    LOCSALKD,LOCSALKD      CHECK SAL LOCKED DATE OR END DATE         
         BZ    VL22                                                             
         CLC   STDATE(2),LOCSALKD                                               
         BH    VL10LP                                                           
         CLC   ENDATE(2),LOCSALKD                                               
         BH    VL10LP                                                           
         B     VLYES                                                            
VL22     OC    LOCEND,LOCEND                                                    
         BZ    VLYES               STILL ACTIVE AT THIS LOCATION                
         CLC   STDATE(2),LOCEND                                                 
         BH    VL10LP                                                           
         CLC   ENDATE(2),LOCEND                                                 
         BH    VL10LP                                                           
         B     VLYES                                                            
*                                                                               
VL50     CLC   INDATE,LOCSTART     INDATE=PAYDATE ENTERED BY USER               
         BL    VL10LP                                                           
         OC    LOCSALKD,LOCSALKD   CHECK SAL LOCKED DATE OR END DATE            
         BZ    VLYES                                                            
         CLC   INDATE,LOCSALKD                                                  
         BH    VL10LP                                                           
         B     VLYES                                                            
*                                                                               
VL60     OI    SPECBIT,SALOLOC     ADDING SAL OUTSIDE LOC DATES                 
         TM    SPECBIT,VALPAYDT    VALIDATING THE PAYDATE?                      
         BO    VLNO                THEN DON'T CHECK FOR ADJUSTMENT              
         TM    SPECBIT,ADJYES      IS THERE AN ADJUSTMENT RATE SET UP?          
         BZ    VLNO                                                             
*                                                                               
VLYES    B     XYES                                                             
VLNO     B     XNO                                                              
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NMOD1 0,**SETUP*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         OI    GENSTAT2,RETEQSEL   RETURN SAME SELECTION (FOR PAGING)           
         MVI   SEQNUM,X'00'                                                     
         L     R1,=A(TOTALTAB)                                                  
         A     R1,RELO                                                          
         ST    R1,ATOTTAB                                                       
         L     R1,=A(TOTDEFLT)                                                  
         A     R1,RELO                                                          
         ST    R1,ATOTDFLT                                                      
         L     R1,=A(PCTSTAB)                                                   
         A     R1,RELO                                                          
         ST    R1,APCTSTAB                                                      
*                                                                               
         SR    R2,R2               NO TABLE NEEDED YET                          
         CLI   ACTEQU,ACTLIST                                                   
         BE    SET20                                                            
         LA    R3,SHMPFKYH                                                      
         CLI   PFKEY,7             UP                                           
         BE    INITPFKY                                                         
         CLI   PFKEY,8             DOWN                                         
         BE    INITPFKY                                                         
*                                                                               
SET10    L     R2,=A(PFTABLE)                                                   
         A     R2,RELO                                                          
         B     INITPFKY                                                         
SET20    L     R2,=A(LPFTABLE)                                                  
         A     R2,RELO                                                          
         LA    R3,SHLPFKYH                                                      
*                                                                               
INITPFKY GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)   INITIALIZE THE PFKEYS           
*                                                                               
SUX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        DISPLAY EXTRA DETAILS                                                  
***********************************************************************         
*                                                                               
SHOWXDET NMOD1 0,*SHOWXD*                                                       
         L     RC,SAVERC                                                        
         LA    R2,SHMDETLH                                                      
         MVC   SHMDETL,SPACES                                                   
         OI    6(R2),X'80'         XMIT                                         
         TM    BITS,NODETAIL                                                    
         BO    SHOW10                                                           
         MVC   SHMDETL,XDETBLK                                                  
         B     SHOWX                                                            
*                                                                               
SHOW10   L     R1,=A(DDNODET)                                                   
         A     R1,RELO                                                          
         MVC   SHMDETL(20),0(R1)                                                
         GOTO1 DICTATE,DMCB,C'SL  ',SHMDETL,0                                   
         OC    SHMDETL,SPACES                                                   
*                                                                               
SHOWX    NI    BITS,X'FF'-NODETAIL                                              
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OFFICE,DEPT,SUBDEPT FOR LIST                                  
***********************************************************************         
*                                                                               
VAL1RLST NMOD1 0,*VALLST*                                                       
         L     RC,SAVERC                                                        
         TM    SHLOFFH+4,X'80'                                                  
         BO    VAL10                                                            
         TM    SHLPERH+4,X'80'                                                  
         BO    XYES                                                             
         TM    SHLMOAH+4,X'80'                                                  
         BO    XYES                                                             
*                                                                               
VAL10    LA    R2,SHLOFFH          OFFICE                                       
         CLI   5(R2),0             ANY DATA?                                    
         BE    VAL20                                                            
*                                                                               
         CLC   SHLOFFH+5(1),LEVELLN         SHOULD = LEN LEVEL A ACC            
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFICE(0),SHLOFF    SAVE OFFICE                                  
         OC    OFFICE,SPACES                                                    
         OI    BITS,YESOFF                                                      
*                                                                               
VAL20    LA    R2,SHLDEPTH         ANY DEPARTMENT                               
         CLI   5(R2),0                                                          
         BE    VAL30                                                            
         TM    BITS,YESOFF         MUST HAVE ENTERED AN OFFICE                  
         BZ    EMISHIGH                                                         
         CLC   5(1,R2),LEVELLN+1                                                
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DEPT(0),8(R2)                                                    
         OC    DEPT,SPACES                                                      
         OI    BITS,YESDPT                                                      
*                                                                               
VAL30    LA    R2,SHLSDPTH         ANY SUB DEPT FILTER                          
         CLI   5(R2),0                                                          
         BE    XNO                                                              
         TM    BITS,YESOFF+YESDPT  MUST HAVE HIGHER LEVELS                      
         BNO   EMISHIGH                                                         
         CLC   5(1,R2),LEVELLN+2                                                
         BH    ETOOLONG                                                         
         BL    ETOOSHRT                                                         
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SUBDPT(0),8(R2)                                                  
         OC    SUBDPT,SPACES                                                    
         OI    BITS,YESSDPT                                                     
*                                                                               
VALX     B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DISPLAY PERSON'S NAME FROM GENERAL PURPOSE NAME ELEMS (5A)             
*        FORMATS NAME IN WORK, USES AIO FOR RECORD                              
***********************************************************************         
*                                                                               
DISPNAME NMOD1 0,*DISPNM*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         L     R6,AIO                                                           
         USING GPNELD,R6                                                        
         LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         MVI   ELCODE,GPNELQ       GENERAL PURPOSE NAME ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   DNX                                                              
*                                                                               
DN10     ZIC   R1,GPNLN                                                         
         SH    R1,=H'4'            3 FOR CODE,LEN,AND TYPE + 1 FOR EX           
         CH    R1,=H'0'                                                         
         BL    DN20                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),GPNNME                                                   
         CLI   GPNTYP,GPNTLST                                                   
         BNE   DNX                                                              
         AR    R3,R1               BUMP LENGTH                                  
         LA    R3,1(R3)            ADD 1 MORE FOR EX                            
         MVI   0(R3),C','                                                       
         LA    R3,2(R3)            1 FOR , THEN 1 MORE                          
*                                                                               
DN20     BAS   RE,NEXTEL           GET FIRST NAME                               
         BE    DN10                                                             
DNX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        GETS START AND END DAYS FOR MONTH IN INDATE                            
***********************************************************************         
*                                                                               
MONDATES NMOD1 0,*MONDTE*                                                       
         L     RC,SAVERC                                                        
         GOTO1 DATCON,DMCB,(1,INDATE),(11,WORK+10)                              
         MVC   WORK(3),WORK+10                                                  
         MVC   WORK+3(3),WORK+15                                                
         XC    BLOCK(L'PVALOUTB),BLOCK                                          
         MVC   BYTE,LANGCODE                                                    
         GOTO1 PERVAL,DMCB,(6,WORK),(BYTE,BLOCK)                                
         LA    R1,BLOCK                                                         
         USING PERVALD,R1                                                       
         MVC   MONFIRST,PVALPSTA                                                
         MVC   MONLAST,PVALPEND                                                 
         XIT1                                                                   
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        RECALC ANY PERRCENTAGES                                                
***********************************************************************         
*                                                                               
PERCENT  NMOD1 0,**PCT***                                                       
         L     RC,SAVERC                                                        
*                                                                               
         GOTO1 =A(PAYCDS),DMCB,RR=RELO    MAKE TABLE OF PAYCODES                
         GOTO1 =A(GETPCTS),DMCB,RR=RELO                                         
         BAS   RE,REMPCTS          REMOVE ALL EXISTING PERCENTS                 
*                                                                               
         USING PHIRECD,R6                                                       
         L     R6,AIO                                                           
         ZICM  R1,PHIKMOA,2                                                     
         LNR   R1,R1                                                            
         STH   R1,WORK2            SAVE MOA IN WORK2                            
         DROP  R6                                                               
*                                                                               
         USING ELEMTABD,R6                                                      
         LA    R6,ELEMBLK          BLOCK OF ELEMENTS                            
         MVI   ELCODE,PDEELQ       X'86' PAYROLL DETAIL ELEM                    
         B     PCT10                                                            
PCT10NX  LA    R6,ELLEN(R6)                                                     
         LA    R1,ELEMEND                                                       
         CR    R6,R1                                                            
         BNL   PCT100                                                           
PCT10    OC    0(ELCHKENT,R6),0(R6)                                             
         BZ    PCT10NX                                                          
PCT10A   CLI   ELSTAT,0              SKIP REVERSALS AND PCTS                    
         BNE   PCT10NX                                                          
*                                                                               
         USING PAYCDTAB,R3                                                      
         L     R3,ADISPBLK           CHECK TABLE FOR CODE                       
         B     PCT20                                                            
PCT20NX  LA    R3,PAYCDLEN(R3)                                                  
         L     R1,PAYEND                                                        
         CR    R3,R1                                                            
         BL    *+6                                                              
         DC    H'0'                CODE NOT FOUND                               
PCT20    OC    PAYCDNUM(PAYCDLEN),PAYCDNUM                                      
         BNZ   *+6                                                              
         DC    H'0'                  CODE NOT FOUND                             
         CLC   ELNUM,PAYCDNUM        MATCH ON PAYROLL NUMBER                    
         BNE   PCT20NX                                                          
         OC    PAYCDPCS,PAYCDPCS     ANY PERCENTAGES TO ADD                     
         BZ    PCT10NX                                                          
*                                                                               
         USING PCTSTABD,R4         BUMP THROUGH PERCENT TABLE AND               
         L     R4,APCTSTAB         ADD ANY PERCENTAGE ELEM                      
         B     PCT30                                                            
PCT30NX  LA    R4,PCTSLEN(R4)                                                   
         CLC   =X'FFFF',0(R4)                                                   
         BE    PCT10NX                                                          
*                                                                               
PCT30    MVC   BYTE,PCTNO          DON'T CALC THIS PCT?                         
         NC    BYTE,PCTSNO                                                      
         CLC   BYTE,PCTSNO         (CAN'T TM)                                   
         BE    PCT30NX             PC=NO OVERRIDE                               
         MVC   BYTE,PCTDATE                                                     
         NC    BYTE,PCTSDATE                                                    
         CLC   BYTE,PCTSDATE       ONLY CALC PCT FOR RANGE                      
         BNE   PCT36                                                            
         LA    R0,STARTWRK                                                      
         ZICM  R1,PCTSSTRT,2       DISP TO PCT START DATE                       
         AR    R1,R0                                                            
         CLC   WORK2(2),0(R1)      IS MOA IN RANGE                              
         BL    PCT30NX                                                          
         ZICM  R1,PCTSEND,2        DISP TO PCT END DATE                         
         AR    R1,R0                                                            
         CLC   WORK2(2),0(R1)      IS MOA IN RANGE                              
         BH    PCT30NX                                                          
*                                                                               
PCT36    ZICM  R1,PCTSNUMB,2       DISP TO PCT PAYROLL NUMBER IN TABLE          
         AR    R1,R3                                                            
         OC    0(1,R1),0(R1)       ANY NUMBER                                   
         BZ    PCT30NX             NO, SKIP                                     
         MVC   BYTE,0(R1)          PCT NUMBER                                   
*                                                                               
         LA    R1,STARTWRK                                                      
         ZICM  R0,PCTSPCT,2        DISP TO PERCENTAGE                           
         AR    R1,R0                                                            
         ZAP   PCTAMT,0(6,R1)                                                   
         ZAP   WORK(12),ELAMT                                                   
         AP    WORK(12),ELADJ                                                   
         MP    WORK(12),PCTAMT                                                  
         CP    WORK(12),=P'0'                                                   
         BE    PCT30NX                                                          
         DP    WORK(12),=P'1000000'                                             
         ZAP   PCTAMT,WORK(8)                                                   
*                                                                               
REC2     USING PDEELD,R2           SEE IF ALREADY A PERCENTAGE                  
         L     R2,AIO                                                           
         BAS   RE,GETEL2                                                        
         B     PCT40                                                            
PCT40NX  BAS   RE,NEXTEL2                                                       
PCT40    BNE   PCT50               NO MATCH GO ADD                              
         CLC   REC2.PDENUM,BYTE                                                 
         BNE   PCT40NX                                                          
         CLC   REC2.PDEDTE,ELDATE                                               
         BNE   PCT40NX                                                          
         AP    REC2.PDEAMT,PCTAMT         ADD TO AMOUNT TOTAL                   
         OC    REC2.PDESTAT,PCTSBIT       SET PCT BIT ON                        
         B     PCT30NX                                                          
         DROP  REC2                                                             
*                                                                               
EL       USING PDEELD,R2                                                        
PCT50    LA    R2,ELEM             ADD NEW PERCENTAGE ELEM                      
         XC    ELEM,ELEM                                                        
         MVI   EL.PDEEL,PDEELQ                                                  
         MVI   EL.PDELN,PDELNQ                                                  
         MVC   EL.PDEDTE,ELDATE                                                 
         MVC   EL.PDENUM,BYTE                                                   
         ZAP   EL.PDEAMT,PCTAMT                                                 
         ZAP   EL.PDEADJ,=P'0'                                                  
         OC    EL.PDESTAT,PCTSBIT       SET PCT BIT ON                          
         MVC   EL.PDESTAT2,ELSTAT2                                              
         BAS   RE,CHKPCTS          CHECK PERCENTS FOR BIT INFO                  
         TM    ELSTAT2,PDESHRTE    IF CALLING PAYCODE IS AN HRATE AND           
         BZ    PCT55               PC1/PC2 PAYCODE IS NOT THEN                  
         TM    STATUS,STHRTE       DON'T SAVE AS HRATE AND VICE VERSA           
         BO    *+8                                                              
         NI    EL.PDESTAT2,X'FF'-PDESHRTE                                       
         B     PCT60                                                            
PCT55    TM    STATUS,STHRTE                                                    
         BZ    PCT60                                                            
         OI    EL.PDESTAT2,PDESHRTE                                             
PCT60    GOTO1 ADDELEM                                                          
         B     PCT30NX                                                          
         DROP  EL                                                               
*                                                                               
PCT100   GOTO1 =A(MAKETAB),DMCB,RR=RELO                                         
PCTX     XIT1                                                                   
         DROP  R6,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
*        CHECK PERCENTAGE TABLE FOR STATUS INFORMATION                          
*        BYTE CONTAINS PAYROLL CODE NUMBER                                      
*        SAVE STATUS BYTE INFO IN STATUS                                        
***********************************************************************         
*                                                                               
CHKPCTS NTR1                                                                    
         USING PAYCDTAB,R1                                                      
         L     R1,ADISPBLK                                                      
         MVI   STATUS,0                                                         
         B     CHK20                                                            
CHK10NX  LA    R1,PAYCDLEN(R1)                                                  
         L     RE,PAYEND                                                        
         CR    R1,RE               CHECK FOR END OF TABLE                       
         BNL   CHKX                                                             
*                                                                               
CHK20    OC    PAYCDNUM(PAYCDLEN),PAYCDNUM                                      
         BZ    CHKX                                                             
         CLC   PAYCDNUM,BYTE       MATCH ON PAYROLL CODE NUMBER                 
         BNE   CHK10NX                                                          
         MVC   STATUS,PAYCDST      SAVE STATUS                                  
CHKX     B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        REMOVE ALL EXISTING PERCENTAGES                                        
***********************************************************************         
*                                                                               
REMPCTS  NTR1                                                                   
         USING PDEELD,R6                                                        
         L     R6,AIO1                                                          
         MVI   ELCODE,PDEELQ       X'86' PAYROLL DETAIL ELEM                    
         BAS   RE,GETEL                                                         
         B     RP10                                                             
RP10NX   BAS   RE,NEXTEL                                                        
RP10     BNE   RPX                                                              
         TM    PDESTAT,PDEPCS                                                   
         BZ    RP10NX                                                           
         MVI   0(R6),X'FF'         DELETE                                       
         B     RP10NX                                                           
*                                                                               
RPX      MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        GET ANY PERCENTAGES FROM PROFILES                                      
***********************************************************************         
*                                                                               
GETPCTS  NMOD1 0,*GETPCT*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         L     R0,AIO3             USE AIO3 FOR COBLOCK                         
         L     R1,=A(COBLOCKX-COBLOCK)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING COBLOCK,R6                                                       
         L     R6,AIO3                                                          
         MVC   COADM,DATAMGR       PASS A(DATA MANAGER)                         
         MVC   COBKEY(COBKEYLN),SPACES                                          
         MVC   COKCPY,CMPY                                                      
         MVC   COKMTHD,SPACES                                                   
         MVC   COKOFC,OFFICE                                                    
         MVC   COKDPT(L'DEPT),DEPT                                              
         MVC   COKSDT(L'SUBDPT),SUBDPT                                          
         MVC   COKPER,PERSON                                                    
         GOTO1 VGETCAP,DMCB,COBLOCK                                             
         CLI   COSTATUS,X'00'      ANY ERRORS                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PCTSTABD,R4                                                      
         L     R4,APCTSTAB         PERCENT TABLE                                
         LA    R3,COBLOCK          PROFILE TABLE                                
*                                                                               
GP20     LA    R2,STARTWRK                                                      
         ZICM  R1,PCTSPCT,2        DISP TO PERCENT SAVE VALUE                   
         AR    R2,R1               ADD TO A(STARTWRK)                           
         ZAP   0(6,R2),=P'0'                                                    
*                                                                               
         ZICM  R1,PCTSPCTD,2       DISP TO PROFILE PERCENT                      
         BZ    GP40                                                             
         AR    R1,R3               ADD TO A(COBLOCK)                            
         ZAP   0(6,R2),0(4,R1)                                                  
*                                                                               
GP40     LA    R4,PCTSLEN(R4)                                                   
         CLC   =X'FF',0(R4)                                                     
         BNE   GP20                                                             
         XIT1                                                                   
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        SETS UP REVERSAL ELEMENTS THAT GETS ADDED TO NEXT                      
*        MONTH'S RECORD FOR THE NEGATIVE AMOUNT                                 
***********************************************************************         
*                                                                               
REVELEM  NMOD1 0,**REV***                                                       
         L     RC,SAVERC                                                        
*                                                                               
         XC    REVELEMS,REVELEMS                                                
*                                                                               
         USING ELEMTABD,R3                                                      
         LA    R3,ELEMBLK          BLOCK OF ELEMENTS                            
REV10    OC    0(ELCHKENT,R3),0(R3)                                             
         BZ    REV50NX                                                          
*                                                                               
         USING PAYCDTAB,R2                                                      
REV10A   L     R2,ADISPBLK         CHECK IF CODE HAS REVERSAL                   
         B     REV20                                                            
REV20NX  LA    R2,PAYCDLEN(R2)                                                  
         L     R1,PAYEND                                                        
         CR    R2,R1                                                            
         BNL   REV50NX                                                          
REV20    OC    PAYCDNUM(PAYCDLEN),PAYCDNUM                                      
         BZ    REV50NX                                                          
         CLC   PAYCDNUM,ELNUM       MATCH ON NUMBER                             
         BNE   REV20NX                                                          
         CLC   PAYCDREV,SPACES      ANY REVERSAL CODE                           
         BNH   REV50NX                                                          
         MVC   REVCODE,PAYCDREV                                                 
         MVC   CODE,PAYCDNME       SAVE ORIGINAL CODE FOR DESC                  
         BAS   RE,MAKEREV          MAKE THE REVERSAL ELEMENT                    
*                                                                               
REV50NX  LA    R3,ELLEN(R3)                                                     
         LA    R1,ELEMEND                                                       
         CR    R3,R1                                                            
         BL    REV10                                                            
*                                                                               
REVX     XIT1                                                                   
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        MAKES REVERSAL ELEMENT FROM ENTRY AT R3                                
***********************************************************************         
*                                                                               
         USING ELEMTABD,R3                                                      
         USING PDEELD,R4                                                        
MAKEREV  NTR1                                                                   
         LA    R4,REVELEM1         USE NEXT AVAILABLE SPACE                     
         OC    REVELEM1,REVELEM1   ROOM FOR 3 ADDS SO 3 ELEMS AVAILABLE         
         BZ    MR10                                                             
         LA    R4,REVELEM2                                                      
         OC    REVELEM2,REVELEM2                                                
         BZ    MR10                                                             
         LA    R4,REVELEM3                                                      
*                                                                               
MR10     MVI   PDEEL,PDEELQ                                                     
         MVC   PDEDTE(2),INDATE    WILL ADD A MONTH TO DATE LATER               
         MVI   PDEDTE+2,X'01'      ALWAYS USE THE FIRST                         
         ZAP   PDEAMT(6),ELAMT                                                  
         OI    PDEAMT+5,X'0D'      MAKE NEGATIVE                                
         ZAP   PDEADJ(6),ELADJ                                                  
         OI    PDEADJ+5,X'0D'      MAKE NEGATIVE                                
         OI    PDESTAT,PDERVRSL                                                 
*                                                                               
         USING PAYCDTAB,R2                                                      
         L     R2,ADISPBLK         GET NUMBER OF REVERSAL CODE                  
         B     MR20                                                             
MR20NX   LA    R2,PAYCDLEN(R2)                                                  
         L     R1,PAYEND                                                        
         CR    R2,R1                                                            
         BL    MR20                                                             
         XC    0(L'REVELEM1,R4),0(R4) DON'T DO REVERSAL                         
         B     XIT                                                              
MR20     OC    PAYCDNUM(PAYCDLEN),PAYCDNUM                                      
         BNZ   MR22                                                             
         XC    0(L'REVELEM1,R4),0(R4)     NO MATCH ON REV CODE                  
         B     XIT                        SO DON'T DO REVERSAL                  
MR22     CLC   PAYCDNME,REVCODE     MATCH ON REVERSAL CODE                      
         BNE   MR20NX                                                           
         MVC   PDENUM,PAYCDNUM      SAVE MATCHING NUMBER                        
*                                                                               
         MVC   PDEDESC(9),AC@REVU          DESCRIPTION                          
         MVI   PDEDESC+8,C'-'                                                   
         MVC   PDEDESC+9(5),CODE                                                
         OC    PDEDESC,SPACES                                                   
         LA    R1,14                                                            
         AH    R1,=Y(PDELNQ)                                                    
         STC   R1,PDELN                                                         
         B     XIT                                                              
         DROP  R4,R2,R3                                                         
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        LOOK FOR X'86' ELEMENTS AND MAKE TABLE                                 
***********************************************************************         
*                                                                               
MAKETAB  NMOD1 0,*MKETAB*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         LA    R0,ELEMBLK          CLEAR BLOCK FOR TABLE                        
         L     R1,=A(ELEMEND-ELEMBLK)                                           
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         XC    TABCOUNT,TABCOUNT   COUNT OF TABLE ENTRIES                       
         MVI   SEQNUM,0                                                         
*                                                                               
         USING ELEMTABD,R4                                                      
         LA    R4,ELEMBLK          MAKE TABLE OF ELEMENTS                       
         B     MT21                                                             
*                                                                               
MT20NX   TM    OPTSTAT,OPTSMON     MONTH OPT, JUST REBUILD FROM AIO1            
         BO    MT30                                                             
         ZIC   R1,SEQNUM           NEXT REC                                     
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
*                                                                               
         USING PHIRECD,R6                                                       
MT21     L     R6,AIO1                                                          
         TM    OPTSTAT,OPTSMON     MONTH OPT, JUST REBUILD FROM AIO1            
         BO    MT22                                                             
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'ACTKEY),SAVEKEY                                         
         LA    R6,BIGKEY                                                        
         MVC   PHIKSEQ,SEQNUM                                                   
         MVC   KEYSAVE,BIGKEY                                                   
         CLI   ACTEQU,ACTDIS                                                    
         BE    *+8                                                              
         MVI   DMCB,X'08'          READ FOR DELETES IF ADD OR CHANGE            
         GOTO1 DATAMGR,DMCB,=C'DMRDHI ',=C'ACCDIR  ',BIGKEY,BIGKEY,0            
         CLC   BIGKEY(21),KEYSAVE                                               
         BNE   MT30                                                             
         MVC   SVDSKADR,PHIKDA                                                  
         MVC   AIO,AIO2                                                         
         LA    R2,BIGKEY                                                        
         AH    R2,LKEY                                                          
         AH    R2,LSTATUS                                                       
         CLI   ACTEQU,ACTDIS                                                    
         BE    *+8                                                              
         MVI   DMCB,X'08'          READ FOR DELETES IF ADD OR CHANGE            
         GOTO1 DATAMGR,DMCB,=C'GETREC ',=C'ACCMST  ',(R2),AIO,WORK              
         BAS   RE,DELOFF           TURN OFF DELETE BITS                         
         MVC   AIO,AIO1                                                         
         CLC   BIGKEY(21),KEYSAVE  SAME UP TO MONTH                             
         OI    BITS,MULTRECS       MORE THAN 1 SEQ NUM                          
*                                                                               
         USING PDEELD,R6                                                        
         L     R6,AIO2             GET ELEMENTS                                 
MT22     MVI   ELCODE,PDEELQ                                                    
         BAS   RE,GETEL                                                         
         B     MT23                                                             
MT22NX   LA    R4,ELLEN(R4)                                                     
         LA    R1,ELEMEND                                                       
         CR    R4,R1                                                            
         BL    *+6                                                              
         DC    H'0'                MUST EXPAND TABLE                            
MT23NX   MVI   ELCODE,PDEELQ                                                    
         BAS   RE,NEXTEL                                                        
MT23     BNE   MT20NX                                                           
         LH    R1,TABCOUNT         COUNT ENTRIES IN TABLE                       
         LA    R1,1(R1)                                                         
         STH   R1,TABCOUNT                                                      
         MVC   ELSEQ,SEQNUM        SAVE SEQ NUM OF RECORD                       
         MVC   ELDATE,PDEDTE       SAVE ENTRY IN TABLE                          
         MVC   ELNUM,PDENUM                                                     
         MVC   ELPRDATE,PDEDTE                                                  
         MVC   ELPRNUM,PDENUM                                                   
         ZAP   ELAMT,PDEAMT                                                     
         ZAP   ELADJ,PDEADJ                                                     
         MVC   ELSTAT,PDESTAT      STATUS BYTE                                  
         MVC   ELSTAT2,PDESTAT2                                                 
*                                                                               
         ZIC   R1,PDELN                                                         
         LA    R0,PDELNQ                                                        
         SR    R1,R0                                                            
         LTR   R1,R1                                                            
         BZ    MT22NX                                                           
         STC   R1,ELDESCLN                                                      
         SH    R1,=H'1'                                                         
         BM    MT22NX                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELDESC(0),PDEDESC                                                
         B     MT22NX                                                           
*                                                                               
MT30     TM    OPTSTAT,OPTXDET     XD=Y OPTION                                  
         BZ    MT40                                                             
         TM    OPTSTAT,OPTSMON                                                  
         BZ    *+12                                                             
         L     R6,AIO1                                                          
         B     *+8                                                              
         L     R6,AIO2                                                          
         USING ACTVD,R6                                                         
         MVI   ELCODE,X'F1'                                                     
         BAS   RE,GETEL                                                         
         BE    MT35                                                             
         OI    BITS,NODETAIL                                                    
         B     MT40                                                             
*                                                                               
         USING XDETD,R3                                                         
MT35     LA    R3,XDETBLK                                                       
         MVC   XDETBLK,SPACES                                                   
         CLI   TWAOFFC,C'*'        IF NOT DDS TERMINAL SUPPRESS DA              
         BNE   MT37                                                             
         MVC   XDDAW,=C'DA='                                                    
         GOTO1 HEXOUT,DMCB,SVDSKADR,XDDA,L'SVDSKADR                             
         MVI   XDCOMMA1,C','                                                    
         MVC   XDDADDW,=C'DADD='                                                
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(X'20',XDDADD)                          
         MVI   XDCOMMA2,C','                                                    
         MVC   XDDCHAW,=C'DCHA='                                                
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(X'20',XDDCHA)                          
         MVI   XDCOMMA3,C','                                                    
         MVC   XDWHOW,=C'WHO='                                                  
         MVC   XDWHO,ACTVSCID                                                   
         B     MT40                                                             
*                                                                               
MT37     MVC   XDDADDWN,=C'DADD='                                               
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(X'20',XDDADDN)                         
         MVI   XDCOMM1N,C','                                                    
         MVC   XDDCHAWN,=C'DCHA='                                               
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(X'20',XDDCHAN)                         
         MVI   XDCOMM2N,C','                                                    
         MVC   XDWHOWN,=C'WHO='                                                 
         MVC   XDWHON,ACTVSCID                                                  
*                                                                               
MT40     LH    R2,TABCOUNT         SORT ON DATE                                 
         L     RF,=V(QSORT)                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(C'N',ELEMBLK),(R2),ELLEN,L'ELDATE,           X        
               ELDATE-ELEMTABD                                                  
         MVC   ELNAME,=C'*HISTAB*'          TABLE HEADER                        
         MVI   SEQNUM,0                                                         
         XC    BIGKEY,BIGKEY                                                    
         MVC   BIGKEY(L'ACTKEY),SAVEKEY                                         
         XIT1                                                                   
         DROP  R6,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*     READS PAYROLL CODE RECORD(S) AND MAKES TABLE OF CODES IN PAYCDBLK         
***********************************************************************         
*                                                                               
PAYCDS   NMOD1 0,*PAYCDS*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         L     R0,ADISPBLK                                                      
         LA    R1,PCTABLN                                                       
         AR    R0,R1                                                            
         ST    R0,PAYEND           SAVE END OF TABLE                            
*                                                                               
         L     R0,ADISPBLK         CLEAR BLOCK FOR TABLE                        
         LA    R1,PCTABLN                                                       
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVI   PSEQNUM,0                                                        
*                                                                               
         USING PAYCDTAB,R3                                                      
         L     R3,ADISPBLK                                                      
         USING PAYRECD,R6                                                       
         NI    SPECBIT,X'FF'-ADJYES                                             
PC10     LA    R6,BIGKEY                                                        
         XC    BIGKEY,BIGKEY                                                    
         MVC   PAYKEY,SPACES                                                    
         MVI   PAYKTYP,PAYKTYPQ    PAYROLL CODE RECORD TYPE                     
         MVI   PAYKSUB,PAYKSUBQ    PAYROLL CODE SUB TYPE                        
         MVC   PAYKCPY,CMPY        COMPANY                                      
         MVC   PAYKSEQ,PSEQNUM     SEQUENCE NUMBER                              
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   PCX                                                              
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,PAYELQ                                                    
         USING PAYELD,R6                                                        
         BAS   RE,GETEL                                                         
         B     PC15                                                             
PC15NX   BAS   RE,NEXTEL                                                        
PC15     BNE   PC30                                                             
         MVC   PAYCDNUM,PAYNUM                                                  
         MVC   PAYCDNME,PAYCODE                                                 
         MVC   PAYCDREV,PAYREV                                                  
         MVC   PAYCDPCS,PAYPCS                                                  
         MVC   PAYCDST,PAYSTAT                                                  
*                                                                               
         TM    PAYSTAT,PAYADJRT    IS THERE AN ADJUSTMENT RATE SET UP?          
         BZ    *+8                                                              
         OI    SPECBIT,ADJYES      MUST ALLOW FOR SPECIAL RULES                 
*                                                                               
         LA    R3,PAYCDLEN(R3)                                                  
         L     R1,PAYEND                                                        
         CR    R3,R1                                                            
         BL    PC15NX                                                           
         DC    H'0'                TOO MANY PAYCODES-INCREASE TABLE             
*                                                                               
PC30     ZIC   R1,PSEQNUM          CHECK FOR NEXT REC                           
         LA    R1,1(R1)                                                         
         STC   R1,PSEQNUM                                                       
         B     PC10                                                             
*                                                                               
PCX      XIT1                                                                   
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ANY OPTIONS                                                   
***********************************************************************         
*                                                                               
VALOPTS  NMOD1 0,*VALOPT*                                                       
         L     RC,SAVERC                                                        
*                                                                               
         MVI   OPTSTAT,0                                                        
         MVI   PCTNO,0                                                          
         MVI   PCTDATE,0                                                        
         MVC   XDETBLK,SPACES                                                   
         LA    R2,SHMDETLH                                                      
         MVC   SHMDETL,SPACES                                                   
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   ACTEQU,ACTLIST                                                   
         BE    *+14                                                             
         MVC   SHMWORD,SPACES                                                   
         OI    SHMWORDH+6,X'80'                                                 
*                                                                               
         LA    R2,CONOPTH                                                       
         CLI   5(R2),0                                                          
         BE    OPTX                                                             
         CLI   ACTEQU,ACTLIST      NOTHING VAILD FOR LIST                       
         BNE   *+8                                                              
         B     EINVLOPT                                                         
*                                                                               
         XC    BLOCK+L'PVALOUTB(200),BLOCK+L'PVALOUTB                           
         GOTO1 SCANNER,DMCB,(15,CONOPTH),(3,BLOCK+L'PVALOUTB)                   
         CLI   DMCB+4,X'00'                                                     
         BE    EINVOPT                                                          
         LA    R4,BLOCK+L'PVALOUTB                                              
         USING SCANBLKD,R4                                                      
OPT10    MVI   SBYTE,OPTMONQ                                                    
         CLC   AC@MNTHU,SC1STFLD                                                
         BE    OPT15                                                            
         CLC   AC@ANNU,SC1STFLD                                                 
         BNE   OPT20                                                            
         MVI   SBYTE,OPTANNQ                                                    
OPT15    BAS   RE,OPTSEC           MUST HAVE OPTION SECURITY                    
         BNE   EINVOPT                                                          
         OI    OPTSTAT,OPTSMON     USING MONTH OPTION                           
         CLC   AC@ANNU,SC1STFLD                                                 
         BNE   *+8                                                              
         OI    OPTSTAT,OPTSANN     USING MONTH PLUS ANNUAL                      
         MVC   SHMWORD,AC@UPDU                                                  
         OI    SHMWORDH+6,X'80'                                                 
         NI    SHMUPDTH+1,X'FF'-X'20'    UNPROTECT                              
         OI    SHMUPDTH+6,X'80'                                                 
         B     OPT100                                                           
*                                                                               
         USING PCTSTABD,R6                                                      
OPT20    L     R6,APCTSTAB          CHECK FOR ANY PERCENTAGES                   
OPT30    CLC   PCTSWORD,SC1STFLD                                                
         BE    OPT40                                                            
         LA    R6,PCTSLEN(R6)                                                   
         CLC   =X'FFFF',0(R6)                                                   
         BE    OPT50                                                            
         B     OPT30                                                            
*                                                                               
OPT40    CLI   SC2NDLEN,2          IF LENGTH IS 2                               
         BNE   OPT45                                                            
         CLC   AC@NOU,SC2NDFLD     PC=NO OPTION                                 
         BNE   OPT45                                                            
         OC    PCTNO,PCTSNO        SET NO BIT ON                                
         B     OPT100                                                           
OPT45    LA    R3,SC2NDFLD                                                      
         MVC   BYTE,SC2NDLEN                                                    
         BAS   RE,VALMOA           VALIDATE MOA RANGE FOR PCT                   
         BNE   EINVOPT                                                          
         OC    PCTDATE,PCTSDATE    SET DATES BIT ON                             
         LA    R1,STARTWRK                                                      
         ZICM  R0,PCTSSTRT,2       DSP TO START DATE                            
         AR    R1,R0                                                            
         MVC   0(2,R1),STMOA                                                    
         LA    R1,STARTWRK                                                      
         ZICM  R0,PCTSEND,2        DSP TO END DATE                              
         AR    R1,R0                                                            
         MVC   0(2,R1),ENDMOA                                                   
         B     OPT100                                                           
*                                                                               
OPT50    DS    0H                                                               
         CLC   SC1STFLD(2),=C'XD'  XD=                                          
         BNE   EINVOPT                                                          
         ZIC   R1,SC2NDLEN                                                      
         BCTR  R1,0                                                             
         EXCLC R1,SC2NDFLD,AC@YESU XD=YES                                       
         BE    OPT60                                                            
         EXCLC R1,SC2NDFLD,AC@NOU  XD=NO                                        
         BNE   EINVOPT                                                          
         B     OPT100                                                           
*                                                                               
OPT60    OI    OPTSTAT,OPTXDET                                                  
*                                                                               
OPT100   LA    R4,SCBLKLQ+5(R4)    NEXT SCANNER LINE                            
         CLI   SC1STLEN,0          ANYTHING THERE                               
         BNE   OPT10                                                            
OPTX     XIT1                                                                   
         DROP  R6,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE AN AMOUNT IN FIELD AT HEADER IN R3                            
*                           - RETURN PL8 IN WORK                                
***********************************************************************         
*                                                                               
VALAMT   NMOD1 0,*VALAMT*                                                       
         L     RC,SAVERC                                                        
         USING ELEMTABD,R4                                                      
         ZAP   WORK(8),=P'0'                                                    
         LR    R2,R3                                                            
         ZIC   R0,0(R2)            NEED TO CHECK FOR /HR                        
         SH    R0,=H'8'                                                         
         TM    1(R2),X'02'         XHEADER                                      
         BNO   *+8                                                              
         SH    R0,=H'8'            R0=MAX LEN OF FIELD                          
*                                                                               
         LA    R1,8(R2)                                                         
         SR    R3,R3                                                            
VA10     CLC   0(L'AC@PERHR,R1),AC@PERHR      /HR                               
         BE    VA30                                                             
         LA    R3,1(R3)            LENGTH TO VALIDATE NUMBEER                   
         LA    R1,1(R1)                                                         
         BCT   R0,VA10                                                          
*                                                                               
         ZIC   R3,5(R2)             VALIDATE AMOUNT                             
VA30     GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(R3)                                  
         CLI   DMCB,X'FF'                                                       
         BE    EINVAMT                                                          
         TM    ELSTAT2,PDESHRTE    HOURLY RATE                                  
         BO    VA40                                                             
         CP    DMCB+4(8),=P'9999999.99'                                         
         BH    EINVAMT                                                          
         CP    DMCB+4(8),=P'-9999999.99'                                        
         BL    EINVAMT                                                          
         ZAP   WORK(8),DMCB+4(8)                                                
         B     VA50                                                             
*                                                                               
VA40     CP    DMCB+4(8),=P'999.99'                                             
         BH    EINVAMT                                                          
         CP    DMCB+4(8),=P'-999.99'                                            
         BL    EINVAMT                                                          
         ZAP   WORK(8),DMCB+4(8)                                                
         B     VAX                 NO ANNUAL OPTION FOR HOURLY RATES            
*                                                                               
VA50     TM    OPTSTAT,OPTSANN     ANNUAL OPTION ON                             
         BNO   VAX                                                              
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VAX                                                              
         ZAP   WORK+10(8),WORK(8)                                               
         DP    WORK+10(8),=P'12'      DIVIDE BY 12                              
         ZAP   WORK(8),WORK+10(6)                                               
*                                                                               
VAX      XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        CHECK OPTION SECURITY FOR MONTH OPTION                                 
***********************************************************************         
*                                                                               
OPTSEC   NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         LA    R2,SBYTE                                                         
         GOTO1 SECRET,DMCB,('SECPOPTP',ASECBLK),(R2)                            
         CLI   DMCB,SECPYES                                                     
         BE    XYES                                                             
         B     XNO                                                              
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        INCLUDES                                                     *         
***********************************************************************         
*                                                                               
*        DDSPOOLD                                                               
*        DDSPLWORKD                                                             
*        DDPERVALD                                                              
*        DDSCANBLKD                                                             
*        FASECRETD                                                              
*        ACCAPWORKD                                                             
*        ACCAPDSECT                                                             
*        ACBMONVALD                                                             
*        ACGENFILE                                                              
*        ACDDEQUS                                                               
*        DDACTIVD                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACBMONVALD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE DDACTIVD                                                       
COBLOCKD DSECT                                                                  
       ++INCLUDE ACCAPBLOCK                                                     
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*        SCREENS                                                      *         
***********************************************************************         
*                                                                               
       ++INCLUDE ACCAPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPF2D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPF3D                                                       
         EJECT                                                                  
***********************************************************************         
*        REMAINING WORK AREA                                          *         
***********************************************************************         
*                                                                               
STARTWRK DS    0F                                                               
RELO     DS    A                                                                
SAVERC   DS    F                   SAVED RC                                     
STADDR   DS    F                   START ADDRESS                                
ATOTTAB  DS    F                   A(TOTALTAB)                                  
ATOTDFLT DS    F                   A(TOTDEFLT)                                  
APCTSTAB DS    F                   A(PCTSTAB)                                   
PAYEND   DS    F                   END OF PAYROLL CODE TABLE                    
WORK2    DS    4F                                                               
SVADDR   DS    F                                                                
ENDADDR  DS    F                                                                
TABCOUNT DS    H                   COUNT OF TABLE ENTRIES                       
STDISP   DS    H                   START DISPLACEMENT                           
PRVSTDSP DS    H                   PREVIOUS START DISPLACEMENT                  
DLINE1   DS    H                   DISPLACEMENT TO LINE 1                       
TEMP     DS    PL2                                                              
*                                                                               
STDATECM DS    H                   PWOS YEAR AND MONTH - COMPLEMENT             
ENDATECM DS    H                   PWOS YEAR AND MONTH - COMPLEMENT             
NXTMOA   DS    H                                                                
STMOA    DS    PL2                                                              
ENDMOA   DS    PL2                                                              
CURRMOA  DS    PL2                                                              
STDATE   DS    PL3                                                              
ENDATE   DS    PL3                                                              
NXTDATE  DS    PL3                                                              
MONFIRST DS    PL3                 FIRST DAY IN MONTH                           
MONLAST  DS    PL3                 LAST DAY IN MONTH                            
YYMMDD   DS    CL3                 PWOS YEAR AND MONTH                          
INDATE   DS    CL3                 PWOS YEAR AND MONTH                          
EBDATE   DS    CL6                 YYMMDD                                       
AMDATE   DS    CL8                 MMMDD/YY                                     
DDSELW   DS    CL6                 'SELECT' FROM DICTATE CALL                   
*                                                                               
METHCODE DS    CL3                 METHOD CODE                                  
METHNUM  DS    XL1                 METHOD NUMBER                                
SVMTHNUM DS    XL1                 METHOD NUMBER                                
PERSON   DS    CL8                 PERSON CODE                                  
OFFICE   DS    CL2                 OFFICE CODE                                  
DEPT     DS    CL3                 DEPARTMENT CODE                              
SUBDPT   DS    CL3                 SUB DEPARTMENT CODE                          
*PTSTART DS    PL3                 DEPT START DATE                              
*PTEND   DS    PL3                 DEPT END DATE                                
*PTSALDT DS    PL3                 DEPT SALARY LOCK DATE                        
*                                                                               
ADJTOT   DS    PL8                 MONTH TOTALS                                 
AMTTOT   DS    PL8                                                              
PENMTOT  DS    PL8                                                              
BENMTOT  DS    PL8                                                              
SALMTOT  DS    PL8                                                              
INDMTOT  DS    PL8                                                              
OTHMTOT  DS    PL8                                                              
PENTOT   DS    PL8                 SCREEN TOTALS                                
BENTOT   DS    PL8                                                              
SALTOT   DS    PL8                                                              
INDTOT   DS    PL8                                                              
PENADJ   DS    PL8                 ADJUSTMENT TOTALS                            
BENADJ   DS    PL8                                                              
SALADJ   DS    PL8                                                              
INDADJ   DS    PL8                                                              
OTHADJ   DS    PL8                                                              
MONTOT   DS    PL8                 MONTH  TOTAL                                 
ALLTOT   DS    PL8                 SCREEN TOTAL                                 
*                                                                               
PCDNUM   DS    XL1                 PAYROLL CODE NUMBER                          
INNUM    DS    XL1                 PAYROLL CODE NUMBER                          
SVAMT    DS    PL6                 SAVED TAPE AMOUNT                            
SVADJ    DS    PL6                                                              
SVSTAT   DS    XL1                 SAVED STATUS                                 
SVSTAT2  DS    XL1                 SAVED STATUS                                 
REVCODE  DS    CL5                 REVERSAL CODE                                
REVNUM   DS    XL1                                                              
CODE     DS    CL5                                                              
PCTAMT   DS    PL6                                                              
FIRSTIME DS    CL1                 INDICATES FIRST TIME IN PROGRAM              
EPDATE   DS    PL3                 LAST DAY OF MOA                              
*                                                                               
BITS     DS    XL1                                                              
YESOFF   EQU   X'80'               OFFICE WAS SPECIFIED                         
YESDPT   EQU   X'40'               DEPARTMENT WAS SPECIFIED                     
YESSDPT  EQU   X'20'               SUBDEPT WAS SPECIFIED                        
NODETAIL EQU   X'10'               NO DETAILS AVAILABLE                         
LASTLINE EQU   X'08'               ON LAST LIST LINE                            
MULTRECS EQU   X'04'               MORE THAN ONE REC FOR THAT DATE              
NEWREC   EQU   X'02'               ADD REC                                      
BYNUM    EQU   X'01'               NEED METHOD REC BY NUM NOT CODE              
*                                                                               
BIT2     DS    XL1                                                              
REDISP   EQU   X'80'                                                            
YTDADJ   EQU   X'40'                                                            
ONEMONTH EQU   X'20'                                                            
KEYCHNG  EQU   X'10'                                                            
HOURLY   EQU   X'08'               THERE IS AN HOURLY RATE IN MONTH             
ADJUSTED EQU   X'04'               THERE IS AN ADJUSTMENT                       
FIRST    EQU   X'02'               FIRST TIME THROUGH                           
NOMONTH  EQU   X'01'               MONTH OPT WAS NOT ENTRD BUT REQUIRED         
*                                                                               
STATUS   DS    XL1                                                              
STHRTE   EQU   X'80'                                                            
STADJRT  EQU   X'40'                                                            
STCRTE   EQU   X'20'                                                            
*                                                                               
SVOPTSTA DS    XL1                 SAVED OPTION STATUS                          
OPTSTAT  DS    XL1                 OPTION STATUS                                
OPTSMON  EQU   X'80'               USING MONTH OPTION                           
OPTSANN  EQU   X'40'               USING ANNUAL OPTION                          
OPTXDET  EQU   X'20'               USING XD=Y OPTION                            
PCTNO    DS    XL1                 OPTION TO OVERRIDE PC TO NO                  
PCTNOPC1 EQU   X'80'                                                            
PCTNOPC2 EQU   X'40'                                                            
PCTDATE  DS    XL1                 OPTION TO USE PC ON RANGE ONLY               
PCTDATE1 EQU   X'80'                                                            
PCTDATE2 EQU   X'40'                                                            
PC1STMOA DS    PL2                                                              
PC1NDMOA DS    PL2                                                              
PC2STMOA DS    PL2                                                              
PC2NDMOA DS    PL2                                                              
PC1PCT   DS    PL6                                                              
PC2PCT   DS    PL6                                                              
*                                                                               
SBYTE    DS    CL1                 SECURITY BYTE                                
NAMEFLDQ EQU   1                   NAME FLIELD EQUATE FOR SECURITY              
OPTMONQ  EQU   3                   MONTH OPTION EQUATE FOR SECURITY             
OPTANNQ  EQU   4                   ANNUAL OPTION EQUATE FOR SECURITY            
*                                                                               
SPECBIT  DS    CL1                                                              
ADJYES   EQU   X'80'               ADJ RATE IS SET UP                           
SALOLOC  EQU   X'40'               DIS/ADD/CHA SAL OUTSIDE OF LOC DATE          
CHAMOA   EQU   X'20'               CHANGE IN MOA                                
VALPAYDT EQU   X'10'               VALIDATE ENTERED PAYDATE W/LOCATIONS         
LOWMOA   EQU   X'08'               STDATE IS BEFORE LOCSTART                    
*                                                                               
SEQNUM   DS    XL1                                                              
PSEQNUM  DS    XL1                                                              
LEVELLN  DS    0CL4                LENGTHS OF ALL LEVELS                        
LEVELLNA DS    CL1                 LENGTH OF A                                  
LEVELLNB DS    CL1                 LENGTH OF B                                  
LEVELLNC DS    CL1                 LENGTH OF C                                  
LEVELLND DS    CL1                 LENGTH OF D                                  
ACCNT    DS    CL12                MY TEMP ACCOUNT FIELD                        
LISTMOA  DS    CL15                SAVE LIST MOA FOR PF12                       
METHNAME DS    CL34                                                             
PERNAME  DS    CL45                                                             
SAVEKEY  DS    XL42                ACCFILE KEY                                  
METHKEY  DS    XL42                METHOD  KEY                                  
KEY2     DS    XL70                ACCFILE KEY                                  
LSTLKEY  DS    CL50                                                             
REVELEMS DS    0CL150                                                           
REVELEM1 DS    CL50                                                             
REVELEM2 DS    CL50                                                             
REVELEM3 DS    CL50                                                             
PVALBLK  DS    CL100                                                            
         DS    0H                                                               
SVDATE   DS    CL2                 SAVED DAY OF MONTH ENTERED (CHAR)            
PREVDATE DS    CL8                 PREVIOUS DATE                                
PREVDTLN DS    XL1                 PREVIOUS DATE LENGTH                         
PREVCODE DS    CL5                 PREVIOUS PAY CODE                            
SVDSKADR DS    XL4                                                              
XDETBLK  DS    CL50                BLOCK FOR EXTRA DETAILS                      
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
* 4,080 BYTES AVAILABLE IN SYSSPARE                                             
BLOCKSD  DSECT                     BLOCKS PUT IN DISPBLK                        
ELNAME   DS    CL8                                                              
ELEMBLK  DS    80CL(ELLEN)                                                      
ELEMEND  DS    C                                                                
         EJECT                                                                  
***********************************************************************         
*        LIST LINE DSECT                                                        
***********************************************************************         
*                                                                               
LSTLINED DSECT                     LIST LINE DSECT                              
LSTHEAD  DS    CL8                                                              
LSTSTRT  DS    CL1                                                              
LSTMONTH DS    CL6                 MONTH/YR                                     
         DS    CL2                                                              
LSTSAL   DS    CL11                DEPT                                         
         DS    CL1                                                              
LSTPEN   DS    CL11                                                             
         DS    CL1                                                              
LSTBEN   DS    CL11                                                             
         DS    CL1                                                              
LSTIND   DS    CL11                                                             
         DS    CL1                                                              
LSTTOT   DS    CL12                                                             
         DS    CL1                                                              
LSTADJ   DS    CL3                                                              
LSTLEN   EQU   *-LSTSTRT                                                        
         EJECT                                                                  
***********************************************************************         
*        DISPLAY LINE DSECT                                                     
***********************************************************************         
*                                                                               
DSPLINED DSECT                     DISPLAY LINE DSECT                           
DSPDATEH DS    CL8                                                              
DSPDATE  DS    CL8                 DATE                                         
DSPCODEH DS    CL8                                                              
DSPCODE  DS    CL5                 PAY CODE                                     
DSPTYPEH DS    CL8                                                              
DSPTYPE  DS    CL1                 TYPE                                         
DSPAMNTH DS    CL8                                                              
DSPAMNT  DS    CL11                AMOUNT                                       
DSPADJH  DS    CL8                                                              
DSPADJ   DS    CL11                ADJUSTMENT                                   
DSPTOTH  DS    CL8                                                              
DSPTOT   DS    CL11                TOTAL                                        
DSPDESCH DS    CL8                                                              
DSPDESC  DS    CL21                DESCRIPTION                                  
DSPLLEN  EQU   *-DSPLINED                                                       
         EJECT                                                                  
***********************************************************************         
*        SUMMARY LINE DSECT                                                     
***********************************************************************         
*                                                                               
SUMLINED DSECT                     SUMMARY LINE DSECT                           
SUMHEAD  DS    CL8                                                              
SUMAMNT  DS    CL11                AMOUNT                                       
         DS    CL1                                                              
SUMADJ   DS    CL11                ADJUSTMENT                                   
         DS    CL1                                                              
SUMTOT   DS    CL11                TOTAL                                        
         DS    CL1                                                              
         DS    CL8                 FOR NEXT FIELD HEADER                        
         DS    CL8                 FOR TITLE FIELD                              
SUMLLEN  EQU   *-SUMLINED                                                       
         EJECT                                                                  
***********************************************************************         
*        EXTRA DETAILS LINE DSECT                                               
***********************************************************************         
*                                                                               
* DETAILS FOR DDS TERMINALS                                                     
*                                                                               
XDETD    DSECT                                                                  
XDLINE   DS    0C                                                               
XDDAW    DS    CL3                 DA=                                          
XDDA     DS    XL8                 DISK ADDRESS                                 
XDCOMMA1 DS    CL1                 COMMA                                        
XDDADDW  DS    CL5                 DADD=                                        
XDDADD   DS    XL6                 DATE ADDED (YYMMDD)                          
XDCOMMA2 DS    CL1                                                              
XDDCHAW  DS    CL5                 DCHA=                                        
XDDCHA   DS    XL6                 DATE CHANGED (YYMMDD)                        
XDCOMMA3 DS    CL1                                                              
XDWHOW   DS    CL4                 WHO=                                         
XDWHO    DS    CL8                 LAST PERSON TO MAKE CHANGE                   
*                                                                               
* DETAILS FOR NON DDS TERMINALS                                                 
*                                                                               
         ORG   XDLINE                                                           
XDDADDWN DS    CL5                 DADD=                                        
XDDADDN  DS    XL6                 DATE ADDED (YYMMDD)                          
XDCOMM1N DS    CL1                                                              
XDDCHAWN DS    CL5                 DCHA=                                        
XDDCHAN  DS    XL6                 DATE CHANGED (YYMMDD)                        
XDCOMM2N DS    CL1                                                              
XDWHOWN  DS    CL4                 WHO=                                         
XDWHON   DS    CL8                 LAST PERSON TO MAKE CHANGE                   
***********************************************************************         
*        TOTALS TABLE DSECT                                                     
***********************************************************************         
*                                                                               
TOTD     DSECT                                                                  
TOTTYPE  DS    XL1                 TYPE INDICATOR                               
TOTCHAR  DS    CL1                 CHARACTER SYMBOL FOR TYPE                    
TOTDISP  DS    XL2                 DISPLACEMENT TO ACCUMULATOR                  
TOTADJ   DS    XL2                 DISPLACEMENT TO ADJUST ACCUMULATOR           
TOTLEN   EQU   *-TOTD                                                           
         EJECT                                                                  
***********************************************************************         
*        BUILD 1R TABLE DSECT                                                   
***********************************************************************         
*                                                                               
BILDD    DSECT                                                                  
BILDLEV  DS    AL2                 DISPLACEMENT TO LEVEL LENGTH                 
BILDCODE DS    AL2                 DISPLACEMENT TO LEVEL CODE                   
BILDLEN  EQU   *-BILDD                                                          
         EJECT                                                                  
***********************************************************************         
*        PERCENTAGE TABLE DSECT                                                 
***********************************************************************         
*                                                                               
PCTSTABD DSECT                                                                  
PCTSWORD DS    CL3                 CHARACTER EQUIVALENT (PC1,PC2,ETC)           
PCTSNUM  DS    XL2                 DISP TO PCT NUMBER IN '86' ELEM              
PCTSBIT  DS    XL1                 BIT TO SET ON IN PDESTAT                     
PCTSNO   DS    XL1                 BIT TO OVERRIDE PCT TO NO                    
PCTSDATE DS    XL1                 BIT TO USE PCT DATES                         
PCTSSTRT DS    XL2                 DISP TO PCT START DATE                       
PCTSEND  DS    XL2                 DISP TO PCT END DATE                         
PCTSNUMB DS    XL2                 DISP TO PCT NUMBER IN PAYCDTAB               
PCTSPCT  DS    XL2                 DISP TO PERCENTAGE VALUE                     
PCTSPCTD DS    XL2                 DISP TO PERCENT VALUE IN PROFILES            
PCTSLEN  EQU   *-PCTSTABD                                                       
         EJECT                                                                  
***********************************************************************         
*        PAYROLL CODES AND NUMBERS DSECT                                        
***********************************************************************         
*                                                                               
PAYCDTAB DSECT                     TABLE OF '85' ELEMS                          
PAYCDNUM DS    XL1                 PAY CODE NUMBER                              
PAYCDNME DS    XL5                 PAYROLL CODE                                 
PAYCDREV DS    XL5                 PAYROLL REVERSAL CODE                        
PAYCDPCS DS    0XL2                PCTS  ******* CHANGE WHEN ADDING PCS         
PAYCDPC1 DS    XL1                 PAYROLL PC1 NUMBER                           
PAYCDPC2 DS    XL1                 PAYROLL PC2 NUMBER                           
PAYCDST  DS    XL1                 STATUS (EQUS AS IN ELEM)                     
PAYCDLEN EQU   *-PAYCDTAB                                                       
PCTABLN  EQU   255*PAYCDLEN                                                     
         EJECT                                                                  
***********************************************************************         
*        TABLE OF 86 ELEMS DSECT                                                
***********************************************************************         
*                                                                               
ELEMTABD DSECT                     TABLE OF '86' ELEMS                          
ELPRDATE DS    XL3                 PREVIOUS DATE                                
ELPRNUM  DS    XL1                 PREVIOUS PAYROLL CODE NUMBER                 
ELSEQ    DS    XL1                 SEQ NUMBER OF RECORD FOR THIS ELEM           
ELDATE   DS    XL3                 DATE                                         
ELNUM    DS    XL1                 PAYROLL CODE NUMBER                          
ELCHKENT EQU   *-ELEMTABD          CHECK FOR ENTRY UP TO THIS POINT             
ELAMT    DS    PL6                 AMOUNT                                       
ELADJ    DS    PL6                 ADJUSTMENT                                   
ELSTAT   DS    XL1                 STATUS BYTE                                  
ELDESCLN DS    XL1                 DESC LENGTH                                  
ELDESC   DS    CL21                DESCRIPTION                                  
ELSTAT2  DS    XL1                 DATE STATUS                                  
ELLEN    EQU   *-ELEMTABD                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'118ACCAP02   05/22/12'                                      
         END                                                                    
