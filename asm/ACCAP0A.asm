*          DATA SET ACCAP0A    AT LEVEL 027 AS OF 12/11/09                      
*PHASE T61D0AA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCAP0A -- PAYROLL CODE/TYPE LIST                    *         
*                                                                     *         
*  COMMENTS:     MAINTAINS PAYROLL CODE RECORDS AND PAYROLL TYPE      *         
*                ELEMENTS IN METHOD RECORDS                           *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS ACCAPFA (LIST)                               *         
*                                                                     *         
*  OUTPUTS:      UPDATED PAYROLL CODE AND METHOD RECORDS              *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
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
* DCUR L19 -- ALLOW FOR 'ADJUST' ATTRIBUTE ON A PAYCODE               *         
* DCUR L20 -- CHANGE ATTRIBUTE TO 'YTDADJ'                            *         
* TKLU 025 24OCT05 <DU01-4857> PATEL LENGTH CHANGE - RELINK UK        *         
* TKLU 026 05SEP08 <BR12931D> OTHERS=DDS OPTION TO SHOW 255 PAYCODES  *         
***********************************************************************         
         TITLE 'T61D0A - PAYROLL CODE/TYPE LIST'                                
T61D0A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1D0A**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T61DFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         OI    GENSTAT1,RDUPAPPL                                                
*                                                                               
         LA    R3,PAYPFKYH                                                      
         SR    R2,R2                                                            
         CLI   PFKEY,7             UP                                           
         BE    PFINIT                                                           
         CLI   PFKEY,8             DOWN                                         
         BE    PFINIT                                                           
         CLI   PFKEY,5             BOTTOM                                       
         BE    PFINIT                                                           
         LA    R2,LPFTABLE                                                      
*                                                                               
*              PROGRAM INITIALIZATION - DON'T CLEAR WORKING STORAGE             
PFINIT   GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)                                   
*                                                                               
         LA    R2,PAYLISTI                                                      
         LA    R3,PAYLISTU                                                      
         GOTO1 DICTATE,DMCB,C'LU  ',(R2),(R3)                                   
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,LVALKEY        VALIDATE LIST KEY                            
         BE    LK                                                               
         CLI   MODE,LVALREC        VALIDATE LISTED RECORD                       
         BE    VL                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY                                                           
***********************************************************************         
*                                                                               
VK       DS    0H                                                               
         OI    GLSTSTAT,APPLCDSP+NOSELFLD+CHNGLIST                              
         MVC   LLIST,=Y(LINNEXTL-LINNUMH)     LENGTH OF LIST LINE               
         XC    METHCODE,METHCODE   METHOD CODE                                  
         XC    TYPEFILT,TYPEFILT                                                
         MVI   BITS,0                                                           
         MVI   STAT,0                                                           
*                                                                               
         BAS   RE,VALOPTS          VALIDATE OPTIONS                             
*                                                                               
*        SEE IF METHOD WAS SPECIFIED                                            
*                                                                               
VK15     DS    0H                                                               
         MVC   PAYMENM,SPACES      CLEAR METHOD NAME                            
         OI    PAYMENMH+6,X'80'                                                 
*                                                                               
         LA    R2,PAYMETHH                                                      
         CLI   5(R2),0             ANY DATA?                                    
         BE    VK100               IF NOT                                       
         OI    BITS,YESMETH        YES METHOD WAS SPECIFIED                     
*                                                                               
         TM    4(R2),X'08'         VALID NUMERIC                                
         BNO   VK20                                                             
*                                                                               
*        READ RECORD BY METHOD NUMBER                                           
*                                                                               
         CLI   5(R2),1             ONLY 1-9                                     
         BNE   EINVMET                                                          
         MVC   METHNUM,8(R2)                                                    
*                                                                               
         LA    R6,BIGKEY           VALIDATE METHOD                              
         USING CAHRECD,R6                                                       
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    ALLOCATION METHOD RECORD TYPE BY NUM         
         MVI   CAHKSUB,CAHKSUBQ    METHOD BY CODE SUB TYPE                      
         MVC   CAHKCPY,CMPY        COMPANY                                      
         MVC   CAHKMTHD,METHNUM    METHOD NUMBER                                
         XC    CAHKOFC,CAHKOFC                                                  
         MVC   AIO,AIO3            METHOD RECORD IN AIO3                        
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   ENOMETH                                                          
         GOTO1 GETREC                                                           
         GOTO1 GETNME,DMCB,AIO3,PAYMENMH                                        
         MVC   AIO,AIO1                                                         
         B     VK50                                                             
         DROP  R6                                                               
*                                                                               
*        READ RECORD BY METHOD CODE                                             
*                                                                               
VK20     MVC   METHCODE,PAYMETH    SAVE METHOD CODE                             
         OC    METHCODE,SPACES                                                  
*                                                                               
         LA    R6,BIGKEY           VALIDATE METHOD                              
         USING CMTRECD,R6                                                       
         MVC   CMTKEY,SPACES                                                    
         MVI   CMTKTYP,CMTKTYPQ    ALLOCATION METHOD RECORD TYPE                
         MVI   CMTKSUB,CMTKSUBQ    METHOD BY CODE SUB TYPE                      
         MVC   CMTKCPY,CMPY        COMPANY                                      
         MVC   CMTKMTHD,METHCODE   METHOD CODE                                  
         MVC   AIO,AIO3            METHOD RECORD IN AIO3                        
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   ENOMETH                                                          
         GOTO1 GETREC                                                           
         GOTO1 GETNME,DMCB,AIO3,PAYMENMH                                        
         MVC   AIO,AIO1                                                         
         B     VK50                                                             
*                                                                               
*        GET BOTH METHNUM AND METHCODE                                          
*                                                                               
VK50     DS    0H                                                               
         USING METELD,R6                                                        
         L     R6,AIO3                                                          
         MVI   ELCODE,METELQ       METHOD OF ALLOCATION ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   ERRINV                                                           
*                                                                               
         MVC   METHNUM,METNUM      SAVE BOTH NUMBER AND CODE                    
         MVC   METHCODE,METCODE                                                 
*                                                                               
         MVC   PAYMETH,METHCODE    DISPLAY METHOD CODE                          
         OI    PAYMETHH+6,X'80'                                                 
         B     VKX                                                              
*                                                                               
VK100    DS    0H                                                               
         OI    GLSTSTAT,OKTOADD    IF NO METHOD, IT'S OK TO ADD TO LIST         
*                                                                               
VKX      DS    0H                                                               
         BAS   RE,VALCODES         VALIDATE ALL CODES FIRST                     
         XC    BIGKEY,BIGKEY                                                    
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE LIST KEY                                                      
*        NEED FOR GENCON- BUT RECORD DOESN'T GET USED- HENCE SEQ NUM 99         
***********************************************************************         
*                                                                               
LK       DS    0H                                                               
         LA    R6,BIGKEY                                                        
         USING PAYRECD,R6                                                       
         MVC   PAYKEY,SPACES                                                    
         MVI   PAYKTYP,PAYKTYPQ    PAYROLL CODE RECORD TYPE                     
         MVI   PAYKSUB,PAYKSUBQ    PAYROLL CODE SUB TYPE                        
         MVC   PAYKCPY,CMPY        COMPANY                                      
         MVI   PAYKSEQ,99                                                       
*                                                                               
LKX      DS    0H                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE LISTED RECORD                                                 
***********************************************************************         
*                                                                               
VL       DS    0H                                                               
         BAS   RE,VALOPTS          VALIDATE OPTIONS                             
         TM    BITS,UPDATEON                                                    
         BNO   ERRNOUP                                                          
*                                                                               
         MVI   SEQNUM,0            START WITH REC SEQ NUMBER 0                  
         MVI   NEXTNUM,1           START WITH ELEMENT NUMBER 1                  
         NI    BITS,X'FF'-NEWELEM-YESXIST       RESET SOME BITS                 
         MVI   IOOPT,C'Y'          TELL GENCON I WILL DO IO                     
*                                                                               
         L     R3,ATHISLST         ADDRESS OF LINE                              
         USING LIND,R3                                                          
*                                                                               
         TM    BITS,YESMETH        WAS METHOD SPECIFIED?                        
         BO    VL100               THEN ONLY CHANGING METHOD RECORD             
         EJECT                                                                  
*----------------------------------------------------------------------         
*        UPDATE PAYROLL CODE RECORD IF NO METHOD IS SPECIFIED                   
*----------------------------------------------------------------------         
*                                                                               
         TM    STAT,STATDONE                                                    
         BO    VL05                                                             
         BAS   RE,VALCODES         VALIDATE ALL CODES FIRST                     
         BE    VL03                                                             
*                                                                               
         LA    R3,PAYFRSTH                                                      
VL01     OC    LINCODE,SPACES                                                   
         CLC   LINCODE,DUPCODE                                                  
         BNE   VL02                                                             
         LA    R2,LINCODEH                                                      
         B     ECDXSTS                                                          
*                                                                               
VL02     LA    R3,LINNEXTL         NEXT LINE                                    
         LA    R1,PAYLSTH                                                       
         CR    R3,R1                                                            
         BH    ERRINV                                                           
         B     VL01                                                             
*                                                                               
VL03     OI    STAT,STATDONE                                                    
         L     R3,ATHISLST         ADDRESS OF LINE                              
*                                                                               
VL05     MVI   SEQNUM,0            START WITH REC SEQ NUMBER 0                  
         CLI   LINCODEH+5,0        ANY CODE INPUT                               
         BNZ   VL10                                                             
         CLI   LINNUMH+5,0         NEW LINE?                                    
         BE    VLX                 EXIT NO CODE ENTERED ON NEW LINE             
         LA    R2,LINCODEH                                                      
         B     ENODCD              IF CHANGING NEED CODE                        
*                                                                               
VL10     DS    0H                                                               
         LA    R6,BIGKEY                                                        
         USING PAYRECD,R6                                                       
         MVC   PAYKEY,SPACES                                                    
         MVI   PAYKTYP,PAYKTYPQ    PAYROLL CODE RECORD TYPE                     
         MVI   PAYKSUB,PAYKSUBQ    PAYROLL CODE SUB TYPE                        
         MVC   PAYKCPY,CMPY        COMPANY                                      
         MVC   PAYKSEQ,SEQNUM      SEQUENCE NUMBER                              
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   VL20                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         MVI   RDUPDATE,C'N'                                                    
         OI    BITS,YESXIST        RECORD ALREADY EXISTS                        
         BAS   RE,GTNXTNUM         GET NEXT AVAILABLE NUMBER                    
         B     VL25                                                             
*                                                                               
*        ADD NEW RECORD                                                         
*                                                                               
VL20     DS    0H                  BUILD KEY                                    
         L     RE,AIO              CLEAR I/O                                    
         L     RF,SIZEIO                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R6,AIO                                                           
         MVC   0(42,R6),KEYSAVE               MOVE IN KEY                       
         MVC   PAYRLEN,=AL2(PAYRFST-PAYKEY)   LENGTH                            
*                                                                               
VL25     DS    0H                                                               
         L     R6,AIO                                                           
*                                                                               
         CLI   LINNUMH+5,0         NEW LINE?                                    
         BNE   VL30                IF NOT THEN CHANGE ELEMENT                   
*                                                                               
*        NEW ELEMENT                                                            
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PAYELD,R6                                                        
         MVI   PAYEL,PAYELQ                                                     
         MVI   PAYLN,PAYLNQ                                                     
         MVC   PAYNUM,NEXTNUM                                                   
         OI    BITS,NEWELEM        SO KNOW TO ADD                               
         B     VL40                THEN GO VALIDATE FIELDS                      
*                                                                               
*        FIND CHANGED ELEMENT                                                   
*                                                                               
VL30     ZIC   R1,LINNUMH+5        GET CURRENT BINARY NUMBER FROM LINE          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,LINNUM(0)                                                    
         CVB   R1,DUB                                                           
         STC   R1,CURRNUM                                                       
*                                                                               
         MVI   ELCODE,PAYELQ                                                    
         USING PAYELD,R6                                                        
         BAS   RE,GETEL                                                         
         BE    VL32                                                             
         B     VL34                                                             
*                                                                               
VL30NX   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   VL34                                                             
*                                                                               
VL32     DS    0H                                                               
         CLC   CURRNUM,PAYNUM      FIND SAME NUMBER                             
         BNE   VL30NX                                                           
         B     VL40                                                             
*                                                                               
VL34     DS    0H                  CHECK FOR NEXT SEQ RECORD                    
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
         NI    BITS,X'FF'-YESXIST                                               
         B     VL10                                                             
*                                                                               
*        VALIDATE FIELDS FOR CODE ELEMENT                                       
*                                                                               
VL40     MVC   WORK(5),LINCODE        HAS CODE CHANGED                          
         OC    WORK(5),SPACES                                                   
         CLC   PAYCODE,WORK                                                     
         BE    VL42                                                             
         CLC   PAYCODE,SPACES                                                   
         BNH   VL41                                                             
*                                                                               
         MVC   CKCODE,PAYCODE      MAKE SURE CHANGED CODE ISN'T                 
         LA    R2,LINCODEH                                                      
         BAS   RE,ANYREV           USED SOMEWHERE AS A REVERSAL                 
         BNE   ECDREV                                                           
*                                                                               
VL41     LA    R2,LINCODEH                                                      
         CLI   5(R2),5             CODE MUST HAVE LENGTH OF <5                  
         BH    ETOOLONG                                                         
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         MVC   PAYCODE,LINCODE                                                  
         OC    PAYCODE,SPACES                                                   
VL42     OI    4(R2),X'20'         VALIDATED                                    
*                                                                               
         LA    R2,LINDESCH         DESCRIPTION                                  
         CLI   5(R2),0                                                          
         BE    ERRMISS                                                          
         MVC   PAYDESC,LINDESC                                                  
         OC    PAYDESC,SPACES                                                   
         OI    4(R2),X'20'         VALIDATED                                    
*                                                                               
         NI    STAT,X'FF'-(STATREV+STATPC+STATHRTE+STATADJ)                     
         XC    PAYREV,PAYREV                                                    
         XC    PAYPC1,PAYPC1                                                    
         XC    PAYPC2,PAYPC2                                                    
         NI    PAYSTAT,X'FF'-PAYSHRTE-PAYADJRT                                  
         LA    R2,LINATTRH         ATTRIBUTES                                   
         CLI   5(R2),0                                                          
         BE    VL50                NO ATTRIBUTES                                
*                                                                               
         XC    SCANBLK,SCANBLK                                                  
         GOTO1 SCANNER,DMCB,(R2),(4,SCANBLK)                                    
         CLI   DMCB+4,0                                                         
         BNE   *+8                                                              
         B     ERRINV                                                           
*                                                                               
         LA    R4,SCANBLK                                                       
VL43     CLC   AC@REVU(3),12(R4)                                                
         BE    VL44                                                             
         CLC   AC@PCU(2),12(R4)                                                 
         BE    VL45                                                             
         CLC   AC@HRATE,12(R4)     HRATE                                        
         BE    VL46                                                             
         CLC   AC@YTDAD,12(R4)       ADJ                                        
         BE    VL47                                                             
         B     ERRINV                                                           
*                                                                               
VL44     TM    BITS,YESXIST        IF RECORD DOESN'T EXIST                      
         BNO   ENOREV              THEN REVERSAL CAN'T                          
         MVC   REVCODE,22(R4)                                                   
         OC    REVCODE,SPACES                                                   
         BAS   RE,VALREV           ROUTINE TO VALIDATE REVERSAL                 
         BNE   ENOREV                                                           
         TM    STAT,STATPC+STATHRTE+STATADJ    REVERSAL MUST BE THE             
         BNZ   ERRINV                          ONLY ATTRIBUTE                   
         OI    STAT,STATREV                                                     
         MVC   PAYREV,REVCODE                                                   
         B     VL49                                                             
*                                                                               
VL45     MVC   CKCODE,22(R4)                                                    
         OC    CKCODE,SPACES                                                    
         BAS   RE,GETNUM           ROUTINE TO GET NUMBER FOR CODE               
         BNE   ERRINV                                                           
         TM    STAT,STATREV+STATADJ   CAN'T COMBINE W/ REV OR ADJ               
         BNZ   ERRINV                                                           
         OI    STAT,STATPC                                                      
         CLC   =C'1',14(R4)                                                     
         BNE   *+14                                                             
         MVC   PAYPC1,CKNUM                                                     
         B     VL49                                                             
         CLC   =C'2',14(R4)                                                     
         BNE   *+14                                                             
         MVC   PAYPC2,CKNUM                                                     
         B     VL49                                                             
         B     ERRINV                                                           
*                                                                               
VL46     TM    STAT,STATREV+STATADJ   CAN'T HAVE HRATE IF REV OR ADJ            
         BNZ   ERRINV                                                           
         OI    STAT,STATHRTE                                                    
         OI    PAYSTAT,PAYSHRTE    HOURLY RATE PAYCODE                          
         B     VL49                                                             
*                                                                               
VL47     TM    STAT,STATPC+STATHRTE+STATREV    ADJ MUST BE THE                  
         BNZ   ERRINV                          ONLY ATTRIBUTE                   
         OI    STAT,STATADJ                                                     
         OI    PAYSTAT,PAYADJRT    YEAR END ADJUSTMENT PAYCODE                  
*                                                                               
VL49     LA    R4,32(R4)                                                        
         CLI   0(R4),X'00'                                                      
         BNE   VL43                                                             
         OI    4(R2),X'20'         VALIDATED                                    
*                                                                               
*        WRITE OR ADD RECORD AND ELEMENT                                        
*                                                                               
VL50     DS    0H                                                               
         TM    BITS,NEWELEM                                                     
         BZ    VL52                                                             
         MVI   ERROPT,C'Y'         PASS BACK ERROR                              
         MVI   ERROR,0                                                          
         GOTO1 ADDELEM                                                          
         MVI   ERROPT,C'N'         RESET                                        
         CLI   ERROR,0                                                          
         BE    VL52                                                             
         CLI   ERROR,TOOLONG       IF REC TOO LONG MAKE NEW SEQ                 
         BNE   VL52                                                             
*                                                                               
* THIS CODE IS NOW UK ONLY (AS OF 5/31/01) BECAUSE THE US VERSION OF            
* HELLO NO LONGER ADDS THE ELEMENT IF IT WILL NOT FIT.                          
*                                                                               
*&&UK                                                                           
         L     R6,AIO              ADDELEM STILL ADDS ELEM IF TOO LONG          
         MVI   ELCODE,PAYELQ       SO REMOVE LAST ELEMENT                       
         BAS   RE,GETEL                                                         
VL51NX   BAS   RE,NEXTEL                                                        
         BNE   VL51                                                             
         ST    R6,SVADDR2                                                       
         B     VL51NX                                                           
*                                                                               
VL51     L     R6,SVADDR2          ADDRESS OF JUST ADDED ELEM                   
         MVI   0(R6),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         GOTO1 PUTREC                                                           
         GOTO1 WRITE               WRITE BACK OLD SEQ RECORD                    
*&&                                                                             
         L     R6,AIO                                                           
         USING PAYRECD,R6                                                       
*                                                                               
         ZIC   R1,PAYKSEQ          GET NEW SEQ NUMBER                           
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM           MAKE NEW RECORD WITH NEXT SEQ NUM            
         NI    BITS,X'FF'-YESXIST                                               
         B     VL10                                                             
*                                                                               
VL52     TM    BITS,YESXIST        DOES RECORD ALREADY EXIST/                   
         BZ    VL54                                                             
*                                                                               
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
         BE    VLX                                                              
         DC    H'0'                                                             
*                                                                               
VL54     DS    0H                                                               
         GOTO1 ADDREC                                                           
         BE    VLX                                                              
         DC    H'0'                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*----------------------------------------------------------------------         
*        UPDATE METHOD RECORD '3E01' IF METHOD IS SPECIFIED                     
*----------------------------------------------------------------------         
*                                                                               
VL100    DS    0H                                                               
         LA    R6,BIGKEY                                                        
         USING CAHRECD,R6                                                       
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    PAYROLL CODE RECORD TYPE                     
         MVI   CAHKSUB,CAHKSUBQ    PAYROLL CODE SUB TYPE                        
         MVC   CAHKCPY,CMPY        COMPANY                                      
         MVC   CAHKMTHD,METHNUM                                                 
         XC    CAHKOFC,CAHKOFC                                                  
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                BETTER BE THERE                              
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
*        LOOK FOR EXISTING ELEMENT                                              
*                                                                               
         ZIC   R1,LINNUMH+5        GET CURRENT BINARY NUMBER FROM LINE          
         BCTR  R1,0                IF NOT NEW                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,LINNUM(0)                                                    
         CVB   R1,DUB                                                           
         STC   R1,CURRNUM                                                       
*                                                                               
VL105    MVI   ELCODE,PATELQ                                                    
         USING PATELD,R6                                                        
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   VL120               NEED TO ADD ELEMENT                          
         B     VL110                                                            
*                                                                               
VL110NX  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   VL120                                                            
*                                                                               
VL110    DS    0H                                                               
         CLC   CURRNUM,PATNUM      FIND SAME NUMBER                             
         BNE   VL110NX                                                          
         B     VL130               FOUND MATCH                                  
*                                                                               
*        NEW ELEMENT                                                            
*                                                                               
VL120    DS    0H                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PATELD,R6                                                        
         MVI   PATEL,PATELQ                                                     
         MVI   PATLN,PATLNQ                                                     
         MVC   PATNUM,CURRNUM                                                   
         OI    BITS,NEWELEM        SO KNOW TO ADD                               
         B     VL130               THEN GO VALIDATE FIELDS                      
*                                                                               
*        VALIDATE FIELDS FOR TYPE ELEMENT                                       
*                                                                               
VL130    LA    R2,LINTYPEH                                                      
         TM    4(R2),X'20'         ALREADY VALIDATED?                           
         BO    VL150                                                            
         CLI   5(R2),0                                                          
         BE    VL139               NO ASSIGNED TYPE                             
*                                                                               
         CLI   5(R2),3             CODE MUST HAVE LENGTH OF 3                   
         BL    ETOOSHRT                                                         
*                                                                               
         LA    R5,TYPETAB          WHAT IS THE TYPE                             
         USING TYPED,R5                                                         
*                                                                               
VL134LP  LA    R1,DICLIST                                                       
         ZICM  R0,TYDISNME,2       DISP TO DICTIONARY WORD                      
         AR    R1,R0                                                            
         CLC   LINTYPE,0(R1)                                                    
         BE    VL138                                                            
         LA    R5,TYPELN(R5)       NEXT ENTRY                                   
         CLI   0(R5),X'00'                                                      
         BE    EINVTYPE                                                         
         B     VL134LP                                                          
*                                                                               
VL138    DS    0H                                                               
         MVC   PATTYPE,TYTYPE                                                   
         B     VL150                                                            
*                                                                               
VL139    DS    0H                                                               
         XC    PATTYPE,PATTYPE                                                  
         B     VL150                                                            
         DROP  R5                                                               
*                                                                               
*        WRITE RECORD AND ELEMENT                                               
*                                                                               
VL150    DS    0H                                                               
         TM    BITS,NEWELEM                                                     
         BZ    VL152                                                            
         GOTO1 ADDELEM                                                          
*                                                                               
VL152    DS    0H                                                               
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
         BE    VLX                                                              
         DC    H'0'                                                             
*                                                                               
VLX      B     XIT                                                              
         XC    BIGKEY,BIGKEY                                                    
         DROP  R6,R3                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OPTIONS                                                       
***********************************************************************         
*                                                                               
VALOPTS  NTR1                                                                   
         XC    TYPEFILT,TYPEFILT                                                
         NI    BITS,X'FF'-(UPDATEON+ALLTYPES+NOTYPES)                           
         LA    R2,CONOPTH                                                       
         CLI   5(R2),0                                                          
         BE    OPTX                                                             
*                                                                               
         XC    SCANBLK,SCANBLK                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,SCANBLK)                                    
         CLI   DMCB+4,0                                                         
         BNE   *+8                                                              
         B     EINVOPT                                                          
         LA    R3,SCANBLK                                                       
OPT10    CLC   AC@TYPEU,12(R3)                                                  
         BE    OPT20                                                            
         CLC   AC@UPDU,12(R3)                                                   
         BNE   EINVOPT                                                          
*&&US                                                                           
* CHECK TO MAKE SURE USER HAS ACCESS TO DO UPDATE                               
         TM    FACFLAG,XIROSYS+XIROMODE+XIWRONGF                                
         BZ    OPT15                                                            
         TM    FACFLAG,XIROMODE    CONNECTED IN READ ONLY MODE?                 
         BO    ERRNAUPD            YES                                          
         TM    FACFLAG,XIWRONGF    CONNECTED TO WRONG FACPAK?                   
         BO    ERRHMADV            YES                                          
         B     ERRNADVU            CONNECTED TO READ ONLY SYSTEM                
*                                                                               
*&&                                                                             
OPT15    BAS   RE,OPTSEC           CHECK IF AUTH. FOR ACTION UPDATE             
         BNE   ERRNOUP                                                          
         OI    BITS,UPDATEON                                                    
         B     OPT50                                                            
*                                                                               
OPT20    CLC   AC@ALLU,22(R3)      SHOW ALL TYPES (OMIT UNASSIGNED)             
         BNE   *+12                                                             
         OI    BITS,ALLTYPES                                                    
         B     OPT50                                                            
*                                                                               
         CLC   AC@UNASU(4),22(R3)     ONLY SHOW UNASSIGNED TYPES                
         BNE   *+12                                                             
         OI    BITS,NOTYPES                                                     
         B     OPT50                                                            
*                                                                               
         LA    R5,TYPETAB          WHAT IS THE TYPE                             
         USING TYPED,R5                                                         
*                                                                               
OPT30    LA    R1,DICLIST                                                       
         ZICM  R0,TYDISNME,2       DISP TO DICTIONARY WORD                      
         AR    R1,R0                                                            
         CLC   0(3,R1),22(R3)      MATCH ON TYPE                                
         BE    OPT40                                                            
         LA    R5,TYPELN(R5)       NEXT ENTRY                                   
         CLI   0(R5),X'00'                                                      
         BE    EINVOPT                                                          
         B     OPT30                                                            
*                                                                               
OPT40    MVC   TYPEFILT,TYTYPE     TYPE FILTER                                  
OPT50    LA    R3,32(R3)                                                        
         CLI   0(R3),X'00'                                                      
         BNE   OPT10                                                            
*                                                                               
OPTX     B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK FIELD SECURITY TO SEE WHETHER TO DISPLAY PERSON NAME             
***********************************************************************         
*                                                                               
OPTSEC   NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         MVI   SBYTE,OPTUPDQ                                                    
         LA    R2,SBYTE                                                         
         GOTO1 SECRET,DMCB,('SECPOPTP',ASECBLK),(R2)                            
         CLI   DMCB,SECPYES                                                     
         BE    XYES                                                             
         B     XNO                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO FIND THE NEXT AVAILABLE NUMBER WHEN ADDING                  
*        A NEW PAYROLL CODE ELEMENT AND PUTS IT IN 'NEXTNUM'                    
***********************************************************************         
*                                                                               
GTNXTNUM NTR1                                                                   
         MVI   NEXTNUM,1                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PAYELQ                                                    
         USING PAYELD,R6                                                        
         BAS   RE,GETEL                                                         
         BE    NN10                                                             
         B     NNX                                                              
*                                                                               
NN10NX   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   NNX                                                              
*                                                                               
NN10     DS    0H                                                               
         ZIC   R1,PAYNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,NEXTNUM                                                       
         B     NN10NX                                                           
*                                                                               
NNX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THAT CODE HAS NOT ALREADY BEEN USED                
***********************************************************************         
*                                                                               
VALCODES NTR1                                                                   
         L     R0,ADISPBLK         PAYROLL CODE TABLE                           
         LA    R1,CTBLKLN                                                       
         CLI   TWAOFFC,C'*'                                                     
         BNE   VC02                                                             
         CLC   CONOTH(3),=C'DDS'                                                
         BNE   VC02                                                             
         LA    R1,CTBLKLNX                                                      
*                                                                               
VC02     AR    R0,R1                                                            
         ST    R0,CTBLKEND         SAVE END OF TABLE                            
*                                                                               
         L     R0,ADISPBLK         PAYROLL CODE TABLE                           
         LR    RE,R0               CLEAR IT                                     
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING CODETABD,R3                                                      
         L     R3,ADISPBLK         PAYROLL CODE TABLE                           
         MVI   SEQNUM,X'00'                                                     
*                                                                               
VC10     LA    R6,BIGKEY                                                        
         USING PAYRECD,R6                                                       
         MVC   PAYKEY,SPACES                                                    
         MVI   PAYKTYP,PAYKTYPQ    PAYROLL CODE RECORD TYPE                     
         MVI   PAYKSUB,PAYKSUBQ    PAYROLL CODE SUB TYPE                        
         MVC   PAYKCPY,CMPY        COMPANY                                      
         MVC   PAYKSEQ,SEQNUM      SEQUENCE NUMBER                              
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   VC50                                                             
         B     VC20                                                             
*                                                                               
VC15     DS    0H                                                               
         GOTO1 SEQ                                                              
         CLC   BIGKEY(3),KEYSAVE   SAME COMPANY                                 
         BNE   VC50                                                             
*                                                                               
VC20     GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,PAYELQ                                                    
         USING PAYELD,R6                                                        
         BAS   RE,GETEL                                                         
         B     VC30                                                             
VC20NX   BAS   RE,NEXTEL                                                        
VC30     BNE   VC15                                                             
*                                                                               
         MVC   CTNUM,PAYNUM                                                     
         MVC   CTCODE,PAYCODE                                                   
         MVC   CTREV,PAYREV                                                     
         MVC   CTPC1,PAYPC1                                                     
         MVC   CTPC2,PAYPC2                                                     
         MVC   CTSTAT,PAYSTAT                                                   
*                                                                               
         LA    R3,CTLEN(R3)                                                     
         L     R1,CTBLKEND         END OF BLOCK                                 
         CR    R3,R1                                                            
         BL    VC20NX                                                           
         B     ETOOBIG             REC TOO BIG ERROR =                          
         DC    H'0'                BLOCK IS FULL - INCREASE CTBLKLN             
*                                                                               
*        REPLACE OLD CODES WITH NEW ONES FROM SCREEN                            
*                                                                               
         USING LIND,R4                                                          
VC50     LA    R4,PAYFRSTH                                                      
*                                                                               
VC55     CLI   LINNUMH+5,0         NO LINE NUM = ADDING NEW CODES               
         BE    VC70                                                             
         CLI   LINCODEH+4,X'20'    ALREADY VALIDATED                            
         BO    VC60                SKIP TO NEXT LINE                            
*                                                                               
         ZIC   R1,LINNUMH+5        GET CURRENT BINARY NUMBER FROM LINE          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,LINNUM(0)                                                    
         CVB   R1,DUB                                                           
         L     R3,ADISPBLK         PAYROLL CODE TABLE                           
         B     VC58                                                             
*                                                                               
VC56     LA    R3,CTLEN(R3)                                                     
         L     R2,CTBLKEND         END OF BLOCK                                 
         CR    R3,R2                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
VC58     CLM   R1,1,CTNUM                                                       
         BNE   VC56                                                             
         OC    LINCODE,SPACES      REPLACE WITH NEW CODE                        
         MVC   CTCODE,LINCODE                                                   
*                                                                               
VC60     LA    R4,LINNEXTL         NEXT LINE                                    
         LA    R1,PAYLSTH                                                       
         CR    R4,R1                                                            
         BH    VC100                                                            
         B     VC55                                                             
*                                                                               
*        ADD NEW CODES FROM SCREEN                                              
*                                                                               
VC70     DS    0H                                                               
         CLC   LINCODE,SPACES                                                   
         BNH   VC74                                                             
*                                                                               
         L     R3,ADISPBLK         PAYROLL CODE TABLE                           
VC72     LA    R3,CTLEN(R3)                                                     
         L     R2,CTBLKEND         END OF BLOCK                                 
         CR    R3,R2                                                            
         BL    *+6                                                              
         DC    H'0'                                                             
         OC    0(CTLEN,R3),0(R3)   END OF LIST                                  
         BNZ   VC72                                                             
*                                                                               
         MVC   CTCODE,LINCODE                                                   
         OC    CTCODE,SPACES                                                    
*                                                                               
VC74     LA    R4,LINNEXTL         NEXT LINE                                    
         LA    R1,PAYLSTH                                                       
         CR    R4,R1                                                            
         BH    VC100                                                            
         CLI   LINCODEH+5,0                                                     
         BE    VC100                                                            
         B     VC72                                                             
         DROP  R4,R3                                                            
*                                                                               
*        NOW CHECK COMPLETE NEW TABLE FOR DUPS                                  
*                                                                               
BMP      USING CODETABD,R4     R4 BUMPS THROUGH TABLE                           
PNT      USING CODETABD,R3     R3 POINTS TO CODE BEING CHECKED                  
VC100    L     R3,ADISPBLK         PAYROLL CODE TABLE                           
VC103    LA    R4,CTLEN(R3)                                                     
VC104    CLC   PNT.CTCODE,BMP.CTCODE        DUPLICATE CODE                      
         BNE   VC105                                                            
         MVC   DUPCODE,PNT.CTCODE                                               
         B     VCXNO                                                            
*                                                                               
VC105    LA    R4,CTLEN(R4)        BUMP R4                                      
         L     R2,CTBLKEND         END OF BLOCK                                 
         CR    R4,R2                                                            
         BNL   VC110                                                            
         OC    0(CTLEN,R4),0(R4)   END OF LIST                                  
         BNZ   VC104               CHECK NEXT CODE                              
*                                                                               
VC110    LA    R3,CTLEN(R3)        POINT TO NEXT CODE                           
         L     R2,CTBLKEND         END OF BLOCK                                 
         CR    R3,R2                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         OC    0(CTLEN,R3),0(R3)   END OF LIST                                  
         BZ    VCXYES              EVERYTHING IS OK                             
         B     VC103                                                            
*                                                                               
VCXYES   B     XYES                                                             
VCXNO    B     XNO                                                              
         DROP  R6,BMP,PNT                                                       
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE A REVERSAL ATTRIBUTE                               
*        THE REVERSAL CODE MUST ALREADY EXIST IN THE TABLE                      
***********************************************************************         
*                                                                               
VALREV   NTR1                                                                   
         USING CODETABD,R3                                                      
         L     R3,ADISPBLK         POINTER TO CODE LIST                         
*                                                                               
VR05     CLC   CTCODE,REVCODE                                                   
         BNE   VR10                                                             
         TM    CTSTAT,PAYSHRTE     REVERSAL CAN'T BE AN HOURLY RATE             
         BO    XNO                                                              
         B     XYES                                                             
*                                                                               
VR10     LA    R3,CTLEN(R3)                                                     
         L     R2,CTBLKEND         END OF BLOCK                                 
         CR    R3,R2                                                            
         BH    XNO                                                              
         CLI   0(R3),X'00'         END OF LIST                                  
         BNE   VR05                CHECK NEXT CODE                              
         B     XNO                                                              
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        MAKES SURE CHANGED PAY CODE ISN'T A REVERSAL SOMEWHERE                 
***********************************************************************         
*                                                                               
ANYREV   NTR1                                                                   
         USING CODETABD,R3                                                      
         L     R3,ADISPBLK         POINTER TO CODE LIST                         
*                                                                               
AR05     CLC   CTREV,CKCODE                                                     
         BE    XNO                                                              
         LA    R3,CTLEN(R3)                                                     
         L     R2,CTBLKEND         END OF BLOCK                                 
         CR    R3,R2                                                            
         BH    XYES                                                             
         CLI   0(R3),X'00'         END OF LIST                                  
         BNE   AR05                CHECK NEXT CODE                              
         B     XYES                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        GETS PAYROLL CODE GIVEN NUMBER IN CKNUM PUT IN CKCODE                  
***********************************************************************         
*                                                                               
GETCODE  NTR1                                                                   
         USING CODETABD,R3                                                      
         L     R3,ADISPBLK         POINTER TO CODE LIST                         
         XC    CKCODE,CKCODE                                                    
GC05     CLC   CTNUM,CKNUM                                                      
         BE    GCX                                                              
         LA    R3,CTLEN(R3)                                                     
         L     R2,CTBLKEND         END OF BLOCK                                 
         CR    R3,R2                                                            
         BH    XNO                                                              
         CLI   0(R3),X'00'         END OF LIST                                  
         BNE   GC05                CHECK NEXT CODE                              
         B     XNO                                                              
GCX      MVC   CKCODE,CTCODE                                                    
         B     XYES                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        GETS PAYROLL NUMBER GIVEN CODE IN CKCODE PUTS IN CKNUM                 
***********************************************************************         
*                                                                               
GETNUM   NTR1                                                                   
         USING CODETABD,R3                                                      
         L     R3,ADISPBLK         POINTER TO CODE LIST                         
         XC    CKNUM,CKNUM                                                      
GN05     CLC   CTCODE,CKCODE                                                    
         BE    GNX                                                              
         LA    R3,CTLEN(R3)                                                     
         L     R2,CTBLKEND         END OF BLOCK                                 
         CR    R3,R2                                                            
         BH    XNO                                                              
         CLI   0(R3),X'00'         END OF LIST                                  
         BNE   GN05                CHECK NEXT CODE                              
         B     XNO                                                              
GNX      MVC   CKNUM,CTNUM                                                      
         B     XYES                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*        ON-SCREEN LIST                                                         
***********************************************************************         
*                                                                               
LR       DS    0H                                                               
         BAS   RE,VALOPTS                                                       
         LA    R1,PAYMETHH         FORCE CURSOR TO METHOD FIELD                 
         ST    R1,ACURFORC                                                      
*                                                                               
         MVI   STAT,0                                                           
         MVI   SEQNUM,0            START WITH FIRST REC                         
*                                                                               
         MVI   NLISTS,14           NUMBER OF LIST LINES                         
*                                                                               
         OI    PAYFRSTH+6,X'20'    PROTECT FIRST NUMBER FIELD                   
         TM    BITS,YESMETH        WAS METHOD SPECIFIED?                        
         BO    LR04                                                             
*                                                                               
         ZIC   R3,NLISTS           NO, THEN PROTECT TYPE COLUMN                 
         LA    R1,PAYFRSTH         ADDRESS OF LIST LINE                         
         USING LIND,R1                                                          
LR02LP   OI    LINTYPEH+6,X'20'    YES THEN PROTECT FIELD                       
         MVC   LINTYPE,SPACES      AND CLEAR TYPE                               
         OI    LINTYPEH+6,X'80'                                                 
         AH    R1,LLIST                                                         
         BCT   R3,LR02LP                                                        
         B     LR05                                                             
*                                                                               
LR04     DS    0H                  WITH METHOD PROTECT ALL BUT TYPE             
         ZIC   R3,NLISTS                                                        
         LA    R1,PAYFRSTH         ADDRESS OF LIST LINE                         
LR04LP   OI    LINCODEH+6,X'20'    YES THEN PROTECT FIELD                       
         OI    LINDESCH+6,X'20'                                                 
         OI    LINATTRH+6,X'20'                                                 
         AH    R1,LLIST                                                         
         BCT   R3,LR04LP                                                        
         DROP  R1                                                               
*                                                                               
LR05     LA    R1,PAYFRSTH         VALIDATE EVERY FIELD                         
         LA    R2,PAYLSTH                                                       
         USING LIND,R2                                                          
         LA    RF,LINATTRH         VERY LAST FIELD IN LIST                      
         DROP  R2                                                               
LR10LP   ZIC   RE,0(R1)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R1),X'02'         EXTENDED FIELD HEADER?                       
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         LTR   RE,RE                                                            
         BM    LR15                                                             
         OI    6(R1),X'80'         TRANSMIT                                     
         OI    4(R1),X'20'         VALIDATE                                     
         ZIC   RE,0(R1)                                                         
         BXLE  R1,RE,LR10LP                                                     
*                                                                               
LR15     DS    0H                                                               
         TM    BITS,YESMETH        METHOD SPECIFIED                             
         BNO   LR18                                                             
         LA    R6,BIGKEY           READ METHOD RECORD                           
         USING CAHRECD,R6                                                       
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    ALLOCATION METHOD RECORD TYPE                
         MVI   CAHKSUB,CAHKSUBQ    METHOD BY CODE SUB TYPE                      
         MVC   CAHKCPY,CMPY        COMPANY                                      
         MVC   CAHKMTHD,METHNUM    METHOD CODE                                  
         XC    CAHKOFC,CAHKOFC                                                  
         MVC   AIO,AIO3            METHOD RECORD IN AIO3                        
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         B     LR18                                                             
*                                                                               
LR17     DS    0H                  CHECK FOR NEXT SEQ NUM RECORD                
         ZIC   R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
*                                                                               
LR18     DS    0H                                                               
         LA    R6,BIGKEY           GET PAYROLL RECORD                           
         USING PAYRECD,R6                                                       
         MVC   PAYKEY,SPACES                                                    
         MVI   PAYKTYP,PAYKTYPQ    PAYROLL CODE RECORD TYPE                     
         MVI   PAYKSUB,PAYKSUBQ    PAYROLL CODE SUB TYPE                        
         MVC   PAYKCPY,CMPY        COMPANY                                      
         MVC   PAYKSEQ,SEQNUM                                                   
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   LRX                                                              
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,GTNXTNUM         GET LAST NUM FOR PF TO BOTTOM                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PAYELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   LR17                                                             
*                                                                               
LR20     DS    0H                                                               
         USING PAYELD,R6                                                        
         MVI   STRTNUM,1           DEFAULT TO BEGINNING                         
         CLC   METHCODE,SVMETHCD   IF METHOD CHANGED                            
         BNE   LR30                THEN DISPLAY FROM TOP                        
         CLC   TYPEFILT,SVTYFILT   IF TYPEFILT CHANGED                          
         BNE   LR30                THEN DISPLAY FROM TOP                        
         CLI   LISTSW,C'T'         DISP SAME PAGE BECAUSE OF CHANGE?            
         BNE   LR22                NO                                           
         CLI   PAYFRSTH+5,0                                                     
         BE    LR22                                                             
         ZIC   R1,PAYFRSTH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,PAYFRST(0)                                                   
         CVB   R1,DUB                                                           
         STC   R1,STRTNUM                                                       
         B     LR30                                                             
*                                                                               
LR22     CLI   PFKEY,7             UP                                           
         BNE   LR24                                                             
         CLI   PAYFRSTH+5,0                                                     
         BE    LR30                                                             
         ZIC   R1,PAYFRSTH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,PAYFRST(0)                                                   
         CVB   R1,DUB                                                           
         CH    R1,=H'1'                                                         
         BE    LR30                ALREADY AT FIRST PAGE                        
         ZIC   R0,NLISTS           SUBTRACT A PAGE                              
         SR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         CH    R1,=H'0'                                                         
         BNH   LR30                                                             
         STC   R1,STRTNUM                                                       
         B     LR30                                                             
*                                                                               
LR24     CLI   PFKEY,8             DOWN                                         
         BE    LR25                                                             
         CLI   PFKEY,0             DOWN - FOR ENTER                             
         BNE   LR26                                                             
LR25     CLI   PAYLSTH+5,0                                                      
         BE    LR30                                                             
         ZIC   R1,PAYLSTH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,PAYLST(0)                                                    
         CVB   R1,DUB                                                           
         STC   R1,STRTNUM                                                       
         B     LR30                                                             
*                                                                               
LR26     CLI   PFKEY,5             BOTTOM                                       
         BNE   LR30                                                             
         ZIC   R1,NEXTNUM                                                       
         SH    R1,=H'3'            BACK UP A LITTLE                             
         STC   R1,STRTNUM                                                       
         B     LR30                                                             
*                                                                               
LR30NX   DS    0H                                                               
         MVI   ELCODE,PAYELQ                                                    
         BAS   RE,NEXTEL                                                        
         BNE   LR17                CHECK FOR NEXT SEQ NUMBER                    
*                                                                               
LR30     DS    0H                                                               
         CLC   PAYNUM,STRTNUM                                                   
         BL    LR30NX                                                           
*                                                                               
LR40     DS    0H                                                               
*                                                                               
         MVC   SVMETHCD,METHCODE   SAVE METHOD                                  
         MVC   SVTYFILT,TYPEFILT   SAVE TYPE FILTER                             
         L     R4,ATHISLST         ADDRESS OF LIST LINE                         
         USING LIND,R4                                                          
*                                                                               
         ST    R6,SVADDR           SAVE R6 FOR NEXTEL                           
         MVC   CURRNUM,PAYNUM                 SAVE CURRENT NUMBER               
         EDIT  (B1,PAYNUM),(3,LINNUM),ALIGN=LEFT                                
         STC   R0,LINNUMH+5                   LENGTH                            
*                                                                               
         MVC   LINCODE,PAYCODE                                                  
         TM    BITS,YESMETH        WAS METHOD SPECIFIED?                        
         BZ    *+8                                                              
         OI    LINCODEH+6,X'20'    YES THEN PROTECT FIELD                       
         TM    BITS,UPDATEON       UPDATE OPTION ON?                            
         BO    *+8                                                              
         OI    LINCODEH+6,X'20'    NO THEN PROTECT FIELD                        
*                                                                               
         MVC   LINDESC,PAYDESC                                                  
         TM    BITS,YESMETH        WAS METHOD SPECIFIED?                        
         BZ    *+8                                                              
         OI    LINDESCH+6,X'20'    YES THEN PROTECT FIELD                       
         TM    BITS,UPDATEON       UPDATE OPTION ON?                            
         BO    *+8                                                              
         OI    LINDESCH+6,X'20'    NO THEN PROTECT FIELD                        
*                                                                               
         TM    BITS,YESMETH        WAS METHOD SPECIFIED?                        
         BZ    *+8                                                              
         OI    LINATTRH+6,X'20'    YES THEN PROTECT FIELD                       
         TM    BITS,UPDATEON       UPDATE OPTION ON?                            
         BO    *+8                                                              
         OI    LINATTRH+6,X'20'    NO THEN PROTECT FIELD                        
         MVC   LINATTR,SPACES                                                   
*                                                                               
         MVC   WORK(60),SPACES     BUILD LINE IN WORK                           
         LA    R3,WORK                                                          
         OC    PAYREV,PAYREV       IS THERE A REVERSAL                          
         BZ    LR42                                                             
         MVC   0(3,R3),AC@REVU                                                  
         MVI   3(R3),C'='                                                       
         MVC   4(5,R3),PAYREV                                                   
         LA    R3,12(R3)                                                        
LR42     OC    PAYPC1,PAYPC1       IS THERE A PC1                               
         BZ    LR43                                                             
         MVC   CKNUM,PAYPC1                                                     
         BAS   RE,GETCODE                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(2,R3),AC@PC                                                    
         MVC   2(2,R3),=C'1='                                                   
         MVC   4(5,R3),CKCODE                                                   
         LA    R3,12(R3)                                                        
LR43     OC    PAYPC2,PAYPC2       IS THERE A PC2                               
         BZ    LR44                                                             
         MVC   CKNUM,PAYPC2                                                     
         BAS   RE,GETCODE                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(2,R3),AC@PC                                                    
         MVC   2(2,R3),=C'2='                                                   
         MVC   4(5,R3),CKCODE                                                   
         LA    R3,12(R3)                                                        
LR44     TM    PAYSTAT,PAYSHRTE    HOURLY RATE PAYCODE                          
         BZ    LR46                                                             
         MVC   0(L'AC@HRATE,R3),AC@HRATE         HRATE                          
         LA    R3,8(R3)                                                         
LR46     TM    PAYSTAT,PAYADJRT    ADJ                                          
         BZ    LR48                                                             
         MVC   0(L'AC@YTDAD,R3),AC@YTDAD                                        
         LA    R3,9(R3)                                                         
LR48     GOTO1 SQUASHER,DMCB,WORK,(C',',60)                                     
         MVC   LINATTR,WORK                                                     
*                                                                               
LR50     DS    0H                  FIND CORRESPONDING TYPE ELEMENT              
         MVC   LINTYPE,SPACES      CLEAR                                        
         OI    LINTYPEH+6,X'80'    TRANSMIT                                     
         TM    BITS,YESMETH        METHOD SPECIFIED                             
         BO    LR55                IF NOT                                       
         OI    LINTYPEH+6,X'20'    THEN PROTECT TYPE                            
         B     LR100               AND DISPLAY LINE                             
*                                                                               
LR55     L     R6,AIO3                                                          
         MVI   ELCODE,PATELQ       PAYROLL TYPE ELEMENT                         
         USING PATELD,R6                                                        
         BAS   RE,GETEL                                                         
         B     LR61                                                             
*                                                                               
LR60NX   DS    0H                                                               
         BAS   RE,NEXTEL                                                        
LR61     BE    LR65                                                             
         TM    BITS,UPDATEON       UPDATE OPTION ON?                            
         BO    *+8                                                              
         OI    LINTYPEH+6,X'20'    NO THEN PROTECT FIELD                        
         TM    BITS,YESMETH        NO ASSINGMENT, IS METHOD SPECIFIED           
         BZ    LR105               IF NOT DON'T FILTER ON TYPE                  
         TM    BITS,ALLTYPES       DISPLAY ALL ASSIGNED                         
         BO    LR107               SKIP                                         
         TM    BITS,NOTYPES        DISPLAY ALL UNASSIGNED                       
         BO    LR105                                                            
         OC    TYPEFILT,TYPEFILT   DISPLAY IF NO TYPE FILTER                    
         BZ    LR105                                                            
         B     LR107                                                            
*                                                                               
LR65     DS    0H                                                               
         CLC   CURRNUM,PATNUM                                                   
         BNE   LR60NX                                                           
*                                                                               
LR70     DS    0H                                                               
         LA    R3,TYPETAB          WHAT IS THE TYPE                             
         USING TYPED,R3                                                         
*                                                                               
LR70LP   CLC   TYTYPE,PATTYPE                                                   
         BE    LR75                                                             
         LA    R3,TYPELN(R3)       NEXT ENTRY                                   
         CLI   0(R3),X'00'                                                      
         BE    LR100                                                            
         B     LR70LP                                                           
*                                                                               
LR75     DS    0H                                                               
         LA    R1,DICLIST                                                       
         ZICM  R0,TYDISNME,2       DISP TO DICTIONARY WORD                      
         AR    R1,R0                                                            
         MVC   LINTYPE,0(R1)                                                    
         TM    BITS,UPDATEON       UPDATE OPTION ON?                            
         BO    *+8                                                              
         OI    LINTYPEH+6,X'20'    NO THEN PROTECT FIELD                        
         B     LR100                                                            
*                                                                               
LR100    DS    0H                                                               
         TM    BITS,YESMETH        IS METHOD SPECIFIED                          
         BZ    LR105               IF NOT DON'T FILTER ON TYPE                  
         TM    BITS,ALLTYPES       DISPLAY ALL ASSIGNED                         
         BNO   LR101                                                            
         OC    PATTYPE,PATTYPE                                                  
         BZ    LR107                                                            
*                                                                               
LR101    TM    BITS,NOTYPES        DISPLAY ALL UNASSIGNED                       
         BNO   LR102                                                            
         OC    PATTYPE,PATTYPE                                                  
         BNZ   LR107                                                            
*                                                                               
LR102    OC    TYPEFILT,TYPEFILT   TYPE FILTER?                                 
         BZ    LR105                                                            
         CLC   TYPEFILT,PATTYPE    SAME TYPE                                    
         BNE   LR107                                                            
LR105    DS    0H                                                               
         GOTO1 LISTMON             WON'T COME BACK AT END OF SCREEN             
LR107    L     R6,SVADDR                                                        
         B     LR30NX                                                           
*                                                                               
LRX      DS    0H                                                               
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        RANDOM STUFF                                                           
***********************************************************************         
*                                  GENERAL MESSAGES                             
ERRMISS  MVI   GERROR1,MISSING                                                  
         B     ERRX                                                             
ERRINV   MVI   GERROR1,INVALID                                                  
         B     ERRX                                                             
*                                                                               
ERRX     DS    0H                                                               
         MVI   GMSYS,X'FF'                                                      
         GOTO1 MYERR                                                            
*                                    ACC MESSAGES                               
ERRNOUP  MVC   GERROR,=AL2(ACENOUPD)                                            
         B     ACCERRX                                                          
ECDREV   MVC   GERROR,=AL2(ACECDREV)                                            
         B     ACCERRX                                                          
ETOOSHRT MVC   GERROR,=AL2(ACELSHO)                                             
         B     ACCERRX                                                          
ETOOLONG MVC   GERROR,=AL2(ACELLONG)                                            
         B     ACCERRX                                                          
EINVOPT  MVC   GERROR,=AL2(ACEINVOP)                                            
         B     ACCERRX                                                          
EINVMET  MVC   GERROR,=AL2(ACEIVMET)                                            
         B     ACCERRX                                                          
ENOMETH  MVC   GERROR,=AL2(ACENOMET)                                            
         B     ACCERRX                                                          
ECDXSTS  MVC   GERROR,=AL2(ACECDXST)                                            
         B     ACCERRX                                                          
ENOREV   MVC   GERROR,=AL2(ACENOREV)                                            
         B     ACCERRX                                                          
EINVTYPE MVC   GERROR,=AL2(ACEIVTYP)                                            
         B     ACCERRX                                                          
ENODCD   MVC   GERROR,=AL2(ACECDDEL)                                            
         B     ACCERRX                                                          
ETOOBIG  MVC   GERROR,=AL2(ACREC2BG)                                            
         B     ACCERRX                                                          
*&&US                                                                           
ERRNAUPD MVC   GERROR,=AL2(ACENAUPD)                                            
         B     ACCERRX                                                          
ERRHMADV MVC   GERROR,=AL2(ACEHMADV)                                            
         MVI   GLTXT,L'FACUPD                                                   
         MVC   GATXT+1(L'FACUPD),FACUPD                                         
         B     ACCERRX                                                          
ERRNADVU MVC   GERROR,=AL2(ACENADVU)                                            
         B     ACCERRX                                                          
*&&                                                                             
*                                                                               
ACCERRX  DS    0H                                                               
         MVI   GMSYS,6             ACC SYSTEM MESSAGES                          
         GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PFKEY TABLE                                                  *         
***********************************************************************         
*                                                                               
LPFTABLE DS    0C                                                               
*                                                                               
* PERSON LIST                                                                   
*                                                                               
         DC    AL1(LPF01X-*,01,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#LIST,8                                                        
LPF01X   EQU   *                                                                
*                                                                               
* HISTORY LIST                                                                  
*                                                                               
         DC    AL1(LPF03X-*,03,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#HIST,8                                                        
         DCDD  AC#LIST,8                                                        
LPF03X   EQU   *                                                                
*                                                                               
* MAD UPDATE                                                                    
*                                                                               
         DC    AL1(LPF06X-*,06,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#MAD,8                                                         
         DCDD  AC#INP,8                                                         
LPF06X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
*                                                                               
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        TABLES                                                                 
***********************************************************************         
*                                                                               
TYPETAB  DC    AL1(PATTSAL),AL2(AC@SALU-DICLIST)                                
         DC    AL1(PATTBEN),AL2(AC@BENU-DICLIST)                                
         DC    AL1(PATTPEN),AL2(AC@PENU-DICLIST)                                
         DC    AL1(PATTIND),AL2(AC@INDU-DICLIST)                                
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        DATA DICTIONARY                                                        
***********************************************************************         
*                                                                               
* INPUT/OUTPUT FOR DICTATE CALL                                                 
*                                                                               
PAYLISTI DS    0C                  DATA DICTIONARY INPUT                        
         DCDDL AC#CYTDA,6          YTDADJ                                       
PAYLSTIX DC    X'00'                                                            
*                                                                               
PAYLISTU DS    0C                  DATA DICTIONARY OUTPUT                       
AC@YTDAD DS    CL6                                                              
PAYLSTUX EQU   *                                                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        INCLUDES                                                               
***********************************************************************         
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* FASECRETD                                                                     
* ACCAPWORKD                                                                    
* ACCAPDSECT                                                                    
* ACGENFILE                                                                     
* ACDDEQUS                                                                      
* FAXTRAINF                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*        SCREENS                                                                
***********************************************************************         
*                                                                               
       ++INCLUDE ACCAPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPFAD                                                       
         EJECT                                                                  
***********************************************************************         
*        REMAINING WORK AREA                                                    
***********************************************************************         
*                                                                               
         ORG   PAYWORK                                                          
RELO     DS    A                                                                
SVADDR   DS    F                   SAVES ADDRESS                                
SVADDR2  DS    F                   SAVES ADDRESS                                
CTBLKEND DS    F                   END OF PAYROLL CODE TABLE                    
BITS     DS    X                                                                
YESMETH  EQU   X'80'               YES METHOD WAS SPECIFIED                     
YESXIST  EQU   X'40'               YES PAYROLL CODE RECORD EXISTS               
NEWELEM  EQU   X'20'               NEW ELEMENT TO ADD                           
UPDATEON EQU   X'10'               ABLE TO CHANGE DISPLAYED ITEMS               
ALLTYPES EQU   X'08'               DISPLAY ALL TYPES (OMIT UNASSIGNED)          
NOTYPES  EQU   X'04'               ONLY DISPLAY UNASSIGNED                      
STAT     DS    XL1                                                              
STATDONE EQU   X'80'                                                            
STATREV  EQU   X'40'               REVERSAL ON LINE                             
STATPC   EQU   X'20'               PC ON LINE                                   
STATHRTE EQU   X'10'               HRATE ON LINE                                
STATADJ  EQU   X'08'               ADJ RATE ON LINE                             
*                                                                               
METHCODE DS    CL3                 METHOD CODE                                  
SVMETHCD DS    CL3                 SAVED METHOD CODE                            
SVTYFILT DS    XL1                                                              
TYPEFILT DS    XL1                 TYPE FILTER                                  
METHNUM  DS    CL1                 METHOD NUM (CHARACTER)                       
CURRNUM  DS    CL1                 CURRENT PAYROLL CODE NUMBER                  
NEXTNUM  DS    XL1                 NEXT AVAILABLE NUMBER FOR ADDING             
STRTNUM  DS    XL1                 NUMBER TO START DISPLAYING WITH              
SEQNUM   DS    XL1                 RECORD SEQUENCE NUMBER                       
REVCODE  DS    CL5                 REVERSAL CODE                                
DUPCODE  DS    CL5                 DUPLICATE CODE                               
CKCODE   DS    CL5                 TEMP CODE FIELD                              
CKNUM    DS    XL1                 TEMP NUMBER FIELD                            
SBYTE    DS    XL1                 FOR SECRET CALL                              
OPTUPDQ  EQU   2                   OPTION UPDATE NUMBER FOR SECRET              
SAVEKEY  DS    XL42                ACCFILE KEY                                  
MYKEY    DS    XL42                ACCFILE KEY                                  
SCANBLK  DS    XL100               SCANNER BLOCK                                
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                                 
***********************************************************************         
*                                                                               
TYPED    DSECT                                                                  
TYTYPE   DS    CL1                 TYPE EQUATE                                  
TYDISNME DS    XL2                 DIS TO TYPE NAME                             
TYPELN   EQU   *-TYPED                                                          
*                                                                               
*                                                                               
CODETABD DSECT                     PAYROLL CODE INFO                            
CTNUM    DS    XL1                 PAYROLL CODE NUMBER                          
CTCODE   DS    CL5                 PAYROLL CODE CODE                            
CTREV    DS    CL5                 REVERSAL                                     
CTPC1    DS    XL1                 PC1 CODE NUMBER                              
CTPC2    DS    XL1                 PC2 CODE NUMBER                              
CTSTAT   DS    XL1                 STATUS                                       
CTLEN    EQU   *-CODETABD                                                       
CTBLKLN  EQU   CTLEN*255           DEFINE BLOCK FOR 255 PAYCODES                
CTBLKLNX EQU   CTLEN*256                                                        
*                                                                               
LIND     DSECT                     LIST LINE                                    
LINNUMH  DS    CL8                                                              
LINNUM   DS    CL3                 NUMBER                                       
LINCODEH DS    CL8                                                              
LINCODE  DS    CL5                 PAYROLL CODE                                 
LINDESCH DS    CL8                                                              
LINDESC  DS    CL15                DESCRIPTION                                  
LINTYPEH DS    CL8                                                              
LINTYPE  DS    CL3                 PAYROLL TYPE                                 
LINATTRH DS    CL8                                                              
LINATTR  DS    CL40                ATTRIBUTES                                   
LINNEXTL DS    0C                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027ACCAP0A   12/11/09'                                      
         END                                                                    
