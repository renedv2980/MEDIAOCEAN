*          DATA SET REGAV19X   AT LEVEL 028 AS OF 05/01/02                      
*PHASE T81319B,+0                                                               
*INCLUDE INVDAY                                                                 
*INCLUDE REBKLST                                                                
*INCLUDE RECUP                                                                  
         TITLE 'T81319 - REPPAK FILE MAINT - RATE CARD AVAIL COPY'              
********************************************************************            
*                                                                               
*        DEC98 - RDETAIL COPY ('Z' INV RATE RECORDS)                            
*                                                                               
********************************************************************            
T81319   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81319,RR=R5                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T81319+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASYSD                                                         
         USING SYSD,R8                                                          
         L     R7,ASPOOLD                                                       
         USING SPOOLD,R7                                                        
         ST    R5,RELO                                                          
*                                                                               
         L     RF,=V(RECUP)                                                     
         L     RE,RELO                                                          
         AR    RF,RE               RELOCATE ADDRESS                             
         ST    RF,VRECUP                                                        
*                                                                               
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC AND PUTREC             
*                                                                               
         LR    R3,RA               MOVE PROFILE TO LOCAL STORAGE                
         AH    R3,=AL2(SFMPROFS-CONHEADH)                                       
         USING SVDSECT,R3                                                       
         MVC   RMPPROFS,SVPGPBIT                                                
         DROP  R3                                                               
*                                                                               
         OI    CONSERVH+6,X'81'    SCREEN IS ALWAYS MODIFIED                    
*                                                                               
MAIN20   DS    0H                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       REPORT                                       
         BE    PREP                                                             
         CLI   MODE,VALREC         VALIDATE RECORDS                             
         BE    VREC                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*****************************************************************               
*        VALIDATE KEY                                                           
*****************************************************************               
VKEY     DS    0H                                                               
         MVC   REPHLD,AGENCY       SAVE THE REP                                 
*                                                                               
         GOTO1 =A(VALSTA),DMCB,RNCFSTAH,RR=RELO                                 
         MVC   FSTAHLD,STAHLD                                                   
*                                                                               
         LA    R2,RNCFCODH         VALIDATE FROM RATE CODE                      
         CLI   5(R2),0                                                          
         JE    ERREND                                                           
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),3             MUST BE AT LEAST 3 CHARS                     
         JL    ERREND                                                           
         MVC   FCODHLD,8(R2)                                                    
         OC    FCODHLD,SPACES                                                   
         MVC   CODHLD,FCODHLD                                                   
*                                                                               
         GOTO1 =A(VALLEN),DMCB,RNCFLENH,RR=RELO                                 
         MVC   FLENHLD,LENHLD                                                   
*                                                                               
         GOTO1 =A(GETQTYR),DMCB,RNCFQTRH,RR=RELO                                
         MVC   FQTRHLD,QTRHLD                                                   
         MVC   FQTRBIT,QTRBIT                                                   
         MVC   FYEARHLD,YEARHLD                                                 
*                                                                               
         GOTO1 =A(CHKRTCD),DMCB,RNCTSTAH,RR=RELO     VALID RATE CODE?           
*                                                                               
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING REINVREC,R6                                                      
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,CPARREP                                                 
         MVC   RINVKSTA,STAHLD                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE                                                  
         BE    VKEYX                                                            
*                                                                               
         MVC   KEY(27),KEYSAVE     SET GENCON ERROR                             
*                                                                               
VKEYX    DS    0H                                                               
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*        VALIDATE RECORD                                                        
*****************************************************************               
VREC     DS    0H                                                               
*                                                                               
         DC    H'00'                                                            
*                                                                               
VRECX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*****************************************************************               
*        PRINT REPORT                                                           
*****************************************************************               
PREP     DS    0H                                                               
*                                                                               
* VALIDATE TO STATION DETAILS                                                   
*                                                                               
         GOTO1 =A(VALSTA),DMCB,RNCTSTAH,RR=RELO                                 
         MVC   TSTAHLD,STAHLD                                                   
*                                                                               
         LA    R2,RNCTCODH         VALIDATE TO RATE CODE                        
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         MVC   TCODHLD,FCODHLD     SAME AS FROM                                 
         B     PR20                                                             
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),3             MUST BE AT LEAST 3 CHARS                     
         JL    ERREND                                                           
         MVC   TCODHLD,8(R2)                                                    
         OC    TCODHLD,SPACES                                                   
         MVC   CODHLD,TCODHLD                                                   
*                                                                               
PR20     DS    0H                                                               
         CLI   RNCTLENH+5,0        ANY LENGTH?                                  
         BNE   *+14                                                             
         MVC   TLENHLD,FLENHLD     SAME AS FROM                                 
         B     PR30                                                             
*                                                                               
         GOTO1 =A(VALLEN),DMCB,RNCTLENH,RR=RELO                                 
         MVC   TLENHLD,LENHLD                                                   
*                                                                               
PR30     DS    0H                                                               
         CLI   RNCTQTRH+5,0        ANY QUARTER?                                 
         BNE   PR40                                                             
         MVC   TQTRHLD,FQTRHLD     SAME AS FROM                                 
         MVC   TQTRBIT,FQTRBIT                                                  
         MVC   TYEARHLD,FYEARHLD                                                
         B     PR50                                                             
*                                                                               
PR40     DS    0H                                                               
         GOTO1 =A(GETQTYR),DMCB,RNCTQTRH,RR=RELO                                
         MVC   TQTRHLD,QTRHLD                                                   
         MVC   TQTRBIT,QTRBIT                                                   
         MVC   TYEARHLD,YEARHLD                                                 
*                                                                               
PR50     DS    0H                                                               
         GOTO1 =A(CHKRTCD),DMCB,RNCTSTAH,RR=RELO     VALID RATE CODE?           
*                                                                               
* START GETTING RATE RECORDS FROM SOURCE                                        
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING REINVREC,KEY                                                     
*                                                                               
         MVI   KEY,X'12'                                                        
         MVC   RINVKREP,REPHLD     REP                                          
         MVC   RINVKSTA,FSTAHLD    STATION                                      
*                                                                               
         GOTO1 HIGH                                                             
         B     PR65                                                             
*                                                                               
PR60     DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
PR65     DS    0H                                                               
         CLC   KEY(17),KEYSAVE     SAME REP/STATION?                            
         BNE   PR300               NO - FINISHED                                
*                                                                               
         CLI   RINVKSRC,0          HEADER RECORD?                               
         BNE   PR60                                                             
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   FKEY,KEY            SAVE AWAY SOURCE KEY                         
         XC    EQUNUM,EQUNUM                                                    
*                                                                               
         MVI   ELCODE,X'06'        LOOK FOR EQUATE FOR RATE                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         USING RIMAELEM,R6                                                      
*                                                                               
PR70     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PR60                                                             
*                                                                               
         CLC   RIMACDE,FCODHLD     SAME RATE CODE?                              
         BNE   PR70                                                             
         CLC   RIMAREP,REPHLD      SAME REP?                                    
         BNE   PR70                                                             
         CLC   RIMALNTH,FLENHLD    SAME LENGTH?                                 
         BNE   PR70                                                             
*                                                                               
         MVC   EQUNUM,RIMANUM      SAVE AWAY EQUATE NUMBER                      
*                                                                               
         MVI   RINVKSRC,C'Z'       RATE RECORD                                  
         MVC   RINVKNUM,EQUNUM     EQUATE NUMBER                                
         MVC   RINVKYR,FYEARHLD    YEAR                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     FOUND THIS RECORD TO ADD?                    
         BNE   PR60                                                             
*                                                                               
         MVC   FRKEY,KEY           SAVE AWAY FROM RATE KEY                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),FKEY                                                     
         MVC   RINVKSTA,TSTAHLD    CHECK DEST. STATION                          
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     DOES DEST. HEADER EXIST?                     
         BNE   PR200                                                            
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
         MVC   TKEY,KEY            SAVE AWAY DEST KEY                           
*                                                                               
         GOTO1 =A(GETEQU#),RR=RELO                                              
         TM    MYFLAG,GOTEQU#      'Z' RECORD EXISTS?                           
         BZ    PR100               NO - ADD NEW ELEMENT IN HDR                  
*                                                                               
         MVI   RINVKSRC,C'Z'       RATE RECORD                                  
         MVC   RINVKNUM,EQUNUM     EQUATE NUMBER                                
         MVC   RINVKYR,TYEARHLD    YEAR                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     FOUND THIS RECORD TO ADD?                    
         BNE   PR120               NO - COPY RATE REC FROM SOURCE               
*                                                                               
         MVC   TRKEY,KEY           SAVE AWAY DEST RATE KEY                      
*                                                                               
         DC    H'00'                                                            
*                                                                               
PR100    DS    0H                  ADD NEW ELEMENT IN HDR                       
         GOTO1 =A(UPHDR06),RR=RELO                                              
*                                                                               
PR120    DS    0H                  ADD DEST RATE RECORD                         
         DC    H'00'                                                            
*                                                                               
PR200    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),FKEY        RESTORE FROM KEY SEQUENCE                    
         GOTO1 HIGH                                                             
         B     PR60                                                             
*                                                                               
PR300    DS    0H                                                               
         DC    H'00'                                                            
*                                                                               
PREPX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
ERREND   GOTO1 ERREX                                                            
*                                                                               
NEXTFLD  ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
         GETEL R6,34,ELCODE                                                     
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*****************************************************************               
*       ADD X'06' ELEMENT IN HEADER FOR THIS RDETAIL                            
*****************************************************************               
UPHDR06  NTR1  BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM           BUILD WEEKLY ELEM W/ COST                    
         LA    R4,ELEM                                                          
         USING RIMAELEM,R4                                                      
*                                                                               
         MVI   RIMACODE,X'06'      ELEM TYPE                                    
         MVI   RIMALEN,RIMALENQ    ELEM LENGTH                                  
         MVC   RIMANUM,EQUNUM      EQUATE #                                     
         MVC   RIMAREP,REPHLD      REP CODE                                     
         MVC   RIMACDE,TCODHLD     RATE CODE                                    
         MVC   RIMALNTH,TLENHLD    SPOT LENGTH                                  
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',UPHDREPF),(0,AIO),(R4),0                        
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
UPHDR06X DS    0H                                                               
         XIT1                                                                   
*                                                                               
UPHDREPF DC    CL8'REPFILE'                                                     
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*****************************************************************               
*        GET EQUATE NUMBER FOR 'Z' RECORD                                       
*        INPUT    AIO MUST HAVE HEADER RECORD                                   
*        OUTPUT   EQUNUM - EQUATE NUMBER FOR 'Z' RECORD                         
*        OUTPUT   MYFLAG - OI W/ GOTEQU# IF 'Z' RECORD EXISTS                   
*****************************************************************               
GETEQU#  NTR1  BASE=*,LABEL=*                                                   
         NI    MYFLAG,X'FF'-GOTEQU#                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'        MASTER AVAIL ELEMENT                         
*                                                                               
         LA    R5,1                NEXT AVAILABLE EQUATE                        
         STC   R5,EQUNUM                                                        
*                                                                               
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
GETEQ10  DS    0H                                                               
         BRAS  RE,NEXTEL           FOUND EQUATE NUMBER IN RECORD?               
         BNE   GETEQU#X            NO                                           
         USING RIMAELEM,R6                                                      
*                                                                               
         CLC   RIMAREP,REPHLD      SAME REP (CHILD)?                            
         BNE   GETEQ50             NO                                           
         CLC   RIMACDE,TCODHLD     SAME AVAIL CODE?                             
         BNE   GETEQ50             NO                                           
         CLC   RIMALNTH,TLENHLD    SAME AVAIL LENGTH?                           
         BNE   GETEQ50             NO                                           
*                                                                               
         MVC   EQUNUM,RIMANUM      FOUND EQUATE NUMBER                          
         OI    MYFLAG,GOTEQU#                                                   
         B     GETEQU#X                                                         
*                                                                               
GETEQ50  DS    0H                                                               
         CLC   EQUNUM,RIMANUM      SAME EQUATE NUMBER?                          
         BNE   *+12                NO - KEEP EQUNUM                             
         LA    R5,1(R5)                                                         
         STC   R5,EQUNUM                                                        
         B     GETEQ10                                                          
*                                                                               
GETEQU#X DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*        VALIDATE STATION                                                       
*****************************************************************               
VALSTA   NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,MISSING                                                    
         L     R2,0(R1)                                                         
*                                                                               
         CLI   5(R2),0             REQUIRED                                     
         JE    ERREND                                                           
         GOTO1 VALISTA                                                          
*                                                                               
         MVC   STAHLD,WORK                                                      
         MVI   STAHLD+4,C'T'                                                    
         CLI   WORK+4,C' '                                                      
         BE    *+10                                                             
         MVC   STAHLD+4(1),WORK+4                                               
         CLI   WORK+40,C' '                                                     
         BE    *+10                                                             
         MVC   STAHLD+4(1),WORK+40 CHECK SATTELITE                              
*                                                                               
VALSTAX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*****************************************************************               
*        VALIDATE LENGTH                                                        
*****************************************************************               
VALLEN   NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)                                                         
*                                                                               
         CLI   5(R2),0             REQUIRED                                     
         JE    ERREND                                                           
*                                                                               
         LR    RE,R2                                                            
         LA    RE,8(RE)            POINT TO FIELD                               
         ZIC   RF,5(R2)                                                         
         ZIC   R1,5(R2)                                                         
*                                                                               
VL10     CLI   0(RE),C'0'          MUST BE NUMBERIC                             
         BL    VL20                                                             
         CLI   0(RE),C'9'                                                       
         BH    VL20                                                             
         LA    RE,1(RE)                                                         
         BCT   RF,VL10                                                          
         B     VL30                                                             
*                                                                               
VL20     MVI   ERROR,INVALID                                                    
         C     RF,=F'1'            IF NOT IN LAST POSITION ERROR                
         JNE   ERREND                                                           
         BCTR  R1,0                CHECK FOR MINUTES/SECONDS INDICATOR          
         CLI   0(RE),C'M'                                                       
         BNE   *+12                                                             
         OI    LENHLD,X'80'                                                     
         B     VL30                                                             
         CLI   0(RE),C'S'                                                       
         JNE   ERREND                                                           
*                                                                               
VL30     BCTR  R1,0                CONVERT TO BINARY                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         STCM  R0,1,LENHLD+1                                                    
*                                                                               
VALLENX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
VARPACK  PACK  DUB,8(0,R2)                                                      
*                                                                               
         EJECT                                                                  
*****************************************************************               
*        GET QTR/YEAR                                                           
*****************************************************************               
GETQTYR  NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,MISSING                                                    
*                                                                               
         L     R2,0(R1)                                                         
         CLI   5(R2),0             REQUIRED                                     
         JE    ERREND                                                           
*                                                                               
         XC    WORK,WORK                                                        
         LA    RE,QTRTABA                                                       
         LA    RF,4                                                             
*                                                                               
GQY10    DS    0H                  CHECK IF QUARTER IS IN TABLE                 
         MVI   ERROR,INVALID                                                    
         CLC   8(2,R2),0(RE)       IN TABLE?                                    
         BE    GQY20                                                            
*                                                                               
         LA    RE,QTRTABAL(RE)     BUMP TO NEXT ENTRY                           
         BCT   RF,GQY10                                                         
         J     ERREND                                                           
*                                                                               
GQY20    DS    0H                                                               
         MVC   QTRHLD,1(RE)                                                     
         NI    QTRHLD,X'0F'        MAKE IT BINARY                               
*                                                                               
         MVC   WORK+1(2),2(RE)     START BRD DATE RANGE                         
         MVC   WORK+4(2),4(RE)     END BRD DATE RANGE                           
         MVC   QTRBIT,6(RE)        QTR BIT                                      
*                                                                               
         CLI   10(R2),C'/'                                                      
         JNE   ERREND                                                           
*                                                                               
         MVC   HALF,11(R2)         CHECK OUT THE YEAR                           
         LA    RE,HALF                                                          
         LA    RF,2                                                             
*                                                                               
GQY30    CLI   0(RE),C'0'          MUST BE NUMERIC                              
         JL    ERREND                                                           
         CLI   0(RE),C'9'                                                       
         JH    ERREND                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,GQY30                                                         
*                                                                               
         XC    DUB,DUB                                                          
*                                                                               
         MVC   DUB(2),11(R2)       MOVE IN YEAR YY                              
         MVC   DUB+2(4),=C'0101'   MM/DD                                        
         GOTO1 DATCON,DMCB,(0,DUB),(3,TMPDTEE)                                  
         MVC   YEARHLD,TMPDTEE     YEAR IN BINARY                               
*                                                                               
GETQYX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
QTRTABA  DC    CL2'Q1',XL5'010F030F80'                                          
         DC    CL2'Q2',XL5'040F060F40'                                          
         DC    CL2'Q3',XL5'070F090F20'                                          
         DC    CL2'Q4',XL5'0A0F0C0F10'                                          
QTRTABAL EQU   7                                                                
         EJECT                                                                  
*****************************************************************               
*        VALIDATE RATE CODE KEY ENTERED (3E)                                    
*****************************************************************               
CHKRTCD  NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,INVALID                                                    
         L     R2,0(R1)                                                         
*                                                                               
         LA    R6,KEY                                                           
         USING RARTREC,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RARTKTYP,X'3E'                                                   
         MVC   RARTKREP,AGENCY                                                  
         MVC   RARTKCOD,CODHLD                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     WAS RECORD FOUND                             
         BNE   RTCDNFND                                                         
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   RTCDKEY,KEY         SAVE AWAY RATE RECORD KEY                    
*                                                                               
         LA    R2,RNCFLENH         SET UP HELLO LOOKUP INFO                     
         XC    DUB,DUB                                                          
         MVC   DUB(1),YEARHLD                                                   
         MVC   DUB+1(2),LENHLD                                                  
         GOTO1 HELLO,DMCB,(C'G',CHKREPF),(X'02',AIO),(3,DUB)                    
         CLI   12(R1),0                                                         
         BNE   RTCDNFND                                                         
*                                                                               
         L     R6,12(R1)                                                        
         USING RALQELEM,R6                                                      
*                                                                               
         MVC   DUB(1),RALQQTR      MOVE QTR FROM RECORD                         
         OC    DUB(1),QTRBIT       OR IT WITH QTR FROM REQUEST                  
         CLC   DUB(1),RALQQTR      IF FIELD CHANGED THEN ERROR                  
         BNE   RTCDNFND                                                         
         B     CHKRTCDX                                                         
*                                                                               
CHKRT10  DS    0H                  VALIDATE QTRS FOR REPORT                     
         BAS   RE,GQTRRFLT                                                      
*                                                                               
         LA    R3,QTRRFILT         REQUESTED QUARTER FILTERS                    
*                                                                               
CHKRT20  DS    0H                                                               
         CLI   0(R3),X'FF'         END OF TABLE                                 
         BE    CHKRTCDX                                                         
*                                                                               
         MVC   DUB(1),RALQQTR      MOVE QTR FROM RECORD                         
         OC    DUB(1),0(R3)        OR IT WITH QTR FROM REQUEST                  
         CLC   DUB(1),RALQQTR      IF FIELD CHANGED THEN ERROR                  
         BNE   INVRQTRE                                                         
*                                                                               
         LA    R3,2(R3)            TEST NEXT QTR REQUESTED                      
         B     CHKRT20                                                          
*                                                                               
CHKRTCDX DS    0H                                                               
         XIT1                                                                   
*                                                                               
RTCDNFND DS    0H                                                               
         LA    R2,RNCFCODH         RATE RECORD NOT FOUND                        
         MVC   RERROR(2),=AL2(RATENFND)                                         
         GOTO1 MYERROR                                                          
*                                                                               
INVRQTRE DS    0H                                                               
         LA    R2,RNCFCODH         INVALID QTR FOR THIS RATE REC                
         MVC   RERROR(2),=AL2(INVRQTR)                                          
         GOTO1 MYERROR                                                          
*                                                                               
CHKREPF  DC    CL8'REPFILE'                                                     
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*        GET QUARTERS REQUESTED FOR REPORT                                      
*****************************************************************               
GQTRRFLT NTR1                                                                   
         MVI   ERROR,MISSING                                                    
*                                                                               
         LA    R2,RNCFQTRH                                                      
         CLI   5(R2),0                                                          
         JE    ERREND                                                           
*                                                                               
         MVI   ERROR,INVALID                                                    
         XC    QTRRFILT,QTRRFILT                                                
*                                                                               
         LA    R2,8(R2)                                                         
         LA    R3,QTRRFILT         STORE QUARTER FILTERS                        
*                                                                               
GQTRR10  DS    0H                                                               
         CLI   0(R2),C'/'          NO MORE QTR FILTERS                          
         BE    GQTRR40                                                          
         CLI   0(R2),C','          SHOULD SEPERATE QUARTERS                     
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
*                                                                               
         LA    RE,QTRRTAB                                                       
         LA    RF,4                                                             
*                                                                               
GQTRR20  DS    0H                                                               
         CLC   0(2,R2),0(RE)       QUARTER IN TABLE?                            
         BE    GQTRR30             MUST BE NUMERIC                              
*                                                                               
         LA    RE,4(RE)                                                         
         BCT   RF,GQTRR20                                                       
*                                                                               
         LA    R2,RNCFQTRH                                                      
         J     ERREND                                                           
*                                                                               
GQTRR30  DS    0H                                                               
         MVC   0(1,R3),3(RE)       GET QTR BIT                                  
         MVC   1(1,R3),2(RE)       QTRHLD BYTE                                  
*                                                                               
         LA    R2,2(R2)            CHECK NEXT QTR REQUESTED                     
         LA    R3,2(R3)                                                         
         B     GQTRR10                                                          
*                                                                               
GQTRR40  DS    0H                                                               
         MVI   0(R3),X'FF'         DENOTE END OF QTR FILTERS                    
*                                                                               
         MVC   HALF,1(R2)          CHECK OUT THE YEAR                           
         LA    RE,HALF                                                          
         LA    RF,2                                                             
*                                                                               
GQTRR50  CLI   0(RE),C'0'          MUST BE NUMERIC                              
         JL    ERREND                                                           
         CLI   0(RE),C'9'                                                       
         JH    ERREND                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,GQTRR50                                                       
*                                                                               
         XC    DUB,DUB                                                          
         XC    TMPDTEE,TMPDTEE                                                  
*                                                                               
         MVC   DUB(2),1(R2)        MOVE IN YEAR YY                              
         MVC   DUB+2(4),=C'0101'   MM/DD                                        
*                                                                               
         GOTO1 DATCON,DMCB,(0,DUB),(3,TMPDTEE)                                  
         MVC   YEARHLD,TMPDTEE     YEAR IN BINARY                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(1),YEARHLD     MOVE YEAR INTO DATE RANGE                    
         MVC   WORK+1(2),=X'010F'                                               
         MVC   WORK+3(1),YEARHLD                                                
         MVC   WORK+4(2),=X'0C0F'                                               
*                                                                               
*--GET THE DATE RANGE FOR BROADCAST YEAR (COMPRESSED)                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(0,TMPDTEE)                                 
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE),(2,STBRDYRC)                             
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK+3),(0,TMPDTEE)                               
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE+6),(2,ENBRDYRC)                           
*                                                                               
GQTRRX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
QTRRTAB  DC    CL2'Q1',XL1'01',XL1'80'                                          
         DC    CL2'Q2',XL1'02',XL1'40'                                          
         DC    CL2'Q3',XL1'03',XL1'20'                                          
         DC    CL2'Q4',XL1'04',XL1'10'                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*****************************************************************               
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* REGAVFFD                                                                      
* DDGENTWA                                                                      
* REGAVWTWA                                                                     
* REGENINVA                                                                     
* REGENARTE                                                                     
* REGAVWORKD                                                                    
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE REGAVFFD                                                       
       ++INCLUDE DDGENTWA                                                       
*                                                                               
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE REGAVD5D          RDETAIL/COPY SCREEN                          
         EJECT                                                                  
       ++INCLUDE REGAVWTWA                                                      
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
REINVREC DSECT                                                                  
       ++INCLUDE REGENINVA                                                      
*                                                                               
       ++INCLUDE REGENARTE                                                      
       ++INCLUDE REGAVWORKD                                                     
*                                                                               
         PRINT ON                                                               
         ORG   SYSSPARE                                                         
*****************************************************                           
*              WORK AREA                                                        
*****************************************************                           
RELO     DS    A                                                                
*                                                                               
SAVEKEY  DS    CL27                                                             
HDRKEY   DS    CL27                HEADER KEY                                   
FKEY     DS    CL27                CURRENT SOURCE HDRKEY                        
FRKEY    DS    CL27                CURRENT SOURCE RATE KEY                      
TOKEY    DS    CL27                CURRENT DESTINATION HDRKEY                   
TORKEY   DS    CL27                CURRENT DESTINATION RATE KEY                 
RTCDKEY  DS    CL27                RATE CODE KEY (3E)                           
*                                                                               
STAHLD   DS    CL5                 STATION                                      
REPHLD   DS    CL2                 REP                                          
CODHLD   DS    CL8                 RATE CODE                                    
LENHLD   DS    XL2                 SPOT LENGTH                                  
YEARHLD  DS    XL1                 YEAR IN BINARY                               
QTRHLD   DS    CL1                 QTR (BINARY)                                 
QTRBIT   DS    XL1                 QUARTER BIT                                  
*                                                                               
* FROM STAION DETAILS                                                           
*                                                                               
FSTAHLD  DS    CL5                 STATION                                      
FCODHLD  DS    CL8                 RATE CODE                                    
FLENHLD  DS    XL2                 SPOT LENGTH                                  
FYEARHLD DS    XL1                 YEAR IN BINARY                               
FQTRHLD  DS    CL1                 QTR (BINARY)                                 
FQTRBIT  DS    XL1                 QUARTER BIT                                  
*                                                                               
* TO STAION DETAILS                                                             
*                                                                               
TSTAHLD  DS    CL5                 STATION                                      
TCODHLD  DS    CL8                 RATE CODE                                    
TLENHLD  DS    XL2                 SPOT LENGTH                                  
TYEARHLD DS    XL1                 YEAR IN BINARY                               
TQTRHLD  DS    CL1                 QTR (BINARY)                                 
TQTRBIT  DS    XL1                 QUARTER BIT                                  
*                                                                               
STENDQTR DS    XL4                 DATE RANGE FOR QTR (COMPRESSED)              
STBRDYRJ DS    XL3                 FIRST WEEK OF BRD YEAR (JULIAN)              
ENBRDYRJ DS    XL3                 LAST WEEK OF BRD YEAR (JULIAN)               
STBRDYRC DS    XL2                 FIRST WEEK OF BRD YEAR (COMPRESSED)          
ENBRDYRC DS    XL2                 LAST WEEK OF BRD YEAR (COMPRESSED)           
*                                                                               
EQUNUM   DS    XL1                 EQUATE NUMBER FOR 'Z' RECORD                 
DEFCOST  DS    F                   RATE                                         
PRVCOST  DS    F                   PREVIOUS COST                                
*                                                                               
ACURHDR  DS    A                   A(CURRENT HEADER IN KEYTAB)                  
*                                                                               
KEYTAB   DS    15CL18              KEY HOLD AREA 15 KEYS 18 BYTES EACH          
KEYTABLN EQU   *-KEYTAB                                                         
         DC    X'FF'                                                            
*                                                                               
BRDTAB   DS    4XL7                BROADCAST STR AND END DATES                  
BRDTABLN EQU   *-BRDTAB            FOR 4 QTRS IN YEAR                           
         DC    X'FF'                                                            
*                                                                               
RMPPROFS DS    CL8                 PROFILE SETTINGS                             
BSVDA    DS    CL4                 SAVED DISK ADDRESS                           
*                                                                               
THISLINE DS    A                   CURRENT LINE ADDRESS                         
*                                                                               
CURSCRN  DS    XL1                 CURRENT SCREEN                               
LISTSCRN EQU   X'D3'               LISTING SCREEN                               
REPSCRN  EQU   X'D4'               REPORT SCREEN                                
WEEKSCRN EQU   X'D2'               WEEKLY SCREEN                                
*                                                                               
MYFLAG   DS    XL1                 FLAGS                                        
VALSCRN  EQU   X'01'               VALIDATE SCREEN AGAIN                        
GOTEQU#  EQU   X'02'               GOT EQUATE # FROM RECORD                     
GOWKSCRN EQU   X'04'               GO TO WEEKLY SCREEN                          
DISPSEL  EQU   X'10'               DISPLAYED SELECTED HEADER IN WK SCRN         
QTHASCST EQU   X'20'               THIS QUARTER HAS A COST                      
*                                                                               
TMPDTEE  DS    CL6                 TEMP DATE (YYMMDD)                           
TMPDTEE2 DS    CL6                 TEMP DATE (YYMMDD)                           
TMPDTEJ  DS    XL3                 TEMP DATE (JULIAN)                           
TMPBRD   DS    CL12                TEMP BROADCASE DATE RANGE (YYMMDD)           
TMPBRDST DS    XL3                 TEMP START BROADCAST WEEK (JULIAN)           
TMPBRDEN DS    XL3                 TEMP END BROADCAST WEEK (JULIAN)             
TMPBRDCU DS    XL3                 TEMP CURRENT BROADCAST WEEK (JULIAN)         
*                                                                               
BRDDTEE  DS    CL12                BROADCAST DATE RANGE (YYMMDD)                
BRDQTRST DS    XL3                 BROADCAST START DATE (JULIAN)                
BRDQTREN DS    XL3                 BROADCAST END DATE (JULIAN)                  
*                                                                               
HDRSTRTJ DS    XL3                 HEADER START (JULIAN)                        
HDRENDJ  DS    XL3                 HEADER END (JULIAN)                          
*                                                                               
DYPTFILT DS    CL1                 DAYPART FILTER                               
WEEKFILT DS    XL3                 WEEKLY FILTER (JULIAN)                       
INV#FILT DS    CL4                 INV# START AT FILTER                         
*                                                                               
DYPTRFLT DS    CL6                 DAYPART FILTER FOR REPORT                    
QTRRFILT DS    XL9                 QUARTERS REQUESTED FOR REPORT                
INVRFILT DS    CL8                 INV # FILTERS FOR REPORT                     
CURQTFLT DS    XL1                 CURRENT QTR BEING FILTERED                   
*                                                                               
ANXTQTR  DS    A                   A(NEXT QTR IN QTR FILTERS REPORT)            
*                                                                               
MAXLST#  EQU   15                  MAX NUMBER OF LIST ENTRIES                   
MAXDPT#  EQU   6                   MAX NUMBER OF DPTS FOR REPORT                
*                                                                               
*****************************************************                           
*      ERROR EQUATES                                                            
*****************************************************                           
RATENFND EQU   814                 RATE RECORD NOT FOUND FOR COMBO              
INITQTR  EQU   815                 INITIALIZE QTR W/ COST                       
INVRQTR  EQU   825                 INVALID QUARTER FOR THIS RATE REC            
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
RINVD    DSECT                                                                  
       ++INCLUDE REGENAVL                                                       
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
T813FFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEAD+3520)-(T813FFD+3072))                                  
         ORG   CONHEAD+3520-SAVAREAL                                            
SAVAREA  DS    0C                                                               
SVLIST   DS    CL268               CALL ROUTINE STACK POINTER                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028REGAV19X  05/01/02'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
*&&DO                                                                           
*                                                                               
         MVC   WORK(1),YEARHLD     MOVE YEAR INTO DATE RANGE                    
         MVC   WORK+3(1),YEARHLD                                                
*                                                                               
*--GET THE BROADCAST DATE RANGE FOR THIS QTR                                    
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(0,TMPDTEE)                                 
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE),(2,STENDQTR)                             
         GOTO1 DATCON,DMCB,(0,BRDDTEE),(19,BRDQTRST)                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK+3),(0,TMPDTEE)                               
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE+6),(2,STENDQTR+2)                         
         GOTO1 DATCON,DMCB,(0,BRDDTEE+6),(19,BRDQTREN)                          
*                                                                               
         XC    WORK,WORK           GET FIRST AND LAST WEEK OF BRD YEAR          
         MVC   WORK+1(2),=X'010F'                                               
         MVC   WORK(1),YEARHLD                                                  
         MVC   WORK+4(2),=X'0C0F'                                               
         MVC   WORK+3(1),YEARHLD                                                
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(0,TMPDTEE)                                 
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE),(19,STBRDYRJ)                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK+3),(0,TMPDTEE)                               
         GOTO1 GETBROAD,DMCB,(1,TMPDTEE),BRDDTEE,GETDAY,ADDAY                   
         GOTO1 DATCON,DMCB,(0,BRDDTEE+6),(19,ENBRDYRJ)                          
*&&                                                                             
