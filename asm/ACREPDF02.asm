*          DATA SET ACREPDF02  AT LEVEL 005 AS OF 02/01/02                      
*PHASE ACDF02A,*                                                                
*INCLUDE PRNTBL                                                                 
ACDF02   TITLE '- TRANSACTION COUNTER AND ERROR REPORT'                         
***********************************************************************         
* COUNT DRAFT TRANSACTIONS ON AN ACCOUNT, COMPARE WITH ASTDRAFT,      *         
* REPORT DISCREPANCIES AND OPTIONALLY FIX INCORRECT ASTDRAFT          *         
*                                                                     *         
* COUNT DRAFT ADVANCE ITEMS ON AN ACCOUNT, COMPARE WITH JCBADV,       *         
* REPORT DISCREPANCIES AND OPTIONALLY FIX INCORRECT JCBADV            *         
*                                                                     *         
* COUNT/COMPARE/(FIX) JCBALL/JCBWOF/JCBRCV/JCBXFR/JCBADVAL            *         
*                                                                     *         
* OPTION 1 = 'Y' - FIX ASTDRAFT                                       *         
* OPTION 2 = 'Y' - FIX JCBADV/JCBALL/JCBWOF/JCBRCV/JCBXFR/JCBADVAL    *         
***********************************************************************         
ACDF02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACDF**                                                       
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING WORKD,RC            RC=A(LOCAL W/S)                              
*                                                                               
         CLI   MODE,RUNFRST        INITIALIZATION                               
         BE    FRST                                                             
         CLI   MODE,REQFRST        BYPASS BLDBUF                                
         BE    RQFRST                                                           
         CLI   MODE,PROCACC        ACCOUNT RECORD                               
         BE    PROA                                                             
         CLI   MODE,ACCLAST        LAST FOR ACCOUNT RECORD                      
         BE    ACCL                                                             
         CLI   MODE,PROCTRNS       TRANSACTION RECORD                           
         BE    TRNS                                                             
         CLI   MODE,RUNLAST        LAST TIME                                    
         BE    LAST                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
FRST     L     RF,ADCOMFAC                                                      
         USING COMFACSD,RF                                                      
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VHELLO,CHELLO                                                    
         ZAP   DUB,=P'0'                                                        
         MVI   FCDRAOVR,FCDRATRY+FCDRATRN                                       
FRSTX    B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* BYPASS BLDBUF                                                       *         
***********************************************************************         
         SPACE 1                                                                
RQFRST   MVI   FCSEQ,FCSEQNEW      BYPASS BLDBUF ROUTINE                        
*                                                                               
RQFRSTX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST FOR ACCOUNT                                                   *         
***********************************************************************         
         SPACE 1                                                                
PROA     XC    SVVALS(SVVALSL),SVVALS                                           
         ZAP   SVJCBADA,=P'0'                                                   
         XC    DFVALS(DFVALSL),DFVALS                                           
         ZAP   DFJCBADA,=P'0'                                                   
         L     R2,ADACC            GET DRAFT COUNTS FROM ACC RECORD             
         USING ACTRECD,R2                                                       
         LA    RE,ACTRECD+ACCORFST                                              
         USING ASTEL,RE                                                         
         SR    RF,RF                                                            
PROA02   CLI   ASTEL,ASTELQ                                                     
         BE    PROA06                                                           
         CLI   ASTEL,JCBELQ                                                     
         BE    PROA08                                                           
         CLI   ASTEL,0                                                          
         BE    PROAX               E-O-R                                        
PROA04   IC    RF,ASTLN                                                         
         AR    RE,RF                                                            
         B     PROA02                                                           
PROA06   MVC   SVASTDRA,ASTDRAFT   KEEP DRAFT COUNT                             
         B     PROA04                                                           
*                                                                               
         USING JCBELD,RE                                                        
PROA08   MVC   SVJCBADV,JCBADV                                                  
         MVC   SVJCBALL,JCBALL                                                  
         MVC   SVJCBWOF,JCBWOF                                                  
         MVC   SVJCBRCV,JCBRCV                                                  
         MVC   SVJCBXFR,JCBXFR                                                  
         ZAP   SVJCBADA,=P'0'                                                   
         OC    JCBADVAL,JCBADVAL   TEST NOT SET (OLD ELEMENT)                   
         BZ    *+10                                                             
         ZAP   SVJCBADA,JCBADVAL                                                
         B     PROA04                                                           
*                                                                               
PROAX    B     EXIT                                                             
         DROP  R2,RE                                                            
         EJECT                                                                  
***********************************************************************         
* LAST FOR ACCOUNT                                                    *         
***********************************************************************         
         SPACE 1                                                                
ACCL     CLC   DFASTDRA,SVASTDRA   TEST ACTUAL VS COUNT ON ACCOUNT REC          
         BE    ACCL20              OK                                           
*                                                                               
         LA    R7,=C'GET'                                                       
         BAS   RE,DUMPIT                                                        
*                                                                               
         MVC   P,SPACES            BLANK LINE                                   
         GOTO1 ACREPORT                                                         
         L     RF,ERRACCS          KEEP COUNT OF ERRORS                         
         LA    RF,1(RF)                                                         
         ST    RF,ERRACCS                                                       
         L     R2,ADACC            BUILD PRINT LINE                             
         USING ACTRECD,R2                                                       
         MVC   PACT,ACTKULA        ACCOUNT CODE                                 
         L     RE,ADACCNAM                                                      
         USING NAMELD,RE                                                        
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+4                                                           
         MVC   PNAM(0),NAMEREC     ACCOUNT NAME                                 
         CURED SVASTDRA,(L'PDSV,PDSV),0                                         
         CURED DFASTDRA,(L'PDAC,PDAC),0                                         
         MVC   PNAR,=CL33'** DRAFT COUNT IN ERROR **'                           
*                                                                               
         CLI   QOPT1,C'Y'          TEST CORRECTIVE ACTION REQUESTED             
         BNE   ACCL16                                                           
         LA    RE,ACTRECD+ACCORFST                                              
         SR    RF,RF                                                            
         USING ASTELD,RE                                                        
ACCL08   CLI   ASTEL,ASTELQ                                                     
         BE    ACCL10                                                           
         CLI   ASTEL,0                                                          
         BE    ACCL12                                                           
         IC    RF,ASTLN                                                         
         AR    RE,RF                                                            
         B     ACCL08                                                           
*                                  UPDATE EXISTING ASTEL                        
ACCL10   MVC   ASTDRAFT,DFASTDRA                                                
         MVC   PNAR,=CL33'ASTEL ELEMENT UPDATED'                                
         B     ACCL14                                                           
*                                  CREATE AN ASTEL                              
ACCL12   LA    RE,WORK2                                                         
         XC    WORK2,WORK2                                                      
         MVI   ASTEL,ASTELQ                                                     
         MVI   ASTLN,ASTLN1Q                                                    
         MVC   ASTDRAFT,DFASTDRA                                                
         GOTO1 VHELLO,PARM,(C'P',ACCFIL),ADACC,WORK2,0                          
         MVC   PNAR,=CL33'ASTEL ELEMENT ADDED'                                  
         CLI   12(R1),0                                                         
         BE    ACCL14                                                           
         MVC   PNAR,=CL33'** CANNOT ADD ASTEL **'                               
         B     ACCL14                                                           
*                                                                               
ACCL14   CLI   RCWRITE,C'N'                                                     
         BE    ACCL16                                                           
         MVI   MODE,WRITACC        TELL MONACC TO UPDATE ACCOUNT                
*                                                                               
ACCL16   GOTO1 ACREPORT                                                         
         LA    R7,=C'PUT'                                                       
         BAS   RE,DUMPIT                                                        
*                                                                               
ACCL20   CLC   DFJCBV(DFJCBVL),SVJCBV                                           
         BE    ACCLX               ACTUAL VS ACCOUNT RECORD OK                  
*                                                                               
         CLC   DFASTDRA,SVASTDRA   TEST BASICS DONE FOR ACCOUNT                 
         BNE   ACCL22                                                           
         MVC   P,SPACES            BLANK LINE                                   
         GOTO1 ACREPORT                                                         
         L     RF,ERRACCS          KEEP COUNT OF ERRORS                         
         LA    RF,1(RF)                                                         
         ST    RF,ERRACCS                                                       
         L     R2,ADACC            BUILD PRINT LINE                             
         USING ACTRECD,R2                                                       
         MVC   PACT,ACTKULA        ACCOUNT CODE                                 
         L     RE,ADACCNAM                                                      
         USING NAMELD,RE                                                        
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+4                                                           
         MVC   PNAM(0),NAMEREC     ACCOUNT NAME                                 
*                                                                               
ACCL22   CLC   SVJCBADV,DFJCBADV   TEST ADVANCED ITEMS                          
         BE    ACCL24                                                           
         CURED SVJCBADV,(L'PDSV,PDSV),0                                         
         CURED DFJCBADV,(L'PDAC,PDAC),0                                         
         MVC   PNAR,=CL33'** ADVANCE ITEM COUNT IN ERROR **'                    
         GOTO1 ACREPORT                                                         
*                                                                               
ACCL24   CLC   SVJCBALL,DFJCBALL   TEST REGULAR ALLOCATION                      
         BE    ACCL26                                                           
         CURED SVJCBALL,(L'PDSV,PDSV),0                                         
         CURED DFJCBALL,(L'PDAC,PDAC),0                                         
         MVC   PNAR,=CL33'** ALLOCATION COUNT IN ERROR **'                      
         GOTO1 ACREPORT                                                         
*                                                                               
ACCL26   CLC   SVJCBWOF,DFJCBWOF   TEST WRITE-OFF                               
         BE    ACCL28                                                           
         CURED SVJCBWOF,(L'PDSV,PDSV),0                                         
         CURED DFJCBWOF,(L'PDAC,PDAC),0                                         
         MVC   PNAR,=CL33'** WRITE-OFF COUNT IN ERROR **'                       
         GOTO1 ACREPORT                                                         
*                                                                               
ACCL28   CLC   SVJCBRCV,DFJCBRCV   TEST WRITE-OFF RECOVERY                      
         BE    ACCL30                                                           
         CURED SVJCBRCV,(L'PDSV,PDSV),0                                         
         CURED DFJCBRCV,(L'PDAC,PDAC),0                                         
         MVC   PNAR,=CL33'** W/O RECOVERY COUNT IN ERROR **'                    
         GOTO1 ACREPORT                                                         
*                                                                               
ACCL30   CLC   SVJCBXFR,DFJCBXFR   TEST TRANSFER                                
         BE    ACCL32                                                           
         CURED SVJCBXFR,(L'PDSV,PDSV),0                                         
         CURED DFJCBXFR,(L'PDAC,PDAC),0                                         
         MVC   PNAR,=CL33'** TRANSFER COUNT IN ERROR **'                        
         GOTO1 ACREPORT                                                         
*                                                                               
ACCL32   CP    SVJCBADA,DFJCBADA   TEST ADVANCE ITEM ALLOCATION                 
         BE    ACCL34                                                           
         CURED SVJCBADA,(L'PDSV+1,PDSV),2,MINUS=YES                             
         CURED DFJCBADA,(L'PDAC+1,PDAC),2,MINUS=YES                             
         MVC   PNAR,=CL33'** ADVANCE ALLOCATION IN ERROR **'                    
         GOTO1 ACREPORT                                                         
*                                                                               
ACCL34   OC    DFJCBPV(DFJCBPVL),DFJCBPV                                        
         BZ    ACCL36                                                           
         MVC   PNAR,=CL33'JOB SET PENDING'                                      
         CLI   QOPT2,C'Y'          TEST UPDATIVE                                
         BE    *+10                                                             
         MVC   PNAR,=CL33'** JOB SHOULD BE SET PENDING **'                      
         GOTO1 ACREPORT                                                         
*                                                                               
ACCL36   DS    0H                  TEST ?                                       
*                                                                               
ACCL38   DS    0H                  TEST ?                                       
*                                                                               
ACCL40   CLI   QOPT2,C'Y'          TEST CORRECTIVE ACTION REQUESTED             
         BNE   ACCL50                                                           
         LA    RE,ACTRECD+ACCORFST                                              
         SR    RF,RF                                                            
         USING JCBELD,RE                                                        
ACCL42   CLI   JCBEL,JCBELQ                                                     
         BE    ACCL44                                                           
         CLI   JCBEL,0                                                          
         BE    ACCL46                                                           
         IC    RF,JCBLN                                                         
         AR    RE,RF                                                            
         B     ACCL42                                                           
*                                                                               
ACCL44   MVC   JCBADV,DFJCBADV     UPDATE EXISTING JCBEL                        
         MVC   JCBALL,DFJCBALL                                                  
         MVC   JCBWOF,DFJCBWOF                                                  
         MVC   JCBRCV,DFJCBRCV                                                  
         MVC   JCBXFR,DFJCBXFR                                                  
         ZAP   JCBADVAL,DFJCBADA                                                
         OC    DFJCBPV(DFJCBPVL),DFJCBPV                                        
         BZ    *+8                                                              
         OI    JCBINDS1,JCBIPEND   INDICATE PENDING ITEM(S)                     
         MVC   PNAR,=CL33'JCBEL ELEMENT UPDATED'                                
         B     ACCL48                                                           
*                                                                               
ACCL46   LA    RE,WORK2            CREATE A JCBEL                               
         XC    WORK2,WORK2                                                      
         MVI   JCBEL,JCBELQ                                                     
         MVI   JCBLN,JCBLNQ                                                     
         MVC   JCBADV,DFJCBADV                                                  
         MVC   JCBALL,DFJCBALL                                                  
         MVC   JCBWOF,DFJCBWOF                                                  
         MVC   JCBRCV,DFJCBRCV                                                  
         MVC   JCBXFR,DFJCBXFR                                                  
         ZAP   JCBADVAL,DFJCBADA                                                
         OC    DFJCBPV(DFJCBPVL),DFJCBPV                                        
         BZ    *+8                                                              
         OI    JCBINDS1,JCBIPEND   INDICATE PENDING ITEM(S)                     
         GOTO1 VHELLO,PARM,(C'P',ACCFIL),ADACC,WORK2,0                          
         MVC   PNAR,=CL33'JCBEL ELEMENT ADDED'                                  
         CLI   12(R1),0                                                         
         BE    ACCL48                                                           
         MVC   PNAR,=CL33'** CANNOT ADD JCBEL **'                               
         B     ACCL48                                                           
*                                                                               
ACCL48   MVI   MODE,WRITACC        TELL MONACC TO UPDATE ACCOUNT                
         GOTO1 ACREPORT                                                         
*                                                                               
ACCL50   DS    0H                                                               
*                                                                               
ACCLX    CLI   RCTRACE,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,TRACEM                                                        
         XC    DFVALS(DFVALSL),DFVALS                                           
         ZAP   DFJCBADA,=P'0'                                                   
         XC    SVVALS(SVVALSL),SVVALS                                           
         ZAP   SVJCBADA,=P'0'                                                   
         B     EXIT                                                             
         DROP  R2,RE                                                            
         EJECT                                                                  
***********************************************************************         
* PROCTRNS                                                            *         
***********************************************************************         
         SPACE 1                                                                
TRNS     L     R2,ADTRANS                                                       
         LR    R4,R2                                                            
         SH    R2,=Y(ACCORFST)     R2=A(TRANSACTION RECORD)                     
         CLI   RCTRACE,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,TRACEM                                                        
         USING ACKEYD,R2                                                        
         TM    ACSTATUS,TRNSDRFT                                                
         BZ    TRNS10                                                           
         ICM   RF,7,DFASTDRA                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,7,DFASTDRA                                                    
*                                                                               
         USING TRNELD,R4                                                        
         CLI   TRNTYPE,99          TEST ADVANCE OR ESTIMATE BILL                
         BE    *+12                                                             
         CLI   TRNTYPE,199                                                      
         BNE   TRNS10                                                           
         ICM   RF,3,DFJCBADV                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,DFJCBADV                                                    
*                                                                               
         USING PTAELD,R3                                                        
TRNS10   LA    R3,TRNELD           LOOK FOR PTAEL                               
         SR    R0,R0                                                            
TRNS12   IC    R0,PTALN                                                         
         AR    R3,R0                                                            
         CLI   PTAEL,0                                                          
         BE    TRNS30                                                           
         CLI   PTAEL,PTAELQ                                                     
         BNE   TRNS12                                                           
         TM    PTASTAT1,PTASPEND   TEST PENDING ITEM                            
         BZ    TRNS28                                                           
         LA    RF,DFJCBALL                                                      
         CLI   PTATYPE,PTATRAL     TEST REGULAR ALLOCATION                      
         BNE   TRNS14                                                           
         CLI   TRNTYPE,99          TEST ADVANCE OR ESTIMATE BILL                
         BE    *+12                                                             
         CLI   TRNTYPE,199                                                      
         BNE   TRNS20                                                           
         AP    DFJCBADA,PTANET     ADD TO ALLOCATION PENDING ON ADVANCE         
TRNS14   LA    RF,DFJCBWOF                                                      
         CLI   PTATYPE,PTATWOF     TEST WRITE-OFF                               
         BE    TRNS20                                                           
         LA    RF,DFJCBRCV                                                      
         CLI   PTATYPE,PTATWOFR    TEST WRITE-OFF RECOVERY                      
         BE    TRNS20                                                           
         LA    RF,DFJCBXFR                                                      
         CLI   PTATYPE,PTATTRFT    TEST TRANSFER                                
         BNE   TRNS28                                                           
*                                                                               
TRNS20   ICM   RE,3,0(RF)          BUMP RELEVANT COUNTER                        
         LA    RE,1(RE)                                                         
         STCM  RE,3,0(RF)                                                       
*                                                                               
TRNS28   B     TRNS12                                                           
*                                                                               
TRNS30   B     TRNSX                                                            
*                                                                               
TRNSX    B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* LAST                                                                *         
***********************************************************************         
         SPACE 1                                                                
LAST     GOTO1 ACREPORT                                                         
         ICM   RF,15,ERRACCS                                                    
         BZ    LAST02                                                           
         CVD   RF,DUB                                                           
         UNPK  PTOT,DUB                                                         
         OI    PTOT+L'PTOT-1,X'F0'                                              
         B     *+10                                                             
LAST02   MVC   PTOT+L'PTOT-2(2),=C'NO'                                          
         MVC   PTNA(17),=C'ACCOUNTS IN ERROR'                                   
         GOTO1 ACREPORT                                                         
LASTX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TRACE ROUTINE - R2=A(TRANS REC)                                     *         
***********************************************************************         
         SPACE 1                                                                
TRACEM   NTR1                                                                   
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
*                                                                               
         MVC   P+24(ACCORFST),0(R2)                                             
         TR    P+24(ACCORFST),CHRTAB                                            
         LA    R0,ACCORFST                                                      
         GOTO1 VHEXOUT,PARM,(R2),WORK2,(R0),HEXSEP                              
         MVC   PSECOND+24(ACCORFST),WORK2                                       
         MVC   PTHIRD+24(ACCORFST),WORK2+(ACCORFST)                             
         SR    R0,R0                                                            
         ICM   R0,3,ACLENGTH-ACKEYD(R2)                                         
         SH    R0,=Y(ACCORFST)                                                  
         LA    R3,ACCORFST(R2)                                                  
         B     IOTPRT                                                           
*                                                                               
IOTPRT   MVI   SKIPSPEC,C'Y'       PRINT KEY                                    
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         LTR   R0,R0                                                            
         BZ    IOTRACX                                                          
*                                                                               
IOTPRT2  LA    R2,100              PRINT RECORD DATA                            
         CR    R0,R2                                                            
         BH    *+6                                                              
         LR    R2,R0                                                            
         BCTR  R2,0                                                             
         EX    R2,*+4                                                           
         MVC   P+24(0),0(R3)                                                    
         EX    R2,*+4                                                           
         TR    P+24(0),CHRTAB                                                   
         GOTO1 VHEXOUT,PARM,(R3),WORK2,1(R2),HEXSEP                             
         EX    R2,*+4                                                           
         MVC   PSECOND+24(0),WORK2                                              
         LA    R1,WORK2+1(R2)                                                   
         EX    R2,*+4                                                           
         MVC   PTHIRD+24(0),0(R1)                                               
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         LA    R2,1(R2)                                                         
         AR    R3,R2                                                            
         SR    R0,R2                                                            
         BP    IOTPRT2                                                          
*                                                                               
IOTRACX  B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------*            
*        DUMP RECORDS                                              *            
*------------------------------------------------------------------*            
         USING ACTRECD,R5                                                       
DUMPIT   NTR1                                                                   
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    EXIT                                                             
         L     R5,ADACC                                                         
         SR    R8,R8                                                            
         ICM   R8,3,ACTKEY+ACCORLEN                                             
         GOTO1 PRNTBL,DMCB,(5,(R7)),(R5),C'DUMP',(R8),=C'2D',(C'P',PRINX        
               T)                                                               
         B     EXIT                                                             
PRNTBL   DC    V(PRNTBL)                                                        
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'100'                                                         
ACCFIL   DC    C'ACCOUNT'                                                       
HEXSEP   DC    C'SEP'                                                           
HEXTOG   DC    C'TOG'                                                           
CHRTAB   DC    (12*16)C'.'             00-BF    TRANSLATE TABLE FOR             
         ORG   CHRTAB+X'40'                     CHARACTER DISPLAY               
         DC    C' '                                                             
         ORG                                                                    
         DC    CL16'.ABCDEFGHI......'  C0-CF                                    
         DC    CL16'.JKLMNOPQR......'  D0-DF                                    
         DC    CL16'..STUVWXYZ......'  E0-EF                                    
         DC    CL16'0123456789......'  F0-FF                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
VHEXOUT  DS    V                                                                
VHELLO   DS    V                                                                
PARM     DS    6F                                                               
ERRACCS  DS    F                                                                
*                                                                               
DFVALS   DS    0X                  ESTABLISHED VALUES                           
DFASTDRA DS    XL3                 DRAFT TRANSACTION COUNT                      
DFJCBV   DS    0X                                                               
DFJCBADV DS    XL2                 ADVANCE ITEMS                                
DFJCBPV  DS    0X                                                               
DFJCBALL DS    XL2                 ALLOCATED ITEMS PENDING                      
DFJCBWOF DS    XL2                 WRITTEN-OFF ITEMS PENDING                    
DFJCBRCV DS    XL2                 WRITE-OFF RECOVERY ITEMS PENDING             
DFJCBXFR DS    XL2                 TRANSFER ITEMS PENDING                       
DFVALSL  EQU   *-DFVALS                                                         
DFJCBPVL EQU   *-DFJCBPV                                                        
DFJCBADA DS    PL6                 ALLOCATION PENDING ON ADVANCE ITEMS          
DFJCBVL  EQU   *-DFJCBV                                                         
*                                                                               
SVVALS   DS    0X                  SAVED VALUES MAP EXACTLY TO DFVALS           
SVASTDRA DS    XL3                 DRAFT TRANSACTION COUNT                      
SVJCBV   DS    0X                                                               
SVJCBADV DS    XL2                 ADVANCE ITEMS                                
SVJCBALL DS    XL2                 ALLOCATED ITEMS PENDING                      
SVJCBWOF DS    XL2                 WRITTEN-OFF ITEMS PENDING                    
SVJCBRCV DS    XL2                 WRITE-OFF RECOVERY ITEMS PENDING             
SVJCBXFR DS    XL2                 TRANSFER ITEMS PENDING                       
SVVALSL  EQU   *-SVVALS                                                         
SVJCBADA DS    PL6                 ALLOCATION PENDING ON ADVANCE ITEMS          
*                                                                               
WORK2    DS    XL256                                                            
         SPACE 3                                                                
*ACREPWORKD                                                                     
*ACGENMODES                                                                     
*ACGENFILE                                                                      
*ACGENBOTH                                                                      
*DDCOMFACSD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDCOMFACSD                                                     
         PRINT ON                                                               
ACWORKD  DSECT                                                                  
         ORG   P                                                                
         DS    XL1                                                              
PACT     DS    CL(L'ACTKULA)                                                    
         DS    XL3                                                              
PNAM     DS    CL(L'NAMEREC)                                                    
         DS    XL3                                                              
PDSV     DS    CL13                                                             
         DS    XL3                                                              
PDAC     DS    CL13                                                             
         DS    XL3                                                              
PNAR     DS    CL33                                                             
         ORG   P                                                                
         DS    XL1                                                              
PTOT     DS    CL13                                                             
         DS    XL4                                                              
PTNA     DS    XL36                                                             
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPDF02 02/01/02'                                      
         END                                                                    
