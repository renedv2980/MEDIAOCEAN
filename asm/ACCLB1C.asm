*          DATA SET ACCLB1C    AT LEVEL 213 AS OF 08/16/00                      
*PHASE T6211CA                                                                  
CLB1C    TITLE '- ACTION HELP'                                                  
CLB1C    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB1C**,R8,R7,CLEAR=YES,RR=RE                                 
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK         RC=A(LOCAL WORKING STORAGE)                  
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         L     R5,ALSVALS                                                       
         LH    R6,=Y(BSDICT-TWAD)                                               
         LA    R6,TWAD(R6)                                                      
         USING BSDICT,R6                                                        
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     LSTFRST             FIRST FOR THIS LIST                          
         B     SCRFRST             FIRST FOR THIS SCREEN                        
         B     EXITY               LAST FOR THIS SCREEN                         
         B     EXITY               FIRST FOR VALIDATE THIS LINE                 
         B     EXITN               VALIDATE COLUMN                              
         B     EXITY               LAST FOR VALIDATE THIS LINE                  
         B     DISCLM              DISPLAY COLUMN                               
         B     GETFRST             GET FIRST RECORD FOR LIST                    
         B     GETNEXT             GET NEXT RECORD                              
         B     EXIT                SET UP MY OWN HEADING                        
         B     VALSEL              VALIDATE SELECT TABLE                        
         B     EXIT                DISPLAY COLUMN TOTAL                         
         EJECT                                                                  
***********************************************************************         
* FIRST FOR THIS SCREEN                                               *         
***********************************************************************         
         SPACE 1                                                                
SCRFRST  DS    0H                                                               
         XC    LSCLM,LSCLM         SET UP PERMANENT DIS=1                       
         MVI   LSCLMN,1                                                         
         MVI   LSCLMLST,C' '                                                    
         MVI   LSCLMLST,C'1'                                                    
         B     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY COLUMN - R1 CONTAINS COLUMN ROUTINE NUMBER                  *         
***********************************************************************         
         SPACE 1                                                                
DISCLM   DS     0H                                                              
         SLL   R1,2                                                             
         B     *+4(R1)                                                          
         B     DISACT      00      ACTION                                       
         B     DISDESC     01      DESCRIPTION                                  
         SPACE 1                                                                
***********************************************************************         
* DISPLAY ACTION                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISACT   OI    FVIHDR+FHATD,FHATHI+FHATPR                                       
         CLI   TLKLEV,0                                                         
         BNE   DACT10                                                           
         MVC   FVIFLD(L'TLKACT),TLKACT                                          
         B     EXIT                                                             
*                                                                               
DACT10   CLI   TLKLEV,X'81'                                                     
         BNE   EXIT                                                             
         LH    RE,=Y(LC@RETRN-TWAD)                                             
         LA    RE,TWAD(RE)                                                      
         MVC   FVIFLD(L'LC@RETRN),0(RE)                                         
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* DISPLAY DESCRIPTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISDESC  CLI   TLKLEV,TLKLSPC                                                   
         BE    EXIT                                                             
         LA    R1,BOPARM                                                        
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,TLKHELP                                                  
         MVI   GTMAXL,MIXHELPL                                                  
         OI    GT1INDS,GT1OWRK                                                  
         CLI   TLKLEV,0                                                         
         BE    DDESC02                                                          
         MVC   GTMSGNO,=AL2(5299)                                               
         MVI   GTLTXT,L'TLKACT                                                  
         LA    RE,TLKACT                                                        
         STCM  RE,7,GTATXT                                                      
DDESC02  LA    RE,FVIFLD                                                        
         STCM  RE,7,GTAOUT                                                      
         MVI   GTMTYP,GTMSCR                                                    
         GOTO1 VGETTXT                                                          
         DROP  R1                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST - BUILD LIST OF VALID ACTIONS                        *         
***********************************************************************         
         SPACE 1                                                                
LSTFRST  DS    0H                                                               
         XR    RF,RF                                                            
         IC    RF,TWASESNL                                                      
         SLL   RF,1                                                             
         LA    RF,TWASESRA-L'TWASESRA(RF)                                       
         MVC   CSRECACT,0(RF)                                                   
         GOTO1 ADDPRE                                                           
         GOTO1 ADDPFK                                                           
         GOTO1 ADDTAB                                                           
         GOTO1 ADDACT,BOPARM,('TLKLSPC',=AL1(FF))                               
         MVI   CSREC,RECBIL                                                     
         MVI   CSACT,ACTHLP                                                     
         B     EXITY                                                            
         SPACE 1                                                                
***********************************************************************         
* ADD PREVIOUS SESSION ACTIONS TO LIST                                *         
***********************************************************************         
         SPACE 1                                                                
ADDPRE   NTR1  ,                                                                
         XR    R3,R3                                                            
         ICM   R3,1,TWASESNL                                                    
         BZ    EXIT                                                             
         LA    R2,TWASESRA+L'CSREC                                              
         LA    RF,ADDACT                                                        
         LA    R1,BOPARM                                                        
APRE02   LA    R0,TLKLPRE(R3)                                                   
         GOTO1 (RF),(R1),((R0),(R2))                                            
         LA    R2,L'CSRECACT(R2)                                                
         BCT   R3,APRE02                                                        
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ADD PFK ACTIONS TO LIST (ADDTAB MAY MISS SOME OF THESE)             *         
***********************************************************************         
         SPACE 1                                                                
ADDPFK   NTR1  ,                                                                
         XR    R3,R3                                                            
         ICM   R3,1,TWASESNL                                                    
         BZ    EXIT                                                             
         SLL   R3,1                                                             
         LA    R3,TWASESRA-2(R3)                                                
         L     R4,APFKTAB                                                       
         USING PFKTABD,R4                                                       
         XR    RF,RF                                                            
APFK02   CLI   PFKTABD,EOT                                                      
         BE    EXIT                                                             
         CLC   PFKKEY,0(R3)                                                     
         BE    APFK10                                                           
         ICM   RF,3,PFKLEN                                                      
         BXH   R4,RF,APFK02                                                     
*                                                                               
APFK10   LA    R4,PFKHEADL(R4)                                                  
APFK12   CLI   PFKTABD,EOT                                                      
         BE    ADDPFKX                                                          
         TM    PFKINDS1,PFKIACTN                                                
         BZ    APFK18                                                           
         TM    PFKINDS3,PFKIPOS                                                 
         BO    APFK18                                                           
         GOTO1 ATSTPFK,PFKTABD                                                  
         BNE   APFK18                                                           
         XR    R0,R0                                                            
*        TM    PFKINDS2,PFKISAVS                                                
*        BZ    *+8                                                              
*        LA    R0,1                                                             
         GOTO1 ADDACT,BOPARM,((R0),PFKACTN)                                     
APFK18   LA    R4,PFKDATBL(R4)                                                  
         B     APFK12                                                           
         DROP  R4                                                               
*                                                                               
ADDPFKX  B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* ADD VALID ACTIONS FROM THE ACTION TABLE                             *         
***********************************************************************         
         SPACE 1                                                                
ADDTAB   NTR1  ,                                                                
         L     R2,AACTTAB                                                       
         USING ACTTABD,R2                                                       
ATAB02   CLI   ACTTABD,EOT                                                      
         BE    ADDTABX                                                          
         TM    ACTINDS1,ACTINOH                                                 
         BO    ATAB08                                                           
         GOTO1 ATSTACT,ACTNUMB                                                  
         BNE   ATAB08                                                           
         MVI   BOHALF1,RECBIL                                                   
         MVC   BOHALF1+1(1),ACTNUMB                                             
         GOTO1 ATSTMIX,BOHALF1                                                  
         BNE   ATAB08                                                           
         L     RF,AMIXNTRY                                                      
         USING MIXTABD,RF                                                       
         TM    MIXINDS1,MIXISEL                                                 
         BO    ATAB08                                                           
         XR    R0,R0                                                            
*        TM    MIXINDS2-MIXTABD(RF),MIXIRSET                                    
*        BO    *+8                                                              
*        LA    R0,1                                                             
         DROP  RF                                                               
         GOTO1 ADDACT,BOPARM,((R0),ACTNUMB)                                     
ATAB08   LA    R2,ACTTABL(R2)                                                   
         B     ATAB02                                                           
*                                                                               
ADDTABX  B     EXIT                                                             
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO ADD ACTION TO SAVED LIST                                 *         
*                                                                     *         
* NTRY: P1 BYTE 0 = TLKLEV NUMBER                                     *         
*             1-3 = A(ACTION CODE)                                    *         
* EXIT: LSTACTN / LSTACT UPDATED                                      *         
***********************************************************************         
         SPACE 1                                                                
ADDACT   NTR1  ,                                                                
         XR    R3,R3                                                            
         ICM   R3,7,1(R1)          R3 = A(ACTION CODE)                          
         IC    R4,0(R1)            R4 = TLKLEV                                  
*                                                                               
         XR    R0,R0               TEST ACTION ALREADY ADDED                    
         ICM   R0,3,LSTACTN                                                     
         BZ    AACT02                                                           
         LA    RF,LSTACT                                                        
         CLC   0(1,R3),1(RF)                                                    
         BE    EXIT                                                             
         LA    RF,L'LSTACT(RF)                                                  
         BCT   R0,*-14                                                          
*                                                                               
AACT02   DS    0H                                                               
*ACT02   GOTO1 ATSTACT,(R3)                                                     
*        BNE   EXIT                                                             
*        MVI   BOHALF1,RECBIL                                                   
*        MVC   BOHALF1+1(1),(R3)                                                
*        GOTO1 ATSTMIX,BOHALF1                                                  
*        BNE   EXIT                                                             
*                                                                               
         LH    RF,LSTACTN                                                       
         LA    RE,1(RF)                                                         
         STH   RE,LSTACTN                                                       
         SLL   RF,1                                                             
         LA    RF,LSTACT(RF)                                                    
         STC   R4,0(RF)                                                         
         MVC   1(1,RF),0(R3)                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET FIRST/NEXT LIST RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
GETFRST  XC    LSTACT#,LSTACT#                                                  
GETNEXT  CLC   LSTACT#,LSTACTN                                                  
         BE    EXITN                                                            
         LH    R4,LSTACT#                                                       
         LA    RF,1(R4)                                                         
         STH   RF,LSTACT#                                                       
         SLL   R4,1                                                             
         LA    R4,LSTACT(R4)                                                    
         MVC   TLKLEV,0(R4)                                                     
         CLI   TLKLEV,TLKLSPC                                                   
         BNE   *+12                                                             
         OI    TLINDS1,TLIPRO                                                   
         B     GETNEXTX                                                         
         GOTO1 ATSTACT,1(R4)                                                    
         L     R2,AACTNTRY                                                      
         USING ACTTABD,R2                                                       
         MVI   BOHALF1,RECBIL                                                   
         MVC   BOHALF1+1(L'ACTNUMB),ACTNUMB                                     
         MVC   TLKACT,BCWORK                                                    
         GOTO1 ATSTMIX,BOHALF1                                                  
         L     R3,AMIXNTRY                                                      
         USING MIXTABD,R3                                                       
         MVC   TLKHELP,MIXHELP                                                  
GETNEXTX B     EXITY                                                            
         DROP  R3,R2                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECT (ASSUME IT'S SELECT)                                *         
***********************************************************************         
         SPACE 1                                                                
*ALSEL   ST    RD,BCSVRD (IFFY)                                                 
VALSEL   MVC   BASACT,TLKACT                                                    
         OI    BASACTH+FHOID,FHOITR                                             
         NI    BASACTH+FHIID,FF-FHIIVA                                          
         GOTO1 AXITSES                                                          
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* GENERAL EXIT POINTS                                                 *         
***********************************************************************         
         SPACE 1                                                                
EXITY    CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,FF                                                             
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* FACTRY                                                                        
         PRINT OFF                                                              
       ++INCLUDE FACTRY                                                         
         PRINT ON                                                               
         SPACE 1                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKLEV   DS    XL1                                                              
TLKLPRE  EQU   X'80'               ON IF PREVIOUSLY SAVED ACTION                
TLKLSPC  EQU   X'40'               EQUATE FOR DUMMY BLANK LINE                  
TLKACT   DS    CL8                 ACTION CODE                                  
TLKHELP  DS    XL2                 MIXHELP                                      
         SPACE 1                                                                
*                                                                               
OVERWRKD DSECT                                                                  
LSTACT#  DS    H                                                                
LSTACTN  DS    H                                                                
LSTACT   DS    255XL2                                                           
         DS    0X                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'213ACCLB1C   08/16/00'                                      
         END                                                                    
