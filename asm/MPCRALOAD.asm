*          DATA SET MPCRALOAD  AT LEVEL 008 AS OF 02/04/93                      
***********************************************************************         
* THIS VERSION LOADS THE PART STATIC/CURRENT FILESET INTO A           *         
* DATASPACE.                                                          *         
***********************************************************************         
*PHASE MPCLOAD,*                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE TIMEOUT                                                                
*&&      SET    TST=Y                                                           
         TITLE 'CRAFT - PUT MPCFL DATA IN DATASPACE'                            
CRALOAD  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**LOAD**,=V(REGSAVE),RA                              
         USING WORKD,RC                                                         
         ST    R1,APARMS           SAVE A(PARMS)                                
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         EJECT                                                                  
***********************************************************************         
* CONTROL                                                             *         
***********************************************************************         
         SPACE 1                                                                
CONTROL  BAS   RE,INIT                                                          
         BL    CTL100              DELETE REQUEST                               
         BNE   CTL010              REFRESH REQUEST                              
*                                                                               
         MVI   FILEID,NULLQ        EXTRACT FROM STATIC FILE                     
         BAS   RE,EXTRACT                                                       
*                                                                               
CTL010   MVI   FILEID,IDXCURRQ     EXTRACT FROM CURRENT FILE                    
         BAS   RE,EXTRACT                                                       
*                                                                               
CTL100   BAS   RE,FINAL                                                         
         SPACE 1                                                                
CONTROLX XBASE ,                                                                
         SPACE 2                                                                
***********************************************************************         
* VARIOUS EXIT POINTS                                                 *         
***********************************************************************         
         SPACE 1                                                                
EXITHI   CR    RA,RB                                                            
         B     EXIT                                                             
         SPACE 1                                                                
EXITLO   CR    RB,RA                                                            
         B     EXIT                                                             
         SPACE 1                                                                
EXITOK   CR    RB,RB                                                            
         SPACE 1                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         LR    RE,RC                                                            
         LH    RF,=Y(WORKX-WORKD)                                               
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         L     RE,=A(IO1-WORKD)                                                 
         LA    RE,WORKD(RE)                                                     
         ST    RE,AIO                                                           
         ZAP   INRECS,=P'0'                                                     
         ZAP   INBYTES,=P'0'                                                    
         ICM   R1,15,APARMS                                                     
         MVC   DSPARMS,0(R1)                                                    
*                                                                               
         MVC   TITLE(L'PRGTITLE),PRGTITLE                                       
         MVC   MID1+1(13),=C'CONTROL CARDS'                                     
         ZAP   PAGE,=P'1'                                                       
         ZAP   LINE,=P'99'                                                      
         CLI   DSACTION,X'FF'      TEST FOR DELETE REQUEST                      
         BE    INIT900                                                          
         SPACE 1                                                                
***********************************************************************         
* OPEN FILES AND CALC INDEX SIZE                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT100  L     RF,=A(UTL)                                                       
         MVI   4(RF),5             MPL SE NUMBER                                
         GOTO1 VDATAMGR,DMCB,DMOPEN,=C'MPL',FILIST,AIO                          
         SPACE 1                                                                
         CLI   DSACTION,X'01'      TEST FOR REFRESH REQUEST                     
         BE    INIT500                                                          
         SPACE 1                                                                
         GOTO1 VDATAMGR,DMCB,GETREC,MPCFLC,=X'00010101',AIO,DMWORK              
         CLI   8(R1),NULLQ                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO                                                           
         USING HDRRECD,R3                                                       
         ICM   R2,15,HDRSNREC      GET RECORD COUNT                             
         GOTO1 (RF),(R1),,MPCFLS                                                
         CLI   8(R1),NULLQ                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO                                                           
         A     R2,HDRSNREC                                                      
*&&TST                                                                          
         LA    R2,20                                                            
*&&                                                                             
         ST    R2,RECCOUNT                                                      
         LA    R2,5(R2)            INCASE HDR/TRL NOT COUNTED + SPARE           
         SLL   R2,3                *8 FOR INDEX AREA                            
         ST    R2,LINDEX                                                        
*                                                                               
         MVC   LOWDATE,=X'B8E1'    <<<<<< SET DYNAMICALLY >>>>>                 
         SPACE 1                                                                
***********************************************************************         
* ESTABLISH DATASPACE AND GET ALET                                    *         
***********************************************************************         
         SPACE 1                                                                
INI300   SAC   512                                                              
         SYSSTATE ASCENV=AR                                                     
         BAS   RE,M31SET                                                        
         MVC   FULL,=AL4((1024*1024*1024)/4096)                                 
*&&TST                                                                          
         MVC   FULL,=AL4(10)                                                    
*&&                                                                             
         DSPSERV CREATE,NAME=DSMPCTXT,STOKEN=DSMPCFL,BLOCKS=(FULL),    *        
               ORIGIN=DSMPCOR                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE 2                                                                
         ALESERV ADD,STOKEN=DSMPCFL,ALET=DSMPCALE                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE 2                                                                
         ZAP   LINE,=P'99'         FORCE NEW PAGE NEXT PRINT LINE               
         MVC   MID1,SPACES                                                      
         SAC   0                                                                
         SYSSTATE ASCENV=P                                                      
         BAS   RE,M24SET                                                        
         B     INITXOK                                                          
         SPACE 1                                                                
***********************************************************************         
* REFRESH DATASPACE (IE RELOAD CURRENT FILE ONLY)                     *         
***********************************************************************         
         SPACE 1                                                                
INIT500  SAC   512                                                              
         SYSSTATE ASCENV=AR                                                     
         BAS   RE,M31SET                                                        
         LAM   R6,R6,DSMPCALE      SET ACCESS REG FOR MPCFIL                    
         SR    R6,R6                                                            
         USING INDEXD,R6                                                        
         MVC   EINDEX,IDXALAST     SET A(END OF INDEX AREA)                     
         LH    R6,=Y(IDXPRELQ)     SKIP PREAMBLE                                
INIT510  TM    IDXDA,IDXCURRQ      SCAN FOR 1ST CURRENT FILE REC                
         BNZ   INIT520                                                          
         LA    R6,INDEXLQ(,R6)                                                  
         B     INIT510                                                          
INIT520  ST    R6,ALASTIDX         SET START POINT FOR CURRENT FILE             
         MVC   ALASTREC,IDXAREC                                                 
         SAC   0                                                                
         SYSSTATE ASCENV=P                                                      
         BAS   RE,M24SET                                                        
         B     EXITHI                                                           
         DROP  R6                                                               
         SPACE 1                                                                
***********************************************************************         
* DELETE ALET AND DATASPACE                                           *         
***********************************************************************         
         SPACE 1                                                                
INIT900  SAC   512                                                              
         SYSSTATE ASCENV=AR                                                     
         BAS   RE,M31SET                                                        
         ALESERV DELETE,ALET=DSMPCALE                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         DSPSERV DELETE,STOKEN=DSMPCFL                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         SAC   0                                                                
         SYSSTATE ASCENV=P                                                      
         BAS   RE,M24SET                                                        
*                                                                               
INIT900X B     EXITLO                                                           
         SPACE 2                                                                
INITXOK  B     EXITOK                                                           
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* SHIFT MPCFLS1 FILE INTO DATASPACE                                   *         
***********************************************************************         
         SPACE 1                                                                
EXTRACT  NTR1  ,                                                                
         SAC   512                                                              
         SYSSTATE ASCENV=AR                                                     
         BAS   RE,M31SET                                                        
         LAM   R4,R4,DSMPCALE      SET ACCESS REG FOR MPCFIL                    
         CPYA  R6,R4                                                            
*                                                                               
         MVC   MPCDRX,MPCDRS       ASSUME STATIC (IE FIRST PASS)                
         MVC   MPCFLX,MPCFLS                                                    
         CLI   FILEID,IDXCURRQ                                                  
         BNE   EXT002                                                           
         MVC   MPCDRX,MPCDRC                                                    
         MVC   MPCFLX,MPCFLC                                                    
         ICM   R6,15,ALASTIDX      SET TO END OF STATIC FILE                    
         ICM   R4,15,ALASTREC                                                   
         B     EXT004                                                           
*                                                                               
EXT002   L     R6,DSMPCOR          SET ORIGIN ADDRESS                           
         USING INDEXD,R6                                                        
         LR    R4,R6                                                            
         L     R1,LINDEX           SET  A(DATA)                                 
         LA    R4,IDXPRELQ(R1,R6)                                               
         ST    R4,EINDEX                                                        
         ST    R4,IDXALAST         SAVE A(END OF INDEX AREA)                    
         LA    R6,IDXPRELQ(,R6)    BUMP PAST PREAMBLE                           
*                                                                               
EXT004   SAC   0                                                                
         SYSSTATE ASCENV=P                                                      
         LA    R2,KEY                                                           
         USING MPCRECD,R2                                                       
         XC    KEY,KEY                                                          
         MVI   MPCRECT,HHPRECTQ    SKIP HEADER REC                              
*                                                                               
         CLI   FILEID,NULLQ        STATIC FILE - SET LOW VIEWING DATE           
         BNE   EXT006                                                           
         MVI   MPCRECT,VEWRECTQ                                                 
         MVC   VEWDATE-VEWRECD(,R2),LOWDATE                                     
*                                                                               
EXT006   BAS   RE,M24SET                                                        
         GOTO1 VDATAMGR,DMCB,DMHIGH,MPCDRX,MPCRECD,MPCRECD                      
         L     R3,AIO                                                           
         B     EXT020                                                           
EXT010   GOTO1 VDATAMGR,DMCB,DMSEQ,MPCDRX,MPCRECD,MPCRECD                       
         CLI   8(R1),NULLQ                                                      
         BE    EXT020                                                           
         CLI   8(R1),X'80'         EOF ONLY ACCEPTABLE ERROR                    
         BE    EXTRACTX                                                         
         DC    H'0'                                                             
         SPACE 1                                                                
EXT020   TM    MPCDSTAT,MPCPASSQ   SKIP PASSIVES                                
         BNZ   EXT010                                                           
         CLI   MPCRECT,VEWRECTQ    IGNORE ANYTHING BEYOND VIEWING RECS          
         BH    EXTRACTX                                                         
         GOTO1 VDATAMGR,DMCB,GETREC,MPCFLX,MPCDDA,AIO,DMWORK                    
         CLI   8(R1),NULLQ                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
         USING MPCRECD,R3                                                       
EXT030   AP    INRECS,=P'1'                                                     
*&&TST                                                                          
         CP    INRECS,=P'10'                                                    
         BNH   *+14                                                             
         ZAP   INRECS,=P'0'                                                     
         B     EXTRACTX                                                         
*&&                                                                             
         SAC   512                                                              
         SYSSTATE ASCENV=AR                                                     
         BAS   RE,M31SET                                                        
         STCM  R4,15,IDXAREC       DEFINE INDEX ENTRY FOR RECORD                
         MVC   IDXDA,KEY+(MPCDDA-MPCRECD)                                       
         TM    IDXDA,IDXCURRQ                                                   
         BZ    *+6                                                              
         DC    H'0'                DA HAS X'80' BIT ON - RETHINK INDEX          
         OC    IDXDA(L'FILEID),FILEID                                           
         SR    RF,RF                                                            
         ICM   RF,3,MPCFLEN                                                     
         LR    R5,RF               SAVE LENGTH TO BUMP TABLE                    
*                                                                               
         CPYA  RE,R4                                                            
         LR    RE,R4                                                            
         LR    R1,RF                                                            
         LR    R0,R3                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CVD   R5,DUB                                                           
         AP    INBYTES,DUB                                                      
         LA    R4,4(R4,R5)         SET A(NEXT ENTRY) LEAVE NULL GAP             
         SRL   R4,2                                                             
         SLL   R4,2                                                             
         LA    R6,INDEXLQ(,R6)     BUMP INDEX                                   
         C     R6,EINDEX                                                        
         BL    *+6                                                              
         DC    H'0'                BLOWN INDEX                                  
         SAC   0                                                                
         SYSSTATE ASCENV=P                                                      
         BAS   RE,M24SET                                                        
         B     EXT010                                                           
         SPACE 2                                                                
EXTRACTX ST    R4,ALASTREC                                                      
         ST    R6,ALASTIDX                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RETURN PARAMS AND PRINT MESSAGE                                     *         
***********************************************************************         
         SPACE 1                                                                
FINAL    NTR1  ,                                                                
         CLI   DSACTION,X'FF'      TEST FOR DELETE REQUEST                      
         BE    FIN020                                                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMCLOSE,=C'MPL',FILIST,AIO                         
         CLI   DSACTION,X'01'      TEST FOR REFRESH REQUEST                     
         BE    FIN010                                                           
         SAC   512                                                              
         SYSSTATE ASCENV=AR                                                     
         BAS   RE,M31SET                                                        
         LAM   R6,R6,DSMPCALE                                                   
         USING INDEXD,R6                                                        
         ICM   R6,15,DSMPCOR                                                    
         MVC   IDXLABEL,=C'**CRAFT*'                                            
         MVC   IDXLODT,LOWDATE                                                  
         CVB   R1,INRECS                                                        
         STCM  R1,15,IDXNRECS      SET RECORD COUNT                             
         ICM   R1,15,ALASTIDX      SET A(END OF USED INDEX)                     
         SH    R1,=Y(INDEXLQ)                                                   
         STCM  R1,15,IDXAEND                                                    
         SAC   0                                                                
         SYSSTATE ASCENV=P                                                      
         BAS   RE,M24SET                                                        
*                                                                               
         MVC   P+1(BLDMS1LQ),BLDMS1                                             
         EDIT  (B4,RECCOUNT),(10,P+1)                                           
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+1(BLDMS2LQ),BLDMS2                                             
         EDIT  (P8,INRECS),(10,P+1)                                             
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+1(BLDMS3LQ),BLDMS3                                             
         EDIT  (P8,INBYTES),(10,P+1)                                            
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+1(BLDMS4LQ),BLDMS4                                             
         GOTO1 VDATCON,DMCB,(X'42',LOWDATE),(X'08',P+26)                        
         OI    P+26,ZEROQ                                                       
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+1(07),=C'STOKEN='                                              
         GOTO1 VHEXOUT,DMCB,DSMPCFL,P+8,8,0                                     
         GOTO1 VPRINTER                                                         
         MVC   P+1(05),=C'ALET='                                                
         GOTO1 VHEXOUT,DMCB,DSMPCALE,P+6,8,0                                    
         GOTO1 VPRINTER                                                         
         B     FIN099                                                           
*                                                                               
FIN010   MVC   P+1(REFMSGLQ),REFMSG                                             
         EDIT  (P8,INRECS),(10,P+1)                                             
         GOTO1 VPRINTER                                                         
         B     FIN099                                                           
         SPACE 1                                                                
FIN020   MVC   P+1(17),=C'DATASPACE DELETED'                                    
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
FIN099   L     R1,APARMS           RETURN PARAMS                                
         MVI   DSACTION,NULLQ                                                   
         MVC   0(DSPARMLQ,R1),DSPARMS                                           
         SPACE 1                                                                
FINALX   B     EXITOK                                                           
         SPACE 2                                                                
         EJECT                                                                  
* SET 24BIT ADDRESSING MODE                                                     
*                                                                               
M24SET   SLL   RE,1                                                             
         SRL   RE,1                                                             
         BSM  0,RE                                                              
         SPACE 1                                                                
* SET 31BIT ADDRESSING MODE                                                     
*                                                                               
M31SET   ICM   RE,8,=X'80'                                                      
         BSM   0,RE                                                             
         EJECT                                                                  
**********************************************************************          
* TABLES AND CONSTANTS                                               *          
**********************************************************************          
         SPACE                                                                  
VCARDS   DC    V(CARDS)                                                         
VCPRINT  DC    V(CPRINT)                                                        
VDATAMGR DC    V(DATAMGR)                                                       
VDATCON  DC    V(DATCON)                                                        
VPRINTER DC    V(PRINTER)                                                       
VHEXOUT  DC    V(HEXOUT)                                                        
*                                                                               
DMREAD   DC    CL8'DMREAD'                                                      
DMOPEN   DC    CL8'DMOPEN'                                                      
DMCLOSE  DC    CL8'DMCLSE'                                                      
DMHIGH   DC    CL8'DMRDHI'                                                      
DMSEQ    DC    CL8'DMRSEQ'                                                      
GETREC   DC    CL8'GETREC'                                                      
*                                                                               
MPCDRC   DC    C'MPCDRC'                                                        
MPCFLC   DC    C'MPCFLC'                                                        
MPCDRS   DC    C'MPCDRS'                                                        
MPCFLS   DC    C'MPCFLS'                                                        
FILIST   DC    C'NMPCDRC '                                                      
         DC    C'NMPCFLC '                                                      
         DC    C'NMPCDRS '                                                      
         DC    C'NMPCFLS '                                                      
         DC    C'X'                                                             
PRGTITLE DC    C'CRAFT   - LOAD CRAFT DATABASE INTO DATASPACE'                  
BLDMS1   DC    C'.......... RECORDS ON FILE.'                                   
BLDMS1LQ EQU   *-BLDMS1                                                         
BLDMS2   DC    C'.......... RECORDS LOADED.'                                    
BLDMS2LQ EQU   *-BLDMS2                                                         
BLDMS3   DC    C'.......... BYTES LOADED.'                                      
BLDMS3LQ EQU   *-BLDMS3                                                         
BLDMS4   DC    C'LOADED VIEWING DATA FROM DDMMMYY ONWARDS'                      
BLDMS4LQ EQU   *-BLDMS4                                                         
REFMSG   DC    C'.......... RECORDS REFRESHED'                                  
REFMSGLQ EQU   *-REFMSG                                                         
DSMPCTXT DC    CL8'MPCFLC  '                                                    
APARMS   DC    A(0)                                                             
         EJECT                                                                  
**********************************************************************          
* LITERAL POOL                                                       *          
**********************************************************************          
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER W/STORAGE                                           *          
**********************************************************************          
         SPACE 1                                                                
WORKD    DSECT                                                                  
EINDEX   DS    A                   A(END OF INDEX TABLE)                        
LINDEX   DS    XL4                 L'(INDEX TO DATASPACE)                       
ALASTIDX DS    A                   A(NEXT ENTRY IN INDEX)                       
ALASTREC DS    A                   A(NEXT RECORD ENTRY)                         
DSPARMS  DS    0XL(DSPARMLQ)                                                    
DSMPCFL  DS    CL8                 STOKEN FOR MPCFLC DATASPACE                  
DSMPCALE DS    XL4                 ADDRESS LIST ENTRY TOKEN FOR MPCFLC          
DSMPCOR  DS    XL4                 ORIGIN OF MPCFLC DATASPACE                   
DSACTION DS    XL1                                                              
DSPARMLQ EQU   *-DSMPCFL                                                        
DMCB     DS    6F                                                               
DMWORK   DS    12D                                                              
AIO      DS    A                                                                
DUB      DS    D                                                                
INRECS   DS    PL8                                                              
INBYTES  DS    PL8                                                              
RECCOUNT DS    XL4                                                              
FULL     DS    F                                                                
MPCDRX   DS    CL6                                                              
MPCFLX   DS    CL6                                                              
FILEID   DS    CL1                                                              
WORK     DS    CL80                                                             
LOWDATE  DS    XL2                 LOW VIEWING DATE FOR DATASPACE               
*                                                                               
KEY      DS    XL(MPCDLQ)                                                       
IO1      DS    CL2002                                                           
WORKX    EQU   *                                                                
         EJECT                                                                  
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* MPCRAEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE MPCRAEQUS                                                      
         PRINT ON                                                               
* DDSCANBLKD                       TO RESOLVE REFERENCES IN MPEQUATES           
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* MPCRAFILD                                                                     
         PRINT OFF                                                              
       ++INCLUDE MPCRAFILD                                                      
         PRINT ON                                                               
* MPCRAGEND                                                                     
         PRINT OFF                                                              
       ++INCLUDE MPCRAGEND                                                      
         PRINT ON                                                               
* MPCRAIDXD                                                                     
         PRINT OFF                                                              
       ++INCLUDE MPCRAIDXD                                                      
         PRINT ON                                                               
         EJECT                                                                  
         DC    C'**WORK**'                                                      
REGSAVE  CSECT                                                                  
         DS    (20000+(WORKX-WORKD))C                                           
         DC    C'***UTL**'                                                      
UTL      CSECT                                                                  
         DC    256X'00'                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008MPCRALOAD 02/04/93'                                      
         END                                                                    
