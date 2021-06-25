*          DATA SET REPRO27S   AT LEVEL 003 AS OF 01/28/97                      
*&&      SET   NOP=N                                                            
*PHASE T80A27B                                                                  
T80A27   TITLE 'REPRO27 - GET DEMOS AND FOOTNOTES'                              
***********************************************************************         
* INPUT:  PARAMETER 1 -  A(WORKD)                - NEWFILE WORKING STRG         
*         PARAMETER 2 -  A(STA)                  - STATION TEXT                 
*         PARAMETER 3 -  A(BOOKLIN)              - FOR PROPOSAL                 
*         PARAMETER 4 -  A(SAVDMOS)              - PRIME FOR PROPOSAL           
*         PARAMETER 5 -  A(DAYS & TIMES)         - FOR DETAIL LINE              
*                                                                               
* OUTPUT:                                                                       
*         AIO5        - 7X12 LIST OF DEMO VALUES                                
*                     - LIST OF PROGRAM NAMES                                   
*                         1 BYTE LEN + 1 BYTE BKIORD + N BYTES TEXT             
*                                                                               
* NOTES:                                                                        
*         THIS ROUTINE USES AIOB(AIOREC) TO BUILD THE FETCH PARAMETER           
*         UPGRADE EXPRESSIONS ARE BUILT RFTBLKL INTO AIOREC                     
*         USES AIO5 TO BUILD DUMMY RECORDS FROM DETAIL CLUSTERS                 
*         MODULE ASSUMES MINIO HAS BEEN INITIALIZED IN AIO7                     
*         PASSES AIO1-4 AND AIO6 TO FETCH                                       
**********************************************************************          
         EJECT                                                                  
PRO27    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 DLWORKL,REPR27**,R7,RR=RE,CLEAR=YES                              
         USING DLWORKD,RC                                                       
         L     R9,0(R1)    <========+                                           
         USING WORKD,R9             º                                           
*                                   º                                           
         L     RA,ATWA              º                                           
         USING TWAD,RA              º                                           
         L     R8,AGWORK            º                                           
         USING GWORKD,R8            º                                           
*                                   º                                           
         L     R2,AIO5         <----+--------SAVE SOME STORAGE                  
         LA    R3,IOAREALN          º                                           
         SR    R4,R4                º                                           
         SR    R5,R5                º                                           
         MVCL  R2,R4                º                                           
*                                   º                                           
         L     R2,AIOREC       <----+--------SAVE SOME STORAGE                  
         LA    R3,IOAREALN          º                                           
         SR    R4,R4                º                                           
         SR    R5,R5                º                                           
         MVCL  R2,R4                º                                           
*                                   º                                           
         ST    RE,DLRELO            SO WE DON'T MESS WITH 23'S SVRELO           
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
*                                                                               
         L     R4,AIOREC           CLEAR THE BLOCK                              
         LH    R5,=Y(IOAREALN)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R4,RE                                                            
         L     R5,AIO5                                                          
         USING DATABLCK,R5                                                      
         L     R4,AIOREC                                                        
*                                                                               
         USING RFTBLKD,R4                                                       
         MVC   RFTACOM,ACOM                    A(COMFACS)                       
         MVC   RFTAIO1,AIO1                    A(2K IO AREA)                    
         MVC   RFTAIO2,AIO6                    A(2K IO AREA)                    
         MVC   RFTAWRK,AIO2                    A(6K WORK AREA)                  
*                                              USES AIO2,AIO3, & AIO4           
         LA    RE,PRBKHOOK                                                      
         STCM  RE,15,RFTHOOKA                           HOOK ROUTINE            
         MVI   RFTCNTL,RFTCHDRQ+RFTCDEMQ+RFTCSLVQ       DATA FLAGS              
         OI    RFTCNTL,RFTCFTNQ     & FOOTNOTE TEST                             
         MVC   RFTCREP,CUAALF                           REP CODE                
         MVI   RFTCSRC,C'N'                                                     
*                                                                               
         EJECT                                                                  
*--------------------------------------------------------                       
*            DEMO                                                               
*--------------------------------------------------------                       
         L     RE,SVPARMS4                                                      
         USING DEMOLIN,RE                                                       
         LA    R3,RFTCDEMS                                                      
         MVC   0(L'RFTCDEMS,R3),DMLNDEMO                                        
         MVI   0(R3),0             ZAP FIRST BYTE - KLUGE                       
         DROP  RE                                                               
*---------*                                                                     
* STATION *                                                                     
*---------*                                                                     
         L     RF,SVPARMS2                                                      
         MVC   RFTCSTAT,0(RF)                 STATION CALL LETTERS              
*                                                                               
**********************                                                          
** DAY/TIME REFETCH **                                                          
**********************                                                          
         MVI   RFTAMODE,RFTADIRQ                        FETCH MODE              
         L     RE,SVPARMS5                                                      
         LA    RF,RFTCDTMS                                                      
*                                                                               
DLNDT02  OC    0(5,RE),0(RE)                                                    
         BZ    DLNDTX                                                           
         MVC   1(5,RF),0(RE)                                                    
         LA    RE,5(RE)                                                         
         LA    RF,RFTCDTLQ(RF)                                                  
         LA    R0,RFTCDTMS+(8*RFTCDTLQ)                                         
         CR    RF,R0                                                            
         BL    DLNDT02                                                          
*                                                                               
DLNDTX   DS    0H                                                               
*-------*                                                                       
* BOOKS *                                                                       
*-------*                                                                       
DLNBK0   DS    0H                                                               
         L     RE,SVPARMS3                                                      
         LA    RF,(NUMBKS*BKLNLENQ)(RE)                                         
         USING BOOKLIN,RE                                                       
         LA    R3,RFTCBKS                                                       
         XC    FTCHUPGD,FTCHUPGD                                                
         LA    R2,FTCHUPGD                                                      
*                                                                               
DLNBK10  DS    0H                                                               
         OC    BKLNUPGD,BKLNUPGD          UPGRADE?                              
         BNZ   DLNBK30                    YES                                   
*                                                                               
         OC    BKLNBK,BKLNBK                                                    
         BZ    DLNBK40                                                          
*                                                                               
         MVC   0(L'BKLNBK,R3),BKLNBK                                            
         MVC   L'BKLNBK+L'BKLNFIL(L'BKLNSPBK,R3),BKLNSPBK                       
         MVC   L'BKLNBK(L'BKLNFIL,R3),BKLNFIL                                   
*                                                                               
         TM    BKLNBK,RPRBKSES+RPRBKSPJ+RPRBKST2+RPRBKSTP                       
         BZ    *+14                 SKIP E/P/T/S BOOKS                          
         XC    0(L'RFTCBKS,R3),0(R3)                                            
         B     DLNBK40                                                          
*                                                                               
         MVI   L'BKLNBK(R3),RPRBKTPQ                                            
*                                                                               
DLNBK22  LA    R3,L'RFTCBKS(R3)                                                 
         B     DLNBK40                                                          
*                                                                               
DLNBK30  DS    0H                           USER BOOK                           
         LA    R0,FTCHUPGD+L'FTCHUPGD-1                                         
         CR    R2,R0                                                            
         BNL   DLNBK40             TOO MANY SKIP                                
*                                                                               
         MVC   0(L'BKLNBK,R2),BKLNBK                                            
         MVI   L'BKLNBK(R2),RPRBKTPQ                                            
         MVC   L'BKLNBK+L'BKLNFIL(L'BKLNSPBK,R2),BKLNSPBK                       
         MVC   L'BKLNBK+L'BKLNFIL+L'BKLNSPBK(L'BKLNXBKS,R2),BKLNXBKS            
         LA    R2,L'BKLNBK+L'BKLNFIL+L'BKLNSPBK+L'BKLNXBKS(R2)                  
         MVI   0(R2),X'05'                                                      
         MVI   1(R2),14                                                         
*                                                                               
         MVC   2(L'BKLNUPGD,R2),BKLNUPGD                                        
         LA    R2,2+L'BKLNUPGD(R2)                                              
*                                                                               
DLNBK40  LA    RE,BKLNLENQ(RE)                                                  
         CR    RE,RF                                                            
         BL    DLNBK10                                                          
         DROP  RE                                                               
         XC    RFTCUPGA,RFTCUPGA                                                
         LA    RE,FTCHUPGD                                                      
         CR    RE,R2                                                            
         BE    *+8                                                              
         STCM  RE,15,RFTCUPGA                                                   
*                                                                               
DLNBKX   DS    0H                                                               
         OC    RFTCUPGA,RFTCUPGA                                                
         BNZ   *+14                                                             
         OC    RFTCBKS(7*RFTCBKLQ),RFTCBKS                                      
         BZ    DLNMDX                                                           
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------                       
*            FETCH CALL - FOR UPDATING EXISTING DETAIL CLUSTERS                 
*--------------------------------------------------------                       
DLNCL50  DS    0H                                                               
         GOTO1 VFETCH,DMCB,AIOREC                                               
*                                                                               
DLNMDX   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FETCH HOOK ROUTINE                                                            
***********************************************************************         
PRBKHOOK NTR1                                                                   
         L     R5,AIO5                                                          
         USING DATABLCK,R5                                                      
         L     R4,AIOREC                                                        
         USING RFTBLKD,R4                                                       
         OC    RFTERR,RFTERR                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RFTMODE,RFTNBKQ     NEW BOOK DATA?                               
         BNO   PRHOOKX             NO                                           
*                                                                               
         NI    MISCFLG1,FF-MF1NEWDM                                             
         L     R3,SVPARMS3         REGULAR BOOK MATCH                           
         LA    RE,(NUMBKS*BKLNLENQ)(R3)                                         
         OC    RFTFBK,RFTFBK       WAS IT A BOOK?                               
         BZ    FH2BKU              NO - ITS AN UPGRADE                          
*                                                                               
         USING BOOKLIN,R3                                                       
FH2BKB   CLC   RFTFBK,BKLNFBK      FIND BOOK                                    
         BE    FH2BKB2             YES                                          
**       CLI   RFTAMODE,RFTADIRQ   DAY/TIME FETCH?                              
**       BNE   FH2BKB4             NO - THEN IT SHOULD HAVE MATCHED             
         CLC   RFTFBK(L'BKLNBK),BKLNFBK                                         
         BNE   FH2BKB4                                                          
         CLC   RFTFBKSV,BKLNSPBK                                                
         BNE   FH2BKB4                                                          
*                                                                               
FH2BKB2  OC    BKLNUPGD,BKLNUPGD   UPGRADE?                                     
         BE    FH2BK5              NO - MATCHED                                 
FH2BKB4  LA    R3,BKLNLENQ(R3)                                                  
         CR    R3,RE                                                            
         BL    FH2BKB                                                           
         DC    H'0'                SHOULD HAVE MATCHED                          
*                                                                               
UBKL1Q   EQU   BKLNFIL-BKLNUPBK                                                 
UBKL2Q   EQU   UBKL1Q+L'BKLNFIL                                                 
*                                                                               
FH2BKU   DS    0H                                                               
         LA    R0,1                INTERNAL ORDER #                             
         L     RF,RFTFUPGA                                                      
FH2BKU2  CLC   BKLNUPBK(UBKL1Q),0(RF)                                           
         BNE   FH2BKU4                                                          
         CLC   BKLNUPBK+UBKL2Q(L'BKLNUPBK-UBKL2Q),UBKL2Q(RF)                    
         BNE   FH2BKU4                                                          
         CLC   BKLNUPGD,2+L'BKLNUPBK(RF)                                        
         BE    FH2BK5              MATCHED                                      
FH2BKU4  LA    R3,BKLNLENQ(R3)                                                  
         CR    R3,RE                                                            
         BL    FH2BKU2                                                          
         DC    H'0'                SHOULD HAVE MATCHED                          
*                                                                               
FH2BK5   DS    0H                                                               
         ZIC   R0,BKLNIORD                                                      
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         STC   R1,BYTE1                                                         
         DROP  R3                                                               
*                                                                               
**************************************                                          
* ADD PROGRAM TEXT TO AIO5                                                      
**************************************                                          
         LA    R6,PROGTXT                                                       
         CLI   RFTFTX1N,0          ANY LINES?                                   
         BE    FH2PRGX             FETCH SAYS NO                                
FH2PRG2  CLI   0(R6),0                                                          
         BE    FH2PRG4                                                          
         ZIC   R1,0(R6)                                                         
         AR    R6,R1                                                            
         B     FH2PRG2                                                          
*                                                                               
FH2PRG4  DS    0H                                                               
         L     RE,RFTFTX1A                                                      
         LTR   RE,RE                                                            
         BZ    FH2PRGX                                                          
         LA    RE,131(RE)                                                       
FH2PRG6  CLI   0(RE),C' '                                                       
         BH    FH2PRG8                                                          
         BCTR  RE,0                                                             
         C     RE,RFTFTX1A                                                      
         BNL   FH2PRG6                                                          
         B     FH2PRGX                                                          
*                                                                               
FH2PRG8  L     RF,RFTFTX1A                                                      
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R6),0(RF)       MOVE TEXT                                    
         LA    RE,3(RE)                                                         
         STC   RE,0(R6)            STORE LENGTH                                 
         STC   R0,1(R6)            STORE INTERNAL BOOK NUMBER                   
*                                                                               
FH2PRGX  DS    0H                                                               
*                                                                               
*------------------------------------------------------------                   
*        ADD DEMO DATA TO AIO5                                                  
*------------------------------------------------------------                   
FH2BK10  DS    0H                                                               
         LA    R3,DMOVALS          FIRST DEMO VALUE                             
*                                                                               
         ZIC   R1,BYTE1                                                         
         LR    RE,R1                                                            
         MH    R1,=Y(L'DMOVAL)                                                  
         LA    R3,DMOVALS(R1)     WHERE THESE VALUES GO                         
*                                                                               
         MVC   0(L'RFTFDEMS,R3),RFTFDEMS                                        
         MVC   4(L'RFTFSHRS,R3),RFTFSHRS                                        
         MVC   8(L'RFTFLVLS,R3),RFTFLVLS                                        
*--------------------------------------------------                             
FH2BX    DS    0H                                                               
**************************************                                          
PRHOOKX  B     EXITOK                                                           
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
EXITL    MVI   BCDUB,0             SET CC LOW                                   
         B     EXITCC                                                           
EXITH    MVI   BCDUB,2             SET CC HIGH                                  
         B     EXITCC                                                           
EXITOK   MVI   BCDUB,1             SET CC EQUAL                                 
EXITCC   CLI   BCDUB,1                                                          
*                                                                               
EXIT     L     R1,CALLR1           RETURN PARAMS TO CALLER                      
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
*                                                                               
REPDIR   DC    CL8'REPDIR'                                                      
REPFIL   DC    CL8'REPFIL'                                                      
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         GETEL R6,RCONELEM-RCONKEY,ELCODE                                       
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
DLWORKD  DSECT                                                                  
DMCB     DS    6F                                                               
SAVRE    DS    F                                                                
DLRELO   DS    A                                                                
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
*                                                                               
LASTDMO  DS    A                                                                
*                                                                               
HALF1    DS    H                                                                
HALF2    DS    H                                                                
BYTE1    DS    X                                                                
BYTE2    DS    X                                                                
BYTE3    DS    X                                                                
*                                                                               
ELCODE   DS    X                                                                
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS                          
MF1DMFFT EQU   X'80'                - NEED DEMO FETCH ON DETAIL                 
MF1MNWRT EQU   X'40'                - NEED MINOWRT                              
MF1NEWDM EQU   X'20'                - NEW DEMO ELEMENT                          
*                                                                               
MNIOFLAG DS    XL1                                                              
MNIOCLSQ EQU   X'80'                                                            
*                                                                               
DLWORKL  EQU   *-DLWORKD                                                        
         EJECT                                                                  
DATABLCK DSECT                                                                  
DMOVALS  DS    0XL(7*12)                                                        
DMOVAL   DS    7XL12                                                            
PROGTXT  DS    0CL1                                                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE REPROLN                                                        
         EJECT                                                                  
* REPROWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE REPROWORK                                                      
         PRINT ON                                                               
* REFETCHD                                                                      
         PRINT OFF                                                              
       ++INCLUDE REFETCHD                                                       
         PRINT ON                                                               
         ORG   RFTBLKD+RFTBLKL                                                  
FTCHUPGD DS    XL(7*(11+14))                                                    
         DS    XL1                 END OF UPGRADES                              
         DS    XL(IOAREALN-(*-RFTBLKD))                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003REPRO27S  01/28/97'                                      
         END                                                                    
