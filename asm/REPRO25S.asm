*          DATA SET REPRO25S   AT LEVEL 008 AS OF 12/12/96                      
*&&      SET   NOP=N                                                            
*PHASE T80A25B                                                                  
T80A25   TITLE 'REPRO25 - PRIME BOOK ALL DEMO FETCH'                            
***********************************************************************         
* INPUT:  PARAMETER 1 -  A(WORKD)                - NEWFILE WORKING STRG         
*         PARAMETER 2 -  A(SAVSTAS)              - FOR PROPOSAL                 
*         PARAMETER 3 -  A(BOOKLIN)              - FOR PRIME BOOK               
*         PARAMETER 4 -  A(SAVDMOS)              - FOR PROPOSAL                 
*                                                                               
* OUTPUT:                                                                       
*                                                                               
* NOTES:                                                                        
*         THIS ROUTINE USES AIOB(AIOREC) TO BUILD THE FETCH PARAMETER           
*         UPGRADE EXPRESSIONS ARE BUILT RFTBLKL INTO AIOREC                     
*         USES AIO5 TO BUILD DUMMY RECORDS FROM DETAIL CLUSTERS                 
*         MODULE ASSUMES MINIO HAS BEEN INITIALIZED IN AIO7                     
*         PASSES AIO1-4 AND AIO6 TO FETCH                                       
**********************************************************************          
         EJECT                                                                  
PRO25    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PRWORKL,REPR25**,R7,RR=RE,CLEAR=YES                              
         USING PRWORKD,RC                                                       
         L     R9,0(R1)    <========+                                           
         USING WORKD,R9             º                                           
*                                   º                                           
         L     RA,ATWA              º                                           
         USING TWAD,RA              º                                           
         L     R8,AGWORK            º                                           
         USING GWORKD,R8            º                                           
*                                   º                                           
         L     R2,AIOREC       <----+--------SAVE SOME STORAGE                  
         LA    R3,IOAREALN          º                                           
         SR    R4,R4                º                                           
         SR    R5,R5                º                                           
         MVCL  R2,R4                º                                           
*                                   º                                           
         ST    RE,PRRELO            SO WE DON'T MESS WITH 23'S SVRELO           
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
*                                                                               
         L     R4,AIOREC           CLEAR THE BLOCK                              
         LH    R5,=Y(IOAREALN)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R4,RE                                                            
         L     R4,AIOREC           CLEAR THE BLOCK                              
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
         MVC   RFTCREP,CUAALF                           REP CODE                
*                                                                               
         BAS   RE,GETNDEMS         GET NUMBER OF DEMOS                          
*------------------------------------------------------*                        
*            RETRIEVE CLUSTERS AND DO REQUIRED FETCHES *                        
*------------------------------------------------------*                        
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDTELQ    DETAIL CLUSTER                               
         BAS   RE,MINIOHI                                                       
         BNE   PBKRDX              ALL DONE                                     
*                                                                               
PBKRD10  L     R6,MINELEM                                                       
         CLI   0(R6),RPRDTELQ      DETAIL  ELEMENT?                             
         BNE   PBKRDX              NO - ALL DONE                                
*                                                                               
         LA    R0,RFTCSTAT         CLEAR FETCH BLOCK                            
         LA    R1,RFTMODE-RFTCSTAT                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
***??    TM    MISCFLG1,MF1DMFFT   DEMOS ALL FETCHED?                           
***??    BNZ   PBKCL0              NO                                           
*                                                                               
         L     RE,SVPARMS3                                                      
         USING BOOKLIN,RE                                                       
*                                                                               
PBKRD12  ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             END OF CLUSTER                               
         BE    PBKCL0              YES - BOOK NOT FETCHED                       
         CLI   0(R6),RPRDVELQ      DEMO VALUE ELEMENT?                          
         BNE   PBKRD12             NO                                           
         USING RPRDVELD,R6                                                      
         CLC   RPRDVBNM,BKLNIORD   INTERNAL ORDER MATCH?                        
         BNE   PBKRD12             NO                                           
         CLC   RPRDVLEN,BOBYTE1                                                 
         BL    PBKCL0                                                           
         DROP  RE                                                               
*                                                                               
         BAS   RE,CHKDEMOS                                                      
         BNE   PBKCL0                                                           
*                                                                               
         B     PBKRNXT             READ NEXT CLUSTER                            
         EJECT                                                                  
***********************************************************************         
* GETS THE NUMBER OF DEMOS SO WE CAN FIGURE WHAT LENGTH THE DEMO                
* VALUE ELEMENTS SHOULD BE                                                      
*                                                                               
* ON ENTRY:    SVPARMS4            A(SAVDMOS)                                   
*                                                                               
* ON EXIT:     BOBYTE1             WHAT L(DEMO VALUE ELEMENT) SHOULD BE         
***********************************************************************         
GETNDEMS NTR1                                                                   
         L     RE,SVPARMS4                                                      
         LA    RF,(NUMDEMS*DMLNLENQ)(RE)   END OF TABLE                         
         USING DEMOLIN,RE                                                       
         LA    R3,RFTCDEMS                                                      
         NI    MISCFLG1,FF-MF1DMFFT                                             
         MVI   BOBYTE1,0           HIGHEST INTERNAL ORDER # OF DEMOS            
*                                                                               
PBKDM10  DS    0H                                                               
         OC    DMLNDEMO,DMLNDEMO                                                
         BZ    PBKDM15                                                          
*                                                                               
         CLC   BOBYTE1,DMLNIORD    NEW HIGHEST INTERNAL ORDER #?                
         BNL   *+10                                                             
         MVC   BOBYTE1,DMLNIORD    YES                                          
*                                                                               
         MVC   0(L'RFTCDEMS,R3),DMLNDEMO                                        
         MVI   0(R3),0             ZAP FIRST BYTE - KLUGE                       
*                                                                               
         TM    DMLNFLG,RPRDMFFT                                                 
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1DMFFT   NEED DEMO FETCH                              
*                                                                               
         LA    R3,L'RFTCDEMS(R3)                                                
PBKDM15  LA    RE,DMLNLENQ(RE)                                                  
         CR    RE,RF                                                            
         BL    PBKDM10                                                          
*                                                                               
         ZIC   R0,BOBYTE1                                                       
         MH    R0,=Y(L'RPRDVDMO)                                                
         AH    R0,=Y(L'RPRDVOVQ)                                                
         STC   R0,BOBYTE1                                                       
*                                                                               
PBKDMX   XIT1                                                                   
         DROP  RE                                                               
         EJECT                                                                  
PBKCL0   L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
*---------------------*                                                         
* STATION FOR CLUSTER *                                                         
*---------------------*                                                         
PBKCL10  L     RF,SVPARMS2                                                      
         LA    RE,(NUMSTAS*STLNLENQ)(RF)   END OF TABLE                         
         USING STALIN,RF                                                        
PBKCL12  CLC   RPRDTSTA,STLNIORD                                                
         BE    PBKCL14                                                          
         LA    RF,STLNLENQ(RF)                                                  
         CR    RF,RE                                                            
         BL    PBKCL12                                                          
         DC    H'0'                                                             
*                                                                               
PBKCL14  MVC   RFTCSTAT,STLNSTA               STATION CALL LETTERS              
         CLI   RFTCSTAT+4,C' '                NEED 'T' SET?                     
         BNE   *+8                            NO                                
         MVI   RFTCSTAT+4,C'T'                                                  
         TM    STLNFLG,RPRSTSTL               SATELLITE REQUEST?                
         BZ    *+8                            NO                                
         MVI   RFTCSTAT+4,C'1'                                                  
         MVI   RFTCSRC,C'N'                   DEMO SOURCE                       
*                                                                               
         BAS   RE,INTOAIO5                                                      
*                                                                               
         CLC   RPRDTINM,BCSPACES   INV # FETCH?                                 
         BH    PBKCL30             YES                                          
         EJECT                                                                  
*                                                                               
**********************                                                          
** DAY/TIME REFETCH **                                                          
**********************                                                          
         MVI   RFTAMODE,RFTADIRQ                        FETCH MODE              
         MVC   RFTCDTMS+1(1),RPRDTDAY                   PRIME DAYS              
         MVC   RFTCDTMS+2(4),RPRDTSTM                   PRIME TIMES             
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROHDRD(R6)                                         
         MVI   ELCODE,RPRDYELQ                  SECONDARY DAYS/TIMES            
         BAS   RE,FIRSTEL                                                       
         BNE   PBKCL22                                                          
         ZIC   RE,1(R6)                                                         
         LA    RE,0(RE,R6)         END OF ELEM                                  
         LA    R6,RPRDYDTM-RPRDYELD(R6)                                         
         LA    RF,RFTCDTMS+RFTCDTLQ                                             
PBKCL20  CR    R6,RE                                                            
         BNL   PBKCL22                                                          
         MVC   1(L'RPRDYDTM,RF),0(R6)                                           
         LA    R6,L'RPRDYDTM(R6)                                                
         LA    RF,RFTCDTLQ(RF)                                                  
         LA    R0,RFTCDTMS+(8*RFTCDTLQ)                                         
         CR    RF,R0                                                            
         BL    PBKCL20                                                          
*                                                                               
PBKCL22  DS    0H                                                               
         B     PBKCL40                                                          
         EJECT                                                                  
*                                                                               
************************                                                        
** INVETORY # REFETCH **                                                        
************************                                                        
PBKCL30  MVI   RFTAMODE,RFTAMSTQ                        FETCH MODE              
         MVC   RFTCINV,RPRDTINM    INVENTORY #                                  
*                                                                               
         OC    RPRDTEFF,RPRDTEFF                                                
         BZ    PBKCL32                                                          
         GOTO1 VDATCON,DMCB,(8,RPRDTEFF),(2,RFTCEFST)     EFF START &           
PBKCL32  OC    RPRDTEEF,RPRDTEEF                                                
         BZ    PBKCL34                                                          
         GOTO1 VDATCON,DMCB,(8,RPRDTEEF),(2,RFTCEFEN)     END DATES             
PBKCL34  DS    0H                                                               
         DROP  R6                                                               
PBKCL40  DS    0H                                                               
         SPACE 2                                                                
         BAS   RE,GETNDEMS                                                      
         SPACE 2                                                                
*------------------*                                                            
** THE PRIME BOOK **                                                            
*------------------*                                                            
PBKBK10  DS    0H                                                               
         L     R6,SVPARMS3                                                      
         USING BOOKLIN,R6                                                       
         LA    R3,FTCHUPGD                                                      
         XC    FTCHUPGD,FTCHUPGD                                                
         XC    RFTCUPGA,RFTCUPGA                                                
         OC    BKLNUPGD,BKLNUPGD              UPGRADE EXPRESSION?               
         BNZ   PBKBK12                        YES                               
*                                                                               
         MVC   RFTCBKS(L'BKLNBK),BKLNBK                                         
         MVC   RFTCBKS+L'BKLNBK+L'BKLNFIL(L'BKLNSPBK),BKLNSPBK                  
         MVC   RFTCBKS+L'BKLNBK(L'BKLNFIL),BKLNFIL                              
*                                                                               
         CLI   RFTAMODE,RFTAMSTQ   INVENOTRY FETCH?                             
         BE    PBKBKX              YES                                          
*                                                                               
         CLI   BKLNFIL,RPRBKINQ    DAYTIME INVENTORY SOURCE?                    
         BNE   PBKBKX              NO                                           
*                                                                               
         TM    BKLNBK,RPRBKSES+RPRBKSPJ+RPRBKST2+RPRBKSTP                       
         BZ    *+14                 SKIP E/P/T/S BOOKS                          
         XC    0(L'RFTCBKS,R3),0(R3)                                            
         B     PBKBKX                                                           
         MVI   RFTCBKS+L'BKLNBK,RPRBKTPQ                                        
         B     PBKBKX                                                           
*                                                                               
PBKBK12  DS    0H                                                               
         CLI   RFTAMODE,RFTAMSTQ   INVENOTRY FETCH?                             
         BE    *+12                YES                                          
         CLI   BKLNFIL,RPRBKINQ    DAYTIME INVENTORY SOURCE?                    
         BE    PBKBKX              YES                                          
*                                                                               
         MVC   0(L'BKLNBK,R3),BKLNBK                                            
         MVC   L'BKLNBK(L'BKLNFIL,R3),BKLNFIL                                   
         MVC   L'BKLNBK+L'BKLNFIL(L'BKLNSPBK,R3),BKLNSPBK                       
         MVC   L'BKLNBK+L'BKLNFIL+L'BKLNSPBK(L'BKLNXBKS,R3),BKLNXBKS            
         LA    R3,L'BKLNBK+L'BKLNFIL+L'BKLNSPBK+L'BKLNXBKS(R3)                  
*                                                                               
         MVI   0(R3),X'05'                                                      
         MVI   1(R3),14                                                         
         MVC   2(L'BKLNUPGD,R3),BKLNUPGD                                        
         LA    R0,FTCHUPGD                                                      
         CR    R0,R3                                                            
         BE    *+8                                                              
         STCM  R0,15,RFTCUPGA                                                   
*                                                                               
PBKBKX   DS    0H                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------                       
*            FETCH CALL - FOR UPDATING EXISTING DETAIL CLUSTERS                 
*--------------------------------------------------------                       
PBKCL50  DS    0H                                                               
         NI    MISCFLG1,X'FF'-MF1MNWRT    NO WRT YET                            
         GOTO1 VFETCH,DMCB,AIOREC                                               
*                                                                               
         TM    MISCFLG1,MF1MNWRT   I/O TESTING                                  
         BZ    PBKCL80                                                          
         B     PBKCL60                                                          
*                                                                               
         L     R3,SVPARMS3                                                      
         USING BOOKLIN,R3                                                       
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)    FIRST ELEMENT                         
PBKCL52  CLI   0(R6),0                    END OF CLUSTER?                       
         BE    PBKCL54                    YES - ADD DEMO VALUE ELEM             
         ZIC   RE,1(R6)                   ELEMENT LENGTH                        
         AR    R6,RE                                                            
         CLI   0(R6),RPRDVELQ             DEMO VALUE ELEMENT?                   
         BNE   PBKCL52                    NO                                    
*                                                                               
         USING RPRDVELD,R6                                                      
         CLC   RPRDVBNM,BKLNIORD                                                
         BNE   PBKCL52                                                          
*                                                                               
         TM    MISCFLG1,MF1MNWRT                                                
         BZ    PBKCL80                                                          
         B     PBKCL60                                                          
*                                                                               
*------------------------------------------------------------                   
*        BUILD MARKER ELEMENT                                                   
*------------------------------------------------------------                   
PBKCL54  DS    0H                                                               
         XC    BOELEM,BOELEM                                                    
         LA    R6,BOELEM                                                        
         MVI   RPRDVEL,RPRDVELQ    DEMO VALUE ELEMENT                           
         MVI   RPRDVLEN,RPRDVOVQ+L'RPRDVDMO                                     
         MVC   RPRDVBNM,BKLNIORD   INTERNAL BOOK ORDER #                        
         DROP  R3,R6                                                            
*                                                                               
         L     R6,AIO5                    ADD IT                                
         LA    R6,RPROR1ST-RPROKEY(R6)    FIRST ELEMENT                         
N        USING RPRDVELD,BOELEM                                                  
*                                                                               
PBKCL56  CLI   0(R6),0                    END OF CLUSTER?                       
         BE    PBKCL58                    YES - ADD DEMO VALUE ELEM             
*                                                                               
         ZIC   RE,1(R6)                   ELEMENT LENGTH                        
         AR    R6,RE                                                            
         CLI   0(R6),RPRDVELQ             DEMO VALUE ELEMENT?                   
         BL    PBKCL56                    NO -SKIP                              
         BH    PBKCL58                    NO -WRITE HERE                        
*                                                                               
         USING RPRDVELD,R6                                                      
         CLC   RPRDVBNM,N.RPRDVBNM        BOOK MATCH?                           
         BL    PBKCL56                    NO                                    
         BE    PBKCL60                                                          
         DROP  R6,N                                                             
*                                                                               
PBKCL58  GOTOX (RECUPQ,AREPRO01),DMCB,(C'R',AIO5),BOELEM,0(R6)                  
*                                                                               
***************************                                                     
** WRITE OUT NEW CLUSTER **                                                     
***************************                                                     
PBKCL60  L     R5,AIO7                                                          
         BAS   RE,FROMAIO5                                                      
         XC    MINEKEY,MINEKEY     RESTORE READ SEQUENCE                        
         MVI   MINEKEY,RPRDTELQ                                                 
         L     R6,MINELEM                                                       
         MVC   MINEKEY+1(7),2(R6)                                               
         BAS   RE,MINIORD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,FROMAIO5                                                      
         BAS   RE,MINIOWRT                                                      
*                                                                               
         XC    MINEKEY,MINEKEY     RESTORE READ SEQUENCE                        
         MVI   MINEKEY,RPRDTELQ                                                 
         L     R6,MINELEM                                                       
         MVC   MINEKEY+1(7),2(R6)                                               
         BAS   RE,MINIORD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    MISCFLG1,FF-MF1MNWRT                                             
*                                                                               
PBKCL80  DS    0H                                                               
         SPACE 2                                                                
*                                                                               
*--------------------------------------------------------                       
*            NEXT DETAIL CLUSTER                                                
*--------------------------------------------------------                       
PBKRNXT  BAS   RE,MINIOSEQ                                                      
         BE    PBKRD10                                                          
         SPACE 2                                                                
*                                                                               
*--------------------------------------------------------                       
*            NO CLUSTERS LEFT                                                   
*--------------------------------------------------------                       
PBKRDX   DS    0H                                                               
         EJECT                                                                  
*******************************************                                     
** MARK PRIME BOOK AND ALL DEMOS FETCHED **                                     
*******************************************                                     
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBKELQ                                                 
         L     R3,SVPARMS3                                                      
         USING BOOKLIN,R3                                                       
         MVC   MINEKEY+1(1),BKLNIORD                                            
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,MINELEM                                                       
         CLI   0(R6),RPRBKELQ                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RPRBKELD,R6                                                      
         CLC   RPRBKIOR,BKLNIORD                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    RPRBKFLG,RPRBKFFT+RPRBKPFT                                       
         BAS   RE,MINIOWRT                                                      
         BAS   RE,MINIOSEQ                                                      
         SPACE 2                                                                
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDMELQ                                                 
         BAS   RE,MINIOHI                                                       
         BNE   DLNMDX                                                           
         L     R6,MINELEM                                                       
DLNMD10  CLI   0(R6),RPRDMELQ                                                   
         BNE   DLNMDX                                                           
         USING RPRDMELD,R6                                                      
         OI    RPRDMFLG,RPRDMFFT                                                
         BAS   RE,MINIOWRT                                                      
         BAS   RE,MINIOSEQ                                                      
         BE    DLNMD10                                                          
DLNMDX   DS    0H                                                               
         DROP  R5,R6                                                            
         SPACE 2                                                                
         BAS   RE,MINIOCLS         CALLER SHOULD DO THIS                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FETCH HOOK ROUTINE                                                            
***********************************************************************         
PRBKHOOK NTR1                                                                   
         L     R4,AIOREC                                                        
         USING RFTBLKD,R4                                                       
         OC    RFTERR,RFTERR                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RFTMODE,RFTNBKQ     NEW BOOK DATA?                               
         BNO   PRHOOKX             NO                                           
         SPACE 2                                                                
**************************************                                          
* HANDLE UPDATING BOOK AND DEMO DATA                                            
*     THE DETAIL CLUSTER IS IN AIO5 AT THIS POINT                               
**************************************                                          
         NI    MISCFLG1,FF-MF1NEWDM                                             
         L     R3,SVPARMS3         REGULAR BOOK MATCH                           
         OC    RFTFBK,RFTFBK       WAS IT A BOOK?                               
         BZ    FH2BKU              NO - ITS AN UPGRADE                          
*                                                                               
         USING BOOKLIN,R3                                                       
FH2BKB   CLC   RFTFBK,BKLNFBK                                                   
         BE    FH2BK5                                                           
         CLI   RFTAMODE,RFTADIRQ   DAY/TIME FETCH?                              
         BE    *+6                 NO - THEN IT SHOULD HAVE MATCHED             
         DC    H'0'                                                             
         CLC   RFTFBK(L'BKLNBK),BKLNFBK                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   RFTFBKSV,BKLNSPBK                                                
         BE    FH2BK5                                                           
         DC    H'0'                                                             
*                                                                               
FH2BKU   L     RF,RFTFUPGA       UPGRADE EXPRESSION MATCH                       
         CLC   BKLNUPBK,0(RF)      BASE BOOK EXPRESSION                         
         BNE   *+14                                                             
         CLC   BKLNUPGD,2+L'BKLNUPBK(RF)                                        
         BE    FH2BK5              HAD BETTER MATCH                             
         DC    H'0'                                                             
*                                                                               
FH2BK5   DS    0H                                                               
         ZIC   R0,BKLNIORD                                                      
         MVC   BYTE1,BKLNFLG                                                    
         DROP  R3                                                               
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)    FIRST ELEMENT                         
FH2BK6   CLI   0(R6),0                    END OF CLUSTER?                       
         BE    FH2BK8                     YES - ADD DEMO VALUE ELEM             
         ZIC   RE,1(R6)                   ELEMENT LENGTH                        
         AR    R6,RE                                                            
         CLI   0(R6),RPRDVELQ             DEMO VALUE ELEMENT?                   
         BNE   FH2BK6                     NO                                    
*                                                                               
         USING RPRDVELD,R6                                                      
         CLM   R0,1,RPRDVBNM       BOOK MATCH?                                  
         BNE   FH2BK6              NO                                           
*                                                                               
         ZIC   RE,1(R6)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),0(R6)     SAVE THE ELEMENT                             
*                                  THEN DELETE IT                               
         GOTOX (RECUPQ,AREPRO01),DMCB,(C'R',AIO5),0(R6)                         
         B     FH2BK10                                                          
*                                                                               
*------------------------------------------------------------                   
*        BUILD PROTOTYPE ELEMENT                                                
*------------------------------------------------------------                   
FH2BK8   DS    0H                                                               
         XC    BOELEM,BOELEM                                                    
         LA    R6,BOELEM                                                        
         MVI   RPRDVEL,RPRDVELQ    DEMO VALUE ELEMENT                           
         MVI   RPRDVLEN,RPRDVOVQ+L'RPRDVDMO                                     
         STC   R0,RPRDVBNM         INTERNAL BOOK ORDER #                        
         OI    MISCFLG1,MF1NEWDM                                                
*                                                                               
*------------------------------------------------------------                   
*        MAKE CHANGES TO ELEMENT                                                
*------------------------------------------------------------                   
FH2BK10  LA    R6,BOELEM                                                        
         LA    R3,RPRDVDMO         FIRST DEMO VALUE                             
         ST    R3,LASTDMO                                                       
         LA    R2,RFTCDEMS         FETCHED DEMO NAMES                           
         SR    R1,R1               INDEX INTO VALUES                            
*                                                                               
FH2BK12  CLI   0(R2),FF            END OF DEMOS?                                
         BZ    FH2BK20             YES                                          
         OC    0(3,R2),0(R2)                                                    
         BZ    FH2BK20              YES                                         
*                                                                               
         L     RF,SVPARMS4                                                      
         LA    RE,(NUMDEMS*DMLNLENQ)(RF)                                        
         USING DEMOLIN,RF                                                       
FH2BK14  CLC   1(L'RFTCDEMS-1,R2),DMLNDEMO+1    FIND KLUGED DEMO                
         BE    FH2BK16             MATCHED                                      
         LA    RF,DMLNLENQ(RF)                                                  
         CR    RF,RE                                                            
         BL    FH2BK14                                                          
         DC    H'0'                SHOULD HAVE MATCHED                          
*                                                                               
FH2BK16  DS    0H                                                               
         STC   R1,BYTE3                                                         
         ZIC   R0,DMLNIORD                                                      
         LR    RE,R0                                                            
         BCTR  R0,0                                                             
         LR    R1,R0                                                            
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,RPRDVDMO(R1)     WHERE THESE VALUES GO                        
*                                                                               
         MH    RE,=Y(L'RPRDVDMO)                                                
         AH    RE,=Y(L'RPRDVOVQ)                                                
*                                                                               
****     TM    BYTE1,RPRBKFFT           BOOK FETCHED?                           
****     BZ    FH2BK17                  NO - COPY EVERY THING                   
         TM    MISCFLG1,MF1NEWDM        NEW ELEMENT?                            
         BNZ   FH2BK17                  YES - COPY EVERY THING                  
         CLM   RE,1,RPRDVLEN                                                    
         BH    FH2BK17                                                          
         OC    0(L'RPRDVDMO,R1),0(R1)   ZERO?                                   
         BNZ   FH2BK18                  NO - SKIP IT                            
****     BZ    FH2BK17                  **** YES                                
****     TM    DMLNFLG,RPRDMFFT         **** DEMO FETCHED?                      
****     BNZ   FH2BK18                  **** YES - SKIP IT                      
         DROP  RF                                                               
*                                                                               
FH2BK17  MH    R0,=Y(L'RPRDVDMO)                                                
         LA    R3,RPRDVDMO                                                      
         AR    R3,R0               WHERE THESE VALUES GO                        
*                                                                               
         LA    RE,RFTFDEMS         DEMO                                         
         ZIC   R0,BYTE3                                                         
         MH    R0,=Y(L'RFTFDEMS)                                                
         AR    RE,R0                                                            
         MVC   0(L'RFTFDEMS,R3),0(RE)                                           
*                                                                               
         LA    RE,RFTFSHRS         SHARE                                        
         ZIC   R0,BYTE3                                                         
         MH    R0,=Y(L'RFTFSHRS)                                                
         AR    RE,R0                                                            
         MVC   4(L'RFTFSHRS,R3),0(RE)                                           
*                                                                               
         LA    RE,RFTFLVLS         LEVEL                                        
         ZIC   R0,BYTE3                                                         
         MH    R0,=Y(L'RFTFLVLS)                                                
         AR    RE,R0                                                            
         MVC   8(L'RFTFLVLS,R3),0(RE)                                           
*                                                                               
         OC    0(L'RPRDVDMO,R3),0(R3)    ANY DATA ADDED?                        
         BZ    FH2BK18                   NO                                     
*                                                                               
         LA    R3,L'RPRDVDMO(R3)                                                
         C     R3,LASTDMO                                                       
         BNH   *+8                                                              
         ST    R3,LASTDMO                                                       
*                                                                               
FH2BK18  DS    0H                                                               
         LA    R2,L'RFTCDEMS(R2)                                                
         ZIC   R1,BYTE3                                                         
         LA    R1,1(R1)                                                         
         B     FH2BK12                                                          
*                                                                               
FH2BK20  DS    0H                                                               
         L     R3,LASTDMO                                                       
         SR    R3,R6               ELEMENT LENGTH                               
         CLM   R3,1,RPRDVLEN       WAS IT LONGER BEFORE?                        
         BL    *+8                 YES                                          
         STC   R3,RPRDVLEN                                                      
         DROP  R6                                                               
*                                                                               
*--------------------------------------------------                             
*           FIND WHERE THE ELEMENT GOES AND WRITE IT                            
*--------------------------------------------------                             
         L     R6,AIO5                                                          
         LA    R6,RPROR1ST-RPROKEY(R6)    FIRST ELEMENT                         
N        USING RPRDVELD,BOELEM                                                  
*                                                                               
FH2BK30  CLI   0(R6),0                    END OF CLUSTER?                       
         BE    FH2BK32                    YES - ADD DEMO VALUE ELEM             
*                                                                               
         ZIC   RE,1(R6)                   ELEMENT LENGTH                        
         AR    R6,RE                                                            
         CLI   0(R6),RPRDVELQ             DEMO VALUE ELEMENT?                   
         BL    FH2BK30                    NO -SKIP                              
         BH    FH2BK32                    NO -WRITE HERE                        
*                                                                               
         USING RPRDVELD,R6                                                      
         CLC   RPRDVBNM,N.RPRDVBNM        BOOK MATCH?                           
         BL    FH2BK30                    NO                                    
         DROP  R6,N                                                             
*                                                                               
FH2BK32  GOTOX (RECUPQ,AREPRO01),DMCB,(C'R',AIO5),BOELEM,0(R6)                  
         OI    MISCFLG1,MF1MNWRT                                                
*                                                                               
*--------------------------------------------------                             
FH2BX    DS    0H                                                               
**************************************                                          
PRHOOKX  B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS TO MAKE SURE THAT ALL THE DEMOS ARE NON ZERO              
*                                                                               
* INPUT:      SVPARMS4 - A(DEMOLIN)                                             
*             R6       - RPRDVELD                                               
*                                                                               
* RETURNS:    CC EQUAL      - ALL DEMOS NON ZERO                                
*                NOT EQUAL  - ZERO DEMO FOUND                                   
*                                                                               
***********************************************************************         
CHKDEMOS NTR1                                                                   
         L     RE,SVPARMS4                                                      
         LA    RF,(NUMDEMS*DMLNLENQ)(RE)   END OF TABLE                         
         USING DEMOLIN,RE                                                       
         USING RPRDVELD,R6                                                      
*                                                                               
CHD5     OC    DMLNDEMO,DMLNDEMO                                                
         BZ    CHD20                                                            
*                                                                               
         ZIC   R1,DMLNIORD                                                      
         BCTR  R1,0                                                             
         MH    R1,=Y(L'RPRDVDMO)                                                
         LA    R1,RPRDVDMO(R1)                                                  
         OC    0(L'RPRDVDMO,R1),0(R1)                                           
         BZ    EXITL                                                            
*                                                                               
CHD20    LA    RE,DMLNLENQ(RE)                                                  
         CR    RE,RF                                                            
         BL    CHD5                                                             
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SETS UP AIO5 SO WE CAN USE RECUP TO ADD ELEMENTS, DELETE ELEMENTS, OR         
* CHANGE THE SIZE OF EXISTING ELEMENTS OF THE CLUSTER IN MINELEM                
***********************************************************************         
INTOAIO5 NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         L     RE,AIO5             COPY KEY, LEN, STAT, & LINK                  
         L     RF,MINBUFF                                                       
         MVC   0(RPROR1ST-RPROKEY,RE),0(RF)                                     
         LA    RE,RPROR1ST-RPROKEY(RE)                CLUSTER GOES HERE         
         LA    RF,IOAREALN-(RPROR1ST-RPROKEY)                                   
*                                                                               
         L     R0,MINELEM          COPY THE ENTIRE CLUSTER                      
         LH    R1,MINELEML                                                      
         MVCL  RE,R0                                                            
*                                                                               
         LH    R1,MINELEML                                                      
         AH    R1,=Y(RPROR1ST-RPROKEY)     L(FAKE RECORD)                       
         L     RE,AIO5             COPY KEY, LEN, STAT, & LINK                  
         STCM  R1,3,RPRORLEN-RPROKEY(RE)                                        
         B     EXITOK                                                           
         DROP  R5                                                               
***********************************************************************         
* SETS UP MINELEM FROM AIO5 BECAUSE WE NEEDED RECUP                             
***********************************************************************         
FROMAIO5 NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         L     RE,AIO5                                                          
         ZICM  RF,RPRORLEN-RPROKEY(RE),2                                        
         SH    RF,=Y(RPROR1ST-RPROKEY)     L'CLUSTER                            
         LA    RE,RPROR1ST-RPROKEY(RE)                                          
*                                                                               
         L     R0,MINELEM                                                       
         LH    R1,MINMAXEL                                                      
         MVCL  R0,RE               COPY CLUSTER                                 
*                                                                               
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS FOR A MINIO ELEMENT                                        
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINEKEY             MINIO ELEMENT KEY SET BY CALLER              
***********************************************************************         
MINIORD  NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,DMCB,('MINRD',(R5))                                       
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE READS HIGH FOR A MINIO ELEMENT.                                  
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINEKEY             MINIO ELEMENT KEY SET BY CALLER              
***********************************************************************         
MINIOHI  NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,DMCB,('MINHI',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    EXITOK                                                           
         B     EXITL               OTHERWISE RETURN 'NO'                        
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE READS SEQUENTIAL FOR A MINIO ELEMENT.                            
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIOSEQ NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,DMCB,('MINSEQ',(R5))                                      
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    EXITOK                                                           
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    EXITL                                                            
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE WRITES OUT A MINIO ELEMENT.                                      
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINELEM             CONTAINS THE MINIO ELEMENT                   
***********************************************************************         
MINIOWRT NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     RF,MINELEM                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),0(RF)                                                 
         MVC   MINEKEY+1(L'RPROKMEL-1),2(RF)                                    
*                                                                               
         OI    MNIOFLAG,MNIOCLSQ   REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 VMINIO,DMCB,('MINWRT',(R5))                                      
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT.                                            
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINELEM             CONTAINS THE MINIO ELEMENT                   
***********************************************************************         
MINIOADD NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     RF,MINELEM                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),0(RF)                                                 
         MVC   MINEKEY+1(L'RPROKMEL-1),2(RF)                                    
*                                                                               
         OI    MNIOFLAG,MNIOCLSQ   REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 VMINIO,DMCB,('MINADD',(R5))                                      
*                                                                               
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         BE    EXITL               YES, RETURN A NO                             
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE DELETES A MINIO ELEMENT.  CALLER IS RESPONSIBLE FOR              
* POINTING TO ELEMENT FIRST.                                                    
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIODEL NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         OI    MNIOFLAG,MNIOCLSQ   REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 VMINIO,DMCB,('MINDEL',(R5))                                      
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE CLOSES MINIO AND FLUSHES OUT THE BUFFERS TO THE MINIO            
* RECORDS.                                                                      
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIOCLS NTR1                                                                   
         TM    MNIOFLAG,MNIOCLSQ   DO WE NEED TO?                               
         BZ    EXITOK              NO                                           
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,DMCB,('MINCLS',(R5))                                      
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
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
PRWORKD  DSECT                                                                  
DMCB     DS    6F                                                               
PRRELO   DS    A                                                                
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
PRWORKL  EQU   *-PRWORKD                                                        
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
**PAN#1  DC    CL21'008REPRO25S  12/12/96'                                      
         END                                                                    
