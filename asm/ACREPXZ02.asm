*          DATA SET ACREPXZ02  AT LEVEL 056 AS OF 08/16/00                      
*PHASE ACXZ02A                                                                  
*INCLUDE XSORT                                                                  
         TITLE 'REPORT ON SCREEN FIELD DATA'                                    
ACXZ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXZ**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXZD,RC                                                         
         EJECT                                                                  
***********************************************************************         
* FIRST FOR REQUEST                                                   *         
***********************************************************************         
                                                                                
REQF00   CLI   MODE,REQFRST                                                     
         BNE   XIT                                                              
         BAS   RE,BLDACC           BUILD ACCOUNT LIST                           
*                                                                               
         XC    DKEY,DKEY                                                        
         LA    R2,DKEY                                                          
         USING TBARECD,R2                                                       
         MVI   TBAKTYP,TBAKTYPQ    LOOK FOR BATCH HEADERS                       
         MVC   TBAKCPY,RCCOMPFL                                                 
         BAS   RE,DMHGH                                                         
*                                                                               
REQF03   LA    R2,DIR                                                           
         CLC   DIR(TBAKUSER-TBAKEY),DKEY                                        
         BNE   REQF09                                                           
         CLI   TBAKBTYP,0                                                       
         BNH   REQF07                                                           
         LA    R2,IO                                                            
         BAS   RE,DMGETR                                                        
         SR    R1,R1                                                            
         LA    R4,TBARFST                                                       
REQF05   CLI   0(R4),0                                                          
         BE    REQF07                                                           
         USING SFSELD,R4                                                        
         CLI   0(R4),SFSELQ        FIND SCREEN FIELD                            
         BNE   REQF06                                                           
         CLI   SFSLN,8                                                          
         BL    REQF06                                                           
         BAS   RE,PSFS             PROCESS SCREEN FIELD ELEMENT                 
REQF06   IC    R1,SFSLN                                                         
         LTR   R1,R1                                                            
         BZ    REQF07                                                           
         AR    R4,R1                                                            
         B     REQF05                                                           
*                                                                               
REQF07   BAS   RE,DMSEQ                                                         
         B     REQF03                                                           
*                                                                               
REQF09   BAS   RE,RPT              PRINT A REPORT                               
XIT      XIT1                                                                   
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD TABLE OF ACCOUNTS                                             *         
***********************************************************************         
                                                                                
BLDACC   NTR1  ,                                                                
         SR    R3,R3               R3=COUNT NUMBER OF ITEMS                     
         L     R4,AULACC           R4=A(UNIT/LEDGER ACCOUNT TABLE)              
         MVC   DKEY,SPACES                                                      
         LA    R2,DKEY                                                          
         USING ACTRECD,R2                                                       
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVC   ACTKUNT(2),QUNIT                                                 
BLDACC1  BAS   RE,DMHGH                                                         
         LA    R2,DIR                                                           
         CLC   ACTKCPY,RCCOMPFL                                                 
         BNE   BLDACC9                                                          
         CLI   QLEDGER,C' '                                                     
         BE    BLDACC3                                                          
         CLC   ACTKUNT(2),QUNIT                                                 
         BNE   BLDACC9                                                          
BLDACC3  CLI   QUNIT,C' '                                                       
         BE    BLDACC4                                                          
         CLC   ACTKUNT,QUNIT                                                    
         BNE   BLDACC9                                                          
BLDACC4  CLC   ACTKUNT(2),=C'SJ'   SKIP SJ                                      
         BNE   BLDACC5                                                          
         MVC   DKEY+1(14),=CL14'SK'                                             
         B     BLDACC1                                                          
*                                                                               
BLDACC5  TM    ACTKSTAT,ACTSABLP    TEST MUST ELEMENT                           
         BNO   BLDACC7                                                          
         AH    R3,=H'1'            TEST MAX                                     
         C     R3,=AL4(MXACC)                                                   
         BNH   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   0(14,R4),ACTKULA    UL/ACCOUNT                                   
         CLI   QOPT1,C'D'          LOOK FOR DEFAULTS                            
         BNE   *+16                                                             
         MVC   0(12,R4),ACTKACT    ACCOUNT                                      
         MVC   12(2,R4),ACTKUNT    UNIT/LEDGER                                  
         LA    R4,L'ULACC(R4)                                                   
*                                                                               
BLDACC7  MVC   DKEY,SPACES                                                      
         MVC   DKEY(L'ACTKCULA),DIR                                             
         LA    RF,DKEY+(L'ACTKCULA-1)                                           
         SR    RE,RE                                                            
         IC    RE,0(RF)            BUMP TO NEXT ACCOUNT                         
         AH    RE,=H'1'                                                         
         STC   RE,0(RF)                                                         
         B     BLDACC1                                                          
*                                                                               
BLDACC9  ST    R3,NUMACC                                                        
         BAS   RE,SETAB            SET UL/ACC TABLE                             
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SET UL/ACC TABLE                                                    *         
***********************************************************************         
                                                                                
SETAB    NTR1  ,                                                                
         CLI   QOPT1,C'D'                                                       
         BNE   SETAB3                                                           
         GOTO1 XSORT,XULAPRM       SORT TABLE BY ACCOUNT/UL                     
SETAB3   L     R1,NUMACC           # RECORDS                                    
         LA    R3,L'ULACC          R3=L'RECORD                                  
         MR    R0,R3                                                            
         L     R2,AULACC           R2=A(START OF TABLE)                         
         AR    R1,R2                                                            
         BCTR  R1,0                R1=END OF TABLE                              
         LR    R0,R3               R0=L'RECORD                                  
         SR    R2,R0               R2=A(START LESS LENGTH OF ENTRY)             
         SR    R1,R2               R1=LENGTH OF TABLE PLUS AN ENTRY             
         BXLE  R0,R0,*             R0=LOWEST POWER OF 2 GE R1                   
         AR    R1,R2               R1=END OF TABLE                              
         LA    R4,L'ULACC          R4=L'KEY                                     
         STM   R0,R4,PULACC        SAVE SEARCH PARAMETERS                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS SCREEN FIELD ELEMENT                                        *         
***********************************************************************         
                                                                                
         USING SFSELD,R4                                                        
PSFS     NTR1  ,                                                                
         MVC   SRCHARG,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,SFSLN                                                         
         SH    R1,=Y(SFSFIELD-SFSELD+1)                                         
         LA    RF,SFSFIELD                                                      
         CLI   SFSFIELD,C'*'       LOOK FOR ACCOUNTS                            
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         LA    RF,1(RF)                                                         
         CH    R1,=H'13'                                                        
         BH    XIT                                                              
         EX    R1,*+4                                                           
         MVC   SRCHARG(0),0(RF)                                                 
*                                                                               
         LA    RF,PULACC           LOOK FOR ULACC                               
         BAS   RE,SRCH                                                          
         BNE   XIT                                                              
         BAS   RE,MULA             MATCH ULA - ADD TO TABLE                     
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SEARCH ROUTINE                                                      *         
***********************************************************************         
                                                                                
SRCH     NTR1  ,                                                                
         LM    R0,R4,0(RF)         LOAD SEARCH REGISTERS                        
         BCTR  R4,0                                                             
         CLI   QOPT1,C'D'                                                       
         BNE   *+8                                                              
         SH    R4,=H'2'            DON'T INCLUDE U/L                            
*                                                                               
SRCH3    SRL   R0,1                1/2 REMAINING TABLE LENGTH                   
         CR    R0,R3               TEST IF LESS THAN AN ENTRY LENGTH            
         BL    XIT                 NOT FOUND - AT END OF TABLE                  
         BXH   R2,R0,SRCH5         COMPUTE NEW TABLE START ADDRESS              
         EX    R4,*+8              SEARCH ACCOUNT VS. TABLE                     
         B     *+10                                                             
         CLC   SRCHARG(0),0(R2)                                                 
         BE    SRCH7               GOT A MATCH                                  
         BH    SRCH3                                                            
SRCH5    SR    R2,R0                                                            
         B     SRCH3                                                            
                                                                                
SRCH7    ST    R2,ARTRN                                                         
         CLI   QOPT1,C'D'                                                       
         BNE   SRCHYES                                                          
         LR    RF,R2               BACKUP TO FIRST OF ENTRIES                   
         SR    RF,R0                                                            
         CLM   RF,7,PULACC                                                      
         BL    SRCHYES                                                          
         CLC   0(12,RF),0(R2)      SAME ACCOUNT                                 
         BNE   SRCHYES                                                          
         LR    R2,RF               R2 TO PREVIOUS                               
         B     SRCH7                                                            
SRCHYES  CR    RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* MATCHED ULA - ADD ENTRY TO TABLE                                    *         
***********************************************************************         
                                                                                
         USING SFSELD,R4                                                        
MULA     NTR1  ,                                                                
         L     RF,ARTRN            RF=A(ACCOUNT IN TABLE)                       
MULA1    L     R5,ASCRTAB          TABLE FOR ULA MATCH                          
         USING SCRD,R5                                                          
         LA    R2,DIR                                                           
         USING TBARECD,R2                                                       
MULA3    CLI   0(R5),X'FF'                                                      
         BE    MULA5                                                            
         CLC   TBAKBTYP,SCRTYP     MATCH TYPE                                   
         BNE   MULA4                                                            
         CLC   SFSFLDN,SCRFLDN     AND FIELD NUMBER                             
         BNE   MULA4                                                            
         CLI   QOPT1,C'D'                                                       
         BNE   MULA7                                                            
         CLC   12(2,RF),SCRUL                                                   
         BE    MULA7                                                            
MULA4    LA    R5,SCRLNQ(R5)                                                    
         B     MULA3                                                            
*                                                                               
MULA5    L     R0,NUMSCR           NUMBER OF ENTRIES                            
         AH    R0,=H'1'                                                         
         ST    R0,NUMSCR                                                        
         CH    R0,=Y(MXFLD)        TEST MAX                                     
         BNH   *+6                                                              
         DC    H'0'                FIELD TABLE IS FULL                          
         XC    0(SCRLNQ,R5),0(R5)                                               
         MVI   SCRLNQ(R5),X'FF'                                                 
         MVC   SCRTYP,TBAKBTYP     ADD NEW ITEM                                 
         MVC   SCRFLDN,SFSFLDN                                                  
         CLI   QOPT1,C'D'                                                       
         BNE   MULA7                                                            
         MVC   SCRUL,12(RF)                                                     
*                                                                               
MULA7    ICM   R1,15,SCRNUM                                                     
         AH    R1,=H'1'            ADD 1 TO THE COUNT                           
         STCM  R1,15,SCRNUM                                                     
         CLI   QOPT1,C'D'                                                       
         BE    XIT                                                              
         LA    RE,L'ULACC(RF)                                                   
         CLC   0(12,RE),0(RF)      CHECK SAME ACCOUNT                           
         BNE   XIT                                                              
         LR    RF,RE                                                            
         B     MULA1                                                            
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                        *         
***********************************************************************         
                                                                                
RPT      NTR1  ,                                                                
         ICM   R3,15,NUMSCR        NUMBER IN TABLE                              
         BZ    XIT                                                              
         GOTO1 XSORT,XSCRPRM                                                    
         L     R5,ASCRTAB          SCREEN FIELD TABLE                           
         USING SCRD,R5                                                          
RPT3     CLC   SCRNUM,=AL4(100)     MUST HAVE AT LEAST 100                      
         BL    RPT5                                                             
         EDIT  (B1,SCRTYP),(3,P+1)                                              
         EDIT  (B1,SCRFLDN),(3,P+7)                                             
         MVC   P+15(2),SCRUL                                                    
         EDIT  (B4,SCRNUM),(6,P+20)                                             
         GOTO1 ACREPORT                                                         
RPT5     LA    R5,SCRLNQ(R5)                                                    
         BCT   R3,RPT3                                                          
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
                                                                                
DMRD     ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMHGH    ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMSEQ    ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMGETR   ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,(R2),DMWORK                        
         B     DMERR                                                            
*                                                                               
DMWRTR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         B     DMERR                                                            
*                                                                               
DMADDR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,(R2),DMWORK                        
         B     DMERR                                                            
*                                                                               
DMPUTR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,(R2),DMWORK                        
*                                                                               
DMERR    MVC   BYTE,8(R1)                                                       
         NI    BYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED               
         CLI   BYTE,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
                                                                                
ACCOUNT  DC    CL8'ACCOUNT'                                                     
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
*                                                                               
XSCRPRM  DS    0F                  XSORT PARAMETERS FOR SCREEN FIELDS           
ASCRTAB  DC    A(SCRTU)            A(MATCHED ULA TABLE)                         
NUMSCR   DC    F'0'                NUMBER OF ITEMS                              
         DC    AL4(SCRLNQ)         RECORD LENGTH                                
         DC    AL4(2)              KEY LENGTH                                   
         DC    AL4(0)              DISP. TO KEY                                 
*                                                                               
XULAPRM  DS    0F                  XSORT PARAMETERS FOR ACCOUNT TABLE           
AULACC   DC    A(ULACC)            A(UL ACCOUNT TABLE)                          
NUMACC   DC    F'0'                NUMBER OF ACCOUNTS                           
         DC    AL4(L'ULACC)        RECORD LENGTH                                
         DC    AL4(L'ULACC)        KEY LENGTH                                   
         DC    AL4(0)              DISP. TO KEY                                 
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
                                                                                
MXACC    EQU   100000                                                           
ULACC    DS    0CL14                                                            
         DS    (MXACC*L'ULACC)C                                                 
*                                                                               
MXFLD    EQU   300                                                              
SCRTU    DS    0CL14                                                            
         DC    X'FF'                                                            
         DS    (MXFLD*SCRLNQ)C                                                  
         EJECT                                                                  
***********************************************************************         
* DSECT FOR LOCAL STORAGE                                             *         
***********************************************************************         
                                                                                
ACXZD    DSECT                                                                  
*                                                                               
SAVRE    DS    F                                                                
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DIR      DS    CL64                DIRECTORY RECORD                             
DA       DS    XL4                                                              
*                                                                               
PULACC   DS    5F                                                               
*                                                                               
SRCHARG  DS    CL14                                                             
*                                                                               
ARTRN    DS    F                                                                
*                                                                               
IO       DS    XL2000                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT SCREEN FIELD TABLE                                            *         
***********************************************************************         
                                                                                
SCRD     DSECT                                                                  
SCRTYP   DS    XL1                 TYPE                                         
SCRFLDN  DS    XL1                 FIELD NUMBER                                 
SCRUL    DS    CL2                 DEFAULT UNIT/LEDGER                          
SCRNUM   DS    XL4                 NUMBER OF MATCHS                             
SCRLNQ   EQU   *-SCRTYP                                                         
         EJECT                                                                  
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*  ACGENMODES                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*  ACREPWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056ACREPXZ02 08/16/00'                                      
         END                                                                    
