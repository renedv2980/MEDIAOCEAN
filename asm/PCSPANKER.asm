*          DATA SET PCSPANKER  AT LEVEL 004 AS OF 05/01/02                      
*PHASE SPANKERB                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE DUMPOUT                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE QSORT                                                                  
*INCLUDE STXITER                                                                
         TITLE 'AGENCY RE-DISTRIBUTION PROGRAM'                                 
**************************************************************                  
* NOTE THAT THIS VERSION SUPPORTS ALPHA AGENCY CODES FOR ACC *                  
**************************************************************                  
         SPACE 1                                                                
SPANKER  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**SPANK,=V(REGSAVE),R9,RA                                      
*                                                                               
         XC    WORK,WORK                                                        
         ST    RB,WORK                                                          
         L     R4,=V(REGSAVE)                                                   
         A     R4,=F'20000'                                                     
         ST    R4,WORK+4                                                        
         OI    WORK+4,X'80'                                                     
         GOTO1 =V(STXITER),DMCB,WORK                                            
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMOPEN',=C'CONTROL',=C'NCTFILE X'               
         L     R8,VCPRINT                                                       
         USING DPRINT,R8           R8=A(PRINT CSECT)                            
         MVC   TITLE(13),=C'CONTROL CARDS'                                      
         EJECT                                                                  
* VALIDATE PARAMETER CARDS                                                      
*                                                                               
SP2      GOTO1 VCARDS,DMCB,CARD,=C'RE00'                                        
         CLC   CARD(2),=C'/*'      TEST FOR E-O-F                               
         BE    SP10                                                             
* PRINT OUT EACH CONTROL CARD                                                   
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R1,PARMTAB                                                       
         USING PARMTABD,R1         R1=A(PARAMETER TABLE)                        
         SR    RF,RF                                                            
*                                                                               
SP4      CLI   0(R1),X'FF'                                                      
         BE    SP6                                                              
         IC    RF,0(R1)            GET EXECUTE LENGTH                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   PARMNAME(0),CARD    COMPARE INPUT CARD WITH TABLE                
         BE    *+12                                                             
         LA    R1,PARMLEN(R1)                                                   
         B     SP4                                                              
         LA    R2,CARD+1(RF)       R2=A(CARD DATA)                              
         ICM   RF,7,PARMROUT       RF=A(VALIDATION ROUTINE)                     
         MVI   VALSW,C'N'          SET VALIDITY SWITCH TO INVALID               
         BASR  RE,RF                                                            
         BE    SP2                                                              
*                                  OUTPUT INVALID CONTROL CARD                  
SP6      MVC   P+10(80),CARD                                                    
         MVC   P+91(22),=C'- INVALID CONTROL CARD'                              
         GOTO1 VPRINTER                                                         
         GOTO1 =V(PRINT),DMCB,=C'CLOSE'                                         
         DC    H'0'                                                             
         DROP  R1                                                               
         EJECT                                                                  
* DEDUCE WHICH INPUT TAPE FILES SHOULD BE MOUNTED FROM AGYTAB                   
*                                                                               
SP10     OC    ASYSNTRY,ASYSNTRY   TEST IF FILETYP CARD PROCESSED               
         BZ    SP6                                                              
         CP    OUTCNT,=P'0'        TEST IF OUTPUT CARD(S) PROCESSED             
         BE    SP6                                                              
*                                                                               
         L     R5,AAGYTAB          R5=A(AGENCY TABLE)                           
         USING AGYTABD,R5                                                       
*                                                                               
SP12     CLI   0(R5),X'FF'         TEST E-O-T                                   
         BE    SP14                                                             
         L     R2,ASETAB                                                        
         USING SETABD,R2           R2=A(SE TABLE)                               
         CLC   SEFILNUM,AGYOLDF    FIND ENTRY FOR OLD FILE IN SETAB             
         BE    *+12                                                             
         LA    R2,SELEN(R2)                                                     
         B     *-14                                                             
         CLI   AGYNEWF,0                                                        
         BE    *+8                                                              
         OI    SEINDS,X'80'        SET INPUT TAPE REQUIRED                      
         LA    R5,AGYLEN(R5)       BUMP TO NEXT AGENCY                          
         B     SP12                                                             
*                                  OPEN ALL OUTPUT FILES & SET A(DTF)           
SP14     L     R2,ASETAB           IN SETAB ENTRIES                             
         L     R3,ADTFTAB          R3=A(DTF TABLE)                              
         CLI   TESTOPT,C'Y'                                                     
         BE    SP50                                                             
*                                                                               
SP16     CLI   0(R2),X'FF'         TEST E-O-T                                   
         BE    SP30                                                             
         TM    SEINDS,X'40'        TEST OUTPUT FILE REQUIRED                    
         BZ    SP18                                                             
         MVC   MESS1+7(L'SENAME),SENAME                                         
         L     R4,0(R3)                                                         
         STCM  R4,7,SEADTF         SET SYSNUM IN OUTPUT MESSAGE                 
         EDIT  (B1,7(R4)),(3,MESS1+41),FILL=0                                   
******   GOTO1 VLOGIO,DMCB,1,(45,MESS1)                                         
******   GOTO1 (RF),(R1),0,(1,WORK)                                             
         OPEN  ((R4),OUTPUT)                                                    
         LA    R3,4(R3)            BUMP TO NEXT DTF                             
*                                                                               
SP18     LA    R2,SELEN(R2)        BUMP TO NEXT SE                              
         B     SP16                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
* PROCESS ALL INPUT FILES                                                       
*                                                                               
SP30     L     R2,ASETAB                                                        
         USING SETABD,R2           R2=A(SE TABLE)                               
*                                                                               
SP32     CLI   0(R2),X'FF'         TEST E-O-T                                   
         BE    SP44                                                             
         TM    SEINDS,X'80'        TEST IF INPUT FILE REQUIRED                  
         BZ    SP42                                                             
         MVC   TITLE,SPACES                                                     
         MVC   TITLE+20(L'SENAME),SENAME                                        
         MVC   TITLE+28(12),=C'RECORD PRINT'                                    
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         ST    R2,ASENTRY          SAVE A(SE TABLE ENTRY)                       
*                                                                               
SP34     MVC   MESS2+7(L'SENAME),SENAME                                         
         L     R4,ADTFIN           SET FILE SYSNUM IN OUTPUT MESSAGE            
         EDIT  (B1,7(R4)),(3,MESS2+40),FILL=0                                   
******   GOTO1 VLOGIO,DMCB,1,(45,MESS2)                                         
******   GOTO1 (RF),(R1),0,(1,WORK)                                             
*****************                                                               
         MVC   40(7,R4),SENAME     SET SENAME AS INPUT DD NAME                  
         OPEN  ((R4),INPUT)        AND OPEN IT                                  
*****************                  PROCESS RECORDS                              
SP36     GET   (R4),IO                                                          
         AP    SEINPCNT,=P'1'                                                   
         BAS   RE,PROCREC                                                       
         B     SP36                                                             
*                                  INPUT TAPE E-O-F ENTERS HERE                 
SP38     CLOSE ((R4))                                                           
*                                                                               
SP40     DS    0H                                                               
*                                                                               
SP42     LA    R2,SELEN(R2)        BUMP TO NEXT SE                              
         B     SP32                                                             
*                                  CLOSE ALL OUTPUT FILES                       
SP44     L     R2,ASETAB                                                        
*                                                                               
SP46     CLI   0(R2),X'FF'         TEST E-O-T                                   
         BE    SP50                                                             
         TM    SEINDS,X'40'        TEST THIS IS AN OUTPUT FILE                  
         BZ    SP48                                                             
         ICM   R4,7,SEADTF                                                      
         CLI   EOFSW,C'Y'          IF EOF RECORD SAVED WRITE NOW                
         BNE   SP47                                                             
         PUT   (R4),EOFSAVE                                                     
         AP    SEOUTCNT,=P'1'                                                   
*                                                                               
SP47     CLOSE ((R4))                                                           
*                                                                               
SP48     LA    R2,SELEN(R2)        BUMP TO NEXT SE                              
         B     SP46                                                             
*                                                                               
SP50     BAS   RE,SETOUT           UPDATE CONTROL FILE ID RECORDS               
         BAS   RE,PRNTOTS          PRINT RECORD TOTALS                          
         GOTO1 VDATAMGR,DMCB,=C'DMCLSE',=C'CONTROL'                             
         XBASE                                                                  
         DROP  R2                                                               
         EJECT                                                                  
* VALIDATE TYPFILE PARAMETER (TYPFILE=FILE NAME), BUILD SETAB FROM              
* CTFILE SYSTEM LIST RECORD AND BUILD AGYTAB FROM CTFILE ID RECORDS.            
*                                                                               
         USING PARMTABD,R1                                                      
VALTYP   NTR1                                                                   
         OC    ASYSNTRY,ASYSNTRY   TEST IF TYPFILE ALREADY READ                 
         BNZ   VALPARMX                                                         
         LA    R4,SYSTAB                                                        
         USING SYSTABD,R4          R4=A(SYSTEM FILE NAME TABLE)                 
*                                                                               
VALTYP2  CLI   SYSFILE,X'FF'       TEST E-O-T                                   
         BE    VALPARMX                                                         
         CLC   SYSFILE,0(R2)       FIND SYSTAB ENTRY                            
         BE    *+12                                                             
         LA    R4,SYSLEN(R4)                                                    
         B     VALTYP2                                                          
         ST    R4,ASYSNTRY         SAVE A(SYSTAB ENTRY)                         
         LA    RE,ALPHATAB         SET A(KEY ALLOCATION TABLE)                  
         MVI   AGYFORM,C'A'                                                     
         TM    SYSINDS,X'C0'                                                    
         BNZ   *+12                                                             
         LA    RE,BINRYTAB                                                      
         MVI   AGYFORM,C'B'                                                     
         ST    RE,AAGYKTAB                                                      
*                                  READ SYSTEM LIST RECORD                      
         LA    R3,IO                                                            
         USING CTWREC,R3                                                        
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'S'                                                     
         MVC   KEY,IO                                                           
         GOTO1 CTIO,DMREAD                                                      
*                                  BUILD SETAB FROM SYSTEM LIST RECORD          
         LA    R3,CTWDATA                                                       
         L     R2,ASETAB                                                        
         USING SETABD,R2                                                        
         SR    RF,RF                                                            
*                                                                               
VALTYP4  CLI   0(R3),0                                                          
         BE    VALTYP8                                                          
         CLI   0(R3),CTLSTELQ                                                   
         BE    VALTYP7                                                          
*                                                                               
VALTYP6  IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VALTYP4                                                          
*                                  PROCESS A SYSTEM LIST ELEMENT                
VALTYP7  CLC   SYSOVNUM,10(R3)     TEST IF SAME SYSTEM NUMBER                   
         BNE   VALTYP6                                                          
         MVC   SENAME,3(R3)                                                     
         MVC   SEOVNUM,10(R3)                                                   
         MVC   SENUM,11(R3)                                                     
         MVC   SEFILNUM,12(R3)                                                  
         ZAP   SEINPCNT,=P'0'                                                   
         ZAP   SEDELCNT,=P'0'                                                   
         ZAP   SEOUTCNT,=P'0'                                                   
         LA    R2,SELEN(R2)        BUMP TO NEXT SE                              
         B     VALTYP6                                                          
*                                                                               
VALTYP8  MVI   SENAME,X'FF'        SET END OF SETAB                             
*                                  READ ID RECORDS & BUILD AGYTAB               
         LA    R3,IO                                                            
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVI   CTIKID,C' '                                                      
         MVC   KEY,IO                                                           
*                                                                               
         LA    R1,DMRDHI                                                        
         B     *+8                                                              
VALTYP10 LA    R1,DMRSEQ                                                        
         GOTO1 CTIO                                                             
         CLC   KEY(1),IO                                                        
         BNE   VALTYP30                                                         
*                                  PROCESS AN ID RECORD                         
         LA    R3,IO+CTIDATA-CTIREC                                             
         SR    RF,RF                                                            
         XC    WORK(2),WORK                                                     
*                                                                               
VALTYP12 CLI   0(R3),0                                                          
         BE    VALTYP10                                                         
         CLI   0(R3),CTAGYELQ      AGENCY ID ELEMENT                            
         BE    VALTYP16                                                         
         CLI   0(R3),CTSYSELQ      SYSTEM ELEMENT                               
         BE    VALTYP18                                                         
*                                                                               
VALTYP14 IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     VALTYP12                                                         
*                                  PROCESS AN AGENCY ID ELEMENT                 
         USING CTAGYD,R3                                                        
VALTYP16 MVC   WORK(2),CTAGYID                                                  
         B     VALTYP14                                                         
*                                  PROCESS A SYSTEM ELEMENT                     
         USING CTSYSD,R3                                                        
VALTYP18 CLC   SYSOVNUM,CTSYSNUM                                                
         BNE   VALTYP14                                                         
         OC    WORK(2),WORK        EXIT IF NO AGENCY ID ELEMENT                 
         BZ    VALTYP10                                                         
*                                  TEST IF AGYTAB ENTRY MADE ALREADY            
         L     R5,AAGYTAB                                                       
         USING AGYTABD,R5          R5=A(AGYTAB)                                 
         L     R2,ASETAB                                                        
*                                                                               
VALTYP20 CLI   0(R5),X'FF'         TEST E-O-T                                   
         BE    VALTYP22                                                         
         CLC   AGYOLDA,WORK                                                     
         BE    VALTYP10                                                         
         LA    R5,AGYLEN(R5)                                                    
         B     VALTYP20                                                         
*                                  ADD AN AGYTAB ENTRY                          
VALTYP22 CLI   SENAME,X'FF'        FIND CURRENT SETAB ENTRY FOR AGENCY          
         BE    VALTYP10                                                         
         CLC   SENUM,CTSYSSE                                                    
         BE    *+12                                                             
         LA    R2,SELEN(R2)                                                     
         B     VALTYP22                                                         
         XC    AGYTABD(AGYLEN),AGYTABD                                          
         MVC   AGYOLDF,SEFILNUM                                                 
         MVC   AGYOLDL,CTSYSAGB                                                 
         PACK  AGYOLDR,CTSYSAGB                                                 
         MVC   AGYOLDA,WORK                                                     
* PRINT OUT THE ENTRY                                                           
         MVC   P(10),IO+15         PRINT OUT ID AND AGYALPHA                    
         MVC   P+12(2),AGYOLDA                                                  
         MVC   P+16(7),SENAME                                                   
         GOTO1 VHEXOUT,DMCB,AGYOLDL,P+25,1,=C'TOG'                              
         GOTO1 VPRINTER                                                         
*                                                                               
VALTYP23 LA    R5,AGYLEN(R5)                                                    
         MVI   0(R5),X'FF'         SET NEW E-O-T                                
         B     VALTYP10                                                         
         SPACE 1                                                                
* SORT THE AGENCY LIST *                                                        
         SPACE 1                                                                
VALTYP30 L     R5,AAGYTAB                                                       
*                                                                               
VALTYP32 CLI   0(R5),X'FF'                                                      
         BE    VALTYP34                                                         
         LA    R5,AGYLEN(R5)                                                    
         B     VALTYP32                                                         
*                                                                               
VALTYP34 LTR   R0,R5                                                            
         BZ    VALTYP40                                                         
         S     R0,AAGYTAB          GIVES LENGTH OF LIST                         
         SRDL  R0,32                                                            
         D     R0,=A(AGYLEN)                                                    
         ST    R1,DMCB+4           SET NUMBER OF ENTRIES                        
         LA    R0,AGYLEN                                                        
         ST    R0,DMCB+8           SET ENTRY LENGTH                             
         LA    R0,L'AGYOLDA                                                     
         ST    R0,DMCB+12          SET KEY LENGTH                               
         LA    R0,AGYOLDA-AGYTABD                                               
         ST    R0,DMCB+16          SET KEY DISPLACEMENT                         
         GOTO1 =V(QSORT),DMCB,AAGYTAB                                           
*                                                                               
VALTYP40 MVI   VALSW,C'Y'                                                       
         B     VALPARMX                                                         
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
* VALIDATE OUTPUT PARAMETERS (OUTPUTN=A1,A2..,AN) WHERE 'N' IS THE              
* LOGICAL FILE NUMBER AND A1-AN ARE 2CHR AGENCY IDS. UPDATE AGYTAB              
* ENTRIES WITH NEW LOGICAL FILE NUMBER AND NEW AGENCY KEY VALUES.               
*                                                                               
VALOUT   NTR1                                                                   
         CLI   PARMPARM,X'FF'      TEST SETAB ENTRY ALREADY USED                
         BE    VALOUTX                                                          
         MVC   WORK(1),PARMPARM    SAVE LOGICAL FILE NUMBER                     
         MVI   PARMPARM,X'FF'      SET ENTRY USED                               
         ICM   R4,15,AAGYKTAB      TEST TYPFILE PARM PROCESSED                  
         BZ    VALOUTX                                                          
         L     R6,ASYSNTRY                                                      
         USING SYSTABD,R6          R6=A(SYSTEM TABLE ENTRY)                     
         L     R3,ASETAB                                                        
         USING SETABD,R3           R3=A(SE TABLE)                               
*                                  FIND SETAB ENTRY FOR OUTPUT FILE             
VALOUT2  CLI   SENAME,X'FF'        TEST E-O-T                                   
         BE    VALOUTX                                                          
         CLC   SEFILNUM,WORK                                                    
         BE    *+12                                                             
         LA    R3,SELEN(R3)                                                     
         B     VALOUT2                                                          
         OI    SEINDS,X'40'        SET OUTPUT FILE REQUIRED                     
         LA    R7,21               R7=MAX NUMBER OF AGYS/CARD                   
*                                  PROCESS AGENCY SPECS                         
VALOUT4  CLI   0(R2),C' '          TEST END OF INPUT                            
         BE    VALOUT8                                                          
         TM    SYSINDS,X'80'                                                    
         BO    *+12                                                             
         CLI   0(R4),X'FF'         TEST END OF KEY VALUES TABLE                 
         BE    VALOUTX                                                          
         CLC   0(2,R2),=C'**'      TEST IF A SKIP AGENCY                        
         BE    VALOUT7                                                          
*                                  FIND AGTAB ENTRY                             
         L     R5,AAGYTAB                                                       
         USING AGYTABD,R5          R5=A(AGENCY TABLE)                           
*                                                                               
VALOUT6  CLI   0(R5),X'FF'         TEST E-O-L                                   
         BE    VALOUTC                                                          
         CLC   AGYOLDA,0(R2)       MATCH ON AGENCY ALPHA                        
         BE    *+12                                                             
         LA    R5,AGYLEN(R5)                                                    
         B     VALOUT6                                                          
VALOUT6A MVC   AGYNEWF,WORK        SET NEW LOGICAL FILE NUMBER                  
         MVC   AGYNEWL,0(R4)       SET NEW AGENCY KEY VALUES                    
         PACK  AGYNEWR,0(1,R4)                                                  
         TM    SYSINDS,X'80'       TEST IF AGENCY VALUE DOES NOT CHANGE         
         BZ    *+16                                                             
         MVC   AGYNEWL,AGYOLDL                                                  
         PACK  AGYNEWR,AGYOLDL                                                  
*                                                                               
VALOUT7  DS    0H                                                               
         LA    R2,3(R2)            BUMP TO NEXT AGENCY                          
         LA    R4,1(R4)                                                         
         BCT   R7,VALOUT4                                                       
*                                                                               
VALOUT8  CLI   CARD+71,C' '        TEST IF CONTINUATION CARD FOLLOWS            
         BE    VALOUTA                                                          
         GOTO1 VCARDS,DMCB,CARD,=C'RE00'                                        
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLC   CARD(8),SPACES                                                   
         BNE   VALOUTX                                                          
         LA    R2,CARD+8                                                        
         LA    R7,21                                                            
         B     VALOUT4                                                          
*                                                                               
VALOUTA  MVI   VALSW,C'Y'                                                       
         AP    OUTCNT,=P'1'        BUMP OUTPUT FILE COUNT                       
         B     VALOUTX                                                          
*                                                                               
VALOUTC  MVC   0(2,R2),=C'??'                                                   
         CLI   TESTOPT,C'Y'                                                     
         BNE   VALOUTX                                                          
         MVC   P(80),CARD                                                       
         GOTO1 VPRINTER                                                         
         B     VALOUT7                                                          
*                                                                               
VALOUTX  DS    0H                                                               
         B     VALPARMX                                                         
         DROP  R3,R5,R6                                                         
         EJECT                                                                  
* VALIDATE CONTROL PARAMETER (CONTROL=UPDATE)                                   
*                                                                               
VALUPT   NTR1                                                                   
         CLC   0(5,R2),=C'TRACE'                                                
         BNE   *+16                                                             
         MVI   UPDTSW,C'T'                                                      
         MVI   VALSW,C'Y'                                                       
         B     VALPARMX                                                         
         CLC   0(6,R2),=C'UPDATE'                                               
         BNE   VALPARMX                                                         
         MVI   UPDTSW,C'Y'                                                      
         MVI   VALSW,C'Y'                                                       
         B     VALPARMX                                                         
         SPACE 1                                                                
* VALIDATE TEST PARAMETER (TEST=YES)                                            
*                                                                               
VALTST   NTR1                                                                   
         MVI   TESTOPT,C'Y'                                                     
         MVI   VALSW,C'Y'                                                       
         B     VALPARMX                                                         
*                                                                               
VALPARMX CLI   VALSW,C'Y'          TEST PARAMETER VALIDITY SWITCH               
         XIT1                                                                   
         DROP  R1                                                               
         EJECT                                                                  
* PROCESS AN INPUT RECORD AND WRITE TO APPROPRIATE OUTPUT FILE(S)               
*                                                                               
PROCREC  NTR1                                                                   
         LA    R4,IO               R4=A(RECORD)                                 
         L     R2,ASYSNTRY                                                      
         USING SYSTABD,R2          R2=A(SYSTEM TABLE ENTRY)                     
         ICM   R3,7,SYSADEF                                                     
         USING RECDEFD,R3          R3=A(RECORD DEFINITION TABLE)                
         LH    R1,0(R4)                                                         
         SH    R1,=H'4'                                                         
         ST    R1,RECORDLN         SET ACTUAL RECORD LENGTH                     
         ST    R4,AREC             SAVE A(RECORD)                               
         LA    R4,4(R4)                                                         
         LA    R1,0(R4,R1)                                                      
         XC    0(2,R1),0(R1)       CLEAR END OF RECORD                          
         L     R5,ASENTRY                                                       
         USING SETABD,R5           R5=A(SE TABLE ENTRY)                         
*                                                                               
PROCREC2 TM    RECINDS,X'80'       TES E-O-T                                    
         BO    PROCREC6                                                         
         CLC   0(2,R4),RECKEYL     TEST KEY RANGE                               
         BL    PROCREC4                                                         
         CLC   0(2,R4),RECKEYH                                                  
         BH    PROCREC4                                                         
*                                  DO 'OR CHARACTER' ON KEY IF REQD             
         OC    RECOCDSP(2),RECOCDSP                                             
         BZ    PROCREC6                                                         
         ZIC   RE,RECOCDSP                                                      
         ZIC   RF,RECOCLEN                                                      
         LA    RE,0(R4,RE)                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    0(0,RE),0(RE)                                                    
         BZ    PROCREC6                                                         
*                                                                               
PROCREC4 LA    R3,RECLEN(R3)       BUMP TO NEXT RECORD TYPE                     
         B     PROCREC2                                                         
*                                                                               
PROCREC6 TM    RECINDS,X'08'       TEST IF PRINT REQD                           
         BZ    *+8                                                              
         BAS   RE,PRNTREC                                                       
         TM    RECINDS,X'10'       TEST IF DELETION REQD                        
         BZ    *+14                                                             
         AP    SEDELCNT,=P'1'      BUMP DELETION COUNT                          
         B     PROCRECX                                                         
         SR    RF,RF                                                            
         ICM   RF,7,RECAROUT       TEST IF A(ROUTINE) REQD                      
         BZ    *+10                                                             
         BASR  RE,RF                                                            
         BNE   PROCRECX                                                         
         MVI   OUTFILE,0                                                        
         OC    RECAGDSP(2),RECAGDSP                                             
         BZ    PROCREC8                                                         
*                                  CONVERT AGENCY KEY VALUE                     
         ZIC   RE,RECAGDSP                                                      
         LA    RE,0(R4,RE)                                                      
         ST    RE,DUB                                                           
         MVC   DUB(1),RECAGTYP                                                  
         CLI   RECAGTYP,AGYA                                                    
         BNE   PROCREC7                                                         
         CLC   0(2,RE),=C'ZZ'      TEST SPECIAL PRINTPAK AGENCY CODE            
         BE    *+14                                                             
         CLC   0(2,RE),=C'00'      TEST FOR SPECIAL AGENCY CODE                 
         BNE   PROCREC7                                                         
         OI    RECINDS,X'01'       SET WRITE TO ALL FILES (TEMP)                
         B     PROCREC8                                                         
*                                                                               
PROCREC7 BAS   RE,AGYCONV                                                       
         BNZ   *+18                                                             
         BAS   RE,PRNTREC          BAD AGENCY - PRINT & DROP RECORD             
         AP    SEDELCNT,=P'1'                                                   
         B     PROCRECX                                                         
         MVC   OUTFILE,AGYLAST+AGYNEWF-AGYTABD                                  
*                                  WRITE RECORD TO OUTPUT FILE(S)               
PROCREC8 L     R5,ASETAB                                                        
         TM    RECINDS,X'65'       TEST IF HEADER/TRAILER/WRITE ALL             
         BNZ   PROCRECA                                                         
         CLI   OUTFILE,0           TEST IF DELETING AN AGENCY                   
         BNE   PROCREC9                                                         
         L     R5,ASENTRY                                                       
         AP    SEDELCNT,=P'1'      BUMP DELETION COUNT                          
         B     PROCRECX                                                         
*                                                                               
PROCREC9 CLC   OUTFILE,SEFILNUM    FIND SE TABLE ENTRY                          
         BE    *+12                                                             
         LA    R5,SELEN(R5)                                                     
         B     *-14                                                             
         TM    SEINDS,X'40'                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R4,7,SEADTF                                                      
         L     R6,AREC                                                          
         PUT   (R4),(R6)                                                        
         AP    SEOUTCNT,=P'1'      BUMP OUTPUT COUNT                            
         B     PROCRECX                                                         
*                                                                               
PROCRECA CLI   0(R5),X'FF'         TEST E-O-T                                   
         BE    PROCRECX                                                         
         TM    SEINDS,X'40'        TEST IF AN OUTPUT FILE                       
         BZ    PROCRECG                                                         
         TM    RECINDS,X'05'       TEST IF WRITING TO ALL FILES                 
         BNZ   PROCRECE                                                         
*                                                                               
         TM    RECINDS,X'40'       TEST FOR HEADER RECORD                       
         BZ    PROCRECC                                                         
         TM    SEINDS,X'20'        IGNORE IF HEADER ALREADY WRITTEN             
         BO    PROCRECG                                                         
         OI    SEINDS,X'20'                                                     
         B     PROCRECE                                                         
*                                                                               
PROCRECC TM    RECINDS,X'20'       TEST FOR TRAILER RECORD                      
         BZ    PROCRECE                                                         
         L     R4,AREC             SAVE TRAILER RECORD & SET SWITCH             
         MVC   EOFSAVE,0(R4)                                                    
         MVI   EOFSW,C'Y'                                                       
         B     PROCRECX                                                         
*                                  WRITE RECORD TO FILE                         
PROCRECE ICM   R4,7,SEADTF                                                      
         L     R6,AREC                                                          
         PUT   (R4),(R6)                                                        
         AP    SEOUTCNT,=P'1'      BUMP OUTPUT COUNT                            
*                                                                               
PROCRECG LA    R5,SELEN(R5)        BUMP TO NEXT SE                              
         B     PROCRECA                                                         
*                                                                               
PROCRECX NI    RECINDS,X'FF'-X'01'                                              
         XIT1                                                                   
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
* CONVERT AGENCY KEY VALUE VIA AGYTAB.                                          
*                                                                               
* ON ENTRY DUB(1)=KEY FORMAT,DUB+1(3)=A(AGENCY KEY VALUE)                       
*                                                                               
AGYCONV  NTR1                                                                   
         ZIC   RE,DUB                                                           
         LA    RF,L'KEYTAB                                                      
         MR    RE,RE                                                            
         LA    R4,KEYTAB-L'KEYTAB(RF)                                           
         USING KEYTABD,R4          R4=A(KEY FORMAT TABLE ENTRY)                 
         L     R3,DUB                                                           
         ZIC   R1,KEYCOMPL         R1=L'EXECUTE                                 
         ZIC   RE,KEYODISP         RE=DISP TO OLD VALUE                         
         ZIC   RF,KEYNDISP         RF=DISP TO NEW VALUE                         
         MVC   WORK(2),0(R3)       SAVE KEY VALUE                               
         L     R6,ASENTRY                                                       
         USING SETABD,R6           R6=A(SE TABLE ENTRY)                         
         CLI   KEYAND,0                                                         
         BE    AGYCONV2                                                         
         ZIC   R2,KEYAND                                                        
         EX    R2,*+8                                                           
         B     *+8                                                              
         NI    0(R3),0             TURN OFF AGENCY IN RECORD                    
         PACK  WORK+2(1),KEYAND                                                 
         IC    R2,WORK+2                                                        
         EX    R2,*+8                                                           
         B     *+8                                                              
         NI    WORK,0              TURN OFF OTHER NIBBLE IN SAVE                
*                                                                               
AGYCONV2 CLC   SEFILNUM,AGYLAST+AGYOLDF-AGYTABD                                 
         BNE   AGYCONV3                                                         
         LA    R5,AGYLAST(RE)      R5=A(LAST AGENCY OLD KEY VALUE)              
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),0(R5)       TEST FOR CHANGE OF AGENCY                    
         BE    AGYCONV6                                                         
*                                  FIND ENTRY IN AGYTAB                         
AGYCONV3 L     R2,AAGYTAB                                                       
         USING AGYTABD,R2          R2=A(AGENCY TABLE)                           
*                                                                               
AGYCONV4 CLI   0(R2),X'FF'         TEST E-O-T                                   
         BNE   *+14                                                             
         XC    AGYLAST,AGYLAST     CLEAR AGENCY VALUES IF BAD CODE              
         B     AGYCONVX                                                         
         CLC   AGYOLDF,SEFILNUM    TEST FOR CORRECT FILE NUMBER                 
         BNE   AGYCONV5                                                         
         LA    R5,0(R2,RE)         R5=A(AGENCY OLD KEY VALUE)                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),0(R5)                                                    
         BE    *+12                                                             
AGYCONV5 LA    R2,AGYLEN(R2)                                                    
         B     AGYCONV4                                                         
         MVC   AGYLAST,0(R2)       SET LAST TIME AGENCY VALUES                  
*                                                                               
AGYCONV6 LA    RE,AGYMVC           'MOVE' OR 'OR' IN NEW KEY VALUE              
         CLI   KEYAND,0                                                         
         BE    *+8                                                              
         LA    RE,AGYOC                                                         
         LA    R5,AGYLAST(RF)                                                   
         EX    R1,0(RE)                                                         
         B     AGYCONVX                                                         
*                                                                               
AGYMVC   MVC   0(0,R3),0(R5)                                                    
AGYOC    OC    0(0,R3),0(R5)                                                    
*                                                                               
AGYCONVX OC    AGYLAST,AGYLAST                                                  
         XIT1                                                                   
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
* PRINT A RECORD IN CHARACTER AND HEX FORMAT                                    
*                                                                               
PRNTREC  NTR1                                                                   
         TM    RECINDS-RECDEFD(R3),X'08'                                        
         BO    PRNTREC1                                                         
         AP    ERRCNT,=P'1'                                                     
         CP    ERRCNT,=P'500'      ONLY PRINT FIRST 500 BAD RECORDS             
         BH    PRNTRECX                                                         
*                                                                               
PRNTREC1 L     R3,RECORDLN                                                      
         L     R2,AREC                                                          
         LA    R2,4(R2)                                                         
         L     RE,ASENTRY                                                       
         MVC   P(L'SENAME),SENAME-SETABD(RE)                                    
         L     RE,ASYSNTRY                                                      
         XR    R4,R4                                                            
         ICM   R4,3,SYSFEDSP-SYSTABD(RE)                                        
         BNZ   PRNTREC4                                                         
*                                                                               
PRNTREC2 SR    R3,R4                                                            
         BNP   PRNTRECX                                                         
         AR    R2,R4                                                            
         LA    R4,L'HEXWORK                                                     
         CR    R4,R3                                                            
         BL    *+6                                                              
         LR    R4,R3                                                            
*                                                                               
PRNTREC4 CP    LINE,=P'57'                                                      
         BL    *+10                                                             
         ZAP   LINE,=P'99'                                                      
         LR    R5,R4                                                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P+10(0),0(R2)                                                    
         GOTO1 VDUMPOUT,DMCB,((R4),P+10),0,0                                    
         GOTO1 VPRINTER                                                         
         GOTO1 VHEXOUT,DMCB,(R2),HEXWORK,(R4),=C'SEP'                           
         LA    R6,HEXWORK                                                       
         LA    R0,2                                                             
*                                                                               
PRNTREC6 EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P+10(0),0(R6)                                                    
         GOTO1 VPRINTER                                                         
         AR    R6,R4                                                            
         BCT   R0,PRNTREC6                                                      
         BASR  RE,RF                                                            
         B     PRNTREC2                                                         
*                                                                               
PRNTRECX XIT1                                                                   
         EJECT                                                                  
* UPDATE CONTROL FILE ID RECORDS WITH NEW AGENCY VALUES.                        
*                                                                               
SETOUT   NTR1                                                                   
         CLI   UPDTSW,C'N'                                                      
         BE    SETOUTX                                                          
         L     R5,ASYSNTRY                                                      
         USING SYSTABD,R5                                                       
         LA    R3,IO                                                            
         USING CTIREC,R3           R3=A(ID RECORD)                              
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   KEY,IO                                                           
         GOTO1 CTIO,DMRDHI         FIRST ID RECORD IS CONTROL SO SKIP           
*                                                                               
SETOUT2  GOTO1 CTIO,DMRSEQ                                                      
         CLI   IO,C'I'                                                          
         BNE   SETOUTX                                                          
*                                  PROCESS AN ID RECORD                         
         MVI   DUB,0                                                            
         LA    R3,IO+CTIDATA-CTIREC                                             
         SR    RF,RF                                                            
*                                                                               
SETOUT4  CLI   0(R3),0                                                          
         BE    SETOUT2                                                          
         CLI   0(R3),CTAGYELQ                                                   
         BE    SETOUT8                                                          
         CLI   0(R3),CTSYSELQ                                                   
         BE    SETOUT12                                                         
*                                                                               
SETOUT6  IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     SETOUT4                                                          
*                                  FIND AGYTAB ENTRY                            
         USING CTAGYD,R3                                                        
SETOUT8  MVC   WORK(2),CTAGYID     SAVE ALPHA-ID                                
         B     SETOUT6                                                          
*                                  PROCESS A SYSTEM ELEMENT                     
         USING CTSYSD,R3                                                        
SETOUT12 CLC   CTSYSNUM,SYSOVNUM   TEST FOR SYSTEM NUMBER                       
         BNE   SETOUT6                                                          
         L     R2,AAGYTAB                                                       
         USING AGYTABD,R2          R2=A(AGENCY TABLE)                           
*                                                                               
SETOUT1A CLI   0(R2),X'FF'         GET ENTRY IN AGENCY TABLE                    
         BE    SETOUT2                                                          
*                                                                               
SETOUT1B CLC   AGYOLDA,WORK                                                     
         BE    SETOUT1D                                                         
*                                                                               
SETOUT1C LA    R2,AGYLEN(R2)                                                    
         B     SETOUT1A                                                         
*                                                                               
SETOUT1D CLI   AGYNEWF,0           TEST IF ON AN OUTPUT FILE                    
         BE    SETOUT2                                                          
*                                                                               
SETOUT14 MVC   CTSYSAGB,AGYNEWL    SET NEW AGENCY BINARY                        
         L     R4,ASETAB                                                        
         USING SETABD,R4                                                        
         CLC   SEFILNUM,AGYNEWF                                                 
         BE    *+12                                                             
         LA    R4,SELEN(R4)                                                     
         B     *-14                                                             
         MVC   CTSYSSE,SENUM       SET NEW SE NUMBER                            
         MVC   ALPHID,WORK         COPY ALPHA ID CODE                           
         MVC   NEWSSENM,CTSYSSE    COPY NEW SE NUMBER                           
         MVC   NEWAGBNM,CTSYSAGB        AND NEW AGENCY BINARY                   
*                                                                               
SETOUT16 MVC   KEY,IO                                                           
         CLI   UPDTSW,C'Y'                                                      
         BE    SETOUT18                                                         
         GOTO1 VPRNTBL,DMCB,0,IO,C'DUMP',1000,=C'2D'                            
         B     SETOUTAC                                                         
*                                                                               
SETOUT18 GOTO1 CTIO,DMWRT          WRITE BACK ID RECORD                         
*                                                                               
SETOUTAC MVC   KEYSAVE,IO          SAVE CURRENT ID RECORD KEY                   
         LA    R3,IO               R3 = A(ACCESS RECORD)                        
         USING CT5REC,R3                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,ALPHID                                                  
         MVC   KEY,IO                                                           
         GOTO1 CTIO,DMREAD         GET THE ACCESS RECORD                        
*                                                                               
         LA    R3,IO+CT5DATA-CT5REC    R3 = A(1ST ELEMENT IN RECORD)            
         SR    RF,RF                                                            
*                                                                               
STOTACLP CLI   0(R3),0             IF NO MORE ELEMENTS                          
         BE    STOTAC22            THEN DONE                                    
         CLI   0(R3),CTSYSELQ      IF SYSTEM AUTHORIZATION ELEMENT              
         BE    STOTAC10            THEN SEE IF WE CAN GET THE SYSTEM            
*                                                                               
STOTACL5 IC    RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         B     STOTACLP            CHECK AGAIN                                  
*                                                                               
         USING CTSYSD,R3                                                        
STOTAC10 CLC   CTSYSNUM,SYSOVNUM   IF NOT THE SAME SYSTEM                       
         BNE   STOTACL5            THEN CHECK NEXT ELEMENT                      
*                                                                               
         MVC   CTSYSSE,NEWSSENM    COPY THE NEW SE NUMBER                       
         MVC   CTSYSAGB,NEWAGBNM        AND THE NEW BINARY AGENCY               
*                                                                               
         MVC   KEY,IO                                                           
         CLI   UPDTSW,C'Y'                                                      
         BE    STOTAC20                                                         
         GOTO1 VPRNTBL,DMCB,0,IO,C'DUMP',1000,=C'2D'                            
         B     STOTAC22                                                         
*                                                                               
STOTAC20 GOTO1 CTIO,DMWRT                                                       
*                                                                               
STOTAC22 MVC   KEY,KEYSAVE         RESTORE LAST ID RECORD KEY                   
         GOTO1 CTIO,DMREAD         READ THE RECORD                              
         B     SETOUT2             AND GO DO READ SEQUENTIAL                    
*                                                                               
SETOUTX  XIT1                                                                   
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
* PRINT CONTROL TOTALS & NEW AGENCY KEY VALUES LISTING                          
*                                                                               
PRNTOTS  NTR1                                                                   
         MVC   TITLE,SPACES                                                     
         MVC   TITLE+18(24),=C'INPUT/OUTPUT FILE COUNTS'                        
         MVC   SUB1(L'SUBA),SUBA                                                
         MVC   SUB2(L'SUBB),SUBB                                                
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         L     R2,ASETAB                                                        
         USING SETABD,R2           R2=A(SE TABLE)                               
*                                                                               
PRNTOTS2 CLI   0(R2),X'FF'         TEST E-O-T                                   
         BE    PRNTOTS8                                                         
         TM    SEINDS,X'C0'        TEST IF INPUT OR OUTPUT FILE                 
         BZ    PRNTOTS6                                                         
         MVC   P(L'SENAME),SENAME                                               
         LA    R3,SEINPCNT                                                      
         LA    R4,P+9                                                           
         LA    R5,3                                                             
*                                                                               
PRNTOTS4 EDIT  (P6,0(R3)),(8,0(R4)),FILL=0                                      
         LA    R3,6(R3)                                                         
         LA    R4,10(R4)                                                        
         BCT   R5,PRNTOTS4                                                      
         GOTO1 VPRINTER                                                         
         BASR  RE,RF                                                            
*                                                                               
PRNTOTS6 LA    R2,SELEN(R2)        BUMP TO NEXT SE                              
         B     PRNTOTS2                                                         
*                                  PRINT NEW AGENCY VALUES LIST                 
PRNTOTS8 MVC   TITLE,SPACES                                                     
         MVC   TITLE+20(21),=C'OLD/NEW AGENCY VALUES'                           
         MVC   SUB1(L'SUBC),SUBC                                                
         MVC   SUB2(L'SUBD),SUBD                                                
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         L     R5,AAGYTAB                                                       
         USING AGYTABD,R5                                                       
*                                                                               
PRNTOTSA CLI   0(R5),X'FF'         TEST E-O-T                                   
         BE    PRNTOTSX                                                         
         MVC   P+2(L'AGYOLDA),AGYOLDA                                           
         L     R2,ASETAB                                                        
*                                                                               
PRNTOTSC CLI   0(R2),X'FF'         TEST E-O-T                                   
         BE    PRNTOTSE                                                         
         CLC   AGYOLDF,SEFILNUM                                                 
         BNE   *+10                                                             
         MVC   P+8(L'SENAME),SENAME                                             
         CLC   AGYNEWF,SEFILNUM                                                 
         BNE   *+10                                                             
         MVC   P+20(L'SENAME),SENAME                                            
         LA    R2,SELEN(R2)                                                     
         B     PRNTOTSC                                                         
*                                                                               
PRNTOTSE GOTO1 VHEXOUT,DMCB,AGYOLDL,P+16,1                                      
         CLI   AGYNEWL,0                                                        
         BE    PRNTOTSG                                                         
         GOTO1 (RF),(R1),AGYNEWL,P+28,1                                         
*                                                                               
PRNTOTSG GOTO1 VPRINTER                                                         
         LA    R5,AGYLEN(R5)       BUMP TO NEXT AGENCY                          
         B     PRNTOTSA                                                         
*                                                                               
PRNTOTSX DS    0H                                                               
         XIT1                                                                   
         DROP  R2,R5                                                            
         EJECT                                                                  
* I/O TO CONTROL FILE                                                           
*                                                                               
* ON ENTRY R1=A(COMMAND)                                                        
*                                                                               
CTIO     NTR1                                                                   
         ST    R1,DMCB                                                          
         GOTO1 VDATAMGR,DMCB,,=C'CTFILE',KEY,IO                                 
         CLI   8(R1),0                                                          
         BE    CTIOX                                                            
         DC    H'0'                                                             
CTIOX    XIT1                                                                   
         EJECT                                                                  
* SPECIAL CODE TO FILTER CERTAIN ACC LEDGERS                                    
*                                                                               
ACFILT   NTR1                                                                   
         CLI   0(R4),X'6F'         SAATCHI ONLY                                 
         BNE   AC99NO                                                           
         CLI   1(R4),X'40'                                                      
         BE    AC99YES             KEEP COMPANY RECORD                          
*                                                                               
         LA    RE,ACLEDTAB                                                      
ACFILT2  CLI   0(RE),X'FF'                                                      
         BE    AC99NO                                                           
         SR    R1,R1                                                            
         IC    R1,2(RE)            R1 LENGTH OF KEY COMPARE                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),1(R4)       COMPARE TO TABLE ENTRY                       
         BE    AC99YES                                                          
         LA    RE,3(RE)                                                         
         B     ACFILT2                                                          
*                                                                               
AC99YES  CR    RB,RB                EQUAL CONDITION ON EXIT                     
         B     ACFILTX                                                          
AC99NO   LTR   RB,RB                NOT EQUAL CONDITION ON EXIT                 
ACFILTX  XIT1                                                                   
*                                                                               
ACLEDTAB DS    0H                                                               
         DC    C'1 ',AL1(1)                                                     
*        DC    C'S ',AL1(2)                                                     
*        DC    C'SI',AL1(2)                                                     
*        DC    C'SJ',AL1(2)                                                     
*        DC    C'SK',AL1(2)                                                     
*        DC    C'SR',AL1(2)                                                     
         DC    X'FF'                                                            
         EJECT                                                                  
* SPECIAL CODE FOR SPOT FILE AGENCY HEADERS                                     
*                                                                               
SPTAGYHD NTR1                                                                   
         MVC   DUB+4(2),1(R4)                                                   
         LA    RE,DUB+4                                                         
         ST    RE,DUB                                                           
         MVI   DUB,AGYA                                                         
         BAS   RE,AGYCONV          CONVERT AGENCY IN SAVE AREA                  
         BZ    SPTAGYHX                                                         
         ZIC   RE,AGYLAST+AGYNEWR-AGYTABD                                       
         LA    RE,HEXTAB(RE)                                                    
         MVC   WORK(1),0(RE)       SAVE NEW HEX AGENCY VALUE                    
         LA    R4,24(R4)                                                        
*                                                                               
SPTAGYH2 CLI   0(R4),0             TEST E-O-R                                   
         BE    SPTAGYHX                                                         
         CLI   0(R4),X'01'         AGENCY ELEMENT                               
         BE    SPTAGYH6                                                         
         CLI   0(R4),X'02'         MEDIA ELEMENT                                
         BE    SPTAGYH8                                                         
*                                                                               
SPTAGYH4 ZIC   R1,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,R1                                                            
         B     SPTAGYH2                                                         
*                                                                               
SPTAGYH6 MVC   92(1,R4),WORK       FIX HEX AGY (AGYPROF+19)                     
         B     SPTAGYH4                                                         
*                                                                               
SPTAGYH8 LA    RE,3(R4)            FIX AGENCY IN MEDIA ELEMENTS                 
         ST    RE,DUB                                                           
         MVI   DUB,AGYL                                                         
         BAS   RE,AGYCONV                                                       
         B     SPTAGYH4                                                         
*                                                                               
SPTAGYHX XIT1                                                                   
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* WORKING STORAGE FOR PROGRAM                                                   
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    CL20                                                             
ALPHID   DS    CL2                                                              
NEWSSENM DS    XL1                                                              
NEWAGBNM DS    XL1                                                              
*                                  A(TABLES)                                    
AAGYTAB  DC    A(AGYTAB)                                                        
ASETAB   DC    A(SETAB)                                                         
ASENTRY  DC    A(0)                                                             
ASYSNTRY DC    A(0)                                                             
AAGYKTAB DC    A(0)                                                             
ADTFTAB  DC    A(DTFTAB)                                                        
ADTFIN   DC    A(TINT)                                                          
*                                  A(ROUTINES)                                  
VCARDS   DC    V(CARDS)                                                         
VCPRINT  DC    V(CPRINT)                                                        
VDATAMGR DC    V(DATAMGR)                                                       
VDUMPOUT DC    V(DUMPOUT)                                                       
VHELLO   DC    V(HELLO)                                                         
VHEXIN   DC    V(HEXIN)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VLOGIO   DC    V(LOGIO)                                                         
VPRINTER DC    V(PRINTER)                                                       
VPRNTBL  DC    V(PRNTBL)                                                        
*                                  CONTROL FILE COMMANDS                        
DMREAD   DC    C'DMREAD '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMWRT    DC    C'DMWRT  '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
         EJECT                                                                  
*                                  SWITCHES ETC.                                
VALSW    DS    C                                                                
UPDTSW   DC    C'N'                                                             
TESTOPT  DC    C'N'                                                             
EOFSW    DC    C'N'                                                             
AGYFORM  DS    C                                                                
OUTCNT   DC    PL2'0'                                                           
ERRCNT   DC    PL6'0'                                                           
OUTFILE  DS    X                                                                
RECORDLN DS    F                                                                
AGYLAST  DC    XL10'00'                                                         
AREC     DS    A                                                                
HEXTAB   DC    C'0123456789ABCDEF'                                              
HEXWORK  DS    2CL100                                                           
*                                                                               
AGYL     EQU   1                                                                
AGYR     EQU   2                                                                
AGYA     EQU   3                                                                
AGYC     EQU   4                                                                
*                                                                               
MESS1    DC    CL45'ENSURE XXXXXXX OUTPUT TAPE MOUNTED ON SYSNNN'               
MESS2    DC    CL45'ENSURE XXXXXXX INPUT TAPE MOUNTED ON SYSNNN'                
MESS3    DC    CL45'EOV/EOF ENQUIRY FOR XXXXXXX INPUT TAPE ?'                   
*                                                                               
SUBA     DC    CL50'SE NAME     INPUT   DELETED    OUTPUT'                      
SUBB     DC    CL50'-------     -----   -------    ------'                      
SUBC     DC    CL50'AGENCY  OLD VALUES  NEW VALUES'                             
SUBD     DC    CL50' CODE   ----------  ----------'                             
*                                  KEY VALUES TABLES                            
ALPHATAB DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',X'FF'                    
BINRYTAB DC    X'102030405060708090A0B0C0D0E0F0',X'FF'                          
*                                  I/O AREAS                                    
CARD     DS    CL80                                                             
KEY      DS    CL25                                                             
KEYSAVE  DS    CL25                                                             
EOFSAVE  DS    CL256                                                            
IO       DS    4000C                                                            
         EJECT                                                                  
*********************************************************************           
*                                                                   *           
* NOTE THAT SETAB HAS A LIMIT ON MAXIMUM SYSTEMS (ON NEXT PAGE )    *           
*                                                                   *           
* BYTE FOLLOWING AL3 IS SYSTEM FILE SET NUMBER. IT MUST AGREE       *           
* WITH ENTRY IN FATAB SELIST ENTRY                                  *           
*********************************************************************           
         SPACE 1                                                                
*                                  INPUT PARAMETER TABLE                        
PARMTAB  DS    0CL20                                                            
         DC    AL1(7),C'TYPFILE=',AL3(VALTYP),8X'00'                            
         DC    AL1(7),C'OUTPUT1=',AL3(VALOUT),AL1(01),7X'00'                    
         DC    AL1(7),C'OUTPUT2=',AL3(VALOUT),AL1(02),7X'00'                    
         DC    AL1(7),C'OUTPUT3=',AL3(VALOUT),AL1(03),7X'00'                    
         DC    AL1(7),C'OUTPUT4=',AL3(VALOUT),AL1(04),7X'00'                    
         DC    AL1(7),C'OUTPUT5=',AL3(VALOUT),AL1(05),7X'00'                    
         DC    AL1(7),C'OUTPUT6=',AL3(VALOUT),AL1(06),7X'00'                    
         DC    AL1(7),C'OUTPUT7=',AL3(VALOUT),AL1(07),7X'00'                    
         DC    AL1(7),C'OUTPUT8=',AL3(VALOUT),AL1(08),7X'00'                    
         DC    AL1(7),C'OUTPUT9=',AL3(VALOUT),AL1(09),7X'00'                    
         DC    AL1(7),C'OUTPUTA=',AL3(VALOUT),AL1(10),7X'00'                    
         DC    AL1(7),C'OUTPUTB=',AL3(VALOUT),AL1(11),7X'00'                    
         DC    AL1(7),C'OUTPUTC=',AL3(VALOUT),AL1(12),7X'00'                    
         DC    AL1(7),C'OUTPUTD=',AL3(VALOUT),AL1(13),7X'00'                    
         DC    AL1(7),C'OUTPUTE=',AL3(VALOUT),AL1(14),7X'00'                    
         DC    AL1(7),C'OUTPUTF=',AL3(VALOUT),AL1(15),7X'00'                    
         DC    AL1(7),C'OUTPUTG=',AL3(VALOUT),AL1(16),7X'00'                    
         DC    AL1(7),C'OUTPUTH=',AL3(VALOUT),AL1(17),7X'00'                    
         DC    AL1(7),C'OUTPUTJ=',AL3(VALOUT),AL1(05),7X'00'                    
         DC    AL1(7),C'OUTPUTK=',AL3(VALOUT),AL1(06),7X'00'                    
         DC    AL1(7),C'OUTPUTL=',AL3(VALOUT),AL1(21),7X'00'                    
         DC    AL1(7),C'OUTPUTM=',AL3(VALOUT),AL1(22),7X'00'                    
         DC    AL1(7),C'OUTPUTN=',AL3(VALOUT),AL1(23),7X'00'                    
         DC    AL1(7),C'OUTPUTQ=',AL3(VALOUT),AL1(26),7X'00'                    
         DC    AL1(7),C'OUTPUTS=',AL3(VALOUT),AL1(28),7X'00'                    
         DC    AL1(7),C'OUTPUTT=',AL3(VALOUT),AL1(29),7X'00'                    
         DC    AL1(7),C'OUTPUTW=',AL3(VALOUT),AL1(32),7X'00'                    
         DC    AL1(7),C'CONTROL=',AL3(VALUPT),8X'00'                            
         DC    AL1(7),C'TEST=YES',AL3(VALTST),8X'00'                            
         DC    X'FF'                                                            
*                                  SYSTEM TABLE                                 
SYSTAB   DS    0CL19                                                            
         DC    C'ACCHST  ',X'06',AL3(ACCDEFN)                                   
         DC    AL2(42,42,49),X'80'                                              
         DC    C'ACCMST  ',X'06',AL3(ACCDEFN)                                   
         DC    AL2(42,42,56),X'80'                                              
         DC    C'SPOTFILE',X'02',AL3(SPTDEFN)                                   
         DC    AL2(13,13,24),X'00'                                              
         DC    C'STAFILE ',X'02',AL3(STADEFN)                                   
         DC    AL2(17,00,00),X'00'                                              
         DC    C'NETFILE ',X'03',AL3(NETDEFN)                                   
         DC    AL2(20,20,27),X'00'                                              
         DC    C'NETSPTF ',X'03',AL3(SPTDEFN)                                   
         DC    AL2(13,13,24),X'00'                                              
         DC    C'NETSTAF ',X'03',AL3(STADEFN)                                   
         DC    AL2(17,00,00),X'00'                                              
         DC    C'MEDFILE ',X'04',AL3(MEDDEFN)                                   
         DC    AL2(15,15,24),X'00'                                              
         DC    C'PRTFILE ',X'04',AL3(PRTDEFN)                                   
         DC    AL2(25,25,33),X'40'                                              
         DC    C'PUBFILE ',X'04',AL3(PUBDEFN)                                   
         DC    AL2(25,25,33),X'40'                                              
         DC    C'BUDFILE ',X'05',AL3(BUDDEFN)                                   
         DC    AL2(32,32,42),X'40'                                              
         DC    C'MPLFILE ',X'05',AL3(MPLDEFN)                                   
         DC    AL2(32,32,42),X'40'                                              
         DC    C'STRFILE ',X'0D',AL3(STRDEFN)                                   
         DC    AL2(13,13,24),X'00'                                              
         DC    X'FF'                                                            
*                                  KEY FORMAT TABLE                             
KEYTAB   DS    0CL4                                                             
         DC    AL1(L'AGYOLDL-1,AGYOLDL-AGYTABD,AGYNEWL-AGYTABD),X'0F'           
         DC    AL1(L'AGYOLDR-1,AGYOLDR-AGYTABD,AGYNEWR-AGYTABD),X'F0'           
         DC    AL1(L'AGYOLDA-1,AGYOLDA-AGYTABD,AGYOLDA-AGYTABD),X'00'           
         DC    AL1(L'AGYOLDL-1,AGYOLDL-AGYTABD,AGYNEWL-AGYTABD),X'00'           
*                                  OUTPUT FILE TABLE                            
DTFTAB   DS    0F                                                               
         DC    A(TOUT1)                                                         
         DC    A(TOUT2)                                                         
         DC    A(TOUT3)                                                         
         DC    A(TOUT4)                                                         
         DC    A(TOUT5)                                                         
         DC    A(TOUT6)                                                         
         DC    A(TOUT7)                                                         
         DC    A(TOUT8)                                                         
         DC    A(TOUT9)                                                         
         DC    A(TOUTA)                                                         
         DC    A(TOUTB)                                                         
         DC    A(TOUTC)                                                         
         DC    A(TOUTD)                                                         
         DC    A(TOUTE)                                                         
         DC    A(TOUTF)                                                         
         DC    A(TOUTG)                                                         
         DC    A(TOUTH)                                                         
         DC    A(TOUTJ)                                                         
         DC    A(TOUTK)                                                         
         DC    A(TOUTL)                                                         
         DC    A(TOUTM)                                                         
         DC    A(TOUTN)                                                         
         DC    A(TOUTQ)                                                         
         DC    A(TOUTT)                                                         
         DC    X'FF'                                                            
*                                  AGENCY VALUES TABLE                          
AGYTAB   DC    X'FF',200XL8'00'                                                 
*                                  SE VALUES TABLE                              
SETAB    DC    100XL32'00'                                                      
         EJECT                                                                  
* ACCOUNT FILE RECORD DEFINITIONS (LAST RECTYPE# WAS 55)                        
*                                                                               
ACCDEFN  DS    0C                                                               
         DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'010001FF',AL1(00,00),AL1(30),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'030003FF',AL1(00,00),AL1(39),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'040004FF',AL1(00,00),AL1(40),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'050005FF',AL1(00,00),AL1(33),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'060006FF',AL1(00,00),AL1(34),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'070007FF',AL1(00,00),AL1(41),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'080008FF',AL1(00,00),AL1(31),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'090009FF',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'0A000AFF',AL1(00,00),AL1(03),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'0B000BFF',AL1(00,00),AL1(04),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'0C000CFF',AL1(00,00),AL1(05),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'0E000EFF',AL1(00,00),AL1(06),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'0F000FFF',AL1(00,00),AL1(42),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'100010FF',AL1(00,00),AL1(37),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'110011FF',AL1(00,00),AL1(43),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'120012FF',AL1(00,00),AL1(44),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'130013FF',AL1(00,00),AL1(45),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'140014FF',AL1(00,00),AL1(46),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'190019FF',AL1(00,00),AL1(32),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'1A001AFF',AL1(00,00),AL1(07),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'1B001BFF',AL1(00,00),AL1(08),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'1C001CFF',AL1(00,00),AL1(47),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'1D001DFF',AL1(00,00),AL1(09),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'1E001EFF',AL1(00,00),AL1(55),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'1F001FFF',AL1(00,00),AL1(10),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'200020FF',AL1(00,00),AL1(11),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'210021FF',AL1(00,00),AL1(12),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'220022FF',AL1(00,00),AL1(13),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'230023FF',AL1(00,00),AL1(14),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'240024FF',AL1(00,00),AL1(15),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'250025FF',AL1(00,00),AL1(16),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'260026FF',AL1(00,00),AL1(17),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'270027FF',AL1(00,00),AL1(18),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'280028FF',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'290029FF',AL1(00,00),AL1(20),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'2A002AFF',AL1(00,00),AL1(21),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'2B002BFF',AL1(00,00),AL1(22),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'2C002CFF',AL1(00,00),AL1(23),X'00',AL3(0)                      
         DC    AL1(2,AGYC)                                                      
         DC    X'2D002D03',AL1(00,00),AL1(24),X'00',AL3(0)                      
         DC    AL1(2,AGYC)                                                      
         DC    X'2D042D04',AL1(00,00),AL1(28),X'00',AL3(0)                      
         DC    AL1(5,AGYC)                                                      
         DC    X'2D052DFF',AL1(00,00),AL1(29),X'00',AL3(0)                      
         DC    AL1(2,AGYC)                                                      
         DC    X'2E002EFF',AL1(00,00),AL1(48),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'2F002FFF',AL1(00,00),AL1(35),X'00',AL3(0)                      
         DC    AL1(2,AGYC)                                                      
         DC    X'300030FF',AL1(00,00),AL1(49),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'310031FF',AL1(00,00),AL1(50),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'320032FF',AL1(00,00),AL1(51),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'330033FF',AL1(00,00),AL1(52),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'340034FF',AL1(00,00),AL1(53),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'350035FF',AL1(00,00),AL1(54),X'00',AL3(0)                      
         DC    AL1(2,AGYC)                                                      
         DC    X'3E003EFF',AL1(00,00),AL1(36),X'00',AL3(0)                      
         DC    AL1(2,AGYC)                                                      
         DC    X'3F003FFF',AL1(00,00),AL1(38),X'00',AL3(0)                      
         DC    AL1(2,AGYC)                                                      
         DC    X'4100FEFF',AL1(00,00),AL1(25),X'00',AL3(ACFILT)                 
         DC    AL1(0,AGYC)                                                      
         DC    X'FFFFFFFF',AL1(00,00),AL1(26),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(27),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         EJECT                                                                  
* MEDIA FILE RECORD DEFINITIONS                                                 
*                                                                               
MEDDEFN  DS    0C                                                               
         DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'C100C1FF',AL1(02,01),AL1(02),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'C100C1FF',AL1(00,00),AL1(03),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'C200C2FF',AL1(00,00),AL1(04),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'C300C3FF',AL1(00,00),AL1(05),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'C400C4FF',AL1(00,00),AL1(06),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'C500C5FF',AL1(00,00),AL1(07),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'C600C6FF',AL1(00,00),AL1(08),X'18',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'C700C7FF',AL1(00,00),AL1(09),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'D100D1FF',AL1(00,00),AL1(10),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'D200D20F',AL1(00,00),AL1(11),X'04',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'D210D2FF',AL1(00,00),AL1(12),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'D300D30F',AL1(00,00),AL1(13),X'04',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'D310D3FF',AL1(00,00),AL1(13),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'D400D4FF',AL1(00,00),AL1(14),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'D500D5FF',AL1(00,00),AL1(15),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'D700D7FF',AL1(00,00),AL1(16),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'E200E2FF',AL1(00,00),AL1(17),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'E300E300',AL1(00,00),AL1(18),X'00',AL3(0)                      
         DC    AL1(27,AGYL)                                                     
         DC    X'E400E400',AL1(00,00),AL1(19),X'04',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'E500E5FF',AL1(00,00),AL1(20),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'E600E6FF',AL1(00,00),AL1(21),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'E700E70F',AL1(00,00),AL1(22),X'04',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'E710E7FF',AL1(00,00),AL1(23),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'D900D9FF',AL1(00,00),AL1(24),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'E800E8FF',AL1(00,00),AL1(25),X'04',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'FFFFFFFF',AL1(00,00),AL1(26),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(27),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         EJECT                                                                  
* SPOT FILE RECORD DEFINITIONS                                                  
*                                                                               
SPTDEFN  DS    0C                                                               
         DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'001000FF',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'021002FF',AL1(00,00),AL1(03),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'030003FF',AL1(00,00),AL1(04),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'050005FF',AL1(00,00),AL1(05),X'00',AL3(0)                      
         DC    AL1(86,AGYA)                                                     
         DC    X'060006FF',AL1(00,00),AL1(06),X'00',AL3(SPTAGYHD)               
         DC    AL1(1,AGYA)                                                      
         DC    X'070007FF',AL1(00,00),AL1(07),X'00',AL3(0)                      
         DC    AL1(3,AGYA)                                                      
         DC    X'080008FF',AL1(00,00),AL1(08),X'00',AL3(0)                      
         DC    AL1(1,AGYA)                                                      
         DC    X'090009FF',AL1(00,00),AL1(09),X'00',AL3(0)                      
         DC    AL1(1,AGYA)                                                      
         DC    X'0A200AFF',AL1(00,00),AL1(09),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0B000BFF',AL1(00,00),AL1(10),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'0C010C0F',AL1(00,00),AL1(24),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0C100CFF',AL1(00,00),AL1(25),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'0D010D02',AL1(00,00),AL1(11),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D030D03',AL1(00,00),AL1(13),X'00',AL3(0)                      
         DC    AL1(8,AGYL)                                                      
         DC    X'0D040D0D',AL1(00,00),AL1(14),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D110D13',AL1(00,00),AL1(15),X'00',AL3(0)                      
         DC    AL1(2,AGYA)                                                      
         DC    X'0D140D17',AL1(00,00),AL1(16),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D200D20',AL1(00,00),AL1(17),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D220D23',AL1(00,00),AL1(18),X'00',AL3(0)                      
         DC    AL1(2,AGYA)                                                      
         DC    X'0D260D26',AL1(00,00),AL1(17),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D270D27',AL1(00,00),AL1(17),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D310D33',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D400D42',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D430D43',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(2,AGYA)                                                      
         DC    X'0D440D76',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D770D77',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(2,AGYA)                                                      
         DC    X'0D780D92',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0E010E01',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0E1C0E1C',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(2,AGYA)                                                      
         DC    X'0E100E2F',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0F020F02',AL1(00,00),AL1(20),X'10',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0A010AFF',AL1(00,00),AL1(21),X'00',AL3(0)                      
         DC    AL1(2,AGYR)                                                      
         DC    X'1000FEFF',AL1(00,00),AL1(22),X'00',AL3(0)                      
         DC    AL1(0,AGYL)                                                      
         DC    X'FFFFFFFF',AL1(00,00),AL1(23),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(24),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         EJECT                                                                  
* STATION FILE RECORD DEFINITIONS                                               
*                                                                               
STADEFN  DS    0C                                                               
         DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'C100C1FF',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(7,AGYA)                                                      
         DC    X'C600C6FF',AL1(00,00),AL1(03),X'00',AL3(0)                      
         DC    AL1(1,AGYA)                                                      
         DC    X'D300D3FF',AL1(00,00),AL1(04),X'00',AL3(0)                      
         DC    AL1(1,AGYA)                                                      
         DC    X'D400D4FF',AL1(00,00),AL1(05),X'00',AL3(0)                      
         DC    AL1(6,AGYA)                                                      
         DC    X'D500D5FF',AL1(00,00),AL1(08),X'00',AL3(0)                      
         DC    AL1(1,AGYA)                                                      
         DC    X'D900D9FF',AL1(00,00),AL1(06),X'00',AL3(0)                      
         DC    AL1(5,AGYA)                                                      
         DC    X'E200E2FF',AL1(00,00),AL1(07),X'00',AL3(0)                      
         DC    AL1(7,AGYA)                                                      
         DC    X'E800E8FF',AL1(00,00),AL1(11),X'00',AL3(0)                      
         DC    AL1(7,AGYA)                                                      
         DC    X'FFFFFFFF',AL1(00,00),AL1(09),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(10),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         EJECT                                                                  
* NETWORK UNTFILE RECORD DEFINITIONS                                            
*                                                                               
NETDEFN  DS    0C                                                               
         DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'020002FF',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(11,AGYL)                                                     
         DC    X'040004FF',AL1(00,00),AL1(03),X'00',AL3(0)                      
         DC    AL1(01,AGYL)                                                     
         DC    X'0A000AFF',AL1(00,00),AL1(04),X'00',AL3(0)                      
         DC    AL1(06,AGYL)                                                     
         DC    X'0C000CFF',AL1(00,00),AL1(05),X'00',AL3(0)                      
         DC    AL1(01,AGYL)                                                     
         DC    X'0D010D01',AL1(00,00),AL1(09),X'00',AL3(0)                      
         DC    AL1(02,AGYL)                                                     
         DC    X'200024FF',AL1(00,00),AL1(06),X'00',AL3(0)                      
         DC    AL1(01,AGYL)                                                     
         DC    X'FFFFFFFF',AL1(00,00),AL1(07),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(08),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         EJECT                                                                  
* PRINT FILE RECORD DEFINITIONS                                                 
*                                                                               
PRTDEFN  DS    0C                                                               
         DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'40404040',AL1(00,00),AL1(05),X'01',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'4100F9FF',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(0,AGYA)                                                      
         DC    X'FFFFFFFF',AL1(00,00),AL1(03),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(04),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         SPACE 1                                                                
* PUBLICATION FILE RECORD DEFINITIONS                                           
*                                                                               
PUBDEFN  DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'4100F9FF',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(7,AGYA)                                                      
         DC    X'FFFFFFFF',AL1(00,00),AL1(03),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(04),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         SPACE 2                                                                
* BUDGET FILE RECORD DEFINITIONS                                                
*                                                                               
BUDDEFN  DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'C200C2FF',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(1,AGYA)                                                      
         DC    X'FFFFFFFF',AL1(00,00),AL1(03),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(04),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         SPACE 2                                                                
* MPL FILE RECORD DEFINITIONS                                                   
*                                                                               
MPLDEFN  DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'E900E9FF',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(2,AGYA)                                                      
         DC    X'FFFFFFFF',AL1(00,00),AL1(03),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(04),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         EJECT                                                                  
         SPACE 2                                                                
* STRAFFIC FILE RECORD DEFINITIONS                                              
*                                                                               
STRDEFN  DS    0C                                                               
         DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0A200AFF',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(02,AGYL)                                                     
         DC    X'FFFFFFFF',AL1(00,00),AL1(03),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(04),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         EJECT                                                                  
* DCB'S FOR TAPE FILES                                                          
*                                                                               
TINT     DCB   DDNAME=TINT,DSORG=PS,MACRF=(GM),EODAD=SP38,             *        
               RECFM=VB,BUFNO=2                                                 
*                                                                               
TOUT1    DCB   DDNAME=TOUT1,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUT2    DCB   DDNAME=TOUT2,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUT3    DCB   DDNAME=TOUT3,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUT4    DCB   DDNAME=TOUT4,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUT5    DCB   DDNAME=TOUT5,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUT6    DCB   DDNAME=TOUT6,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUT7    DCB   DDNAME=TOUT7,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUT8    DCB   DDNAME=TOUT8,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUT9    DCB   DDNAME=TOUT9,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUTA    DCB   DDNAME=TOUTA,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUTB    DCB   DDNAME=TOUTB,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUTC    DCB   DDNAME=TOUTC,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUTD    DCB   DDNAME=TOUTD,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUTE    DCB   DDNAME=TOUTE,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUTF    DCB   DDNAME=TOUTF,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUTG    DCB   DDNAME=TOUTG,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUTH    DCB   DDNAME=TOUTH,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUTJ    DCB   DDNAME=TOUTJ,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUTK    DCB   DDNAME=TOUTK,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUTL    DCB   DDNAME=TOUTL,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUTM    DCB   DDNAME=TOUTM,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUTN    DCB   DDNAME=TOUTN,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUTQ    DCB   DDNAME=TOUTQ,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
*                                                                               
TOUTT    DCB   DDNAME=TOUTT,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=4004,BUFNO=2                        
         EJECT                                                                  
*=========================================================*                     
* SSB ENTRY USED TO SUPPRESS RECOVERY                     *                     
*=========================================================*                     
         SPACE 1                                                                
         ENTRY SSB                                                              
SSB      DC    F'2'                                                             
         SPACE 2                                                                
* DSECT TO COVER INPUT PARAMETER TABLE                                          
*                                                                               
PARMTABD DSECT                                                                  
PARMEXLN DS    AL1                 LENGTH FOR EXECUTE                           
PARMNAME DS    CL8                 PARAMETER OPERAND                            
PARMROUT DS    AL3                 A(VALIDATION ROUTINE)                        
PARMPARM DS    XL8                 EXTRA VALUES FOR ROUTINE                     
PARMLEN  EQU   *-PARMTABD                                                       
         SPACE 1                                                                
* DSECT TO COVER SYSTEM TABLE                                                   
*                                                                               
SYSTABD  DSECT                                                                  
SYSFILE  DS    CL8                 FILE SET NAME                                
SYSOVNUM DS    X                   SYSTEM OVERLAY NUMBER                        
SYSADEF  DS    AL3                 A(RECORD DEFN TABLE)                         
SYSKEYLN DS    AL2                 KEY LENGTH                                   
SYSLNDSP DS    AL2                 DISP TO RECORD LENGTH                        
SYSFEDSP DS    AL2                 DISP TO FIRST ELEMENT                        
SYSINDS  DS    X                   INDICATORS                                   
SYSLEN   EQU   *-SYSTABD                                                        
         SPACE 1                                                                
* DSECT TO COVER SE TABLE                                                       
*                                                                               
SETABD   DSECT                                                                  
SENAME   DS    CL7                 SE NAME                                      
SEOVNUM  DS    X                   SE OVERLAY NUMBER                            
SENUM    DS    X                   SE NUMBER                                    
SEFILNUM DS    X                   SE FILE SET NUMBER                           
SEINDS   DS    X                   SE INDICATORS                                
SEADTF   DS    AL3                 A(OUTPUT TAPE DTF)                           
SEINPCNT DS    PL6                 INPUT FILE COUNT                             
SEDELCNT DS    PL6                 DELETED RECORD COUNT                         
SEOUTCNT DS    PL6                 OUTPUT FILE COUNT                            
SELEN    EQU   *-SETABD                                                         
         EJECT                                                                  
* DSECT TO COVER RECORD DEFINITION TABLE                                        
*                                                                               
RECDEFD  DSECT                                                                  
RECKEYL  DS    XL2                 LOW KEY VALUE                                
RECKEYH  DS    XL2                 HIGH KEY VALUE                               
RECOCDSP DS    AL1                 OC DISP IN KEY                               
RECOCLEN DS    AL1                 OC LENGTH                                    
RECTYPE  DS    AL1                 RECORD TYPE                                  
RECINDS  DS    X                   INDICATORS                                   
*                                  X'80' = EOT                                  
*                                  X'40' = HEADER REC                           
*                                  X'20' = TRAILER REC                          
*                                  X'10' = DELETION REQ'D                       
*                                  X'08' = PRINT REQ'D                          
*                                  X'04' = ? (ASK BEAKY)                        
*                                  X'01' = WRITE TO ALL FILES                   
RECAROUT DS    AL3                 A(RECORD UPDATE ROUTINE)                     
RECAGDSP DS    AL1                 DISP OF AGENCY IN KEY                        
RECAGTYP DS    AL1                 AGENCY FORMAT IN KEY                         
RECLEN   EQU   *-RECDEFD                                                        
         SPACE 1                                                                
* DSECT TO COVER KEY FORMAT TABLE                                               
*                                                                               
KEYTABD  DSECT                                                                  
KEYCOMPL DS    X                   L'AGENCY COMPARE-1                           
KEYODISP DS    X                   DISP TO OLD AGENCY                           
KEYNDISP DS    X                   DISP TO NEW AGENCY                           
KEYAND   DS    X                   KEY 'AND' VALUE                              
         SPACE 1                                                                
* DSECT TO COVER AGENCY VALUES TABLE                                            
*                                                                               
AGYTABD  DSECT                                                                  
AGYOLDF  DS    C                   OLD FILE SET NUMBER                          
AGYOLDL  DS    X                   OLD AGENCY LEFT                              
AGYOLDR  DS    X                   OLD AGENCY RIGHT                             
AGYOLDA  DS    CL2                 OLD AGENCY ALPHA                             
AGYNEWF  DS    C                   NEW FILE SET NUMBER                          
AGYNEWL  DS    X                   NEW AGENCY LEFT                              
AGYNEWR  DS    X                   NEW AGENCY RIGHT                             
AGYLEN   EQU   *-AGYTABD                                                        
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004PCSPANKER 05/01/02'                                      
         END                                                                    
