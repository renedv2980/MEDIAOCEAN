*          DATA SET DDSPANKER  AT LEVEL 011 AS OF 05/24/19                      
*PHASE SPANKERA                                                                 
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
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRTREC                                                                 
*INCLUDE QSORT                                                                  
SPANKER  TITLE 'AGENCY RE-DISTRIBUTION PROGRAM'                                 
SPANKER  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**SPANK,=V(REGSAVE),R9,RA                                      
         L     R8,VCPRINT                                                       
         USING DPRINT,R8           R8=A(PRINT CSECT)                            
         MVC   TITLE(13),=C'Control Cards'                                      
         B     SP2                                                              
                                                                                
EXITNEQ  CR    RB,RC                                                            
         J     *+6                                                              
EXITEQ   CR    RB,RB                                                            
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETER CARDS                                                      
***********************************************************************         
SP2      GOTOR VCARDS,DMCB,CARD,=C'RE00'                                        
         CLC   CARD(2),=C'/*'      TEST FOR E-O-F                               
         BE    SP10                                                             
         MVC   P(L'CARD),CARD                                                   
         GOTOR VPRINTER            PRINT ALL THE PARAMETER CARDS                
                                                                                
         USING PARMTABD,R1         R1=A(PARAMETER TABLE)                        
         LARL  R1,PARMTAB                                                       
         SR    RF,RF                                                            
                                                                                
SP4      CLI   PARMTABD,X'FF'                                                   
         BE    SP6                                                              
         IC    RF,0(R1)            GET EXECUTE LENGTH                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   PARMNAME(0),CARD    COMPARE INPUT CARD WITH TABLE                
         BE    SP5                                                              
SP4A     AHI   R1,PARMLEN                                                       
         B     SP4                                                              
                                                                                
SP5      LA    R2,CARD+1(RF)       Point to where C'=' should be                
         CLI   0(R2),C'='          Just to make sure                            
         BNE   SP4A                Not a match, try again                       
         AHI   R2,1                R2=A(CARD DATA)                              
         ICM   RF,7,PARMROUT       RF=A(VALIDATION ROUTINE)                     
         MVI   VALSW,C'N'          SET VALIDITY SWITCH TO INVALID               
         GOTOR (RF)                                                             
         BE    SP2                                                              
                                                                                
SP6      CLI   VALSW,C'O'          Over ride message                            
         BE    *+10                Yes                                          
         MVC   P(20),=C'INVALID CONTROL CARD'                                   
         GOTOR VPRINTER                                                         
                                                                                
         MVC   P+10(80),CARD       OUTPUT INVALID CONTROL CARD                  
         GOTOR VPRINTER                                                         
         GOTOR VPRINT,DMCB,=C'CLOSE'                                            
         DC    H'0'                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DEDUCE WHICH INPUT TAPE FILES SHOULD BE MOUNTED FROM AGYTAB                   
***********************************************************************         
SP10     OC    ASYSNTRY,ASYSNTRY   TEST IF FILETYP CARD PROCESSED               
         BZ    SP6                                                              
         CP    OUTCNT,=P'0'        TEST IF OUTPUT CARD(S) PROCESSED             
         BE    SP6                                                              
                                                                                
         L     R5,AAGYTAB          R5=A(AGENCY TABLE)                           
         USING AGYTABD,R5                                                       
SP12     CLI   AGYTABD,X'FF'       TEST E-O-T                                   
         BE    SP14                                                             
         L     R2,ASETAB                                                        
         USING SETABD,R2           R2=A(SE TABLE)                               
         CLC   SEFILNUM,AGYOLDF    FIND ENTRY FOR OLD FILE IN SETAB             
         BE    *+12                                                             
         AHI   R2,SELEN                                                         
         B     *-14                                                             
         CLI   AGYNEWF,0                                                        
         BE    *+8                                                              
         OI    SEINDS,X'80'        SET INPUT TAPE REQUIRED                      
         AHI   R5,AGYLENQ          BUMP TO NEXT AGENCY                          
         B     SP12                                                             
*                                  OPEN ALL OUTPUT FILES & SET A(DTF)           
SP14     L     R2,ASETAB           IN SETAB ENTRIES                             
         L     R3,ADTFTAB          R3=A(DTF TABLE)                              
         CLI   TESTOPT,C'Y'                                                     
         BE    SP52                                                             
                                                                                
SP16     CLI   SETABD,X'FF'        TEST E-O-T                                   
         BE    SP30                                                             
         TM    SEINDS,X'40'        TEST OUTPUT FILE REQUIRED                    
         BZ    SP18                                                             
         MVC   MESS1+7(L'SENAME),SENAME                                         
         L     R4,0(R3)                                                         
         STCM  R4,7,SEADTF         SET SYSNUM IN OUTPUT MESSAGE                 
         EDIT  (B1,7(R4)),(3,MESS1+41),FILL=0                                   
         OPEN  ((R4),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         AHI   R3,4                BUMP TO NEXT DTF                             
                                                                                
SP18     AHI   R2,SELEN            BUMP TO NEXT SE                              
         B     SP16                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS ALL INPUT FILES                                                       
***********************************************************************         
SP30     L     R2,ASETAB                                                        
         USING SETABD,R2           R2=A(SE TABLE)                               
                                                                                
SP32     CLI   SETABD,X'FF'        TEST E-O-T                                   
         BE    SP44                                                             
         TM    SEINDS,X'80'        TEST IF INPUT FILE REQUIRED                  
         BZ    SP42                                                             
         MVC   TITLE,SPACES                                                     
         MVC   TITLE+20(L'SENAME),SENAME                                        
         MVC   TITLE+28(12),=C'Record Print'                                    
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         ST    R2,ASENTRY          SAVE A(SE TABLE ENTRY)                       
                                                                                
         MVC   MESS2+7(L'SENAME),SENAME                                         
         L     R4,ADTFIN           SET FILE SYSNUM IN OUTPUT MESSAGE            
         EDIT  (B1,7(R4)),(3,MESS2+40),FILL=0                                   
         MVC   40(7,R4),SENAME     SET SENAME AS INPUT DD NAME                  
         OPEN  ((R4),INPUT)        AND OPEN IT                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
SP36     GET   (R4),IO             PROCESS RECORDS                              
         AP    SEINPCNT,=P'1'                                                   
         GOTOR PRCREC                                                           
         B     SP36                                                             
                                                                                
SP38     CLOSE ((R4))              INPUT TAPE E-O-F ENTERS HERE                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
SP42     AHI   R2,SELEN            BUMP TO NEXT SE                              
         B     SP32                                                             
                                                                                
SP44     L     R2,ASETAB           CLOSE ALL OUTPUT FILES                       
SP46     CLI   SETABD,X'FF'        TEST E-O-T                                   
         BE    SP52                                                             
         TM    SEINDS,X'40'        TEST THIS IS AN OUTPUT FILE                  
         BZ    SP50                                                             
         ICM   R4,7,SEADTF                                                      
         CLI   EOFSW,C'Y'          IF EOF RECORD SAVED WRITE NOW                
         BNE   SP48                                                             
         PUT   (R4),EOFSAVE                                                     
         AP    SEOUTCNT,=P'1'                                                   
                                                                                
SP48     CLOSE ((R4))                                                           
                                                                                
SP50     AHI   R2,SELEN            BUMP TO NEXT SE                              
         B     SP46                                                             
                                                                                
SP52     GOTOR UPDCTF              UPDATE CONTROL FILE ID RECORDS               
         GOTOR PRTTOT              PRINT RECORD TOTALS                          
         GOTOR ERRPRT              PRINT ERRORS                                 
         GOTOR VDATAMGR,DMCB,=C'DMCLSE',=C'CONTROL'                             
         XBASE ,                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE TYPFILE PARAMETER (TYPFILE=FILE NAME), BUILD SETAB FROM              
* CTFILE SYSTEM LIST RECORD AND BUILD AGYTAB FROM CTFILE ID RECORDS.            
***********************************************************************         
         USING PARMTABD,R1                                                      
VALTYP   NTR1  ,                                                                
         BAS   RE,CHKOPEN                                                       
         OC    ASYSNTRY,ASYSNTRY   TEST IF TYPFILE ALREADY READ                 
         BNZ   VALPARMX                                                         
         L     R4,=A(SYSTAB)                                                    
         USING SYSTABD,R4          R4=A(SYSTEM FILE NAME TABLE)                 
                                                                                
VALTYP02 CLI   SYSTABD,X'FF'       TEST E-O-T                                   
         BE    VALPARMX                                                         
         CLC   SYSFILE,0(R2)       FIND SYSTAB ENTRY                            
         BE    *+12                                                             
         AHI   R4,SYSLEN                                                        
         B     VALTYP02                                                         
*                                                                               
         ST    R4,ASYSNTRY         SAVE A(SYSTAB ENTRY)                         
         LA    RE,ALPHATAB         SET A(KEY ALLOCATION TABLE)                  
         TM    SYSINDS,X'C0'                                                    
         BNZ   *+8                                                              
         LA    RE,BINRYTAB                                                      
         ST    RE,AAGYKTAB                                                      
                                                                                
         USING CTWREC,IO           READ SYSTEM LIST RECORD                      
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,CTWKTYPQ                                                 
         MVI   CTWKREC,CTWKRSYS                                                 
         MVC   CTWKSUB,SYSOVNUM    READ SPECIFIC SYSTEM LIST                    
         MVC   KEY,IO                                                           
         GOTOR CTIO,DMREAD                                                      
                                                                                
         LA    R3,CTWDATA          BUILD SETAB FROM SYSTEM LIST RECORD          
         L     R2,ASETAB                                                        
         USING SETABD,R2                                                        
         SR    R0,R0                                                            
VALTYP04 CLI   0(R3),0                                                          
         BE    VALTYP08                                                         
         CLI   0(R3),CTLSTELQ      X'A4'                                        
         BNE   VALTYP06                                                         
         CLC   SYSOVNUM,10(R3)     TEST IF SAME SYSTEM NUMBER                   
         BNE   VALTYP06                                                         
         MVC   SENAME,3(R3)                                                     
         MVC   SEOVNUM,10(R3)                                                   
         MVC   SENUM,11(R3)                                                     
         MVC   SEFILNUM,12(R3)                                                  
         ZAP   SEINPCNT,=P'0'                                                   
         ZAP   SEDELCNT,=P'0'                                                   
         ZAP   SEOUTCNT,=P'0'                                                   
         AHI   R2,SELEN            BUMP TO NEXT SE                              
*                                                                               
VALTYP06 IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     VALTYP04                                                         
*                                  PROCESS A SYSTEM LIST ELEMENT                
VALTYP08 MVI   SETABD,X'FF'        SET END OF SETAB                             
         USING CTIREC,IO           READ ID RECORDS & BUILD AGYTAB               
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVI   CTIKID,C' '                                                      
         MVC   KEY,IO                                                           
*                                                                               
         LA    R1,DMRDHI                                                        
         B     *+8                                                              
VALTYP10 LA    R1,DMRSEQ                                                        
         GOTOR CTIO                                                             
         CLI   CTIKTYP,CTIKTYPQ                                                 
         BNE   VALTYP30                                                         
*                                                                               
         XR    R3,R3               MAKE SURE A ZERO MARKS END OF RECORD         
         ICM   R3,3,CTILEN                                                      
         LA    R3,CTIREC(R3)                                                    
         MVI   0(R3),0                                                          
*                                                                               
         LA    R3,CTIDATA          PROCESS AN ID RECORD                         
         SR    R0,R0                                                            
         XC    WORK(2),WORK                                                     
                                                                                
VALTYP12 CLI   0(R3),0                                                          
         BE    VALTYP10                                                         
                                                                                
         USING CTAGYD,R3                                                        
         CLI   CTAGYEL,CTAGYELQ    AGENCY ID ELEMENT                            
         BNE   *+14                                                             
         MVC   WORK(2),CTAGYID                                                  
         B     VALTYP14                                                         
                                                                                
         USING CTSYSD,R3                                                        
         CLI   CTSYSEL,CTSYSELQ    SYSTEM ELEMENT                               
         BNE   VALTYP14                                                         
         OC    WORK(2),WORK        EXIT IF NO AGENCY ID ELEMENT                 
         BZ    VALTYP10                                                         
         CLC   SYSOVNUM,CTSYSNUM                                                
         BE    VALTYP18                                                         
                                                                                
VALTYP14 IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     VALTYP12                                                         
*                                  PROCESS AN AGENCY ID ELEMENT                 
*                                  PROCESS A SYSTEM ELEMENT                     
*                                  TEST IF AGYTAB ENTRY MADE ALREADY            
VALTYP18 L     R5,AAGYTAB                                                       
         USING AGYTABD,R5          R5=A(AGYTAB)                                 
         L     R2,ASETAB                                                        
                                                                                
VALTYP20 CLC   AGYALFA,WORK                                                     
         BE    VALTYP10                                                         
         CLI   AGYTABD,X'FF'       TEST E-O-T                                   
         BE    *+12                                                             
         AHI   R5,AGYLENQ                                                       
         B     VALTYP20                                                         
*                                  ADD AN AGYTAB ENTRY                          
VALTYP22 CLI   SETABD,X'FF'        FIND CURRENT SETAB ENTRY FOR AGENCY          
         BE    VALTYP10                                                         
         CLC   SENUM,CTSYSSE                                                    
         BE    *+12                                                             
         AHI   R2,SELEN                                                         
         B     VALTYP22                                                         
         XC    AGYTABD(AGYLENQ),AGYTABD                                         
         MVC   AGYOLDF,SEFILNUM                                                 
         MVC   AGYOLDL,CTSYSAGB                                                 
         PACK  AGYOLDR,CTSYSAGB                                                 
         MVC   AGYALFA,WORK                                                     
                                                                                
         MVC   P(10),IO+15         PRINT OUT ID AND AGYALFA                     
         MVC   P+12(2),AGYALFA                                                  
         MVC   P+16(7),SENAME                                                   
         GOTOR VHEXOUT,DMCB,AGYOLDL,P+25,1,=C'TOG'                              
         GOTOR VPRINTER                                                         
                                                                                
         AHI   R5,AGYLENQ                                                       
         C     R5,=A(AGYTABX)                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
         MVI   AGYTABD,X'FF'       SET NEW E-O-T                                
         B     VALTYP10                                                         
                                                                                
* SORT THE AGENCY LIST                                                          
                                                                                
VALTYP30 L     R5,AAGYTAB                                                       
                                                                                
VALTYP32 CLI   AGYTABD,X'FF'                                                    
         BE    *+12                                                             
         AHI   R5,AGYLENQ                                                       
         B     VALTYP32                                                         
         LR    R0,R5                                                            
         S     R0,AAGYTAB          GIVES LENGTH OF LIST                         
         SRDL  R0,32                                                            
         D     R0,=A(AGYLENQ)                                                   
         ST    R1,DMCB+4           SET NUMBER OF ENTRIES                        
         LHI   R0,AGYLENQ                                                       
         ST    R0,DMCB+8           SET ENTRY LENGTH                             
         LA    R0,L'AGYALFA                                                     
         ST    R0,DMCB+12          SET KEY LENGTH                               
         LA    R0,AGYALFA-AGYTABD                                               
         ST    R0,DMCB+16          SET KEY DISPLACEMENT                         
         GOTOR VQSORT,DMCB,AAGYTAB                                              
                                                                                
VALTYP40 MVI   VALSW,C'Y'                                                       
         B     VALPARMX                                                         
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
***********************************************************************         
* VALDDS - SUPPORT DDSIO= CARD                                                  
***********************************************************************         
VALDDS   NTR1  ,                                                                
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6                                                   
         MVI   VALSW,C'Y'                                                       
         B     VALPARMX                                                         
                                                                                
VALDSP   NTR1  ,                                                                
         CLI   OPENSW,C'Y'                                                      
         BE    VALDSP20            Too late                                     
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),CARD+7                                     
         MVI   VALSW,C'Y'                                                       
         B     VALPARMX                                                         
*                                                                               
VALDSP20 MVC   P(L'MSG2),MSG2  DSPACE needs to be first                         
         MVI   VALSW,C'O'          Over-ride message                            
         B     VALPARMX                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE OUTPUT PARAMETERS (OUTPUTN=A1,A2..,AN) WHERE 'N' IS THE              
* LOGICAL FILE NUMBER AND A1-AN ARE 2CHR AGENCY IDS. UPDATE AGYTAB              
* ENTRIES WITH NEW LOGICAL FILE NUMBER AND NEW AGENCY KEY VALUES.               
***********************************************************************         
VALOUT   NTR1  ,                                                                
         BAS   RE,CHKOPEN                                                       
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
         NI    OUTIND,X'FF'-OUTXAI INIT OUTPUT CARD INDICATOR                   
                                                                                
VALOUT02 CLI   SENAME,X'FF'        FIND SETAB ENTRY FOR OUTPUT FILE             
         BE    VALOUTX                                                          
         CLC   SEFILNUM,WORK                                                    
         BE    *+12                                                             
         AHI   R3,SELEN                                                         
         B     VALOUT02                                                         
         OI    SEINDS,X'40'        SET OUTPUT FILE REQUIRED                     
         LA    R0,21               R0=MAX NUMBER OF AGYS/CARD                   
*                                  PROCESS AGENCY SPECS                         
VALOUT04 CLI   0(R2),C' '          TEST END OF INPUT                            
         BE    VALOUT20                                                         
         TM    SYSINDS,X'80'                                                    
         BO    VALOUT05                                                         
         CLI   0(R4),X'FF'         TEST END OF KEY VALUES TABLE                 
         BNE   VALOUT05                                                         
         TM    SYSINDS,X'01'       ALLOW EXTRAS?                                
         BZ    VALOUTX             NO                                           
         OI    OUTIND,OUTXAI                                                    
*                                  PROCESS AGENCY SPECS                         
VALOUT05 CLC   0(2,R2),=C'**'      TEST IF A SKIP AGENCY                        
         BE    VALOUT18                                                         
                                                                                
         L     R5,AAGYTAB          FIND AGTAB ENTRY                             
         USING AGYTABD,R5          R5=A(AGENCY TABLE)                           
VALOUT06 CLI   AGYTABD,X'FF'       TEST E-O-L                                   
         BE    VALOUT28                                                         
         CLC   AGYALFA,0(R2)       MATCH ON AGENCY ALPHA                        
         BE    VALOUT08                                                         
         AHI   R5,AGYLENQ                                                       
         B     VALOUT06                                                         
*                                                                               
VALOUT08 TM    SYSINDS,X'01'       ALLOW EXTRAS / ZERO VALUES                   
         BZ    VALOUT16            NO                                           
         CLI   AGYOLDL,0           IF IT HAS A ZERO VALUE, LEAVE ZERO           
         BE    VALOUT12                                                         
         TM    OUTIND,OUTXAI       ONLY ZERO VALUES ALLOWED AS EXTRAS           
         BZ    VALOUT16                                                         
         B     VALERRX                                                          
*                                                                               
VALOUT12 TM    OUTIND,OUTXAI       ZERO VALUES NEED TO BE EXTRAS                
         BZ    VALERRX                                                          
*                                                                               
VALOUT14 MVC   AGYNEWF,WORK        SET NEW LOGICAL FILE NUMBER                  
         MVC   AGYNEWL,AGYOLDL     SET NEW AGENCY KEY VALUES                    
         PACK  AGYNEWR,AGYOLDL                                                  
         B     VALOUT18                                                         
*                                                                               
VALOUT16 MVC   AGYNEWF,WORK        SET NEW LOGICAL FILE NUMBER                  
         MVC   AGYNEWL,0(R4)       SET NEW AGENCY KEY VALUES                    
         PACK  AGYNEWR,0(1,R4)                                                  
         TM    SYSINDS,X'80'       TEST IF AGENCY VALUE DOES NOT CHANGE         
         BZ    VALOUT18                                                         
         MVC   AGYNEWL,AGYOLDL                                                  
         PACK  AGYNEWR,AGYOLDL                                                  
                                                                                
VALOUT18 AHI   R2,3                BUMP TO NEXT AGENCY                          
         TM    OUTIND,OUTXAI       STOP INCREMENTING                            
         BO    *+8                 Yes                                          
         AHI   R4,1                                                             
         BCT   R0,VALOUT04                                                      
         DROP  R6                                                               
                                                                                
VALOUT20 CLI   CARD+71,C' '        TEST IF CONTINUATION CARD FOLLOWS            
         BE    VALOUT26                                                         
         GOTOR VCARDS,DMCB,CARD,=C'RE00'                                        
         MVC   P(L'CARD),CARD                                                   
         GOTOR VPRINTER                                                         
                                                                                
         CLC   CARD(8),SPACES                                                   
         BNE   VALOUTX                                                          
         LA    R2,CARD+8                                                        
         LA    R0,21                                                            
         B     VALOUT04                                                         
                                                                                
VALOUT26 MVI   VALSW,C'Y'                                                       
         AP    OUTCNT,=P'1'        BUMP OUTPUT FILE COUNT                       
         B     VALOUTX                                                          
                                                                                
VALOUT28 MVC   0(2,R2),=C'??'                                                   
         L     RE,ASYSNTRY                                                      
         USING SYSTABD,RE                                                       
         CLI   SYSOVNUM,X'0D'      TEST STRF                                    
         BE    VALOUT30            SO FEW USERS, IT'S NOT FATAL                 
         DROP  RE                                                               
         CLI   TESTOPT,C'Y'                                                     
         BNE   VALOUTX                                                          
                                                                                
VALOUT30 MVC   P(80),CARD                                                       
         GOTOR VPRINTER                                                         
         B     VALOUT18                                                         
                                                                                
VALOUTX  B     VALPARMX                                                         
VALERRX  LTR   RB,RB                                                            
         B     VALPARMX                                                         
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE CONTROL PARAMETER (CONTROL=UPDATE)                                   
***********************************************************************         
VALUPT   NTR1  ,                                                                
         CLC   =C'TRACE',0(R2)                                                  
         BNE   VALUPT10                                                         
         MVI   UPDTSW,C'T'                                                      
         MVI   VALSW,C'Y'                                                       
         B     VALPARMX                                                         
                                                                                
VALUPT10 CLC   =C'UPDATE',0(R2)                                                 
         BNE   VALPARMX                                                         
         CLI   OPENSW,C'Y'                                                      
         BNE   VALUPT20                                                         
         MVC   P(L'MSG1),MSG1      CONTROL=UPDATE must be sooner                
         MVI   VALSW,C'O'          Over-ride message                            
         B     VALPARMX                                                         
                                                                                
VALUPT20 MVI   UPDTSW,C'Y'                                                      
         MVI   VALSW,C'Y'                                                       
         B     VALPARMX                                                         
***********************************************************************         
* VALIDATE TEST PARAMETER (TEST=YES)                                            
***********************************************************************         
VALTST   NTR1  ,                                                                
         CLC   =C'YES',0(R2)                                                    
         BNE   VALPARMX                                                         
         MVI   TESTOPT,C'Y'                                                     
         MVI   VALSW,C'Y'                                                       
         B     VALPARMX                                                         
                                                                                
VALPARMX CLI   VALSW,C'Y'          TEST PARAMETER VALIDITY SWITCH               
         J     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS AN INPUT RECORD AND WRITE TO APPROPRIATE OUTPUT FILE(S)               
***********************************************************************         
PRCREC   NTR1  ,                                                                
         LA    R4,IO               R4=A(RECORD)                                 
         L     R2,ASYSNTRY                                                      
         USING SYSTABD,R2          R2=A(SYSTEM TABLE ENTRY)                     
         ICM   R3,7,SYSADEF                                                     
         USING RECDEFD,R3          R3=A(RECORD DEFINITION TABLE)                
         MVI   DEFINED,C'Y'                                                     
         LH    R1,0(R4)                                                         
         SHI   R1,4                                                             
         ST    R1,RECORDLN         SET ACTUAL RECORD LENGTH                     
         ST    R4,AREC             SAVE A(RECORD)                               
         AHI   R4,4                                                             
         AR    R1,R4                                                            
         XC    0(2,R1),0(R1)       CLEAR END OF RECORD                          
         L     R5,ASENTRY                                                       
         USING SETABD,R5           R5=A(SE TABLE ENTRY)                         
                                                                                
PRCREC02 TM    RECINDS,X'80'       TEST E-O-T                                   
         BZ    *+12                                                             
         MVI   DEFINED,C'N'                                                     
         B     PRCREC06                                                         
                                                                                
         CLC   0(2,R4),RECKEYL     TEST KEY RANGE                               
         BL    PRCREC04                                                         
         CLC   0(2,R4),RECKEYH                                                  
         BH    PRCREC04                                                         
                                                                                
         OC    RECOCVAL,RECOCVAL   DO 'OR CHARACTER' ON KEY IF REQD             
         BZ    PRCREC06                                                         
         SR    RE,RE                                                            
         IC    RE,RECOCDSP                                                      
         SR    RF,RF                                                            
         IC    RF,RECOCLEN                                                      
         AR    RE,R4                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    0(0,RE),0(RE)                                                    
         BZ    PRCREC06                                                         
                                                                                
PRCREC04 AHI   R3,RECLEN           BUMP TO NEXT RECORD TYPE                     
         B     PRCREC02                                                         
                                                                                
PRCREC06 TM    RECINDS,X'08'       TEST IF PRINT REQD                           
         BO    *+12                                                             
         CLI   DEFINED,C'N'                                                     
         BNE   *+8                                                              
         GOTOR PRTREC                                                           
         TM    RECINDS,X'10'       TEST IF DELETION REQD                        
         BZ    *+14                                                             
         AP    SEDELCNT,=P'1'      BUMP DELETION COUNT                          
         B     PRCRECX                                                          
                                                                                
         SR    RF,RF                                                            
         ICM   RF,7,RECAROUT       TEST IF A(ROUTINE) REQD                      
         BZ    PRCREC07                                                         
         GOTOR (RF)                                                             
         BE    PRCREC07                                                         
         OI    RECINDS,X'01'       SET WRITE TO ALL FILES (KEEP IT)             
         B     PRCREC12                                                         
                                                                                
PRCREC07 MVI   OUTFILE,0                                                        
         OC    RECAGDSP(2),RECAGDSP                                             
         BZ    PRCREC12                                                         
                                                                                
         SR    RE,RE               CONVERT AGENCY KEY VALUE                     
         IC    RE,RECAGDSP                                                      
         AR    RE,R4                                                            
         ST    RE,DUB                                                           
         MVC   DUB(1),RECAGTYP                                                  
         CLI   RECAGTYP,AGYA                                                    
         BNE   PRCREC08                                                         
         CLC   0(2,RE),=C'ZZ'      TEST SPECIAL PRINTPAK AGENCY CODE            
         BE    *+14                                                             
         CLC   0(2,RE),=C'00'      TEST FOR SPECIAL AGENCY CODE                 
         BNE   PRCREC08                                                         
         OI    RECINDS,X'01'       SET WRITE TO ALL FILES (TEMP)                
         B     PRCREC12                                                         
                                                                                
PRCREC08 MVI   ISINERR,C'N'                                                     
         GOTOR AGYCNV                                                           
         BNZ   PRCREC10                                                         
         MVI   ISINERR,C'Y'                                                     
         GOTOR PRTREC              BAD AGENCY - PRINT & DROP RECORD             
         AP    SEDELCNT,=P'1'                                                   
         B     PRCRECX                                                          
                                                                                
PRCREC10 MVC   OUTFILE,AGYLAST+AGYNEWF-AGYTABD                                  
                                                                                
PRCREC12 L     R5,ASETAB           WRITE RECORD TO OUTPUT FILE(S)               
         TM    RECINDS,X'65'       TEST IF HEADER/TRAILER/WRITE ALL             
         BNZ   PRCREC16                                                         
         CLI   OUTFILE,0           TEST IF DELETING AN AGENCY                   
         BNE   PRCREC14                                                         
         L     R5,ASENTRY                                                       
         AP    SEDELCNT,=P'1'      BUMP DELETION COUNT                          
         B     PRCRECX                                                          
                                                                                
PRCREC14 CLC   OUTFILE,SEFILNUM    FIND SE TABLE ENTRY                          
         BE    *+12                                                             
         AHI   R5,SELEN                                                         
         B     PRCREC14                                                         
         TM    SEINDS,X'40'                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R4,7,SEADTF                                                      
         L     R6,AREC                                                          
         PUT   (R4),(R6)                                                        
         AP    SEOUTCNT,=P'1'      BUMP OUTPUT COUNT                            
         B     PRCRECX                                                          
                                                                                
PRCREC16 CLI   0(R5),X'FF'         TEST E-O-T                                   
         BE    PRCRECX                                                          
         TM    SEINDS,X'40'        TEST IF AN OUTPUT FILE                       
         BZ    PRCREC22                                                         
         TM    RECINDS,X'05'       TEST IF WRITING TO ALL FILES                 
         BNZ   PRCREC20                                                         
                                                                                
         TM    RECINDS,X'40'       TEST FOR HEADER RECORD                       
         BZ    PRCREC18                                                         
         TM    SEINDS,X'20'        IGNORE IF HEADER ALREADY WRITTEN             
         BO    PRCREC22                                                         
         OI    SEINDS,X'20'                                                     
         B     PRCREC20                                                         
                                                                                
PRCREC18 TM    RECINDS,X'20'       TEST FOR TRAILER RECORD                      
         BZ    PRCREC20                                                         
         L     R4,AREC             SAVE TRAILER RECORD & SET SWITCH             
         MVC   EOFSAVE,0(R4)                                                    
         MVI   EOFSW,C'Y'                                                       
         B     PRCRECX                                                          
                                                                                
PRCREC20 ICM   R4,7,SEADTF         WRITE RECORD TO FILE                         
         L     R6,AREC                                                          
         PUT   (R4),(R6)                                                        
         AP    SEOUTCNT,=P'1'      BUMP OUTPUT COUNT                            
                                                                                
PRCREC22 AHI   R5,SELEN            BUMP TO NEXT SE                              
         B     PRCREC16                                                         
                                                                                
PRCRECX  NI    RECINDS,X'FF'-X'01'                                              
         J     EXIT                                                             
         LTORG                                                                  
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
CHKOPEN  NTR1                                                                   
         CLI   OPENSW,C'Y'                                                      
         JE    EXIT                                                             
         MVI   OPENSW,C'Y'                                                      
         MVI   CTFLIST,C'N'                                                     
         CLI   UPDTSW,C'Y'         Allow update                                 
         BNE   *+8                 No                                           
         MVI   CTFLIST,C'U'        Yes                                          
         GOTOR VDATAMGR,DMCB,=C'DMOPEN',=C'CONTROL',CTFLIST                     
         J     EXIT                                                             
                                                                                
CTFLIST  DC    C'NCTFILE X'                                                     
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONVERT AGENCY KEY VALUE VIA AGYTAB.                                          
*                                                                               
* ON ENTRY DUB(1)=KEY FORMAT,DUB+1(3)=A(AGENCY KEY VALUE)                       
***********************************************************************         
AGYCNV   NTR1  ,                                                                
         SR    RE,RE                                                            
         IC    RE,DUB                                                           
         LA    RF,L'KEYTAB                                                      
         MR    RE,RE                                                            
         L     R4,=A(KEYTAB-L'KEYTAB)                                           
         AR    R4,RF                                                            
         USING KEYTABD,R4          R4=A(KEY FORMAT TABLE ENTRY)                 
         L     R3,DUB                                                           
         SR    R1,R1                                                            
         IC    R1,KEYCOMPL         R1=L'EXECUTE                                 
         SR    RE,RE                                                            
         IC    RE,KEYODISP         RE=DISP TO OLD VALUE                         
         SR    RF,RF                                                            
         IC    RF,KEYNDISP         RF=DISP TO NEW VALUE                         
         MVC   WORK(2),0(R3)       SAVE KEY VALUE                               
         L     R6,ASENTRY                                                       
         USING SETABD,R6           R6=A(SE TABLE ENTRY)                         
         CLI   KEYAND,0                                                         
         BE    AGYCNV02                                                         
         SR    R2,R2                                                            
         IC    R2,KEYAND                                                        
         EX    R2,*+8                                                           
         B     *+8                                                              
         NI    0(R3),0             TURN OFF AGENCY IN RECORD                    
         PACK  WORK+2(1),KEYAND                                                 
         IC    R2,WORK+2                                                        
         EX    R2,*+8                                                           
         B     AGYCNV02                                                         
         NI    WORK,0              TURN OFF OTHER NIBBLE IN SAVE                
                                                                                
AGYCNV02 CLC   SEFILNUM,AGYLAST+AGYOLDF-AGYTABD                                 
         BNE   AGYCNV04                                                         
         LA    R5,AGYLAST(RE)      R5=A(LAST AGENCY OLD KEY VALUE)              
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),0(R5)       TEST FOR CHANGE OF AGENCY                    
         BE    AGYCNV10                                                         
                                                                                
AGYCNV04 L     R2,AAGYTAB          FIND ENTRY IN AGYTAB                         
         USING AGYTABD,R2          R2=A(AGENCY TABLE)                           
                                                                                
AGYCNV06 CLI   AGYTABD,X'FF'       TEST E-O-T                                   
         BNE   *+14                                                             
         XC    AGYLAST,AGYLAST     CLEAR AGENCY VALUES IF BAD CODE              
         B     AGYCONVX                                                         
                                                                                
         CLC   AGYOLDF,SEFILNUM    TEST FOR CORRECT FILE NUMBER                 
         BNE   AGYCNV08                                                         
         LA    R5,0(R2,RE)         R5=A(AGENCY OLD KEY VALUE)                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),0(R5)                                                    
         BE    *+12                                                             
AGYCNV08 AHI   R2,AGYLENQ                                                       
         B     AGYCNV06                                                         
         MVC   AGYLAST,0(R2)       SET LAST TIME AGENCY VALUES                  
                                                                                
AGYCNV10 LA    RE,AGYMVC           'MOVE' OR 'OR' IN NEW KEY VALUE              
         CLI   KEYAND,0                                                         
         BE    *+8                                                              
         LA    RE,AGYOC                                                         
         LA    R5,AGYLAST(RF)                                                   
         EX    R1,0(RE)                                                         
         B     AGYCONVX                                                         
                                                                                
AGYMVC   MVC   0(0,R3),0(R5)                                                    
AGYOC    OC    0(0,R3),0(R5)                                                    
                                                                                
AGYCONVX OC    AGYLAST,AGYLAST                                                  
         J     EXIT                                                             
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* PRINT A RECORD IN CHARACTER AND HEX FORMAT                                    
***********************************************************************         
PRTREC   NTR1  ,                                                                
         CLI   ISINERR,C'Y'                                                     
         BE    PRTREC02                                                         
         CLI   DEFINED,C'N'                                                     
         BE    PRTREC02                                                         
         TM    RECINDS-RECDEFD(R3),X'08'                                        
         BO    PRTREC04                                                         
         B     PRTRECX                                                          
                                                                                
PRTREC02 GOTOR PRCERR              HANDLE ERRORS                                
         BE    PRTRECX                                                          
                                                                                
PRTREC04 L     R3,RECORDLN                                                      
         L     R2,AREC                                                          
         AHI   R2,4                                                             
         L     RE,ASENTRY                                                       
         MVC   P(L'SENAME),SENAME-SETABD(RE)                                    
         L     RE,ASYSNTRY                                                      
         XR    R4,R4                                                            
         ICM   R4,3,SYSFEDSP-SYSTABD(RE)                                        
         BNZ   PRTREC08                                                         
                                                                                
PRTREC06 SR    R3,R4                                                            
         BNP   PRTRECX                                                          
         AR    R2,R4                                                            
         LA    R4,L'HEXWORK                                                     
         CR    R4,R3                                                            
         BL    PRTREC08                                                         
         LR    R4,R3                                                            
                                                                                
PRTREC08 CP    LINE,=P'57'                                                      
         BL    *+10                                                             
         ZAP   LINE,=P'99'                                                      
                                                                                
         CLI   DEFINED,C'N'                                                     
         BNE   PRTREC10                                                         
         MVI   DEFINED,0                                                        
         MVC   P(50),=CL50'The following record was not defined'                
         GOTOR VPRINTER                                                         
                                                                                
PRTREC10 LR    R5,R4                                                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P+10(0),0(R2)                                                    
         GOTOR VDUMPOUT,DMCB,((R4),P+10),0,0                                    
         GOTOR VPRINTER                                                         
         GOTOR VHEXOUT,DMCB,(R2),HEXWORK,(R4),=C'SEP'                           
         LA    R6,HEXWORK                                                       
         LA    R0,2                                                             
                                                                                
PRTREC12 EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P+10(0),0(R6)                                                    
         GOTOR VPRINTER                                                         
         AR    R6,R4                                                            
         BCT   R0,PRTREC12                                                      
         GOTOR (RF)                                                             
         B     PRTREC06                                                         
                                                                                
PRTRECX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* UPDATE CONTROL FILE ID AND ACCESS RECORDS WITH NEW AGENCY VALUES              
***********************************************************************         
UPDCTF   NTR1  ,                                                                
         CLI   UPDTSW,C'N'                                                      
         BE    UPDCTFX                                                          
         L     R5,ASYSNTRY                                                      
         USING SYSTABD,R5                                                       
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   KEY,CTIKEY                                                       
         GOTOR CTIO,DMRDHI         FIRST ID RECORD IS CONTROL SO SKIP           
                                                                                
UPDCTF02 GOTOR CTIO,DMRSEQ                                                      
         CLI   CTIKTYP,CTIKTYPQ                                                 
         BNE   UPDCTFX                                                          
         MVC   KEY,CTIKEY                                                       
                                                                                
         LA    R3,CTIDATA          PROCESS ID RECORD                            
         SR    R0,R0                                                            
UPDCTF04 CLI   0(R3),0                                                          
         BE    UPDCTF02                                                         
                                                                                
         USING CTAGYD,R3                                                        
         CLI   CTAGYEL,CTAGYELQ                                                 
         BNE   *+14                                                             
         MVC   WORK(L'CTAGYID),CTAGYID                                          
         B     UPDCTF06                                                         
                                                                                
         USING CTSYSD,R3                                                        
         CLI   CTSYSEL,CTSYSELQ                                                 
         BNE   UPDCTF06                                                         
         CLC   CTSYSNUM,SYSOVNUM   MATCH SEOV NUMBER                            
         BE    UPDCTF08                                                         
                                                                                
UPDCTF06 IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     UPDCTF04                                                         
                                                                                
UPDCTF08 L     R2,AAGYTAB          PROCESS A SYSTEM ELEMENT                     
         USING AGYTABD,R2          R2=A(AGENCY TABLE)                           
                                                                                
UPDCTF10 CLI   AGYTABD,X'FF'       GET ENTRY IN AGENCY TABLE                    
         BE    UPDCTF02                                                         
         CLC   AGYALFA,WORK                                                     
         BE    *+12                                                             
         AHI   R2,AGYLENQ                                                       
         B     UPDCTF10                                                         
         CLI   AGYNEWF,0           TEST IF ON AN OUTPUT FILE                    
         BE    UPDCTF02                                                         
                                                                                
         MVC   CTSYSAGB,AGYNEWL    SET NEW AGENCY BINARY                        
         L     R4,ASETAB                                                        
         USING SETABD,R4                                                        
         CLC   SEFILNUM,AGYNEWF                                                 
         BE    *+12                                                             
         AHI   R4,SELEN                                                         
         B     *-14                                                             
         MVC   CTSYSSE,SENUM       SET NEW SE NUMBER                            
                                                                                
         CLI   UPDTSW,C'Y'                                                      
         BE    UPDCTF12                                                         
         GOTOR VPRTREC,DMCB,CTIREC,('CTIDATA-CTIREC',CTILEN-CTIREC),   *        
               VPRINT,VHEXOUT,0,0                                               
         B     UPDCTF14                                                         
                                                                                
UPDCTF12 GOTOR CTIO,DMWRT          WRITE BACK ID RECORD                         
                                                                                
         USING CT5REC,IO                                                        
UPDCTF14 TM    AGYINDS,AGYIUPDT    TEST ACCESS RECORD UPDATED                   
         BNZ   UPDCTF02                                                         
                                                                                
         MVC   KEYSAVE,KEY         SAVE ID RECORD KEY                           
                                                                                
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGYALFA                                                 
         MVC   KEY,CT5KEY                                                       
         GOTOR CTIO,DMREAD         GET THE ACCESS RECORD                        
                                                                                
         LA    R3,CT5DATA                                                       
         SR    R0,R0                                                            
         USING CTSYSD,R3                                                        
UPDCTF16 CLI   CTSYSEL,0           IF NO MORE ELEMENTS                          
         BNE   *+6                 THEN DONE                                    
         DC    H'0'                                                             
         CLI   CTSYSEL,CTSYSELQ    IF SYSTEM AUTHORIZATION ELEMENT              
         BNE   *+14                THEN SEE IF WE CAN GET THE SYSTEM            
         CLC   CTSYSNUM,SYSOVNUM   IF NOT THE SAME SYSTEM                       
         BE    *+14                THEN CHECK NEXT ELEMENT                      
         IC    R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     UPDCTF16            CHECK AGAIN                                  
                                                                                
         MVC   CTSYSSE,SENUM       SET THE NEW SE NUMBER                        
         MVC   CTSYSAGB,AGYNEWL    AND THE NEW BINARY AGENCY                    
         OI    AGYINDS,AGYIUPDT    SET ACCESS RECORD UPDATED                    
                                                                                
         CLI   UPDTSW,C'Y'                                                      
         BE    UPDCTF18                                                         
         GOTOR VPRTREC,DMCB,CT5REC,('CT5DATA-CT5REC',CT5LEN-CT5REC),   *        
               VPRINT,VHEXOUT,0,0                                               
         B     UPDCTF20                                                         
                                                                                
UPDCTF18 GOTOR CTIO,DMWRT                                                       
                                                                                
UPDCTF20 MVC   KEY,KEYSAVE         RESTORE LAST ID RECORD KEY                   
         GOTOR CTIO,DMREAD         READ THE RECORD                              
         B     UPDCTF02            AND GO DO READ SEQUENTIAL                    
                                                                                
UPDCTFX  J     EXIT                                                             
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
***********************************************************************         
* PRINT CONTROL TOTALS & NEW AGENCY KEY VALUES LISTING                          
***********************************************************************         
PRTTOT   NTR1  ,                                                                
         MVC   TITLE,SPACES                                                     
         MVC   TITLE+18(24),=C'Input/Output File Counts'                        
         MVC   SUB1(L'SUBA),SUBA                                                
         MVC   SUB2(L'SUBB),SUBB                                                
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         L     R2,ASETAB                                                        
         USING SETABD,R2           R2=A(SE TABLE)                               
                                                                                
PRTTOT02 CLI   SETABD,X'FF'        TEST E-O-T                                   
         BE    PRTTOT08                                                         
         TM    SEINDS,X'C0'        TEST IF INPUT OR OUTPUT FILE                 
         BZ    PRTTOT06                                                         
         MVC   P(L'SENAME),SENAME                                               
         LA    R3,SEINPCNT                                                      
         LA    R4,P+9                                                           
         LA    R5,3                                                             
                                                                                
PRTTOT04 EDIT  (P6,0(R3)),(9,0(R4)),FILL=0                                      
         AHI   R3,L'SEINPCNT                                                    
         LA    R4,11(R4)                                                        
         BCT   R5,PRTTOT04                                                      
         GOTOR VPRINTER                                                         
         GOTOR (RF)                                                             
                                                                                
PRTTOT06 AHI   R2,SELEN            BUMP TO NEXT SE                              
         B     PRTTOT02                                                         
                                                                                
PRTTOT08 MVC   TITLE,SPACES        PRINT NEW AGENCY VALUES LIST                 
         MVC   TITLE+20(21),=C'Old/New Agency Values'                           
         MVC   SUB1(L'SUBC),SUBC                                                
         MVC   SUB2(L'SUBD),SUBD                                                
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         L     R5,AAGYTAB                                                       
         USING AGYTABD,R5                                                       
                                                                                
PRTTOT10 CLI   AGYTABD,X'FF'       TEST E-O-T                                   
         BE    PRTTOTX                                                          
         MVC   P+2(L'AGYALFA),AGYALFA                                           
         L     R2,ASETAB                                                        
                                                                                
PRTTOT12 CLI   SETABD,X'FF'        TEST E-O-T                                   
         BE    PRTTOT14                                                         
         CLC   AGYOLDF,SEFILNUM                                                 
         BNE   *+10                                                             
         MVC   P+8(L'SENAME),SENAME                                             
         CLC   AGYNEWF,SEFILNUM                                                 
         BNE   *+10                                                             
         MVC   P+20(L'SENAME),SENAME                                            
         AHI   R2,SELEN                                                         
         B     PRTTOT12                                                         
                                                                                
PRTTOT14 GOTOR VHEXOUT,DMCB,AGYOLDL,P+16,1                                      
         CLI   AGYNEWF,0                                                        
         BH    *+12                                                             
         CLI   AGYNEWL,0                                                        
         BE    PRTTOT16                                                         
         GOTOR (RF),(R1),AGYNEWL,P+28,1                                         
                                                                                
PRTTOT16 GOTOR VPRINTER                                                         
         AHI   R5,AGYLENQ          BUMP TO NEXT AGENCY                          
         B     PRTTOT10                                                         
                                                                                
PRTTOTX  J     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* I/O TO CONTROL FILE - NTRY KEY=KEY ARGUMENT, R1=A(COMMAND)                    
***********************************************************************         
CTIO     NTR1  ,                                                                
         ST    R1,DMCB                                                          
         CLI   UPDTSW,C'T'                                                      
         BNE   CTIO02                                                           
         MVC   P+1(8),0(R1)                                                     
         GOTOR VHEXOUT,PARM,KEY,P+10,L'CTIKEY,=C'TOG'                           
         GOTOR VPRINTER                                                         
                                                                                
CTIO02   GOTOR VDATAMGR,DMCB,,=C'CTFILE',KEY,IO                                 
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   UPDTSW,C'T'                                                      
         JNE   EXIT                                                             
         GOTOR VHEXOUT,PARM,IO,P+10,L'CTIKEY,=C'TOG'                            
         GOTOR VPRINTER                                                         
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SPECIAL CODE FOR SPOT FILE AGENCY HEADERS                                     
***********************************************************************         
         USING AGYHDRD,R4                                                       
SPTAGY   NTR1  ,                                                                
*        CLI   AGYPAHEX,X'00'      OR IS THE HEX CODE 00?                       
*        BE    SPTAGYX                                                          
                                                                                
         MVC   DUB+4(2),AGYKAGY                                                 
         LA    RE,DUB+4                                                         
         ST    RE,DUB                                                           
         MVI   DUB,AGYA                                                         
         GOTOR AGYCNV              CONVERT AGENCY IN SAVE AREA                  
         BZ    SPTAGYX                                                          
         SR    RE,RE                                                            
         IC    RE,AGYLAST+AGYNEWR-AGYTABD                                       
         LA    RE,HEXTAB(RE)                                                    
         MVC   WORK(1),0(RE)       SAVE NEW HEX AGENCY VALUE                    
*                                                                               
         DROP  R4                                                               
         AHI   R4,24                                                            
                                                                                
SPTAGY02 CLI   0(R4),0             TEST E-O-R                                   
         BE    SPTAGYX                                                          
         CLI   0(R4),AGYELQ        AGENCY ELEMENT                               
         BE    SPTAGY06                                                         
         CLI   0(R4),AGYMEDEQ      MEDIA ELEMENT                                
         BE    SPTAGY08                                                         
                                                                                
SPTAGY04 SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     SPTAGY02                                                         
                                                                                
         USING AGYEL,R4                                                         
SPTAGY06 MVC   AGYPAHEX,WORK       FIX HEX AGY (AGYPROF+19)                     
         B     SPTAGY04                                                         
         DROP  R4                                                               
                                                                                
         USING AGYMEDEL,R4                                                      
SPTAGY08 LA    RE,AGYMEDBT         FIX AGENCY IN MEDIA ELEMENTS                 
         ST    RE,DUB                                                           
         MVI   DUB,AGYL                                                         
         GOTOR AGYCNV                                                           
         B     SPTAGY04                                                         
         DROP  R4                                                               
                                                                                
SPTAGYX  J     EXITEQ                                                           
                                                                                
SPTAGYXN J     EXITNEQ                                                          
         EJECT                                                                  
***********************************************************************         
* SPECIAL CODE FOR REP LOCAL INVOICES ON THE SPOT XFILE                         
***********************************************************************         
SPT0EB3  NTR1  ,                                                                
         OC    21(2,R4),21(R4)                                                  
         BNZ   *+10                                                             
         MVC   21(2,R4),=C'UV'     UNIVISION IS ONLY ONE USING THESE            
         J     EXITEQ                                                           
         EJECT                                                                  
***********************************************************************         
* ERROR PROCESSING                                                              
***********************************************************************         
PRCERR   NTR1  ,                                                                
         L     R2,AERRTAB                                                       
         L     R3,AREC                                                          
         AHI   R3,4                                                             
                                                                                
PRCERR02 OC    0(2,R2),0(R2)                                                    
         BZ    PRCERR04                                                         
         CLC   0(2,R2),0(R3)                                                    
         BE    *+12                                                             
         AHI   R2,6                                                             
         B     PRCERR02                                                         
         ICM   RF,15,2(R2)         INCREMENT COUNT OF BAD RECORDS               
         AHI   RF,1                                                             
         STCM  RF,15,2(R2)                                                      
         CR    RB,RB                                                            
         B     PRCERRX                                                          
                                                                                
PRCERR04 MVC   0(2,R2),0(R3)                                                    
         LHI   RF,1                                                             
         STCM  RF,15,2(R2)                                                      
         CLI   *,255                                                            
                                                                                
PRCERRX  J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                                                                               
***********************************************************************         
ERRPRT   NTR1  ,                                                                
         MVC   TITLE+18(24),=CL24'Error Counts'                                 
         MVC   SUB1,SPACES                                                      
         MVC   SUB1+00(07),=C'Bad key'                                          
         MVC   SUB2,SPACES                                                      
         MVC   SUB1+10(05),=C'Count'                                            
         ZAP   LINE,=P'99'                                                      
         ZAP   PAGE,=P'1'                                                       
         L     R2,AERRTAB                                                       
                                                                                
ERRPRT02 OC    0(2,R2),0(R2)                                                    
         BZ    ERRPRTX                                                          
         GOTOR VHEXOUT,DMCB,(R2),P,2,0                                          
         EDIT  (B4,2(R2)),(10,P+10),0,ALIGN=LEFT                                
         GOTOR VPRINTER                                                         
         AHI   R2,6                                                             
         B     ERRPRT02                                                         
                                                                                
ERRPRTX  J     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
* Storage variables                                                             
***********************************************************************         
                                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
WORK     DS    CL20                                                             
DEFINED  DS    XL1                                                              
ISINERR  DS    XL1                                                              
OPENSW   DC    C'N'                                                             
OUTIND   DC    XL1'00'             OUTPUT INDICATOR                             
OUTXAI   EQU   X'80'               . EXTRA ALPHA ID (WITH A ZERO CODE)          
                                                                                
AAGYTAB  DC    A(AGYTAB)                                                        
ASETAB   DC    A(SETAB)                                                         
ASENTRY  DC    A(0)                                                             
ASYSNTRY DC    A(0)                                                             
AAGYKTAB DC    A(0)                                                             
ADTFTAB  DC    A(DTFTAB)                                                        
ADTFIN   DC    A(TINT)                                                          
                                                                                
VCARDS   DC    V(CARDS)                                                         
VCPRINT  DC    V(CPRINT)                                                        
VDATAMGR DC    V(DATAMGR)                                                       
VDUMPOUT DC    V(DUMPOUT)                                                       
VHELLO   DC    V(HELLO)                                                         
VHEXIN   DC    V(HEXIN)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VLOGIO   DC    V(LOGIO)                                                         
VPRINTER DC    V(PRINTER)                                                       
VPRINT   DC    V(PRINT)                                                         
VQSORT   DC    V(QSORT)                                                         
VPRTREC  DC    V(PRTREC)                                                        
                                                                                
DMREAD   DC    C'DMREAD  '                                                      
DMRDHI   DC    C'DMRDHI  '                                                      
DMWRT    DC    C'DMWRT   '                                                      
DMRSEQ   DC    C'DMRSEQ  '                                                      
                                                                                
AERRTAB  DC    A(ERRTAB)                                                        
                                                                                
VALSW    DS    C                                                                
UPDTSW   DC    C'N'                                                             
TESTOPT  DC    C'N'                                                             
EOFSW    DC    C'N'                                                             
OUTCNT   DC    PL2'0'                                                           
ERRCNT   DC    PL6'0'                                                           
OUTFILE  DS    X                                                                
RECORDLN DS    F                                                                
AGYLAST  DC    XL10'00'                                                         
AREC     DS    A                                                                
HEXTAB   DC    C'0123456789ABCDEF'                                              
HEXWORK  DS    2CL100                                                           
                                                                                
AGYL     EQU   1                                                                
AGYR     EQU   2                                                                
AGYA     EQU   3                                                                
AGYC     EQU   4                                                                
                                                                                
MSG1     DC    C'CONTROL=UPDATE card must be before TYPE= or OUTPUT='           
MSG2     DC    C'FBSA - DSPACE= card needs to be first'                         
MESS1    DC    CL45'Ensure XXXXXXX output tape mounted on SYSNNN'               
MESS2    DC    CL45'Ensure XXXXXXX input tape mounted on SYSNNN'                
MESS3    DC    CL45'EOV/EOF enquiry for XXXXXXX input tape?'                    
                                                                                
SUBA     DC    CL50'SE Name      Input    Deleted     Output'                   
SUBB     DC    CL50'-------      -----    -------     ------'                   
SUBC     DC    CL50'Agency  Old Values  New Values'                             
SUBD     DC    CL50' Code   ----------  ----------'                             
                                                                                
ALPHATAB DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',X'FF'                    
BINRYTAB DC    X'102030405060708090A0B0C0D0E0F0',X'FF'                          
                                                                                
CARD     DS    CL80                                                             
KEY      DS    CL25                                                             
KEYSAVE  DS    CL25                                                             
EOFSAVE  DS    CL256                                                            
IO       DS    6200C                                                            
         EJECT                                                                  
*********************************************************************           
* NOTE THAT SETAB HAS A LIMIT ON MAXIMUM SYSTEMS (ON NEXT PAGE )    *           
*                                                                   *           
* BYTE FOLLOWING AL3 IS SYSTEM FILE SET NUMBER. IT MUST AGREE       *           
* WITH ENTRY IN FATAB SELIST ENTRY (See SYSFILESETNUMBER=)          *           
*********************************************************************           
                                                                                
PARMTAB  DS    0XL20                                                            
         DC    AL1(7),C'TYPFILE ',AL3(VALTYP),8X'00'                            
         DC    AL1(7),C'OUTPUT1 ',AL3(VALOUT),AL1(01),7X'00' SPOT1/NET1         
         DC    AL1(7),C'OUTPUT2 ',AL3(VALOUT),AL1(02),7X'00' SPOT2/NET2         
         DC    AL1(7),C'OUTPUT3 ',AL3(VALOUT),AL1(03),7X'00' SPOT3/NET3         
         DC    AL1(7),C'OUTPUT4 ',AL3(VALOUT),AL1(04),7X'00' SPOT4/NET4         
         DC    AL1(7),C'OUTPUT5 ',AL3(VALOUT),AL1(05),7X'00' SPOT5              
         DC    AL1(7),C'OUTPUT6 ',AL3(VALOUT),AL1(06),7X'00' SPOT6              
         DC    AL1(7),C'OUTPUT7 ',AL3(VALOUT),AL1(07),7X'00' SPOT7              
         DC    AL1(7),C'OUTPUT8 ',AL3(VALOUT),AL1(08),7X'00' NET1               
         DC    AL1(7),C'OUTPUT9 ',AL3(VALOUT),AL1(09),7X'00' NET2               
         DC    AL1(7),C'OUTPUTA ',AL3(VALOUT),AL1(10),7X'00' NET3               
         DC    AL1(7),C'OUTPUTB ',AL3(VALOUT),AL1(11),7X'00' SPOTB              
         DC    AL1(7),C'OUTPUTC ',AL3(VALOUT),AL1(12),7X'00' NET4               
         DC    AL1(7),C'OUTPUTD ',AL3(VALOUT),AL1(13),7X'00' n/d                
         DC    AL1(7),C'OUTPUTE ',AL3(VALOUT),AL1(14),7X'00' SPOTE              
         DC    AL1(7),C'OUTPUTF ',AL3(VALOUT),AL1(15),7X'00' SPOTF              
         DC    AL1(7),C'OUTPUTG ',AL3(VALOUT),AL1(16),7X'00' SPOTG              
         DC    AL1(7),C'OUTPUTH ',AL3(VALOUT),AL1(17),7X'00' SPOTH              
         DC    AL1(7),C'OUTPUTI ',AL3(VALOUT),AL1(18),7X'00' n/d                
         DC    AL1(7),C'OUTPUTJ ',AL3(VALOUT),AL1(19),7X'00' NET5               
         DC    AL1(7),C'OUTPUTK ',AL3(VALOUT),AL1(20),7X'00' NET6               
         DC    AL1(7),C'OUTPUTL ',AL3(VALOUT),AL1(21),7X'00' SPOTL              
         DC    AL1(7),C'OUTPUTM ',AL3(VALOUT),AL1(22),7X'00' SPOTM              
         DC    AL1(7),C'OUTPUTN ',AL3(VALOUT),AL1(23),7X'00' SPOTN              
         DC    AL1(7),C'OUTPUTO ',AL3(VALOUT),AL1(24),7X'00' n/d                
         DC    AL1(7),C'OUTPUTP ',AL3(VALOUT),AL1(25),7X'00' NET7               
         DC    AL1(7),C'OUTPUTQ ',AL3(VALOUT),AL1(26),7X'00' SPOTQ              
         DC    AL1(7),C'OUTPUTR ',AL3(VALOUT),AL1(27),7X'00' NETR               
         DC    AL1(7),C'OUTPUTS ',AL3(VALOUT),AL1(28),7X'00' SPOTS              
         DC    AL1(7),C'OUTPUTT ',AL3(VALOUT),AL1(29),7X'00' NETT               
         DC    AL1(7),C'OUTPUTU ',AL3(VALOUT),AL1(30),7X'00' SPOTU              
         DC    AL1(7),C'OUTPUTV ',AL3(VALOUT),AL1(31),7X'00' SPOTV              
         DC    AL1(7),C'OUTPUTW ',AL3(VALOUT),AL1(32),7X'00' NETW               
         DC    AL1(7),C'OUTPUTX ',AL3(VALOUT),AL1(33),7X'00' n/d                
         DC    AL1(7),C'OUTPUTY ',AL3(VALOUT),AL1(34),7X'00' SPOTY              
         DC    AL1(7),C'OUTPUTZ ',AL3(VALOUT),AL1(35),7X'00' SPOTZ              
         DC    AL1(7),C'OUTPUT0 ',AL3(VALOUT),AL1(36),7X'00' SPOT0              
         DC    AL1(8),C'OUTPUTIT',AL3(VALOUT),AL1(37),7X'00' *IT                
         DC    AL1(8),C'OUTPUTTT',AL3(VALOUT),AL1(38),7X'00' *TT                
         DC    AL1(8),C'OUTPUTTU',AL3(VALOUT),AL1(39),7X'00' *TU                
         DC    AL1(8),C'OUTPUTI1',AL3(VALOUT),AL1(40),7X'00' *I1                
         DC    AL1(8),C'OUTPUTI3',AL3(VALOUT),AL1(42),7X'00' *I3                
         DC    AL1(8),C'OUTPUTO1',AL3(VALOUT),AL1(43),7X'00' *O1                
         DC    AL1(8),C'OUTPUTU1',AL3(VALOUT),AL1(44),7X'00' *U1                
         DC    AL1(8),C'OUTPUTY1',AL3(VALOUT),AL1(45),7X'00' *Y1                
         DC    AL1(8),C'OUTPUTN1',AL3(VALOUT),AL1(46),7X'00' *N1                
         DC    AL1(8),C'OUTPUTN2',AL3(VALOUT),AL1(47),7X'00' *N2                
         DC    AL1(8),C'OUTPUTU2',AL3(VALOUT),AL1(48),7X'00' *U2                
         DC    AL1(7),C'CONTROL ',AL3(VALUPT),8X'00'                            
         DC    AL1(5),C'DDSIO   ',AL3(VALDDS),8X'00'                            
         DC    AL1(6),C'DSPACE  ',AL3(VALDSP),8X'00'                            
         DC    AL1(4),C'TEST    ',AL3(VALTST),8X'00'                            
         DC    X'FF'                                                            
                                                                                
SYSTAB   DS    0XL19               SYSTEM TABLE                                 
         DC    C'ACCHST  ',X'06',AL3(ACCDEFN)                                   
         DC    AL2(42,42,49),X'80'                                              
         DC    C'ACCMST  ',X'06',AL3(ACCDEFN)                                   
         DC    AL2(42,42,56),X'80'                                              
         DC    C'SPOTFILE',X'02',AL3(SPTDEFN)                                   
         DC    AL2(13,13,24),X'01'                                              
         DC    C'SPTFILE ',X'02',AL3(SPTDEFN)                                   
         DC    AL2(13,13,24),X'01'                                              
         DC    C'STAFILE ',X'02',AL3(STADEFN)                                   
         DC    AL2(17,00,00),X'00'                                              
         DC    C'XSPFILE ',X'02',AL3(XSPDEFN)                                   
         DC    AL2(32,32,42),X'00'                                              
         DC    C'NETFILE ',X'03',AL3(NETDEFN)                                   
         DC    AL2(20,20,27),X'00'                                              
         DC    C'NETSPTF ',X'03',AL3(SPTDEFN)                                   
         DC    AL2(13,13,24),X'01'                                              
         DC    C'NETSTAF ',X'03',AL3(STADEFN)                                   
         DC    AL2(17,00,00),X'00'                                              
         DC    C'NETXSPF ',X'03',AL3(XSPDEFN)                                   
         DC    AL2(32,32,42),X'00'                                              
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
         DC    C'REPFILE ',X'08',AL3(REPDEFN)                                   
         DC    AL2(27,27,34),X'80'                                              
         DC    X'FF'                                                            
                                                                                
KEYTAB   DS    0XL4                KEY FORMAT TABLE                             
         DC    AL1(L'AGYOLDL-1,AGYOLDL-AGYTABD,AGYNEWL-AGYTABD),X'0F'           
         DC    AL1(L'AGYOLDR-1,AGYOLDR-AGYTABD,AGYNEWR-AGYTABD),X'F0'           
         DC    AL1(L'AGYALFA-1,AGYALFA-AGYTABD,AGYALFA-AGYTABD),X'00'           
         DC    AL1(L'AGYOLDL-1,AGYOLDL-AGYTABD,AGYNEWL-AGYTABD),X'00'           
                                                                                
DTFTAB   DS    0A                  OUTPUT FILE TABLE                            
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
         DC    A(TOUTI)                                                         
         DC    A(TOUTJ)                                                         
         DC    A(TOUTK)                                                         
         DC    A(TOUTL)                                                         
         DC    A(TOUTM)                                                         
         DC    A(TOUTN)                                                         
         DC    A(TOUTO)                                                         
         DC    A(TOUTP)                                                         
         DC    A(TOUTQ)                                                         
         DC    A(TOUTR)                                                         
         DC    A(TOUTS)                                                         
         DC    A(TOUTT)                                                         
         DC    A(TOUTU)                                                         
         DC    A(TOUTV)                                                         
         DC    A(TOUTW)                                                         
         DC    A(TOUTX)                                                         
         DC    A(TOUTY)                                                         
         DC    A(TOUTZ)                                                         
         DC    A(TOUT0)                                                         
         DC    A(TOUTIT)                                                        
         DC    A(TOUTTT)                                                        
         DC    A(TOUTTU)                                                        
         DC    A(TOUTI1)                                                        
         DC    A(TOUTI3)                                                        
         DC    A(TOUTO1)                                                        
         DC    A(TOUTU1)                                                        
         DC    A(TOUTY1)                                                        
         DC    A(TOUTN1)                                                        
         DC    A(TOUTN2)                                                        
         DC    A(TOUTU2)                                                        
         DC    X'FF'                                                            
                                                                                
         DS    0D                                                               
         DC    CL8'*AGYTAB*'                                                    
AGYTAB   DC    X'FF',800XL(AGYLENQ)'00'                                         
AGYTABX  EQU   *                                                                
                                                                                
         DS    0D                                                               
         DC    CL8'*SETAB*'                                                     
SETAB    DC    128XL(SELEN)'00'                                                 
         EJECT                                                                  
* ACCOUNT FILE RECORD DEFINITIONS (LAST RECTYPE# WAS 59)                        
                                                                                
ACCDEFN  DS    0X                                                               
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
         DC    X'180018FF',AL1(00,00),AL1(56),X'00',AL3(0)                      
         DC    AL1(2,AGYC)                                                      
         DC    X'190019FF',AL1(00,00),AL1(32),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'1A001AFF',AL1(00,00),AL1(07),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'1B001BFF',AL1(00,00),AL1(08),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'1C001C40',AL1(00,00),AL1(47),X'00',AL3(0)                      
         DC    AL1(2,AGYC)                                                      
         DC    X'1C411CFF',AL1(00,00),AL1(47),X'00',AL3(0)                      
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
         DC    AL1(2,AGYC)                                                      
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
         DC    X'360036FF',AL1(00,00),AL1(58),X'00',AL3(0)                      
         DC    AL1(1,AGYC)                                                      
         DC    X'370037FF',AL1(00,00),AL1(57),X'00',AL3(0)                      
         DC    AL1(2,AGYC)                                                      
         DC    X'380038FF',AL1(00,00),AL1(59),X'00',AL3(0)                      
         DC    AL1(2,AGYC)                                                      
         DC    X'3E003EFF',AL1(00,00),AL1(36),X'00',AL3(0)                      
         DC    AL1(2,AGYC)                                                      
         DC    X'3F003FFF',AL1(00,00),AL1(38),X'00',AL3(0)                      
         DC    AL1(2,AGYC)                                                      
         DC    X'4100FEFF',AL1(00,00),AL1(25),X'00',AL3(0)                      
         DC    AL1(0,AGYC)                                                      
         DC    X'FFFFFFFF',AL1(00,00),AL1(26),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(27),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         EJECT                                                                  
* SPOT FILE RECORD DEFINITIONS                                                  
                                                                                
SPTDEFN  DS    0X                                                               
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
         DC    X'060006FF',AL1(00,00),AL1(06),X'00',AL3(SPTAGY)                 
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
         DC    X'0D0E0D0E',AL1(00,00),AL1(26),X'00',AL3(0)                      
         DC    AL1(4,AGYL)                                                      
         DC    X'0D110D13',AL1(00,00),AL1(15),X'00',AL3(0)                      
         DC    AL1(2,AGYA)                                                      
         DC    X'0D140D17',AL1(00,00),AL1(16),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D180D19',AL1(00,00),AL1(15),X'00',AL3(0)                      
         DC    AL1(2,AGYA)                                                      
         DC    X'0D200D20',AL1(00,00),AL1(17),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D220D23',AL1(00,00),AL1(18),X'00',AL3(0)                      
         DC    AL1(2,AGYA)                                                      
         DC    X'0D260D26',AL1(00,00),AL1(17),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D270D28',AL1(00,00),AL1(17),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D290D29',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(3,AGYL)                                                      
         DC    X'0D2A0D2A',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(3,AGYL)                                                      
         DC    X'0D310D33',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D340D34',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(4,AGYL)                                                      
         DC    X'0D350D38',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D3A0D3A',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(4,AGYL)                                                      
         DC    X'0D3B0D3D',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D3E0D3E',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(4,AGYL)                                                      
         DC    X'0D400D42',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D430D43',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(2,AGYA)                                                      
         DC    X'0D440D53',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D540D54',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(2,AGYA)                                                      
         DC    X'0D550D76',AL1(00,00),AL1(19),X'00',AL3(0)                      
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
         DC    X'0E700E70',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0F020F02',AL1(00,00),AL1(20),X'10',AL3(0)                      
         DC    AL1(0,0)                                                         
**nop    DC    X'0A010AFF',AL1(00,00),AL1(21),X'00',AL3(0)                      
**nop    DC    AL1(2,AGYR)                                                      
         DC    X'1000FEFF',AL1(00,00),AL1(22),X'00',AL3(0)                      
         DC    AL1(0,AGYL)                                                      
         DC    X'FFFFFFFF',AL1(00,00),AL1(23),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(24),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         EJECT                                                                  
* STATION FILE RECORD DEFINITIONS                                               
                                                                                
STADEFN  DS    0X                                                               
         DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'C100C1FF',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(7,AGYA)                                                      
         DC    X'C200C2FF',AL1(00,00),AL1(12),X'00',AL3(0)                      
         DC    AL1(1,AGYA)                                                      
         DC    X'C600C6FF',AL1(00,00),AL1(03),X'00',AL3(0)                      
         DC    AL1(1,AGYA)                                                      
         DC    X'D200D2FF',AL1(00,00),AL1(13),X'00',AL3(0)                      
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
         DC    X'E700E7FF',AL1(00,00),AL1(13),X'00',AL3(0)                      
         DC    AL1(1,AGYA)                                                      
         DC    X'E800E8FF',AL1(00,00),AL1(11),X'00',AL3(0)                      
         DC    AL1(7,AGYA)                                                      
         DC    X'FFFFFFFF',AL1(00,00),AL1(09),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(10),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         EJECT                                                                  
* XSPFILE - EXPANDED SPOT FILE                                                  
                                                                                
XSPDEFN  DS    0X                                                               
         DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'021002FF',AL1(00,00),AL1(03),X'00',AL3(0)                      
         DC    AL1(1,AGYL)                                                      
         DC    X'0A1D0A1D',AL1(00,00),AL1(06),X'00',AL3(0)                      
         DC    AL1(02,AGYL)                                                     
         DC    X'0A200A20',AL1(00,00),AL1(06),X'00',AL3(0)                      
         DC    AL1(02,AGYL)                                                     
         DC    X'0A2D0A2D',AL1(00,00),AL1(06),X'00',AL3(0)                      
         DC    AL1(02,AGYL)                                                     
         DC    X'0A350A35',AL1(00,00),AL1(06),X'00',AL3(0)                      
         DC    AL1(02,AGYL)                                                     
         DC    X'0A610A61',AL1(00,00),AL1(06),X'00',AL3(0)                      
         DC    AL1(02,AGYL)                                                     
*                                                                               
* CANADIAN DESKTOP ORDER RECORDS                                                
         DC    X'0D010D01',AL1(00,00),AL1(14),X'00',AL3(0)                      
         DC    AL1(20,AGYL)                                                     
         DC    X'0D020D02',AL1(00,00),AL1(14),X'00',AL3(0)                      
         DC    AL1(16,AGYL)                                                     
         DC    X'0D030D03',AL1(00,00),AL1(14),X'00',AL3(0)                      
         DC    AL1(22,AGYL)                                                     
         DC    X'0D050D05',AL1(00,00),AL1(14),X'00',AL3(0)                      
         DC    AL1(18,AGYL)                                                     
         DC    X'0D060D06',AL1(00,00),AL1(14),X'00',AL3(0)                      
         DC    AL1(14,AGYL)                                                     
         DC    X'0D070D07',AL1(00,00),AL1(14),X'00',AL3(0)                      
         DC    AL1(19,AGYL)                                                     
         DC    X'0D080D08',AL1(00,00),AL1(14),X'00',AL3(0)                      
         DC    AL1(12,AGYL)                                                     
         DC    X'0D0A0D0A',AL1(00,00),AL1(14),X'00',AL3(0)                      
         DC    AL1(27,AGYL)                                                     
*                                                                               
         DC    X'0D0B0D0B',AL1(00,00),AL1(01),X'00',AL3(0)                      
         DC    AL1(19,AGYL)                                                     
         DC    X'0D0C0D0F',AL1(00,00),AL1(05),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0D1B0D1B',AL1(00,00),AL1(05),X'00',AL3(0)                      
         DC    AL1(19,AGYL)                                                     
         DC    X'0D360D36',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(20,AGYL)                                                     
         DC    X'0D370D37',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(19,AGYL)                                                     
         DC    X'0D390D39',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(19,AGYL)                                                     
         DC    X'0D3A0D3A',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(4,AGYL)                                                      
         DC    X'0D3B0D3B',AL1(00,00),AL1(23),X'00',AL3(0)                      
         DC    AL1(19,AGYL)                                                     
         DC    X'0D3F0D3F',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(4,AGYL)                                                      
         DC    X'0D730D73',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(17,AGYL)                                                     
         DC    X'0E030E0B',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(2,AGYL)                                                      
         DC    X'0E0C0E0C',AL1(00,00),AL1(10),X'00',AL3(0)                      
         DC    AL1(12,AGYL)                                                     
         DC    X'0E0D0E0D',AL1(00,00),AL1(11),X'00',AL3(0)                      
         DC    AL1(17,AGYL)                                                     
         DC    X'0E100E12',AL1(00,00),AL1(12),X'00',AL3(0)                      
         DC    AL1(13,AGYL)                                                     
         DC    X'0E130E13',AL1(00,00),AL1(22),X'00',AL3(0)                      
         DC    AL1(20,AGYL)                                                     
         DC    X'0E150E15',AL1(00,00),AL1(20),X'00',AL3(0)                      
         DC    AL1(02,AGYA)                                                     
         DC    X'0EA30EA3',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(2,AGYA)                                                      
         DC    X'0EB30EB3',AL1(00,00),AL1(02),X'00',AL3(SPT0EB3)                
         DC    AL1(21,AGYA)                                                     
         DC    X'FFFFFFFF',AL1(00,00),AL1(03),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(04),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         EJECT                                                                  
* NETWORK UNTFILE RECORD DEFINITIONS                                            
                                                                                
NETDEFN  DS    0X                                                               
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
         DC    X'0D070D07',AL1(00,00),AL1(10),X'00',AL3(0)                      
         DC    AL1(02,AGYL)                                                     
         DC    X'200032FF',AL1(00,00),AL1(06),X'00',AL3(0)                      
         DC    AL1(01,AGYL)                                                     
         DC    X'400040FF',AL1(00,00),AL1(07),X'00',AL3(0)                      
         DC    AL1(01,AGYL)                                                     
         DC    X'FFFFFFFF',AL1(00,00),AL1(08),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(11),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         EJECT                                                                  
* PRINT FILE RECORD DEFINITIONS                                                 
                                                                                
PRTDEFN  DS    0X                                                               
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
                                                                                
* PUBLICATION FILE RECORD DEFINITIONS                                           
                                                                                
PUBDEFN  DS    0X                                                               
         DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'4100F9FF',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(7,AGYA)                                                      
         DC    X'FFFFFFFF',AL1(00,00),AL1(03),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(04),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
                                                                                
* BUDGET FILE RECORD DEFINITIONS                                                
*                                                                               
BUDDEFN  DS    0X                                                               
         DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'C200C2FF',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(1,AGYA)                                                      
         DC    X'FFFFFFFF',AL1(00,00),AL1(03),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(04),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
                                                                                
* MPL FILE RECORD DEFINITIONS                                                   
*                                                                               
MPLDEFN  DS    0X                                                               
         DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'E900E9FF',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(2,AGYA)                                                      
         DC    X'FFFFFFFF',AL1(00,00),AL1(03),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(04),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         EJECT                                                                  
* STRAFFIC FILE RECORD DEFINITIONS                                              
*                                                                               
STRDEFN  DS    0X                                                               
         DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0A200AFF',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(02,AGYL)                                                     
         DC    X'FFFFFFFF',AL1(00,00),AL1(03),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(04),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         EJECT                                                                  
* REPFILE RECORD DEFINITIONS                                                    
                                                                                
REPDEFN  DS    0X                                                               
         DC    X'00000000',AL1(00,00),AL1(01),X'40',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'010001FF',AL1(00,00),AL1(02),X'00',AL3(0)                      
         DC    AL1(25,AGYA)                                                     
         DC    X'020002FF',AL1(00,00),AL1(03),X'00',AL3(0)                      
         DC    AL1(20,AGYA)                                                     
         DC    X'030003FF',AL1(00,00),AL1(04),X'00',AL3(0)                      
         DC    AL1(23,AGYA)                                                     
         DC    X'040004FF',AL1(00,00),AL1(05),X'00',AL3(0)                      
         DC    AL1(23,AGYA)                                                     
         DC    X'050005FF',AL1(00,00),AL1(06),X'00',AL3(0)                      
         DC    AL1(23,AGYA)                                                     
         DC    X'060006FF',AL1(00,00),AL1(07),X'00',AL3(0)                      
         DC    AL1(22,AGYA)                                                     
         DC    X'070007FF',AL1(00,00),AL1(08),X'00',AL3(0)                      
         DC    AL1(23,AGYA)                                                     
         DC    X'08000AFF',AL1(00,00),AL1(09),X'00',AL3(0)                      
         DC    AL1(25,AGYA)                                                     
         DC    X'0B000BFF',AL1(00,00),AL1(10),X'00',AL3(0)                      
         DC    AL1(16,AGYA)                                                     
         DC    X'0C000CFF',AL1(00,00),AL1(11),X'00',AL3(0)                      
         DC    AL1(02,AGYA)                                                     
         DC    X'0D000DFF',AL1(00,00),AL1(12),X'00',AL3(0)                      
         DC    AL1(23,AGYA)                                                     
         DC    X'0E000EFF',AL1(00,00),AL1(12),X'00',AL3(0)                      
         DC    AL1(14,AGYA)                                                     
         DC    X'0F000FFF',AL1(00,00),AL1(12),X'00',AL3(0)                      
         DC    AL1(23,AGYA)                                                     
         DC    X'110011FF',AL1(00,00),AL1(13),X'00',AL3(0)                      
         DC    AL1(06,AGYA)                                                     
         DC    X'120012FF',AL1(00,00),AL1(14),X'00',AL3(0)                      
         DC    AL1(10,AGYA)                                                     
         DC    X'130013FF',AL1(00,00),AL1(15),X'00',AL3(0)                      
         DC    AL1(16,AGYA)                                                     
         DC    X'140014FF',AL1(00,00),AL1(16),X'00',AL3(0)                      
         DC    AL1(17,AGYA)                                                     
         DC    X'15011501',AL1(00,00),AL1(60),X'00',AL3(0)                      
         DC    AL1(23,AGYA)                                                     
         DC    X'15021502',AL1(00,00),AL1(61),X'00',AL3(0)                      
         DC    AL1(13,AGYA)                                                     
         DC    X'15031503',AL1(00,00),AL1(62),X'00',AL3(0)                      
         DC    AL1(15,AGYA)                                                     
         DC    X'15041505',AL1(00,00),AL1(63),X'00',AL3(0)                      
         DC    AL1(13,AGYA)                                                     
         DC    X'15061506',AL1(00,00),AL1(64),X'00',AL3(0)                      
         DC    AL1(16,AGYA)                                                     
         DC    X'15071507',AL1(00,00),AL1(65),X'00',AL3(0)                      
         DC    AL1(12,AGYA)                                                     
         DC    X'160016FF',AL1(00,00),AL1(17),X'00',AL3(0)                      
         DC    AL1(14,AGYA)                                                     
         DC    X'170017FF',AL1(00,00),AL1(70),X'00',AL3(0)                      
         DC    AL1(25,AGYA)                                                     
         DC    X'180018FF',AL1(00,00),AL1(18),X'00',AL3(0)                      
         DC    AL1(24,AGYA)                                                     
         DC    X'190019FF',AL1(00,00),AL1(19),X'00',AL3(0)                      
         DC    AL1(17,AGYA)                                                     
         DC    X'1A001AFF',AL1(00,00),AL1(20),X'00',AL3(0)                      
         DC    AL1(25,AGYA)                                                     
         DC    X'1B001BFF',AL1(00,00),AL1(21),X'00',AL3(0)                      
         DC    AL1(15,AGYA)                                                     
         DC    X'1C001CFF',AL1(00,00),AL1(22),X'00',AL3(0)                      
         DC    AL1(13,AGYA)                                                     
         DC    X'1D001DFF',AL1(00,00),AL1(23),X'00',AL3(0)                      
         DC    AL1(17,AGYA)                                                     
         DC    X'1E001EFF',AL1(00,00),AL1(24),X'00',AL3(0)                      
         DC    AL1(16,AGYA)                                                     
         DC    X'1F001FFF',AL1(00,00),AL1(24),X'00',AL3(0)                      
         DC    AL1(16,AGYA)                                                     
         DC    X'200020FF',AL1(00,00),AL1(66),X'00',AL3(0)                      
         DC    AL1(01,AGYA)                                                     
         DC    X'210021FF',AL1(00,00),AL1(25),X'00',AL3(0)                      
         DC    AL1(15,AGYA)                                                     
         DC    X'220022FF',AL1(00,00),AL1(25),X'00',AL3(0)                      
         DC    AL1(13,AGYA)                                                     
         DC    X'230023FF',AL1(00,00),AL1(26),X'00',AL3(0)                      
         DC    AL1(23,AGYA)                                                     
         DC    X'240024FF',AL1(00,00),AL1(27),X'00',AL3(0)                      
         DC    AL1(24,AGYA)                                                     
         DC    X'250025FF',AL1(00,00),AL1(27),X'00',AL3(0)                      
         DC    AL1(24,AGYA)                                                     
         DC    X'260026FF',AL1(00,00),AL1(28),X'00',AL3(0)                      
         DC    AL1(20,AGYA)                                                     
         DC    X'270027FF',AL1(00,00),AL1(29),X'00',AL3(0)                      
         DC    AL1(01,AGYA)                                                     
         DC    X'280028FF',AL1(00,00),AL1(30),X'00',AL3(0)                      
         DC    AL1(13,AGYA)                                                     
         DC    X'290029FF',AL1(00,00),AL1(31),X'00',AL3(0)                      
         DC    AL1(11,AGYA)                                                     
         DC    X'2A002AFF',AL1(00,00),AL1(32),X'00',AL3(0)                      
         DC    AL1(22,AGYA)                                                     
         DC    X'2B002BFF',AL1(00,00),AL1(33),X'00',AL3(0)                      
         DC    AL1(21,AGYA)                                                     
         DC    X'2C002CFF',AL1(00,00),AL1(34),X'00',AL3(0)                      
         DC    AL1(04,AGYA)                                                     
         DC    X'2D002DFF',AL1(00,00),AL1(35),X'00',AL3(0)                      
         DC    AL1(12,AGYA)                                                     
         DC    X'2E002EFF',AL1(00,00),AL1(36),X'00',AL3(0)                      
         DC    AL1(15,AGYA)                                                     
         DC    X'300030FF',AL1(00,00),AL1(37),X'00',AL3(0)                      
         DC    AL1(17,AGYA)                                                     
         DC    X'310031FF',AL1(00,00),AL1(38),X'00',AL3(0)                      
         DC    AL1(22,AGYA)                                                     
         DC    X'320032FF',AL1(00,00),AL1(39),X'00',AL3(0)                      
         DC    AL1(24,AGYA)                                                     
         DC    X'330033FF',AL1(00,00),AL1(40),X'00',AL3(0)                      
         DC    AL1(17,AGYA)                                                     
         DC    X'340034FF',AL1(00,00),AL1(41),X'00',AL3(0)                      
         DC    AL1(20,AGYA)                                                     
         DC    X'350035FF',AL1(00,00),AL1(42),X'00',AL3(0)                      
         DC    AL1(13,AGYA)                                                     
         DC    X'360036FF',AL1(00,00),AL1(43),X'00',AL3(0)                      
         DC    AL1(17,AGYA)                                                     
         DC    X'370037FF',AL1(00,00),AL1(44),X'00',AL3(0)                      
         DC    AL1(13,AGYA)                                                     
         DC    X'380038FF',AL1(00,00),AL1(45),X'00',AL3(0)                      
         DC    AL1(19,AGYA)                                                     
         DC    X'390039FF',AL1(00,00),AL1(46),X'00',AL3(0)                      
**NO-OP  DC    AL1(13,AGYA)                                                     
         DC    AL1(11,AGYA)                                                     
         DC    X'3A003AFF',AL1(00,00),AL1(47),X'00',AL3(0)                      
         DC    AL1(22,AGYA)                                                     
         DC    X'3B003BFF',AL1(00,00),AL1(48),X'00',AL3(0)                      
         DC    AL1(23,AGYA)                                                     
         DC    X'3C003CFF',AL1(00,00),AL1(49),X'00',AL3(0)                      
         DC    AL1(24,AGYA)                                                     
         DC    X'3D003DFF',AL1(00,00),AL1(49),X'00',AL3(0)                      
         DC    AL1(23,AGYA)                                                     
         DC    X'3E003EFF',AL1(00,00),AL1(49),X'00',AL3(0)                      
         DC    AL1(10,AGYA)                                                     
         DC    X'410041FF',AL1(00,00),AL1(50),X'00',AL3(0)                      
         DC    AL1(07,AGYA)                                                     
         DC    X'420042FF',AL1(00,00),AL1(50),X'00',AL3(0)                      
         DC    AL1(20,AGYA)                                                     
         DC    X'43014301',AL1(00,00),AL1(50),X'00',AL3(0)                      
         DC    AL1(07,AGYA)                                                     
         DC    X'430243FF',AL1(00,00),AL1(67),X'00',AL3(0)                      
         DC    AL1(15,AGYA)                                                     
         DC    X'440044FF',AL1(00,00),AL1(51),X'00',AL3(0)                      
         DC    AL1(23,AGYA)                                                     
         DC    X'450045FF',AL1(00,00),AL1(51),X'00',AL3(0)                      
         DC    AL1(01,AGYA)                                                     
         DC    X'460046FF',AL1(00,00),AL1(51),X'00',AL3(0)                      
         DC    AL1(22,AGYA)                                                     
         DC    X'470047FF',AL1(00,00),AL1(51),X'00',AL3(0)                      
         DC    AL1(21,AGYA)                                                     
         DC    X'490049FF',AL1(00,00),AL1(54),X'00',AL3(0)                      
         DC    AL1(16,AGYA)                                                     
         DC    X'4A004AFF',AL1(00,00),AL1(55),X'00',AL3(0)                      
         DC    AL1(17,AGYA)                                                     
         DC    X'4B004BFF',AL1(00,00),AL1(56),X'00',AL3(0)                      
         DC    AL1(21,AGYA)                                                     
         DC    X'4C004CFF',AL1(00,00),AL1(68),X'00',AL3(0)                      
         DC    AL1(16,AGYA)                                                     
         DC    X'4D004DFF',AL1(00,00),AL1(57),X'00',AL3(0)                      
         DC    AL1(19,AGYA)                                                     
         DC    X'4F004FFF',AL1(00,00),AL1(69),X'00',AL3(0)                      
         DC    AL1(22,AGYA)                                                     
         DC    X'510051FF',AL1(00,00),AL1(52),X'00',AL3(0)                      
         DC    AL1(07,AGYA)                                                     
         DC    X'520052FF',AL1(00,00),AL1(52),X'00',AL3(0)                      
         DC    AL1(20,AGYA)                                                     
         DC    X'530053FF',AL1(00,00),AL1(58),X'00',AL3(0)                      
         DC    AL1(20,AGYA)                                                     
         DC    X'620062FF',AL1(00,00),AL1(59),X'00',AL3(0)                      
         DC    AL1(04,AGYA)                                                     
         DC    X'FFFFFFFF',AL1(00,00),AL1(53),X'20',AL3(0)                      
         DC    AL1(0,0)                                                         
         DC    X'0000FFFF',AL1(00,00),AL1(04),X'98',AL3(0)                      
         DC    AL1(0,0)                                                         
         EJECT                                                                  
* DCB'S FOR TAPE FILES                                                          
                                                                                
TINT     DCB   DDNAME=TINT,DSORG=PS,MACRF=(GM),EODAD=SP38,             *        
               RECFM=VB,BUFNO=2                                                 
                                                                                
TOUT0    DCB   DDNAME=TOUT0,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUT1    DCB   DDNAME=TOUT1,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUT2    DCB   DDNAME=TOUT2,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUT3    DCB   DDNAME=TOUT3,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUT4    DCB   DDNAME=TOUT4,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUT5    DCB   DDNAME=TOUT5,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUT6    DCB   DDNAME=TOUT6,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUT7    DCB   DDNAME=TOUT7,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUT8    DCB   DDNAME=TOUT8,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUT9    DCB   DDNAME=TOUT9,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTA    DCB   DDNAME=TOUTA,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTB    DCB   DDNAME=TOUTB,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTC    DCB   DDNAME=TOUTC,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTD    DCB   DDNAME=TOUTD,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTE    DCB   DDNAME=TOUTE,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTF    DCB   DDNAME=TOUTF,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTG    DCB   DDNAME=TOUTG,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTH    DCB   DDNAME=TOUTH,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTI    DCB   DDNAME=TOUTI,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTJ    DCB   DDNAME=TOUTJ,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTK    DCB   DDNAME=TOUTK,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTL    DCB   DDNAME=TOUTL,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTM    DCB   DDNAME=TOUTM,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTN    DCB   DDNAME=TOUTN,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTO    DCB   DDNAME=TOUTO,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTP    DCB   DDNAME=TOUTP,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTQ    DCB   DDNAME=TOUTQ,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTR    DCB   DDNAME=TOUTR,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTS    DCB   DDNAME=TOUTS,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTT    DCB   DDNAME=TOUTT,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTU    DCB   DDNAME=TOUTU,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTV    DCB   DDNAME=TOUTV,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTW    DCB   DDNAME=TOUTW,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTX    DCB   DDNAME=TOUTX,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTY    DCB   DDNAME=TOUTY,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTZ    DCB   DDNAME=TOUTZ,DSORG=PS,MACRF=(PM),                       *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTIT   DCB   DDNAME=TOUTIT,DSORG=PS,MACRF=(PM),                      *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTTT   DCB   DDNAME=TOUTTT,DSORG=PS,MACRF=(PM),                      *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTTU   DCB   DDNAME=TOUTTU,DSORG=PS,MACRF=(PM),                      *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTI1   DCB   DDNAME=TOUTI1,DSORG=PS,MACRF=(PM),                      *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTI3   DCB   DDNAME=TOUTI3,DSORG=PS,MACRF=(PM),                      *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTO1   DCB   DDNAME=TOUTO1,DSORG=PS,MACRF=(PM),                      *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTU1   DCB   DDNAME=TOUTU1,DSORG=PS,MACRF=(PM),                      *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTY1   DCB   DDNAME=TOUTY1,DSORG=PS,MACRF=(PM),                      *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTN1   DCB   DDNAME=TOUTN1,DSORG=PS,MACRF=(PM),                      *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTN2   DCB   DDNAME=TOUTN2,DSORG=PS,MACRF=(PM),                      *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
TOUTU2   DCB   DDNAME=TOUTU2,DSORG=PS,MACRF=(PM),                      *        
               RECFM=VB,BLKSIZE=32760,LRECL=6200,BUFNO=2                        
                                                                                
         ENTRY SSB                                                              
         DS    0L                                                               
         DC    CL16'*SSB*SSB*SSB*SSB'                                           
SSB      DC    XL2'0000',X'FF',X'02',252X'00'                                   
                                                                                
ERRTAB   DC    (64*1024)X'00'                                                   
         EJECT                                                                  
* DSECT TO COVER INPUT PARAMETER TABLE                                          
                                                                                
PARMTABD DSECT                                                                  
PARMEXLN DS    AL1                 LENGTH FOR EXECUTE                           
PARMNAME DS    CL8                 PARAMETER OPERAND                            
PARMROUT DS    AL3                 A(VALIDATION ROUTINE)                        
PARMPARM DS    XL8                 EXTRA VALUES FOR ROUTINE                     
PARMLEN  EQU   *-PARMTABD                                                       
                                                                                
* DSECT TO COVER SYSTEM TABLE                                                   
                                                                                
SYSTABD  DSECT                                                                  
SYSFILE  DS    CL8                 FILE SET NAME                                
SYSOVNUM DS    X                   SYSTEM OVERLAY NUMBER                        
SYSADEF  DS    AL3                 A(RECORD DEFN TABLE)                         
SYSKEYLN DS    AL2                 KEY LENGTH                                   
SYSLNDSP DS    AL2                 DISP TO RECORD LENGTH                        
SYSFEDSP DS    AL2                 DISP TO FIRST ELEMENT                        
SYSINDS  DS    X                   INDICATORS                                   
SYSLEN   EQU   *-SYSTABD                                                        
                                                                                
* DSECT TO COVER SE TABLE                                                       
                                                                                
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
                                                                                
RECDEFD  DSECT                                                                  
RECKEYL  DS    XL2                 LOW KEY VALUE                                
RECKEYH  DS    XL2                 HIGH KEY VALUE                               
RECOCVAL DS    0AL2                                                             
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
                                                                                
* DSECT TO COVER KEY FORMAT TABLE                                               
                                                                                
KEYTABD  DSECT                                                                  
KEYCOMPL DS    X                   L'AGENCY COMPARE-1                           
KEYODISP DS    X                   DISP TO OLD AGENCY                           
KEYNDISP DS    X                   DISP TO NEW AGENCY                           
KEYAND   DS    X                   KEY 'AND' VALUE                              
                                                                                
* DSECT TO COVER AGENCY VALUES TABLE                                            
                                                                                
AGYTABD  DSECT                                                                  
AGYOLDF  DS    C                   OLD FILE SET NUMBER                          
AGYOLDL  DS    X                   OLD AGENCY LEFT                              
AGYOLDR  DS    X                   OLD AGENCY RIGHT                             
AGYALFA  DS    CL2                 OLD AGENCY ALPHA                             
AGYNEWF  DS    C                   NEW FILE SET NUMBER                          
AGYNEWL  DS    X                   NEW AGENCY LEFT                              
AGYNEWR  DS    X                   NEW AGENCY RIGHT                             
AGYINDS  DS    X                   INDICATORS                                   
AGYIUPDT EQU   X'80'               ACCESS RECORD ALREADY UPDATED                
AGYLENQ  EQU   *-AGYTABD                                                        
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE CTGENFILE                                                      
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011DDSPANKER 05/24/19'                                      
         END                                                                    
