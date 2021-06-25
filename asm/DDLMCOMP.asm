*          DATA SET DDLMCOMP   AT LEVEL 020 AS OF 10/02/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE LMCOMPA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DDINFO                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE XSORT                                                                  
*INCLUDE KHDUMMY                                                                
*                                                                               
**********************************************************************          
* THIS PROGRAM COMPARES THE CONTENTS OF TWO LOAD MODULES REFERENCED BY          
* DDNAMES OLDLMMOD AND NEWLMMOD.                                                
*                                                                               
* IF THE PROGRAM IS INVOKED WITH PARM "OBJLEVEL", THEN THIS TYPICALLY           
* MEANS THAT WE'RE DOING A FINAL COMPILE.                                       
* WHEN THIS PROGRAM IS INVOKED VIA THE PANAPT RVP ACTION, THE PARM              
* WILL BE "LINKDATE".                                                           
*                                                                               
* WHEN THE PARM IS "OBJLEVEL", THIS PROGRAM LOCATES THE EMBEDDED OBJECT         
* MODULE LEVEL STAMPS AND PRINTS AN INFORMATIONAL REPORT. IN A                  
* SUBSEQUENT STEP, THE REPORTS ARE COMPARED BY THE PCOMPARE UTILITY,            
* SO THAT THE PROGRAMMER CAN EASILY SEE WHICH NEW OBJECT MODULES (IF            
* ANY) ARE LINKED INTO THE "NEW" LOAD MODULE.                                   
*                                                                               
* THE PROGRAM ALSO UNCONDITIONALLY EXTRACTS THE LINK DATE/TIME OF EACH          
* LOAD MODULE AND COMPARE THEM. IF THE "NEW" LOAD MODULE HAS BEEN               
* LINKED *PRIOR* TO THE "OLD", A WARNING IS PRINTED. FURTHER ACTION             
* IS DEPENDENT UPON A PARAMETER PASSED TO THIS PROGRAM VIA R1:                  
*   PARM=OBJLEVEL: EXIT WITH RC=4 (JUST WARN THE PROGRAMMER)                    
*   PARM=LINKDATE: EXIT WITH RC=8 (PREVENT PROMOTION OF A REGRESSED             
*                                  LOAD MODULE!)                                
*                                                                               
* WHEN THE PARM IS "LINKDATE", IF REGRESSION ON THE LINKDATE IS                 
* DETECTED, THE VERIFICATION WILL BE UNSUCCESSFUL. LIKEWISE, IF THE             
* ZAP COUNT IN THE "NEW" LOAD MODULE IS NON-ZERO, THE VERIFICATION              
* WILL BE UNSUCCESSFUL.                                                         
*                                                                               
* THIS PROGRAM IS VERY SENSITIVE TO THE FORMAT OF EACH LOAD MODULE.             
* THESE ARE THE ASSUMPTIONS:                                                    
* (1) THE 1ST RECORD MUST START WITH X'2080' OR X'2000'                         
*     THESE ARE CSD RECORDS.                                                    
* (2) FOLLOWING THAT, ARE IDR RECORDS  X'80'                                    
*     THESE RECORDS INDICATE THE ZAP COUNT AND THE LINK DATE.                   
* (3) RIGHT AFTER THEM, MUST BE A CONTROL RECORD X'01' OR X'0D'                 
* (4) THEN COMES X'02' OR X'03' OR X'0E' OR X'01' AGAIN CONTROL RECORDS         
* (5) X'02' ARE SKIPPED                                                         
* (6) ONLY X'03' AND X'01' ARE CONSIDERED.                                      
* (7) X'0E' IS ALSO CONSIDERED AND IT IS THE LAST RECORD.                       
*                                                                               
* FOR MORE INFOMATION ON LOAD MODULE RECORD FORMAT, SEE                         
* TITLE: Z/OS V1R3.0-V1R4.0 MVS PROGRAM MANAGEMENT: ADVANCED FACILITIES         
* DOCUMENT NUMBER: SA22-7644-02                                                 
* APPENDIX B.2                                                                  
*                                                                               
**********************************************************************          
         EJECT                                                                  
LMCOMP   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*LMCOMP*,=V(REGSAVE),R7                                        
         USING DPRINT,RA                                                        
         L     RA,=V(CPRINT)                                                    
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
*                                                                               
         LR    R1,RC                                                            
         SHI   R1,4                                                             
         L     R1,0(R1)                                                         
         L     R1,0(R1)            A(PARM= FROM EXEC JCL CARD)                  
*                                                                               
         MVI   MODE,OBJLEVEL       POSIT OBJLEVEL PARM                          
         CLC   0(2,R1),=H'8'                                                    
         BL    MAIN                NO PARM PRESENT                              
         CLC   0(2,R1),=H'12'                                                   
         BL    MAIN00              ADDITIONAL PARMS ARE NOT PRESENT             
         MVC   PAPTLEVL,10(R1)     SAVE PANAPT LEVEL                            
         MVC   MEMBRNAM,14(R1)     SAVE PAN MEMBER NAME                         
         MVC   BASELM,24(R1)       BASE LOADMOD EXISTENCE FLAG                  
         MVC   TESTLM,25(R1)       TEST LOADMOD EXISTENCE FLAG                  
         MVC   RESTAGE,26(R1)      RESTAGE PERMITTED FLAG (Y/N)                 
*                                                                               
MAIN00   DS    0H                                                               
         CLC   =C'OBJLEVEL',2(R1)  USUALLY FINAL COMPILE                        
         BE    MAIN                                                             
         CLC   =C'LINKDATE',2(R1)  USUALLY RVP                                  
         BNE   MAIN                                                             
*                                                                               
         MVI   MODE,LINKDATE                                                    
*                                                                               
         GOTO1 =V(DDINFO),DMCB,=C'MSGFILE ',=AL2(DINRTDSN)                      
         LTR   RF,RF               IS MSGFILE DD ALLOCATED?                     
         BNZ   MAIN                NO                                           
         MVI   MSGFLFLG,C'Y'       YES: OPEN IT                                 
         OPEN  (MSGFILE,OUTPUT)                                                 
*                                                                               
MAIN     DS    0H                                                               
         L     RF,PSAAOLD-PSA(,0)       GET CURRENT/HOME ASCB                   
         L     RF,(ASCBASXB-ASCB)(,RF)  GET ASXB ADDRESS                        
         L     RF,(ASXBSENV-ASXB)(,RF)  GET ACEE ADDRESS                        
         CLC   =CL4'ACEE',0(RF)         VALID ACEE?                             
         JE    *+6                      YES: EXTRACT RACF USERID                
         DC    H'0'                     NO: IMPOSSIBLE                          
         MVC   RACFUSER,(ACEEUSRI-ACEE)(RF)                                     
*                                                                               
         LA    R6,IDRTABLE         RETRIEVE IDR DATA                            
         USING IDRDATAD,R6                                                      
*                                                                               
MAIN15   DS    0H                                                               
         C     R6,=A(OLD_IDR)                                                   
         BNE   NEXT20                                                           
         CLI   BASELM,C'Y'         PRODUCTION LOAD MODULE EXISTS?               
         BNE   NXTFILE             NO                                           
*                                                                               
         LA    R2,BOOKNAM1                                                      
         LA    R9,MEMNAME1                                                      
         LA    R8,OLDLMMOD                                                      
         BAS   RE,GETDSNS                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R4,15,ALVLTAB                                                    
         B     MAIN20                                                           
*                                                                               
NEXT20   DS    0H                                                               
         C     R6,=A(NEW_IDR)                                                   
         BNE   NEXT30                                                           
         LA    R2,BOOKNAM2                                                      
         LA    R9,MEMNAME2                                                      
         LA    R8,NEWLMMOD                                                      
         BAS   RE,GETDSNS                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R4,15,ALVLTAB                                                    
         B     MAIN20                                                           
*                                                                               
NEXT30   DS    0H                                                               
         C     R6,=A(OST_IDR)                                                   
         BE    *+6                                                              
         DC    H'0'                WHERE IS R6 POINTING ?!?                     
*                                                                               
         CLI   TESTLM,C'Y'         STAGED LOAD MODULE EXISTS?                   
         BNE   PRTSTAMP            NO                                           
         CLI   MODE,LINKDATE       RVP ACTION?                                  
         BNE   PRTSTAMP                                                         
         CLC   PAPTLEVL,=C'STGE'   YES: GOING TO STGE LEVEL?                    
         BNE   PRTSTAMP                                                         
*                                                                               
         MVC   BOOKNAM3,BOOKNAM2                                                
         LA    R2,BOOKNAM3                                                      
         LA    R9,MEMNAME1                                                      
         LA    R8,OLDSTMOD                                                      
         BAS   RE,GETDSNS                                                       
         BNE   PRTSTAMP                                                         
         SR    R4,R4                                                            
*                                                                               
MAIN20   DS    0H                                                               
         ST    R4,APHASTAB                                                      
         USING PTD,R4                                                           
         OPEN  ((R8))                                                           
         SR    R3,R3                                                            
*                                                                               
*FIND THE 1ST CESD RECORD (X'2080' OR X'2000')                                  
*                                                                               
LOOP1    DS    0H                                                               
         GET   (R8)                                                             
         CLC   0(2,R1),=X'2080'    CESD RECORD                                  
         BE    L000                                                             
         CLC   0(2,R1),=X'2000'    CESD RECORD                                  
         BE    L000                                                             
         B     LOOP1                                                            
*                                                                               
NEXT     DS    0H                                                               
         GET   (R8)                                                             
         CLC   0(2,R1),=X'2080'    CESD RECORD                                  
         BE    L000                                                             
         CLC   0(2,R1),=X'2000'    CESD RECORD                                  
         BNE   OUT                                                              
*                                                                               
*EXTRACT THE CESD DATA (PHASE NAME, LMOD LOC, LENGTH AND TYPE)                  
*                                                                               
L000     DS    0H                                                               
         OC    APHASTAB,APHASTAB   PICK UP EMBEDDED CSECT INFO?                 
         BZ    NEXT                NO                                           
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,6(R1)          BYTE COUNT OF CESD DATA                      
         SRL   R2,4                /CESD DATA SIZE,16                           
         LA    R1,8(R1)            BEGINNING OF CESD DATA                       
*                                                                               
L005     DS    0H                                                               
         LLC   RE,8(R1)            CESD TYPE                                    
         SLL   RE,28                                                            
         SRL   RE,28               GET RID OF 1ST NIBBLE                        
         STC   RE,BYTE                                                          
*                                                                               
         CLI   BYTE,SD                                                          
         BE    L010                                                             
         CLI   BYTE,PC                                                          
         BE    L010                                                             
         CLI   BYTE,CM                                                          
         BE    L010                                                             
         CLI   BYTE,PR                                                          
         BE    L010                                                             
         B     L025                                                             
*                                                                               
L010     DS    0H                                                               
         MVC   PTNAM,0(R1)         ENTRY NAME                                   
         MVC   PTTYP,BYTE          ENTRY TYPE                                   
         MVC   PTLOC,9(R1)         ENTRY LOCATION                               
         MVC   PTLEN,13(R1)        ENTRY LENGTH                                 
*                                                                               
         AHI   R4,PTQ                                                           
         LA    R3,1(R3)                                                         
*                                                                               
L025     AHI   R1,16                                                            
         BCT   R2,L005                                                          
         B     NEXT                                                             
         EJECT                                                                  
OUT      DS    0H                                                               
         OC    APHASTAB,APHASTAB   PICK UP EMBEDDED CSECT INFO?                 
         BZ    CHKIDR              NO                                           
*                                                                               
         ST    R1,FULL                                                          
         GOTO1 =V(XSORT),DMCB,APHASTAB,(R3),PTQ,3,0                             
         MVC   0(2,R4),=X'FFFF'    END OF TABLE                                 
         L     R1,FULL                                                          
         B     CHKIDR                                                           
*                                                                               
* EXAMINE IDR RECORDS                                                           
*                                                                               
LOOP2    DS    0H                                                               
         GET   (R8)                                                             
*                                                                               
CHKIDR   DS    0H                                                               
         CLI   0(R1),X'80'         IDR RECORD                                   
         BNE   LOOP2X                                                           
*                                                                               
* NOTE THAT BOTH THE ZAP AND BIND DATES ARE STORED IN YYDDDF (PACKED)           
* FORMAT. I.E., WE DON'T KNOW THE CENTURY. SO WE ALWAYS FORCE                   
* THE DATE TO THE 20TH CENTURY, THEN HAVE DATCON CONVERT THE DATE INTO          
* YYMMDD "NON-FUNNY" FORMAT. THEN WE CALL DATCON ONCE MORE, AND CONVERT         
* THAT YYMMDD DATE INTO A DATE WHICH INCLUDES THE CENTURY. THAT CAUSES          
* DATCON TO USE ITS STANDARD RULES TO DERIVE THE CENTURY FOR US.                
*                                                                               
         TM    2(R1),X'01'         ZAP DATA?                                    
         BNO   CHKIDR10                                                         
         LLC   R0,3(R1)            YES                                          
         N     R0,=X'0000003F'     THESE BITS CONTAIN THE ZAP COUNT             
         CVD   R0,DUB                                                           
         MVC   ZAPCOUNT,DUB+4                                                   
         IF (CP,ZAPCOUNT,NE,=P'0') IF THE ZAP COUNT ISN'T ZERO:                 
           MVC   WORK+1(3),6(R1)     ZAP DATE: YYDDDF                           
           MVI   WORK,0              ALWAYS FORCE 20TH CENTURY                  
           GOTO1 =V(DATCON),DMCB,(6,WORK),(X'20',WORK+4)                        
           GOTO1 =V(DATCON),DMCB,WORK+4,(15,ZAPDATE)                            
         ENDIF ,                                                                
         B     LOOP2                                                            
*                                                                               
CHKIDR10 DS    0H                                                               
         TM    2(R1),X'08'         USER DATA FROM IDENTIFY FUNCTION?            
         BNO   *+14                                                             
         MVC   LINKUSER,13(R1)     YES: SAVE THE LINKER'S USERID                
         B     LOOP2                                                            
*                                                                               
         TM    2(R1),X'02'         LINKAGE EDITOR DATA                          
         BNO   LOOP2                                                            
*                                  SAVE TIME AND DATE LAST LINKED               
* DERIVE LINKDATE OF "OLD" AND "NEW" LOAD MODULES.                              
*                                                                               
         MVC   WORK+1(3),15(R1)    LINK DATE: YYDDDF                            
         MVI   WORK,0              ALWAYS FORCE 20TH CENTURY                    
         LR    R0,R1               SAVE R1                                      
         GOTO1 =V(DATCON),DMCB,(6,WORK),(X'20',WORK+4)                          
         GOTO1 =V(DATCON),DMCB,WORK+4,(15,BINDDATE)                             
         LR    R1,R0               RESTORE R1                                   
*                                                                               
         CLI   1(R1),21            MAKE SURE RECORD IS LONG ENOUGH...           
         BL    LOOP2               ... TO CONTAIN THE TIME                      
         MVC   BINDTIME,18(R1)                                                  
         B     LOOP2                                                            
*                                                                               
LOOP2X   DS    0H                                                               
         L     R4,APHASTAB                                                      
*                                                                               
*MUST BE A CONTROL RECORD X'01' OR X'0D'                                        
*                                                                               
         CLI   0(R1),X'01'                                                      
         BE    GETONE                                                           
         CLI   0(R1),X'0D'                                                      
         BE    GETONE                                                           
         DC    H'0'                MISSING CONTROL RECORD ?!?                   
         EJECT                                                                  
GETONE   DS    0H                                                               
         CLI   MODE,LINKDATE       IF WE GOT HERE VIA RVP, DON'T...             
         BE    CLOSELM             ...BOTHER LOOKING FOR LEVEL STAMPS           
*                                                                               
         LA    R2,ALOADMOD         WHERE TO PUT A(LOADMOD)                      
*                                                                               
* LOAD THE LOAD MODULE INTO CORE SO WE CAN SEARCH FOR ITS EMBEDDED              
* LEVEL STAMP. NOTE: MODULES WITH "RMODE ANY" WILL BE LOADED ABOVE THE          
* LINE, SO IT BEHOOVES US TO BE IN XA MODE WHILE WE SEARCH FOR THE              
* LEVEL STAMP!                                                                  
*                                                                               
         SAM31 ,                   ENTER XA MODE                                
*                                                                               
         LOAD  EPLOC=(R9),DCB=(R8),LOADPT=(R2)                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL LOAD OF LOAD MODULE             
         N     R1,=X'00FFFFFF'     HOB OF R1 = APF AUTHORIZATION CODE           
*                                  R1 = L'LOADMOD IN DOUBLEWORDS                
         BNZ   *+6                 MAKE SURE A LENGTH WAS RETURNED              
         DC    H'0'                A DEATH HERE MEANS THAT THE LOADMOD          
*                                   IS > 16M LONG, OR HAS RMODE(SPLIT).         
*                                   WE'LL NEED TO INVOKE THE CSVQUERY           
*                                   MACRO IF THIS EVER HAPPENS.                 
         MHI   R1,8                R1 = L'LOADMOD IN BYTES                      
         L     RF,ALOADMOD         RF = A(LOAD MODULE)                          
         AR    R1,RF               L'LOADMOD                                    
         ST    R1,ALOADMODX        A(END OF LOADMOD)                            
*                                                                               
LOOP3    DS    0H                                                               
         L     RF,ALOADMOD         RF = A(LOAD MODULE)                          
         SR    R0,R0                                                            
         ICM   R0,7,PTLOC          A(CSECT)                                     
         AR    RF,R0               RF = OFFSET INTO LOAD MODULE                 
         SR    R0,R0                                                            
         ICM   R0,7,PTLEN          L'CSECT                                      
         CHI   R0,60                                                            
         BL    SKIP                TOO SHORT TO EXAMINE                         
         LR    RE,RF               RE = A(START OF CSECT)                       
         AR    RE,R0               RE = A(END OF CSECT)                         
*                                                                               
*SEARCH FOR BOOK, LEVEL, DATE                                                   
*                                                                               
LOOP4    DS    0H                                                               
         L     R0,ALOADMODX                                                     
         SHI   R0,L'AUDBK                                                       
         CR    RF,R0                                                            
         BH    SKIP                AVOID S0C4 ON THE CLC                        
         CLC   AUDBK,0(RF)         C'BOOK='                                     
         BNE   *+10                                                             
         MVC   PTBKNAM,5(RF)                                                    
*                                                                               
         L     R0,ALOADMODX                                                     
         SHI   R0,L'AUDLV                                                       
         CR    RF,R0                                                            
         BH    SKIP                AVOID S0C4 ON THE CLC                        
         CLC   AUDLV,0(RF)         C'LEVEL='                                    
         BNE   *+10                                                             
         MVC   PTBKLEV#,6(RF)                                                   
*                                                                               
         L     R0,ALOADMODX                                                     
         SHI   R0,L'AUDDT                                                       
         CR    RF,R0                                                            
         BH    SKIP                AVOID S0C4 ON THE CLC                        
         CLC   AUDDT,0(RF)         C' DATE='                                    
         BNE   LP4_50                                                           
*                                                                               
         CLI   6(RF),C'0'          OLD-STYLE DATE FORMAT?                       
         BL    LP4_20                                                           
         MVC   PTBKDATE,6(RF)      YES: MM/DD/YY (OR DD/MM/YY IN UK)            
         B     LP4_60              'TIME=' WON'T BE PRESENT                     
*                                                                               
LP4_20   DS    0H                                                               
         MVC   THREE,6(RF)         NEW-STYLE: MMMDD/YY                          
         LA    R2,MONTAB                                                        
LP4_30   CLC   THREE,0(R2)                                                      
         BE    LP4_40                                                           
         CLI   0(R2),X'FF'                                                      
         BE    LP4_60              MONTH NOT FOUND: THIS ISN'T A DATE           
         AHI   R2,L'MONTAB                                                      
         B     LP4_30                                                           
*                                                                               
LP4_40   DS    0H                                                               
         MVC   PTBKDATM,3(R2)      MONTH NUMBER                                 
         MVI   PTBKSLS1,C'/'                                                    
         MVC   PTBKDATD,9(RF)      DAY NUMBER                                   
         MVI   PTBKSLS2,C'/'                                                    
         MVC   PTBKDATY,12(RF)     YEAR NUMBER                                  
*                                                                               
LP4_50   DS    0H                                                               
         CLC   AUDTM,0(RF)                                                      
         BNE   *+10                                                             
         MVC   PTBKTIME,6(RF)                                                   
*                                                                               
         L     R0,ALOADMODX                                                     
         SHI   R0,13               L'" COMPILED ON "                            
         CR    RF,R0                                                            
         BH    SKIP                AVOID S0C4 ON THE CLC                        
         CLC   =C' COMPILED ON ',0(RF)  IS COMPILED INFO PRESENT?               
         BNE   LP4_60                                                           
*                                                                               
         MVC   PTCOMPBY,34(RF)     USERID OF PERSON WHO DID THE COMPILE         
         L     R0,ALOADMODX                                                     
         SHI   R0,14+42            L'" AS RMXXXXXXXX" + OFFSET                  
         CR    RF,R0                                                            
         BH    SKIP                AVOID S0C4 ON THE CLC                        
         CLC   =C' AS ',42(RF)     ...THEN CHECK FOR RM BOOK NAME               
         BNE   LP4_60                                                           
         MVC   PTBKRELO,46(RF)     DISPLACEMENTS ARE HARD AS NAILS              
*                                                                               
LP4_60   DS    0H                                                               
         LA    RF,1(RF)            CHECK NEXT CHARACTER                         
         CR    RF,RE               END OF CONTROL SECTION?                      
         BL    LOOP4               NO: KEEP LOOKING                             
*                                                                               
SKIP     DS    0H                                                               
         AHI   R4,PTQ              BUMP TO NEXT CSECT                           
         CLC   0(2,R4),=X'FFFF'    EOT?                                         
         BNE   LOOP3                                                            
*                                                                               
         DELETE EPLOC=(9)          YES: UNLOAD THE LOAD MODULE                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL UNLOAD                          
*                                                                               
         SAM24 ,                   EXIT XA MODE                                 
*                                                                               
CLOSELM  DS    0H                                                               
         CLOSE ((R8))                                                           
*                                                                               
NXTFILE  DS    0H                                                               
         LA    R6,IDRDATAQ(,R6)    NEXT IDR TABLE ENTRY                         
         CLI   0(R6),X'FF'                                                      
         BNE   MAIN15                                                           
         DROP  R6                                                               
         EJECT                                                                  
PRTSTAMP DS    0H                                                               
         LHI   R3,2                TWO TABLES TO PROCESS (OLD AND NEW)          
*                                                                               
         CLI   BASELM,C'Y'         PRODUCTION LOAD MODULE EXISTS?               
         BNE   NEXTMOD             NO                                           
*                                                                               
         MVC   TITLE(17),=CL17'CSECT LISTING OF '                               
         MVC   TITLE+17(L'BOOKNAM1),BOOKNAM1                                    
*                                                                               
         CLI   MODE,LINKDATE       IF WE GOT HERE VIA RVP, WE DON'T...          
         BE    *+16                ...HAVE THE LEVEL STAMPS                     
         MVC   MID3,MIDHEAD1                                                    
         MVC   MID4,MIDHEAD2                                                    
*                                                                               
         USING IDRDATAD,R2                                                      
         LA    R2,OLD_IDR          R2 = A(CURRENT LOADMOD IDR FIELDS)           
         MVI   MID1,0                                                           
         MVC   MID2(13),=C'LAST BIND ON '                                       
         MVC   WORK(4),BINDDATE    X'0CYYDDDF'                                  
         GOTO1 =V(DATCON),DMCB,(6,WORK),(23,MID2+13)                            
         CP    BINDTIME,=P'0'                                                   
         BE    PRTHEAD                                                          
         MVC   MID2+24(2),=CL2'AT'                                              
         UNPK  WORK(7),BINDTIME                                                 
         MVC   MID2+27(2),WORK+1                                                
         MVI   MID2+29,C':'                                                     
         MVC   MID2+30(2),WORK+3                                                
         MVI   MID2+32,C':'                                                     
         MVC   MID2+33(2),WORK+5                                                
         MVC   MID2+36(2),=C'BY'                                                
         MVC   MID2+39(8),LINKUSER                                              
         B     PRTHEAD                                                          
         EJECT                                                                  
NXTTAB   DS    0H                                                               
         MVC   OUTOLD(DCBLQ),OUTNEW                                             
         MVC   TITLE(17),=CL17'CSECT LISTING OF '                               
         MVC   TITLE+17(L'BOOKNAM2),BOOKNAM2                                    
*                                                                               
         LA    R2,NEW_IDR                                                       
         MVI   MID1,0                                                           
         MVC   MID2(13),=C'LAST BIND ON '                                       
         MVC   WORK(4),BINDDATE    X'0CYYDDDF'                                  
         GOTO1 =V(DATCON),DMCB,(6,WORK),(23,MID2+13)                            
         CP    BINDTIME,=P'9999999'                                             
         BE    PRTHEAD                                                          
         MVC   MID2+24(2),=CL2'AT'                                              
         UNPK  WORK(7),BINDTIME                                                 
         MVC   MID2+27(2),WORK+1                                                
         MVI   MID2+29,C':'                                                     
         MVC   MID2+30(2),WORK+3                                                
         MVI   MID2+32,C':'                                                     
         MVC   MID2+33(2),WORK+5                                                
         MVC   MID2+36(2),=C'BY'                                                
         MVC   MID2+39(8),LINKUSER                                              
*                                                                               
PRTHEAD  DS    0H                                                               
         LA    RF,OUTOLD                                                        
         LHI   R0,CMPRECLN                                                      
         STH   R0,DCBLRECL-IHADCB(RF)                SOFT LRECL                 
         MHI   R0,100                                                           
         STH   R0,DCBBLKSI-IHADCB(RF)                SOFT BLKSIZE               
*                                                                               
         OPEN  (OUTOLD,OUTPUT)                                                  
         ICM   R4,15,ALVLTAB                                                    
*                                                                               
         MVC   MID2+48(11),=C'ZAP COUNT: '                                      
         EDIT  ZAPCOUNT,(2,MID2+59),ZERO=NOBLANK,ALIGN=LEFT                     
         IF (CP,ZAPCOUNT,NE,=P'0') IF THE ZAP COUNT ISN'T ZERO:                 
           MVC   MID2+62(2),=C'ON'                                              
           MVC   WORK(4),ZAPDATE     X'0CYYDDDF'                                
           GOTO1 =V(DATCON),DMCB,(6,WORK),(23,MID2+65)                          
         ENDIF ,                                                                
         DROP  R2                                                               
*                                                                               
         CLI   MODE,LINKDATE       IF WE GOT HERE VIA RVP, WE DON'T...          
         BNE   LOOP5               ...HAVE THE LEVEL STAMPS, SO JUST...         
         GOTO1 =V(PRINTER)         ...PRINT THE LINKDATE AND ZAP COUNT          
         B     CLOSEMOD                                                         
*                                                                               
LOOP5    DS    0H                                                               
         GOTO1 =V(HEXOUT),DMCB,PTLOC,PLOC,L'PTLOC                               
         MVC   PNAM,PTNAM                                                       
         GOTO1 =V(HEXOUT),DMCB,PTLEN,PLEN,L'PTLEN                               
         MVC   PBKNAM,PTBKNAM                                                   
         MVC   PBKLEV#,PTBKLEV#                                                 
         MVC   PBKDAT,PTBKDATE                                                  
         MVC   PBKTIME,PTBKTIME                                                 
         MVC   PCOMPBY,PTCOMPBY    PRINT USERID, BUT DON'T PCOMPARE IT          
         MVC   PBRELO,PTBKRELO     PRINT THE RELO NAME, BUT DON'T               
*                                   PCOMPARE ON IT (BECAUSE OLD RELOS           
*                                   WON'T HAVE THE NAME EMBEDDED IN             
*                                   THE LEVEL STAMP)                            
         PUT   OUTOLD,P                                                         
*                                                                               
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         AHI   R4,PTQ                                                           
         CLC   0(2,R4),=X'FFFF'                                                 
         BNE   LOOP5                                                            
*                                                                               
         OPEN  (PCMPCRDS,OUTPUT)   CREATE PCOMPARE CONTROL CARDS                
         MVC   CARD,SPACES                                                      
         MVC   CARD(18),=C'COMPARE OUTPUT(NO)'                                  
         PUT   PCMPCRDS,CARD                                                    
         MVC   CARD,SPACES                                                      
         MVC   CARD(36),=C'OLD FILEORG(SEQUENTIAL) COL(SSS EEE)'                
         LA    R0,PCOMPARS-P+1     START PCOMPARE COLUMN NUMBER                 
         EDIT  (R0),(3,CARD+28),FILL=0                                          
         LA    R0,PCOMPARX-P       END PCOMPARE COLUMN NUMBER                   
         EDIT  (R0),(3,CARD+32),FILL=0                                          
         PUT   PCMPCRDS,CARD                                                    
         MVC   CARD(3),=C'NEW'     'NEW' OPTIONS SAME AS 'OLD' OPTIONS          
         PUT   PCMPCRDS,CARD                                                    
         CLOSE (PCMPCRDS)                                                       
*                                                                               
CLOSEMOD DS    0H                                                               
         MVC   LINE,=PL2'75'                                                    
*                                                                               
         CLOSE OUTOLD                                                           
*                                                                               
NEXTMOD  DS    0H                                                               
         BCT   R3,NXTTAB                                                        
         DROP  R4                                                               
         EJECT                                                                  
         MVC   TITLE,SPACES        IN CASE THERE ARE ERRORS OR WARNINGS         
         MVC   TITLE(21),=C'ERROR/WARNING LISTING'                              
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   MID3,SPACES                                                      
         MVC   MID4,SPACES                                                      
*                                                                               
         LA    R3,OLD_IDR                                                       
OLD      USING IDRDATAD,R3                                                      
         LA    R4,NEW_IDR                                                       
NEW      USING IDRDATAD,R4                                                      
*                                                                               
         IF (CP,NEW.ZAPCOUNT,NE,=P'0'),AND,   IF "NEW" LOADMOD ZAPPED           
            (CLC,NEW.ZAPDATE,GE,NEW.BINDDATE) ... *AFTER* THE BIND:             
           MVI   RETCODE,8                      FATAL ERROR!                    
           MVC   P(43),=C'***ERROR*** THIS LOAD MODULE WAS ZAPPED ON '          
           MVC   WORK(4),NEW.ZAPDATE X'0CYYDDDF'                                
           GOTO1 =V(DATCON),DMCB,(6,WORK),(23,P+43)                             
           BAS   RE,PUTLINE                                                     
           GOTO1 =V(PRINTER)                                                    
           MVC   P+5(L'BOOKNAM2),BOOKNAM2                                       
           BAS   RE,PUTLINE                                                     
           GOTO1 =V(PRINTER)                                                    
         ENDIF ,                                                                
*                                                                               
         CP    OLD.BINDDATE,NEW.BINDDATE   COMPARE LINK DATES                   
         BL    CHKUSER             NEW LINK DATE SUBSEQUENT TO OLD: OK          
         BH    *+14                NEW LINK DATE *PRIOR* TO OLD: BAD            
         CP    OLD.BINDTIME,NEW.BINDTIME   SAME DAY: COMPARE THE TIMES          
         BNH   CHKUSER                                                          
*                                                                               
         MVI   RETCODE,8           FATAL ERROR                                  
         MVC   P(74),=C'***ERROR*** THIS LOAD MODULE LINKED *PRIOR* TO +        
               ITS PRODUCTION LOAD MODULE:'                                     
         BAS   RE,PUTLINE                                                       
         GOTO1 =V(PRINTER)                                                      
         MVC   P+5(L'BOOKNAM2),BOOKNAM2                                         
         BAS   RE,PUTLINE                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
CHKUSER  DS    0H                                                               
         CLI   TESTLM,C'Y'         STAGED LOAD MODULE EXISTS?                   
         BNE   EXIT                NO                                           
         CLI   MODE,LINKDATE       RVP ACTION?                                  
         BNE   EXIT                                                             
         CLC   PAPTLEVL,=C'STGE'   YES: GOING TO STGE LEVEL?                    
         BNE   EXIT                                                             
         CLI   RESTAGE,C'Y'        RESTAGE PERMITTED?                           
         BE    EXIT                YES                                          
*                                                                               
         MVI   RETCODE,8           FATAL ERROR                                  
         LA    R3,OST_IDR                                                       
         CLC   OLD.LINKUSER,SPACES IS STAGED "A" VERSION LINKER KNOWN?          
         BE    STGEERR             NO                                           
         CLC   RACFUSER,OLD.LINKUSER  LINKER UID MATCHES RVP SUBMITTER?         
         BE    STGEERR                                                          
*                                                                               
         MVC   P(52),=C'**WARNING** THIS LOAD MODULE WAS LINKED BY XXXX+        
               XXXX:'                                                           
         MVC   P+43(8),OLD.LINKUSER  LINKER'S USERID                            
         BAS   RE,PUTLINE                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
STGEERR  DS    0H                                                               
         MVC   P(49),=C'***ERROR*** THIS LOAD MODULE IS CURRENTLY STAGE+        
               D:'                                                              
         BAS   RE,PUTLINE                                                       
         GOTO1 =V(PRINTER)                                                      
         MVC   P+5(L'BOOKNAM3),BOOKNAM3                                         
         BAS   RE,PUTLINE                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(69),=C'TO OVERWRITE A STAGED LOAD MODULE, RESUBMIT THE+        
                RVP WITH RESTAGE=YES.'                                          
         BAS   RE,PUTLINE                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         DROP  OLD,NEW                                                          
*                                                                               
EXIT     DS    0H                                                               
         CLI   RETCODE,0           ANY ERRORS FOUND?                            
         BE    XBASE               NO                                           
         CLI   MODE,LINKDATE       YES: IS THIS AN RVP?                         
         BNE   WARNONLY            NO                                           
         CLI   MSGFLFLG,C'Y'       IS MSGFILE ALLOCATED?                        
         BNE   XBASE               NO                                           
         CLOSE MSGFILE                                                          
         B     XBASE               LEAVE RETURN CODE AS AN 8                    
WARNONLY DS    0H                                                               
         MVI   RETCODE,4           DOWNGRADE SEVERITY TO 4                      
*                                                                               
XBASE    DS    0H                                                               
         XBASE RC=RETCODE,RL=1                                                  
         EJECT                                                                  
GETDSNS  NTR1                                                                   
*                                                                               
*GET THE DSNAME AND MEMBER NAME                                                 
*                                                                               
         LA    R0,DCBDDNAM-IHADCB(,R8)     R8 = A(DCB)                          
*                                                                               
         GOTO1 =V(DDINFO),DMCB,(8,(R0)),TUNITDSN,0                              
         LTR   RF,RF                                                            
         BNZ   NO                                                               
         L     RE,DMCB+8                                                        
         MVC   0(L'BOOKNAM1,R2),0(RE)  R2 = A(RETURNED PANLIB DSN)              
*                                                                               
         GOTO1 =V(DDINFO),DMCB,(8,(R0)),TUNITMEM,0                              
         LTR   RF,RF                                                            
         BNZ   NO                                                               
         L     RE,DMCB+8                                                        
         MVC   0(L'MEMNAME1,R9),0(RE)  R9 = A(RETURNED PAN MEMBERNAME)          
*                                                                               
         LA    RE,L'BOOKNAM1-1(,R2)                                             
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         LA    RE,1(RE)                                                         
         MVI   0(RE),C'('                                                       
         MVC   1(L'MEMNAME1,RE),0(R9)                                           
         AHI   RE,L'MEMNAME1                                                    
         CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
         MVI   1(RE),C')'                                                       
         B     YES                 R2 = A(FULL DSN WITH MEMBERNAME)             
*                                                                               
         ANSR                                                                   
         EJECT                                                                  
PUTLINE  NTR1                                                                   
*                                                                               
         CLI   MODE,LINKDATE       IS THIS AN RVP?                              
         BNE   PUTLINEX            NO: DON'T BOTHER WITH MESSAGE FILE           
         CLI   MSGFLFLG,C'Y'       IS MSGFILE ALLOCATED?                        
         BNE   PUTLINEX            NO                                           
         PUT   MSGFILE,P                                                        
*                                                                               
PUTLINEX DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
ALOADMOD DS    A                                                                
ALOADMODX DS   A                                                                
DMCB     DS    8F                                                               
APHASTAB DC    A(0)                A(CURRENT PHASE TABLE)                       
RACFUSER DS    CL8                 SUBMITTER'S RACF USERID                      
BASELM   DC    C'Y'                LOADLIB BASENAME LOADMOD EXISTS?             
TESTLM   DC    C'Y'                LOADLIB TESTNAME LOADMOD EXISTS?             
RESTAGE  DC    C'N'                STAGED LOAD MODULE PERMITTED?                
PAPTLEVL DC    CL4' '              PANAPT LEVEL                                 
MEMBRNAM DC    CL10' '             PANVALET MEMBER NAME                         
MSGFLFLG DC    C'N'                'Y': MSGFILE DD IS ALLOCATED                 
THREE    DS    CL3                                                              
BYTE     DS    X                                                                
RETCODE  DC    X'0'                RETURN CODE TO Z/OS                          
WORK     DS    CL80                                                             
CARD     DS    CL80                                                             
*                                                                               
TUNITDSN DC    AL2(DINRTDSN)   TEXT UNIT FOR GETTING PDS LIBRARY NAME           
TUNITMEM DC    AL2(DINRTMEM)   TEXT UNIT FOR GETTING PDS MEMBER NAME            
MEMNAME1 DS    CL10                                                             
MEMNAME2 DS    CL10                                                             
BOOKNAM1 DS    CL44                                                             
BOOKNAM2 DS    CL44                                                             
BOOKNAM3 DS    CL44                                                             
*                                                                               
*                                  PANVALET LEVEL STAMP                         
AUDBK    DC    C'BOOK='                                                         
         DS    CL10                                                             
         DS    C                   VERY OLD LEVEL STAMPS WON'T HAVE...          
*                                  ...A BLANK JUST BEFORE 'LEVEL=' !            
AUDLV    DC    C'LEVEL='                                                        
         DS    CL3                                                              
AUDDT    DC    C' DATE='                                                        
         DS    CL8                                                              
AUDTM    DC    C' TIME='                                                        
         DS    CL8                                                              
*                                                                               
MODE     DS    C                   PROGRAM FUNCTION                             
LINKDATE EQU   C'L'                 PERFORM RVP VERIFICATION CHECKS             
OBJLEVEL EQU   C'O'                 COMPARE OBJECT MODULE LEVEL STAMPS          
*                                                                               
MIDHEAD1 DC    CL132'LOCATN  BOOKNAME    LVL    DATE      TIME     CSEC+        
               T     LEN    RELONAME    COMP. BY'                               
MIDHEAD2 DC    CL132'------  --------    ---  --------  --------  -----+        
               ---  ------  --------    --------'                               
*                                                                               
         DS    0D                                                               
         DC    C'**DCBS**'                                                      
OLDLMMOD DCB   DDNAME=OLDLMMOD,DSORG=PS,MACRF=GL,EODAD=CLOSELM                  
NEWLMMOD DCB   DDNAME=NEWLMMOD,DSORG=PS,MACRF=GL,EODAD=CLOSELM                  
OLDSTMOD DCB   DDNAME=OLDSTMOD,DSORG=PS,MACRF=GL,EODAD=CLOSELM                  
*                                                                               
OUTOLD   DCB   DDNAME=OUTOLD,DSORG=PS,MACRF=PM,RECFM=FB                         
DCBLQ    EQU   *-OUTOLD                                                         
OUTNEW   DCB   DDNAME=OUTNEW,DSORG=PS,MACRF=PM,RECFM=FB                         
*                                                                               
MSGFILE  DCB   DDNAME=MSGFILE,DSORG=PS,MACRF=PM,LRECL=80,RECFM=FB               
PCMPCRDS DCB   DDNAME=PCMPCRDS,DSORG=PS,MACRF=PM,LRECL=80,RECFM=FB              
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(LMCOMP),V(DUMMY)                                               
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    C'*IDRTAB*'                                                      
IDRTABLE DS    0X                                                               
*                                  "OLD" LOADMOD IDR DATA                       
OLD_IDR  DC    4PL4'0',AL4(PHASETAB),CL8' '                                     
*                                  "NEW" LOADMOD IDR DATA                       
NEW_IDR  DC    2PL4'0',PL4'9999999',PL4'0',AL4(PHASETB2),CL8' '                 
*                                  "OLD" STAGED LOADMOD IDR DATA                
OST_IDR  DC    4PL4'0',AL4(0),CL8' '                                            
         DC    X'FF'               EOT                                          
*                                                                               
MONTAB   DS    0CL5                                                             
         DC    CL3'JAN',CL2'01'                                                 
         DC    CL3'FEB',CL2'02'                                                 
         DC    CL3'MAR',CL2'03'                                                 
         DC    CL3'APR',CL2'04'                                                 
         DC    CL3'MAY',CL2'05'                                                 
         DC    CL3'JUN',CL2'06'                                                 
         DC    CL3'JUL',CL2'07'                                                 
         DC    CL3'AUG',CL2'08'                                                 
         DC    CL3'SEP',CL2'09'                                                 
         DC    CL3'OCT',CL2'10'                                                 
         DC    CL3'NOV',CL2'11'                                                 
         DC    CL3'DEC',CL2'12'                                                 
         DC    X'FF'                                                            
*                                                                               
         DS    0D                                                               
         DC    C'PHASETAB'                                                      
PHASETAB DS    1000XL(PTQ)         "OLD" LOADMOD LEVEL STAMPS                   
*                                                                               
         DS    0D                                                               
         DC    C'PHASETB2'                                                      
PHASETB2 DS    1000XL(PTQ)         "NEW" LOADMOD LEVEL STAMPS                   
         EJECT                                                                  
***********************************************************************         
* LINKED MODULES COMPARING WITH                                                 
***********************************************************************         
IDRDATAD DSECT ,                   DATA EXTRACTED FROM IDR RECORDS              
ZAPCOUNT DS    PL4                 ZAP COUNT                                    
ZAPDATE  DS    PL4                 ZAP DATE                                     
BINDDATE DS    PL4                 LAST LINKED DATE                             
BINDTIME DS    PL4                 LAST LINKED TIME                             
ALVLTAB  DS    AL4                 A(LEVEL STAMP TABLE)                         
LINKUSER DS    CL8                 USERID OF PROGRAMMER WHO LINKED THIS         
IDRDATAQ EQU   *-IDRDATAD                                                       
         SPACE 3                                                                
**********************************************************************          
* PHASE TABLE ENTRY                                                             
**********************************************************************          
PTD      DSECT                                                                  
PTLOC    DS    XL3                                                              
PTNAM    DS    CL8                                                              
PTLEN    DS    XL3                                                              
PTTYP    DS    X                   ONLY THE 2ND NIBBLE IS USED                  
SD       EQU   X'00'                                                            
PC       EQU   X'04'                                                            
CM       EQU   X'05'                                                            
PR       EQU   X'06'                                                            
PTBKNAM  DS    CL10                                                             
PTBKLEV# DS    CL3                                                              
*                                                                               
PTBKDATE DS    0CL8                MM/DD/YY IN US, DD/MM/YY IN UK               
*&&US                                                                           
PTBKDATM DS    CL2                 MM                                           
*&&                                                                             
*&&UK                                                                           
PTBKDATD DS    CL2                 DD                                           
*&&                                                                             
PTBKSLS1 DS    C                   C'/'                                         
*&&US                                                                           
PTBKDATD DS    CL2                 DD                                           
*&&                                                                             
*&&UK                                                                           
PTBKDATM DS    CL2                 MM                                           
*&&                                                                             
PTBKSLS2 DS    C                   C'/'                                         
PTBKDATY DS    CL2                 YY                                           
*                                                                               
PTBKTIME DS    CL8                                                              
PTBKRELO DS    CL10                RELO NAME ("RM" BOOK)                        
PTCOMPBY DS    CL8                                                              
*                                                                               
PTQ      EQU   *-PTD                                                            
         EJECT                                                                  
* INCLUDE DDDPRINT                                                              
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         ORG   P                                                                
PLOC     DS    CL6                                                              
         DS    2C                                                               
PCOMPARS EQU   *                                                                
PBKNAM   DS    CL10                                                             
         DS    2C                                                               
PBKLEV#  DS    CL3                                                              
         DS    2C                                                               
PBKDAT   DS    CL8                                                              
         DS    2C                                                               
PBKTIME  DS    CL8                                                              
         DS    2C                                                               
PNAM     DS    CL8                                                              
         DS    2C                                                               
PLEN     DS    CL6                                                              
PCOMPARX EQU   *                                                                
         DS    2C                                                               
PBRELO   DS    CL10                                                             
         DS    2C                                                               
PCOMPBY  DS    CL8                                                              
CMPRECLN EQU   *-PLOC              L'ENTIRE RECORD                              
         ORG                                                                    
         PRINT OFF                                                              
         DCBD  DSORG=PS,DEVD=DA                                                 
         IHAPSA                                                                 
         IHAACEE                                                                
         IHAASCB                                                                
         IHAASXB                                                                
         IEFZB4D2                                                               
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020DDLMCOMP  10/02/19'                                      
         END                                                                    
