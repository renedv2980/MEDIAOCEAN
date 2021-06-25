*          DATA SET UNLDEXO    AT LEVEL 024 AS OF 05/01/02                      
*PHASE UNLDEXOA                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE RECUP                                                                  
         TITLE 'UNLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
UNLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,*UNLDEXT,RR=R5                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
UNXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         ST    R5,RELO                                                          
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,VPRNTBL                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BNE   UNX01                                                            
         GOTO1 VPRINTER            INITIALIZE                                   
         B     UNXIT                                                            
UNX01    CLI   PLIST,X'01'                                                      
         BE    UNXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    UNXEOF              END-OF-FILE                                  
         B     UNXIT                                                            
         SPACE 2                                                                
UNXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     UNXIT                                                            
         SPACE 2                                                                
UNXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     UNXIT                                                            
         SPACE 2                                                                
UNXIT    XMOD1 1                                                                
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                                          
*                                                                 *             
*******************************************************************             
         SPACE 1                                                                
UNXREC   DS    0H                                                               
*                                                                               
         L     R3,AREC                                                          
         CLI   0(R3),X'04'         UNITS                                        
         BNE   UNXPURGE                                                         
         USING NURECD,R3                                                        
         CLI   NUKAM,X'63'         OMNY ONLY                                    
         BNE   UNXPURGE                                                         
         CLC   =X'B66A',NUKCLT     CLIENT = NTK ONLY                            
         BNE   UNXPURGE                                                         
* ESTIMATE 202                                                                  
         CLI   NUKEST,202          ESTIMATE                                     
         BNE   UNX20                                                            
         CLC   =C'LEA',NUKNET       NETWORK                                     
         BNE   UNXPURGE                                                         
         CLC   NUKDATE,=X'BC99'                                                 
         BL    UNXPURGE                                                         
         CLC   NUKDATE,=X'BCBD'                                                 
         BH    UNXPURGE                                                         
         MVI   ELCODE,1                                                         
         L     R3,AREC                                                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,R3                                                      
         CLI   NUPRD,X'05'                                                      
         BNE   UNXPURGE                                                         
         B     UNX40                                                            
*                                                                               
* - ESTIMATE 201                                                                
UNX20    DS     0H                                                              
         L     R3,AREC                                                          
         USING NURECD,R3                                                        
         CLI   NUKEST,201                                                       
         BNE   UNX30                                                            
         CLC   =C'ANE',NUKNET       ANE                                         
         BNE   UNX22                                                            
UNX21    CLC   NUKDATE,=X'BC5C'                                                 
         BL    UNXPURGE                                                         
         CLC   NUKDATE,=X'BC98'                                                 
         BH    UNXPURGE                                                         
         B     UNX20X                                                           
UNX22    DS    0H                                                               
         CLC   =C'TNN',NUKNET                                                   
         BNE   UNX24                                                            
         CLC   NUKDATE,=X'BC99'                                                 
         BL    UNXPURGE                                                         
         CLC   NUKDATE,=X'BCBD'                                                 
         BH    UNXPURGE                                                         
*                                                                               
         CLC   NUKDATE,=X'BCA2'                                                 
         BNE   UNX23                                                            
         CLI   NUKSUB,3                                                         
         BNE   UNX20X                                                           
         MVI   NUKSUB,11                                                        
         B     UNX20X                                                           
UNX23    CLC   NUKDATE,=X'BCB7'                                                 
         BNE   UNX20X                                                           
         CLI   NUKSUB,5                                                         
         BNE   UNX20X                                                           
         MVI   NUKSUB,11                                                        
         B     UNX20X                                                           
UNX24    DS    0H                                                               
         CLC   =C'TWC',NUKNET                                                   
         BNE   UNX26                                                            
         CLC   NUKDATE,=X'BC84'    APR4                                         
         BNE   UNX24B                                                           
         CLI   NUKSUB,18                                                        
         BNL   UNX24B                                                           
         ZIC   R1,NUKSUB                                                        
         A     R1,=F'85'                                                        
         STC   R1,NUKSUB                                                        
UNX24B   CLC   NUKDATE,=X'BC7C'                                                 
         BNE   UNX25                                                            
         CLI   NUKSUB,52                                                        
         BH    UNX25                                                            
         ZIC   R1,NUKSUB                                                        
         A     R1,=F'85'                                                        
         STC   R1,NUKSUB                                                        
UNX25    CLC   NUKDATE,=X'BC7C'                                                 
         BL    UNXPURGE                                                         
         CLC   NUKDATE,=X'BC98'                                                 
         BH    UNXPURGE                                                         
         B     UNX20X                                                           
*                                                                               
UNX26    DS    0H                                                               
         CLC   =C'LEA',NUKNET                                                   
         BNE   UNXPURGE                                                         
         CLC   NUKDATE,=X'BC99'                                                 
         BL    UNXPURGE                                                         
         CLC   NUKDATE,=X'BCBD'                                                 
         BH    UNXPURGE                                                         
UNX20X   DS    0H                                                               
         L     R3,AREC                                                          
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,R3                                                      
         CLI   NUPRD,X'06'                                                      
         BNE   UNXPURGE                                                         
         B     UNX40                                                            
*                                                                               
* - ESTIMATE 203                                                                
UNX30    DS    0H                                                               
         L     R3,AREC                                                          
         USING NURECD,R3                                                        
         CLI   NUKEST,203                                                       
         BNE   UNXPURGE                                                         
         CLC   =C'CTV',NUKNET                                                   
         BNE   UNX32                                                            
         CLC   NUKDATE,=X'BC7C'                                                 
         BL    UNXPURGE                                                         
         CLC   NUKDATE,=X'BC98'                                                 
         BH    UNXPURGE                                                         
         B     UNX30X                                                           
UNX32    DS    0H                                                               
         CLC   =C'LEA',NUKNET                                                   
         BNE   UNXPURGE                                                         
         CLC   NUKDATE,=X'BC99'                                                 
         BL    UNXPURGE                                                         
         CLC   NUKDATE,=X'BCBD'                                                 
         BH    UNXPURGE                                                         
UNX30X   DS    0H                                                               
         L     R3,AREC                                                          
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,R3                                                      
         CLI   NUPRD,X'08'                                                      
         BNE   UNXPURGE                                                         
         B     UNX40                                                            
UNX40    DS    0H                                                               
         L     R3,AREC                                                          
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,R3                                                      
         CLC   NUAFFTIM,=X'00000000'    NO AFFID TIME                           
         BNE   UNXPURGE                                                         
         L     R3,AREC                                                          
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUSDRD,R3                                                        
         CLC   NUSDAFDT,=X'00000000'     NO AFFID DATE                          
         BNE   UNXPURGE                                                         
*                                                                               
         MVI   ELCODE,X'10'        MUST HAVE BILL ELEMENT                       
         L     R3,AREC                                                          
         BAS   RE,GETEL                                                         
         BNE   UNXPURGE                                                         
*                                                                               
         L     R3,AREC                                                          
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,R3                                                      
         OI    NUUNITST,X'40'      PRE-EMTP UNIT                                
*                                                                               
UNXX     L     R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
*                                                                               
         L     R3,AREC                                                          
         USING NURECD,R3                                                        
         EDIT  (B1,NUKEST),(3,P+1)                                              
         MVC   P+5(4),NUKNET                                                    
         MVC   P+10(6),NUKPROG                                                  
         GOTO1 =V(DATCON),DMCB,(2,NUKDATE),(7,P+18)                             
         MVI   P+25,C'-'                                                        
         EDIT  (B1,NUKSUB),(3,P+26),ALIGN=LEFT                                  
         GOTO1 VPRINTER                                                         
         GOTO1 VPRNTBL,DMCB,,AREC,C'DUMP',40,=C'1D'                             
*                                                                               
UNXXX    B     UNXKEEP                                                          
*                                                                               
         EJECT                                                                  
* - BYTE CONTAINS PRODUCT NUMBER                                                
NEWPRD   NTR1                                                                   
         CLI   BYTE,0                                                           
         BE    NEWX                                                             
         LA    R3,TINLIST          OLD LIST                                     
NEW10    CLC   BYTE,3(R3)                                                       
         BE    NEW20               GOT PRODCUT                                  
         LA    R3,4(R3)                                                         
         CLI   0(R3),0                                                          
         BNE   NEW10                                                            
         DC    H'0'                CAN'T FIND PRODUCT / DIE                     
*                                                                               
NEW20    LA    R4,TYCLIST          NEW LIST                                     
NEW25    CLC   0(3,R3),0(R4)                                                    
         BE    NEW40                                                            
         LA    R4,4(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   NEW25                                                            
         DC    H'0'                CAN'T MATCH OLD/NEW PRODUCT / DIE            
NEW40    MVC   BYTE,3(R4)                                                       
NEWX     XIT1                                                                   
         EJECT                                                                  
TINLIST  DS    0H                                                               
         DC    X'C2C1C248C2C7E637C2C84024C2C9E301C2D3D68D'                      
         DC    X'C2D4C663C2D7D40AC2D7E4C8C2E3E3C2C3C1C486'                      
         DC    X'C3C2E22CC3C3C32EC3C3D76FC3C3E6AAC3C44002'                      
         DC    X'C3C6C32DC3C6D35EC3C6D46BC3C6D643C3D1E24E'                      
         DC    X'C3D4C44FC3D7C32FC3D7D460C3D9C6D9C3D9E3CD'                      
         DC    X'C3D9E4B0C3D9E64DC3E3D480C3E4E908C3E6C6DA'                      
         DC    X'C3E6D29CC3E6E3CEC4C4C684C4C4E585C4C6C2CF'                      
         DC    X'C4C6D3C7C4C6D493C4C6D7D4C4D3C40DC4D9C4B6'                      
         DC    X'C4D9C6B7C4E5D3BFC4E5D7D3C4E5E3BEC5C3D15C'                      
         DC    X'C5C6D469C5D2D26EC5D3D429C6D3C6B1C6D44032'                      
         DC    X'C6D4C389C6D4C86CC6D4D18BC6D4D299C6D7E473'                      
         DC    X'C6E2D90EC6E3400BC7C7C80FC7C9C73CC7D7E454'                      
         DC    X'C7D9E9AFC8C1C28FC8C1C395C8C1D410C8C2D242'                      
         DC    X'C8C3E650C8C6D494C8C7C979C8C8C411C8D1D238'                      
         DC    X'C8D4C355C8D4C453C8D4C664C8D4D651C8D7E456'                      
         DC    X'C8D9D9B9C8E2C390C8E2C69AC9C3C645C9C3D412'                      
         DC    X'D1C34022D1C7D287D2C2C761D2C5D27AD2C5D513'                      
         DC    X'D2C5D914D2C7C971D2C9C1BBD2D2D239D2D4C662'                      
         DC    X'D2D5C1BCD2D7E45AD2E6C3A4D3C2C778D3C8D9A6'                      
         DC    X'D3D1C7C5D3D2C3C6D3D2C559D3D3E947D3D4C372'                      
         DC    X'D3D5C2ADD3D5D7B4D3D7E465D4C1C705D4C2C206'                      
         DC    X'D4C2C774D4C2D709D4C3D770D4C4C49ED4C4D24B'                      
         DC    X'D4C4D75BD4C5C1A3D4C5D903D4C6C24AD4C6C73A'                      
         DC    X'D4C6D496D4C7C304D4C7C69FD4C7C957D4C7D6A0'                      
         DC    X'D4C8C23BD4D1D298D4D44015D4D4C452D4D6E32B'                      
         DC    X'D4D7E458D4D94030D4E2C246D4E2C3A1D4E2E6A8'                      
         DC    X'D4E3C34CD4E3D45FD4E3D9A2D4E3E4A9D5C2D216'                      
         DC    X'D5C2D58ED6C3D167D6D44036D6D4C87DD6E4E323'                      
         DC    X'D7C3C82AD7D2C3CBD7D3D1CAD7D3E3C9D7D44035'                      
         DC    X'D7D94034D7E3C125D7E3C88CD7E3E817D7E4D733'                      
         DC    X'D7E8D7A7D9C2C118D9C4C707D9D3C219D9D9C6D6'                      
         DC    X'D9D9D1D1D9D9D2D2D9E2C2CCD9E2C883D9E2D1D7'                      
         DC    X'D9E2D2D8E2C1C291E2C2C41AE2C2E3B3E2C3C2BD'                      
         DC    X'E2C3C83FE2C3E697E2C4C41BE2C4C69BE2C4E59D'                      
         DC    X'E2C6D468E2C8D91CE2D2D275E2D3D35DE2D4C47F'                      
         DC    X'E2D4C87BE2D4E4B2E2D5C726E2D7D3D0E2E2C28A'                      
         DC    X'E2E2C392E2E2C766E2E2D749E2E2D941E2E2E476'                      
         DC    X'E2E3C2B8E2E3C481E2E3D421E2E3D7C1E2E6D7BA'                      
         DC    X'E3C3C33DE3C3D91DE3C8C2B5E3D1C7C3E3D2C3C4'                      
         DC    X'E3D2C43EE3D4C744E3D4D477E3D6D788E3D6E21E'                      
         DC    X'E3D7E6ABE3D9C127E3D9E631E3E2D282E3E2D3C0'                      
         DC    X'E3E3D3A5E3E6C6D5E3E8D70CE4C3C840E4C3D76A'                      
         DC    X'E4D2D26DE4D4C87CE4D6E47EE5C9D7ACE5D44020'                      
         DC    X'E6D1401FE6E34028E7E3D9AED7D6D3FF00000000'                      
         EJECT                                                                  
TYCLIST  DS    0H                                                               
         DC    X'C2D1D263C2D2C362C2D7E40CC2E3D945C2E3E306'                      
         DC    X'C3C3E625C3D9C64BC3D9E256C3D9E311C3D9E427'                      
         DC    X'C3D9E626C3E6C64CC3E6D77EC3E6E257C3E6E312'                      
         DC    X'C4C4C62AC4C4D932C4C4E57FC4C6C213C4C6D30B'                      
         DC    X'C4C6D418C4C6D71AC4D6C679C4D6E529C4D9C480'                      
         DC    X'C4D9C681C4E5D303C4E5D719C4E5E302C6C6D477'                      
         DC    X'C6D3C62DC6D3D375C6D3D552C6D4C26BC6D4D134'                      
         DC    X'C6D4D764C7D9E72BC8D9D924D1C7D23BD1C7E26F'                      
         DC    X'D2C5D53FD2C5D74FD2C5D930D2C9C140D2D5C15C'                      
         DC    X'D2E6C33CD2E6E26ED3C2E35BD3C8D941D3D1C709'                      
         DC    X'D3D2C30AD3D3D75ED3D3E278D3D3E92FD3D5D742'                      
         DC    X'D4C4C437D4C4C949D4C6D34AD4C7C646D4D5D447'                      
         DC    X'D4E3C248D5C2C55AD5C2D535D7C6D469D7D2C30F'                      
         DC    X'D7D2D776D7D3D10ED7D3D23DD7D3E30DD7E2E36A'                      
         DC    X'D7E3C820D7E3D673D7E3D715D7E5C971D7E8C651'                      
         DC    X'D7E8D550D7E8D73AD7E8E255D9C6D35FD9D3D37C'                      
         DC    X'D9D7D37BD9D9C61CD9D9D116D9D9D217D9D9D74E'                      
         DC    X'D9E2C210D9E2C66DD9E2C828D9E2D11DD9E2D21E'                      
         DC    X'D9E2D47AD9E2D560D9E2D758E2C3C201E2C3C66C'                      
         DC    X'E2C3C81FE2C6D44DE2D1C767E2D2E668E2D4E431'                      
         DC    X'E2D7D259E2D7D314E2E2D733E2E3C236E2E3D705'                      
         DC    X'E2E6C843E2E6D744E3C1402EE3D1C707E3D2C308'                      
         DC    X'E3D2C439E3D6D738E3D7E621E3E2C654E3E2D22C'                      
         DC    X'E3E2D304E3E2D553E3E3D761E3E6C274E3E6C61B'                      
         DC    X'E3E6D75DE5C9D722E5C9E372E5D4403EE5D7E47D'                      
         DC    X'E5E2E370E7D1C765E7D2E666E7E3D923D7D6D3FF'                      
         DC    X'D4C7D682D4E3D983000000000000000000000000'                      
         EJECT                                                                  
*                                                                               
*                                                                               
* END-OF-FILE PROCESSING                                                        
*                                                                               
UNXEOF   DS    0H                                                               
*                                                                               
         MVI   SPACING+3,C'2'      PRINT A HEADLINE FOR TOTALS                  
         GOTO1 VPRINTER                                                         
         MVI   SPACING+3,C'1'                                                   
         MVC   P+10(26),=C'SUMMARY OF RECORDS CHANGED'                          
         L     R4,COUNT                                                         
         EDIT  (R4),(10,P+37)                                                   
         GOTO1 VPRINTER                                                         
         B     UNXIT                                                            
*                                                                               
         PRINT GEN                                                              
         GETEL R3,27,ELCODE                                                     
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         DS    0F                                                               
COUNT    DC    F'0'                                                             
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
RELO     DS    A                                                                
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         SPACE 1                                                                
VPRNTBL  DS    A                                                                
MFULL    DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    CL1                                                              
ELCODE   DS    CL1                                                              
NEW      DS    CL20                                                             
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE NEGENPACK                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPLAN                                                      
       ++INCLUDE SPTRNREV                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024UNLDEXO   05/01/02'                                      
         END                                                                    
